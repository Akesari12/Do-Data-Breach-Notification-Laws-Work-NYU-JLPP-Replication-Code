pacman::p_load(augsynth, # augmented synthetic control
               tidyverse, # for tidyverse 
               here, # for reproducibility
               ggthemes,
               anytime,
               choroplethr,
               choroplethrMaps,
               Hmisc,
               gridExtra,
               grid,
               cowplot,
               DiagrammeR,
               rsvg,
               DiagrammeRsvg,
               dagitty,
               ggdag)  

source(here('notebooks', 'ag_notice.R'))

bad_fits <- ppool_syn_summ$att %>%
  filter(Time >= -5 & Time < 0) %>%
  group_by(Level) %>%
  summarise(avg_imbalance = mean(Estimate)) %>%
  filter(abs(avg_imbalance) > 5) %>%
  filter(Level != 'Average') %>%
  pull(Level)


consumer_ag_notice <- consumer_ag_notice %>%
  filter(!state %in% c(bad_fits)) 

ppool_syn <- multisynth(id_theft_per_capita ~ ag_notice_required | 
                          percent_broadband +  
                          percent_over_60 + year_lag + two_year_lag + 
                          three_year_lag + four_year_lag,  
                        state, 
                        year, 
                        consumer_ag_notice,
                        fixedeff = T,
                        n_lags = 5,
                        n_leads = 3)         

ppool_syn_summ <- summary(ppool_syn)

## percent reduction

base_id_theft_rate <- consumer_ag_notice %>%
  filter(state %in% c(ppool_syn_summ$att$Level)) %>%
  filter(ag_notice_required == 1) %>%
  summarise(mean_post_treat = mean(id_theft_per_capita)) %>%
  pull(mean_post_treat)

est_id_theft_rate <- est_id_theft_rate <- ppool_syn_summ$att %>% 
  filter(Time >= 0) %>% 
  summarise(mean_att = mean(Estimate)) %>%
  pull(mean_att)

percent_reduction <- est_id_theft_rate/base_id_theft_rate*100

## barplot

good_fits_ag_notice_bar <- ppool_syn_summ$att %>%
  filter(Time > 0) %>%
  group_by(Level) %>%
  summarise(mean_estimate = mean(Estimate)) %>%
  mutate(direction = case_when(
    Level == 'Average' ~ 'average',
    mean_estimate >= 0 ~ 'increase',
    mean_estimate < 0 ~ 'decrease',
    TRUE ~ as.character(mean_estimate)
  )) %>%
  ggplot(aes(x = reorder(Level, -mean_estimate), 
             y = mean_estimate,
             fill = direction,
             color = direction)) +
  geom_bar(stat = 'identity') +
  scale_color_manual(values = c('green',
                                'blue',
                                'red')) +
  scale_fill_manual(values=c("green",
                             "blue",
                             'red')) +
  theme_classic() +
  ggtitle('ATTs for Good Fits for State Regulator Notification',
          subtitle = paste0('Average ATT ', 
                            round(percent_reduction, 2),
                            '%')) +
  xlab('State') +
  ylab('Mean ATT') +
  theme(legend.position = 'none') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(plot.title=element_text(hjust = .5, size = 20),
        plot.subtitle = element_text(hjust = 0.5,size = 15),
        axis.text = element_text(size = 20))

last_times <- ppool_syn_summ$att %>%
  filter(!is.na(Estimate),
         Time >= -ppool_syn_summ$n_lags,
         Time <= ppool_syn_summ$n_leads) %>%
  group_by(Level) %>%
  summarise(last_time = max(Time)) 

good_fits_ag_trends <- ppool_syn_summ$att %>%
  filter(Time >= -5) %>%
  filter(!Level %in% c('Nebraska', 'Arizona')) %>%
  inner_join(last_times) %>%
  mutate(label = ifelse(Time == last_time, Level, NA),
         is_avg = ifelse(("Average" %in% Level) * (Level == "Average"),
                         "A", "B")) %>%
  ggplot(aes(x = Time, y = Estimate, group = Level, color = is_avg, alpha = is_avg)) +
  scale_color_manual(values = c('B' = 'grey', 'A' = 'red')) +
  scale_alpha_manual(values=c(1, 0.5)) +
  geom_point() +
  geom_line() + 
  ggrepel::geom_label_repel(ggplot2::aes(label = label),
                            nudge_x = 1, na.rm = T) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0) + 
  theme_classic() +
  theme(axis.title = element_text(),
        legend.position = "bottom") +
  ggtitle('Trends for Good Fits for Notification to Regulators') +
  xlab('Time Before and After Enactment of Notification to Regulators') +
  ylab('Identity Theft Per 100K') +
  theme(legend.position = "none") +
  #facet_wrap(~Level) +
  theme(strip.text.x = element_text(size = 15)) + 
  theme(legend.text=element_text(size= 15)) +
  theme(axis.title = element_text(size = 20)) +
  theme(plot.title=element_text(hjust = .5, size = 20),
        axis.text = element_text(size = 20),
        axis.title.x = element_text(vjust = -.5))

