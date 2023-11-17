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

time_synth <- multisynth(id_theft_per_capita ~ ag_notice_required | 
                           percent_broadband +  
                           percent_over_60 + year_lag + two_year_lag + 
                           three_year_lag,  
                         state, 
                         year, 
                         consumer_ag_notice,
                         time_cohort = T, 
                         n_lags = 5,
                         n_leads = 5) 

summ_time_synth <- summary(time_synth)

## percent reduction


valid_states <- consumer_ag_notice %>%
  filter(ag_notice_required == 1) %>%
  pull(state)
  
base_id_theft_rate <- consumer_ag_notice %>%
  filter(state %in% c(valid_states)) %>%
  filter(ag_notice_required == 1) %>%
  summarise(mean_post_treat = mean(id_theft_per_capita)) %>%
  pull(mean_post_treat)

est_id_theft_rate <-  summ_time_synth$att %>% 
  drop_na() %>%
  filter(Time >= 0) %>% 
  summarise(mean_att = mean(Estimate)) %>%
  pull(mean_att)

percent_reduction <- est_id_theft_rate/base_id_theft_rate*100

## barplot

time_ag_notice_bar_effects <- summ_time_synth$att %>%
  filter(Time > 0) %>%
  filter(!Level %in% c(2017, 2018)) %>%
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
  ggtitle('Time Cohort ATTs for State Regulator Notification',
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


last_times <- summ_time_synth$att %>%
  filter(!is.na(Estimate),
         Time >= -ppool_syn_summ$n_lags,
         Time <= ppool_syn_summ$n_leads) %>%
  group_by(Level) %>%
  summarise(last_time = max(Time)) 

time_ag_notice_plot <- summ_time_synth$att %>%
  filter(Time >= -5) %>%
  filter(Level != 2017) %>%
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
  ggtitle('Time Cohorts \nfor Notification to Regulators') +
  xlab('Time Before and After Enactment of Notification to Regulators') +
  ylab('Identity Theft Per 100K') +
  theme(legend.position = "none") +
  theme(strip.text.x = element_text(size = 15)) + 
  theme(legend.text=element_text(size= 15)) +
  theme(axis.title = element_text(size = 20)) +
  theme(plot.title=element_text(hjust = .5, size = 20),
        axis.text = element_text(size = 20),
        axis.title.x = element_text(vjust = -.5))
