pacman::p_load(augsynth, # augmented synthetic control
               tidyverse, # for tidyverse 
               here, # for reproducibility
               ggthemes)  

# load panel data

consumer_sentinel <- read_csv(here('data', 'panel_data_ext.csv'))

consumer_sentinel <- consumer_sentinel %>%
  mutate(state = ifelse(state == 'Ilinois', 'Illinois', state),
         state = ifelse(state == 'Massachusets', 'Massachusetts', state),
         state = ifelse(state == 'Hawai', "Hawaii", state)) 

# join with online posting

timeline_notice <- read_csv(here('data', 'notification_days.csv'))

timeline_notice <- timeline_notice %>%
  rename(state = `Jurisdictions`,
         #citation = `Citation\n\n`,
         timeline = Consumer_notification_days,
         #harm = `Harm Required`,
         effective_date = `Consumer_notification_days_effective`) %>%
  select(state, timeline, effective_date) %>%
  mutate(effective_date = ifelse(str_detect(timeline, ".*[0-9].*"),
                                 effective_date,
                                 0)) %>%
  mutate(timeline = ifelse(str_detect(timeline, ".*[0-9].*"),
                                      1,
                                      0)) %>%
  mutate(across(everything(),
                ~ str_remove_all(., '\\n')))

# covariate data

covariate_table <- read_csv(here('data', 'internet_age_covariates.csv'))

consumer_timeline_notice <- consumer_sentinel %>%
  left_join(timeline_notice) %>%
  left_join(covariate_table) %>%
  drop_na() %>%
  filter(!state %in% c('Puerto Rico', 
                       'District of Columbia'))

# multisynth

consumer_timeline_notice <- consumer_timeline_notice %>%
  filter(!state %in% c('Alabama', 'Maine', 'Colorado', 'Arizona', 'Louisiana',
                       'South Dakota', 'Oregon', 'Delaware',
                       'Washington', 'New Mexico', 'Texas', 'Maryland', 'Rhode Island')) %>%
  #filter(state != 'California') %>%
  #filter(year < 2017) %>%
  #mutate(effective_date = ifelse(str_detect(harm, 'NO'),
  #                               effective_date,
  #                               0)) %>%
  mutate(effective_date = as.Date(effective_date, format = '%m/%d/%Y')) %>%
  mutate(effective_year = format(effective_date, format = '%Y')) %>%
  #filter(effective_year <= 2018) %>%
  mutate(effective_year = ifelse(is.na(effective_year),
                                 Inf, effective_year)) %>%
  mutate(timeline_notice_required = 1 * (year >= effective_year)) %>%
  mutate(log_id_theft_per_capita = log10(id_theft_per_capita)) %>%
  group_by(state) %>%
  mutate(year_lag = lag(id_theft_per_capita, order_by=year, n = 1)) %>%
  mutate(two_year_lag = lag(id_theft_per_capita, order_by=year, n = 2)) %>%
  mutate(three_year_lag = lag(id_theft_per_capita, order_by=year, n = 3)) %>%
  mutate(four_year_lag = lag(id_theft_per_capita, order_by=year, n = 4)) %>%
  mutate(five_year_lag = lag(id_theft_per_capita, order_by=year, n = 5)) %>%
  ungroup()

ppool_syn <- multisynth(id_theft_per_capita ~ timeline_notice_required | 
                          percent_broadband +  
                          percent_over_60 + year_lag + two_year_lag + 
                          three_year_lag,  
                        state, 
                        year, 
                        consumer_timeline_notice,
                        n_leads = 5)         

ppool_syn_summ <- summary(ppool_syn)

## percent reduction

base_id_theft_rate <- consumer_timeline_notice %>%
  filter(state %in% c(ppool_syn_summ$att$Level)) %>%
  filter(timeline == 1) %>%
  summarise(mean_post_treat = mean(id_theft_per_capita)) %>%
  pull(mean_post_treat)

est_id_theft_rate <- ppool_syn_summ$att %>% 
  filter(Time >= 0) %>% 
  summarise(mean_att = mean(Estimate)) %>%
  pull(mean_att)

percent_reduction <- est_id_theft_rate/base_id_theft_rate*100

## barplot

timeline_notice_bar_effects <- ppool_syn_summ$att %>%
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
  ggtitle('Average Treatment Effects by State for Time Limit on Consumer Notice',
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

## point/line plot
last_times <- ppool_syn_summ$att %>%
  filter(!is.na(Estimate),
         Time >= -ppool_syn_summ$n_lags,
         Time <= ppool_syn_summ$n_leads) %>%
  group_by(Level) %>%
  summarise(last_time = max(Time)) 

timeline_notice_plot <- ppool_syn_summ$att %>%
  filter(Time >= -5) %>%
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
  ggtitle('Staggered Synthetic Controls \nfor Time Limit for Notice to Consumers') +
  xlab('Time Before and After Enactment of Time Limit for Notice to Consumers') +
  ylab('Identity Theft Per 100K') +
  theme(legend.position = "none") +
  #facet_wrap(~Level) +
  theme(strip.text.x = element_text(size = 15)) + 
  theme(legend.text=element_text(size= 15)) +
  theme(axis.text = element_text(size = 8)) +
  theme(axis.title = element_text(size = 20)) +
  theme(plot.title=element_text(hjust = .5, size = 20),
        plot.subtitle = element_text(hjust = 0.5,size = 15),
        axis.text = element_text(size = 20))

## save plots
ggsave(filename = here('images', 'timeline_notice_bar_effects.png'),
       plot = timeline_notice_bar_effects,
       device = 'png',
       scale = 1,
       width = 4512,
       height = 2072,
       units = 'px')

ggsave(filename = here('images', 'timeline_notice.png'),
       plot = timeline_notice_plot,
       device = 'png',
       scale = 1,
       width = 4512,
       height = 2072,
       units = 'px')