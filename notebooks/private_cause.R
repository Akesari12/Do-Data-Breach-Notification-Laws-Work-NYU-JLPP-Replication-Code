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

private_cause <- read_csv(here('data', 'cause_of_action_effective_date.csv'))

private_cause <- private_cause %>%
  rename(state = `Jurisdictions\n`,
         citation = `Citation\n\n`, 
         online_posting = `Explicit Private Right of Action?\n\n`,
         effective_date = Effective_date) %>%
  mutate(across(everything(),
                ~ str_remove_all(., '\\n')))

# covariate data

covariate_table <- read_csv(here('data', 'internet_age_covariates.csv'))

consumer_private_cause <- consumer_sentinel %>%
  left_join(private_cause) %>%
  left_join(covariate_table) %>%
  drop_na() %>%
  filter(!state %in% c('Puerto Rico', 
                       'District of Columbia'))

# multisynth

consumer_private_cause <- consumer_private_cause %>%
  filter(state != 'California', 
         state != 'New Hampshire', 
         state != 'Tennessee') %>%
  #filter(year < 2017) %>%
  mutate(effective_date = as.Date(effective_date, format = '%m/%d/%Y')) %>%
  mutate(effective_year = format(effective_date, format = '%Y')) %>%
  mutate(effective_year = ifelse(is.na(effective_year),
                                 Inf, effective_year)) %>%
  mutate(private_cause_required = 1 * (year >= effective_year)) %>%
  mutate(log_id_theft_per_capita = log10(id_theft_per_capita)) %>%
  group_by(state) %>%
  mutate(year_lag = lag(id_theft_per_capita, order_by=year, n = 1)) %>%
  mutate(two_year_lag = lag(id_theft_per_capita, order_by=year, n = 2)) %>%
  mutate(three_year_lag = lag(id_theft_per_capita, order_by=year, n = 3)) %>%
  mutate(four_year_lag = lag(id_theft_per_capita, order_by=year, n = 4)) %>%
  mutate(five_year_lag = lag(id_theft_per_capita, order_by=year, n = 5)) %>%
  ungroup()

ppool_syn <- multisynth(id_theft_per_capita ~ private_cause_required | 
                        percent_broadband +  
                          percent_over_60 + year_lag + two_year_lag + 
                          three_year_lag + four_year_lag,  
                        state, 
                        year, 
                        consumer_private_cause,
                        fixedeff = T,
                        n_lags = 5,
                        n_leads = 5)         

ppool_syn_summ <- summary(ppool_syn)

## percent reduction

base_id_theft_rate <- consumer_private_cause %>%
  filter(state %in% c(ppool_syn_summ$att$Level)) %>%
  filter(private_cause_required == 1) %>%
  summarise(mean_post_treat = mean(id_theft_per_capita)) %>%
  pull(mean_post_treat)

est_id_theft_rate <- est_id_theft_rate <- ppool_syn_summ$att %>% 
  filter(Time >= 0) %>% 
  summarise(mean_att = mean(na.omit(Estimate))) %>%
  pull(mean_att)

percent_reduction <- est_id_theft_rate/base_id_theft_rate * 100

## barplot

private_cause_bar_effects <- ppool_syn_summ$att %>%
  filter(Time > 0) %>%
  group_by(Level) %>%
  summarise(mean_estimate = mean(na.omit(Estimate))) %>%
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
  scale_color_manual(values = c('green', 'blue', 'red')) +
  scale_fill_manual(values = c("green", "blue", 'red')) +
  theme_classic() +
  ggtitle('Average Treatment Effects by State for Private Cause of Action',
          subtitle = paste0('Average ATT ', 
                            round(percent_reduction, 2),
                            '%')) +
  xlab('State') +  # Label for x-axis
  ylab('Mean ATT') +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 20),  # Adjusting x-axis text
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 15),
        axis.title = element_text(size = 20))  # Adjusting axis title size


## save plots

private_cause_plot <- ppool_syn_summ$att %>%
  #filter(Level == 'Average') %>%
  ggplot(aes(x = Time, y = Estimate, color = Level)) +
  geom_point() +
  geom_line() + 
  geom_vline(xintercept = 0) + 
  theme_classic() +
  theme(axis.title = element_text(),
        legend.position = "bottom") +
  ggtitle('Staggered Synthetic Controls for Private Cause of Action') +
  xlab('Time') +
  ylab('Identity Theft Per 100K') +
  theme(legend.position = "none") +
  facet_wrap(~Level) +
  theme(strip.text.x = element_text(size = 20)) +
  theme(axis.text = element_text(size = 20)) +
  theme(axis.title = element_text(size = 20)) +
  theme(plot.title=element_text(hjust = .5, size = 20),
        axis.text = element_text(size = 20))

ggsave(filename = here('images', 'private_cause.png'),
       plot = private_cause_plot,
       device = 'png',
       scale = 1,
       width = 4512,
       height = 2072,
       units = 'px')

ggsave(filename = here('images', 'private_cause_bar_effects.png'),
       plot = private_cause_bar_effects,
       device = 'png',
       scale = 1,
       width = 4512,
       height = 2072,
       units = 'px')
