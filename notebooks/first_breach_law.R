pacman::p_load(augsynth, # augmented synthetic control
               tidyverse, # for tidyverse 
               here, # for reproducibility
               ggthemes,
               anytime)  

# load panel data

consumer_sentinel <- read_csv(here('data', 'panel_data_ext.csv'))
consumer_sentinel <- consumer_sentinel %>%
  filter(year != 2011)

consumer_sentinel <- consumer_sentinel %>%
  mutate(state = ifelse(state == 'Ilinois', 'Illinois', state),
         state = ifelse(state == 'Massachusets', 'Massachusetts', state),
         state = ifelse(state == 'Hawai', "Hawaii", state)) 

# first breach notification

breach_notification_laws <- read_csv(here('data', 'bloomberg_law_summary.csv'))

breach_notification_laws <- breach_notification_laws %>%
  rename(state = `Jurisdictions\n`) %>%
  rename(effective_date = `Effective Date\n\n`) %>%
  mutate(effective_date = str_remove(effective_date, "\\.")) %>%
  mutate(effective_date = str_extract(effective_date, "\\d{1,2}/\\d{1,2}/\\d{4}")) %>%
  select(state, effective_date) %>%
  mutate(across(everything(),
                ~ str_remove_all(., '\\n'))) %>%
  mutate(effective_date = case_when(state == 'Alabama' ~ '5/1/2018',
                                    state == 'District of Columbia' ~ '6/17/2020',
                                    state == 'Mississippi' ~ '7/1/2011',
                                    state == 'New Mexico' ~ '6/16/2017',
                                    state == 'Puerto Rico' ~ '1/5/2006',
                                    state == 'Texas' ~ '9/1/2005',
                                    state == 'U.S. Virgin Islands' ~ '10/17/2005',
                                    state == 'Virginia' ~ '7/1/2008',
                                    TRUE ~ effective_date))

# covariate data

covariate_table <- read_csv(here('data', 'internet_age_covariates.csv'))

# join consumer sentinel and first breach notification law

consumer_breach_law <- consumer_sentinel %>%
  left_join(breach_notification_laws) %>%
  left_join(covariate_table) %>%
  drop_na()

# multisynth
consumer_breach_law <- consumer_breach_law %>%
  filter(!state %in% c("California")) %>%
  filter(year != 2011) %>%
  #filter(state != 'New Mexico', state != "California") %>%
  #filter(year > 2003) %>%
  filter(year < 2017) %>%
  mutate(effective_date = as.Date(effective_date, format = '%m/%d/%Y')) %>%
  mutate(effective_year = format(effective_date, format = '%Y')) %>%
  mutate(effective_year = ifelse(is.na(effective_year),
                                 Inf, effective_year)) %>%
  mutate(breach_law = 1 * (year >= effective_year)) %>%
  mutate(log_id_theft_per_capita = log10(id_theft_per_capita)) %>%
  group_by(state) %>%
  mutate(year_lag = lag(id_theft_per_capita, order_by=year, n = 1)) %>%
  mutate(two_year_lag = lag(id_theft_per_capita, order_by=year, n = 2)) %>%
  mutate(three_year_lag = lag(id_theft_per_capita, order_by=year, n = 3)) %>%
  mutate(four_year_lag = lag(id_theft_per_capita, order_by=year, n = 4)) %>%
  mutate(five_year_lag = lag(id_theft_per_capita, order_by=year, n = 5)) %>%
  ungroup()


ppool_syn <- multisynth(id_theft_per_capita ~ breach_law | percent_broadband + 
                          percent_internet_connection + percent_over_60 +
                          year_lag + two_year_lag + three_year_lag,  
                        state, 
                        year, 
                        consumer_breach_law,
                        fixedff = TRUE,
                        n_lags = 5,
                        n_leads = 3)         

ppool_syn_summ <- summary(ppool_syn)

## percent reduction

base_id_theft_rate <- consumer_breach_law %>%
  filter(state %in% c(ppool_syn_summ$att$Level)) %>%
  filter(breach_law == 1) %>%
  summarise(mean_post_treat = mean(id_theft_per_capita)) %>%
  pull(mean_post_treat)

est_id_theft_rate <- ppool_syn_summ$att %>% 
  filter(Time >= 0) %>% 
  summarise(mean_att = mean(Estimate)) %>%
  pull(mean_att)

percent_reduction <- est_id_theft_rate/base_id_theft_rate * 100

## barplot

first_breach_bar_effects <- ppool_syn_summ$att %>%
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
  ggtitle('Average Treatment Effects by State for Baseline Law',
          subtitle = paste0('Average ATT ', 
                            round(percent_reduction, 2),
                            '%')) +
  xlab('State') +
  ylab('Mean ATT') +
  theme(legend.position = 'none') +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 20),  # Adjusting x-axis text
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 15),
        axis.title = element_text(size = 20))  # Adjusting axis title size
# save image

ppool_syn_summ$att %>%
  ggplot(aes(x = Time, y = Estimate, color = Level)) +
  geom_point() +
  geom_line() + 
  geom_vline(xintercept = 0) + 
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        legend.position = "bottom") +
  ggtitle('Synthetic Controls for First Breach Notification Law') +
  xlab('Time') +
  ylab('Identity Theft Per Capita') +
  theme(legend.position = "none") +
  facet_wrap(~Level) +
  theme(plot.title=element_text(hjust = .5, size = 20),
        axis.text = element_text(size = 20))

ggsave(filename = here('images', 'first_breach_bar_effects.png'),
       plot = first_breach_bar_effects,
       device = 'png',
       scale = 1,
       width = 4512,
       height = 2072,
       units = 'px') +
  theme(plot.title=element_text(hjust = .5, size = 20))

#filter(!state %in% c('Alaska',
#                     'Arizona',
#                     'Arkansas',
#                     'Colorado',
#                     'Connecticut',
#                     'Delaware',
#                     'Florida',
#                     'Georgia',
#                     'Idaho',
#                     'Illinois',
#                     'Indiana',
#                     'Louisiana',
#                     'Maine',
#                     'Montana',
##                     'Nebraska',
#                     'Nevada',
#                     'New Jersey',
#                     'New York',
#                     'North Carolina',
#                     'North Dakota',
#                     'Ohio',
#                     'Pennsylvania',
#                     'Rhode Island',
#                     'Tennessee',
#                     'Texas',
#                     'Washington',
#                     'Wisconsin')) %>%
