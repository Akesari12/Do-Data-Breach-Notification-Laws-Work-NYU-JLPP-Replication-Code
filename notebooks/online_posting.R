pacman::p_load(augsynth, # augmented synthetic control
               tidyverse, # for tidyverse 
               here, # for reproducibility
               ggthemes)  

# load panel data

consumer_sentinel <- read_csv(here('data', 'panel_data.csv'))

consumer_sentinel <- consumer_sentinel %>%
  mutate(state = ifelse(state == 'Ilinois', 'Illinois', state),
         state = ifelse(state == 'Massachusets', 'Massachusetts', state),
         state = ifelse(state == 'Hawai', "Hawaii", state)) 

# join with online posting

online_posting <- read_csv(here('data', 'online_posting_effective_date.csv'))

online_posting <- online_posting %>%
  rename(state = `Jurisdictions\n`,
         citation = `Citation\n\n`, 
         online_posting = `Online Posting of Data Breaches\n\n`,
         effective_date = Effective_Date) %>%
  mutate(across(everything(),
                ~ str_remove_all(., '\\n')))

# covariate data

covariate_table <- read_csv(here('data', 'internet_age_covariates.csv'))

consumer_online_posting <- consumer_sentinel %>%
  left_join(online_posting) %>%
  left_join(covariate_table) %>%
  drop_na()

# multisynth

consumer_online_posting <- consumer_online_posting %>%
  #filter(state != 'Illinois') %>%
  filter(state != 'Oregon') %>%
  filter(year < 2017) %>%
  filter(!state %in% c('Oregon', 'Vermont', 'Washington', 'Montana')) %>%
  mutate(effective_date = as.Date(effective_date, format = '%m/%d/%Y')) %>%
  mutate(effective_year = format(effective_date, format = '%Y')) %>%
  mutate(effective_year = ifelse(is.na(effective_year),
                                          Inf, effective_year)) %>%
  mutate(online_posting_required = 1 * (year >= effective_year)) %>%
  mutate(log_id_theft_per_capita = log10(id_theft_per_capita)) %>%
  group_by(state) %>%
  mutate(year_lag = lag(id_theft_per_capita, order_by=year, k = 12)) %>%
  mutate(two_year_lag = lag(id_theft_per_capita, order_by=year, k = 24)) %>%
  mutate(three_year_lag = lag(id_theft_per_capita, order_by=year, k = 36)) %>%
  mutate(four_year_lag = lag(id_theft_per_capita, order_by=year, k = 48)) %>%
  mutate(five_year_lag = lag(id_theft_per_capita, order_by=year, k = 60)) %>%
  ungroup()

ppool_syn <- multisynth(id_theft_per_capita ~ online_posting_required | 
                          percent_internet_connection +  
                          percent_over_60 + year_lag + two_year_lag + 
                          three_year_lag + four_year_lag, 
                        state, 
                        year, 
                        consumer_online_posting,
                        fixedeff = TRUE,
                        n_leads = 5,
                        n_lags = 5)         

ppool_syn_summ <- summary(ppool_syn)

online_posting_plot <- ppool_syn_summ$att %>%
  ggplot(aes(x = Time, y = Estimate, color = Level)) +
  geom_point() +
  geom_line() + 
  geom_vline(xintercept = 0) + 
  theme_classic() +
  theme(axis.title = element_text(),
        legend.position = "bottom") +
  ggtitle('Synthetic Controls for Online Posting of Notices') +
  xlab('Time') +
  ylab('Identity Theft Per 100K') +
  theme(legend.position = "none") +
  facet_wrap(~Level)+
  theme(strip.text.x = element_text(size = 30)) +
  theme(axis.text = element_text(size = 20)) +
  theme(axis.title = element_text(size = 20))

ggsave(filename = here('images', 'online_posting.png'),
       plot = online_posting_plot,
       device = 'png',
       scale = 1,
       width = 4512,
       height = 2072,
       units = 'px')
