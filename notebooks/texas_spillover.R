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

encryption_exception <- read_csv(here('data', 'encryption_effective_date.csv'))

encryption_exception <- encryption_exception %>%
  rename(state = `Jurisdictions\n`,
         citation = `Citation\n\n`, 
         harm = `Apply to Encrypted Data?\n\n`,
         effective_date = `Effective_Date`) %>%
  mutate(across(everything(),
                ~ str_remove_all(., '\\n')))

# covariate data

covariate_table <- read_csv(here('data', 'internet_age_covariates.csv'))

consumer_encryption_exception <- consumer_sentinel %>%
  left_join(encryption_exception) %>%
  left_join(covariate_table) %>%
  drop_na() %>%
  filter(!state %in% c('Puerto Rico', 
                       'District of Columbia'))

# multisynth

consumer_encryption_exception <- consumer_encryption_exception %>%
  filter(state != 'Illinois', state != "Nebraska", state != 'Tennessee') %>%
  mutate(effective_date = as.Date(effective_date, format = '%m/%d/%Y')) %>%
  mutate(effective_year = format(effective_date, format = '%Y')) %>%
  mutate(effective_year = ifelse(is.na(effective_year),
                                 Inf, effective_year)) %>%
  mutate(encryption_exception_required = 1 * (year >= effective_year)) %>%
  mutate(log_id_theft_per_capita = log10(id_theft_per_capita)) %>%
  group_by(state) %>%
  mutate(year_lag = lag(id_theft_per_capita, order_by=year, k = 12)) %>%
  mutate(two_year_lag = lag(id_theft_per_capita, order_by=year, k = 24)) %>%
  mutate(three_year_lag = lag(id_theft_per_capita, order_by=year, k = 36)) %>%
  mutate(four_year_lag = lag(id_theft_per_capita, order_by=year, k = 48)) %>%
  mutate(five_year_lag = lag(id_theft_per_capita, order_by=year, k = 60)) %>%
  ungroup() %>%
  filter(effective_year == Inf | state == 'Texas')

consumer_encryption_exception <- plm::make.pbalanced(consumer_encryption_exception, index = c("state", "year"))

ppool_syn <- augsynth(id_theft_per_capita ~ encryption_exception_required | 
                        percent_broadband +  
                        percent_over_60 + year_lag + two_year_lag + 
                        three_year_lag + four_year_lag,     
                      state, 
                      year, 
                      consumer_encryption_exception,
                      progfun = 'Ridge',
                      fixedeff = T,
                      scm = T)  

ppool_syn_summ <- summary(ppool_syn)

texas <- consumer_encryption_exception %>%
  filter(state == 'Texas') %>%
  rename(Time = year) %>%
  left_join(ppool_syn_summ$att) %>%
  select(Time, id_theft_per_capita, Estimate) %>%
  mutate(estimated_id_theft = id_theft_per_capita + Estimate) %>%
  mutate(percent_change = estimated_id_theft/lag(estimated_id_theft)) %>%
  #  mutate(percent_change = (estimated_id_theft - id_theft_per_capita)/id_theft_per_capita) %>%
  select(Time, percent_change) %>%
  rename(year = Time)

top_weights <- data.frame(ppool_syn$weights) %>%
  top_n(6)

top_donors <- rownames(top_weights)

texas_spillover <- consumer_encryption_exception %>%
  filter(year >= 2009 & year < 2015) %>%
  filter(state %in% c(top_donors)) %>%
  left_join(texas) %>%
  #select(state, year, id_theft_per_capita, percent_change) %>%
  mutate(estimated_id_theft = lag(id_theft_per_capita) - (lag(id_theft_per_capita) * percent_change)) %>%
  #mutate(estimated_id_theft = id_theft_per_capita - (percent_change*id_theft_per_capita)) %>%
  select(state, year, id_theft_per_capita, estimated_id_theft, percent_change) %>%
  ggplot() +
  geom_line(aes(x = year, 
                y = id_theft_per_capita,
                color = 'observed id theft per capita',
  )) +
  geom_line(aes(x = year,
                y = estimated_id_theft,
                color = 'estimated id theft per capita')) +
  facet_wrap(~state) +
  ggtitle('Observed v. Estimated ID Theft if Texas Law Spilled Over') +
  ylab('ID THeft Per 100K') +
  xlab("Year") +
  theme_classic() +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1))