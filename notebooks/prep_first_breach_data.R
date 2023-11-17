library(tidyverse)
library(here)

# load panel data

consumer_sentinel <- read_csv(here('data', 'panel_data.csv'))

consumer_sentinel <- consumer_sentinel %>%
  mutate(state = ifelse(state == 'Ilinois', 'Illinois', state),
         state = ifelse(state == 'Massachusets', 'Massachusetts', state),
         state = ifelse(state == 'Hawai', "Hawaii", state)) 

# extend to 2000

summary_consumer_breach <- consumer_sentinel %>% 
  group_by(year) %>% 
  summarise(id_theft_total = sum(id_theft_raw)) 

year <- c(2000, 2001)
id_theft_total <- c(31103, 86168)

old_data <- data.frame(year, id_theft_total)

summary_consumer_breach_ext <- rbind(old_data, summary_consumer_breach)

write_csv(summary_consumer_breach_ext,
          here('data', 'summary_by_year_extended.csv'))

pop_table <- consumer_sentinel %>%
  filter(year == 2002) %>%
  mutate(population = id_theft_raw/id_theft_per_capita*100000) %>%
  select(state, population)  

extended_estimates <- consumer_sentinel %>%
  filter(year == 2002) %>%
  mutate(total_id_theft = sum(id_theft_raw)) %>%
  mutate(prop = id_theft_raw/total_id_theft) %>%
  select(state, prop) %>%
  mutate(id_theft_2000 = prop * 31103,
         id_theft_2001 = prop * 86168) %>%
  select(-prop) %>% 
  pivot_longer(!state, ) %>%
  rename(year = name,
         id_theft_raw = value) %>%
  mutate(year = as.numeric(str_sub(year,start = -4))) %>%
  left_join(pop_table) %>%
  mutate(id_theft_per_capita = id_theft_raw/population*100000) %>%
  select(-population)

consumer_sentinel <- extended_estimates %>%
  full_join(consumer_sentinel) %>%
  select(-`...1`)

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
  drop_na() %>%
  #filter(year < 2017) %>%
  mutate(effective_date = as.Date(effective_date, format = '%m/%d/%Y')) %>%
  mutate(effective_year = format(effective_date, format = '%Y')) %>%
  mutate(effective_year = ifelse(is.na(effective_year),
                                 Inf, effective_year)) %>%
  mutate(breach_law = 1 * (year >= effective_year)) %>%
  mutate(id_theft_per_capita = case_when(year == 2004 ~ id_theft_per_capita/.25,
                                         year == 2005 ~ id_theft_per_capita/.25,
                                         TRUE ~ id_theft_per_capita)) %>%
  mutate(id_theft_raw = case_when(year == 2004 ~ id_theft_raw/.25,
                                         year == 2005 ~ id_theft_raw/.25,
                                         TRUE ~ id_theft_raw)) %>%
  arrange(state, year) %>%
  select(state, year, id_theft_per_capita, id_theft_raw, breach_law) 

write_csv(consumer_breach_law, here('data', 'panel_data_ext.csv'))


consumer_breach_law %>%
  group_by(year) %>% 
  summarise(id_theft_total = sum(id_theft_raw)) %>%
  ggplot() +
  geom_line(aes(x = year, y = id_theft_total))

consumer_breach_law %>%
  ggplot() +
  geom_line(aes(x = year, 
                y = id_theft_raw, 
                group = state,
                color = state)) + 
  theme(legend.position = 'none')



         