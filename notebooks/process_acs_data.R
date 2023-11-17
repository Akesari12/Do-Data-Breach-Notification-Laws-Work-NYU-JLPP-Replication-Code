pacman::p_load(augsynth, # augmented synthetic control
               tidyverse, # for tidyverse 
               here, # for reproducibility
               ggthemes,
               tidycensus)  

#census_api_key('61f7f27f6fa2b7c45238297a4ea1dcbf7ba1dfa6', install = TRUE, overwrite = TRUE)

## List of Variables
#vars_table <- load_variables(2010, 'acs5')

#### Over 60 2010-2020 ####

vars <- c('B01001_001',
          paste0('B01001_0', c(18:25)),
          paste0('B01001_0', c(42:49)))

build_over_60_table <- function(year, vars = vars, acs = 'acs5') {
  acs_table <- get_acs(geography = 'state',
                       variables = vars,
                       year = year,
                       survey = acs)
  
  acs_table <- acs_table %>%
    select(-GEOID, -moe) %>%
    pivot_wider(names_from = variable,
                values_from = estimate) %>%
    rowwise() %>%
    mutate(percent_over_60 = sum(c_across(B01001_018:B01001_049))/B01001_001) %>%
    rename(state = NAME) %>%
    mutate(year = year) %>%
    select(state, year, percent_over_60)
  
  return(acs_table)
}

years <- c(2005:2020)

over_sixty <- data.frame()

for (year in years){
  if (year < 2010){
    temp_table <- build_over_60_table(year, vars = vars, acs = 'acs1')
    
    over_sixty <- rbind(over_sixty, temp_table)
  }
  else{
    temp_table <- build_over_60_table(year, vars)
    
    over_sixty <- rbind(over_sixty, temp_table)
  }

}

### Census Over 60 ###

#census_2000_vars <- load_variables(2000, 'sf1')

build_over_60_census_table <- function(year, vars = census_vars) {
  census_table <- get_decennial(geography = 'state',
                variables = vars,
                year = year)
  
  census_table <- census_table %>%
    select(-GEOID) %>%
    pivot_wider(names_from = variable,
                values_from = value) %>%
    rowwise() %>%
    mutate(percent_over_60 = as.numeric(sum(c_across(P012018:P012049))/P012001)) %>%
    rename(state = NAME) %>%
    mutate(year = year) %>%
    select(state, year, percent_over_60)
  
  return(census_table)
}

census_vars <- c('P012001',
          paste0('P0120', c(18:25)),
          paste0('P0120', c(42:49)))

census_2000_table <- build_over_60_census_table(2000, census_vars)

### Internet Access 2015-2020 ###

#vars_table_2015 <- load_variables(2015, 'acs1')
years <- c(2015:2020)

internet_vars <- c(paste0('B28002_00', c(1:3)))

build_internet_access_table <- function(year, vars = internet_vars, acs = 'acs1') {
  internet_table <- get_acs(geography = 'state',
                            variables = internet_vars,
                            year = year,
                            survey = acs)
  
  internet_table <- internet_table %>%
    select(-GEOID, -moe) %>%
    pivot_wider(names_from = variable,
                values_from = estimate) %>%
    rowwise() %>%
    mutate(percent_internet_connection = B28002_002/B28002_001) %>%
    mutate(percent_broadband = (1-(B28002_003/B28002_001))*percent_internet_connection) %>%
    rename(state = NAME) %>%
    mutate(year = year) %>%
    select(state, year, percent_internet_connection, percent_broadband)
  
  return(internet_table)
}

internet_table <- data.frame()

for (year in years){
  if (year < 2017){
    temp_table <- build_internet_access_table(year, vars)
    
    internet_table <- rbind(internet_table, temp_table)
  }
  else{
    temp_table <- build_internet_access_table(year, vars, 'acs5')
    
    internet_table <- rbind(internet_table, temp_table)
  }

}

### Join all tables ###

broadband_2000_2014 <- read_csv(here('data', 'broadband_2000_2014.csv'))

broadband_2000_2014 <- broadband_2000_2014 %>%
  select(-abbreviation) %>%
  pivot_longer(!state, 
               names_to = 'year', 
               values_to = 'percent_broadband') %>%
  mutate(year = as.numeric(year)) %>%
  mutate(percent_broadband = str_remove(percent_broadband, '%')) %>%
  mutate(percent_broadband = as.numeric(percent_broadband)) %>%
  mutate(percent_broadband = percent_broadband/100)

internet_2000_2014 <- read_csv(here('data', 'internet_access_2000_2014.csv'))

internet_2000_2014 <- internet_2000_2014 %>%
  select(-abbreviation) %>%
  pivot_longer(!state,
               names_to = 'year',
               values_to = 'percent_internet_connection') %>%
  mutate(year = as.numeric(year)) %>%
  mutate(percent_internet_connection = str_remove(percent_internet_connection, '%')) %>%
  mutate(percent_internet_connection = as.numeric(percent_internet_connection)) %>%
  mutate(percent_internet_connection = percent_internet_connection/100)

internet <- broadband_2000_2014 %>%
  left_join(internet_2000_2014) 

internet <- internet %>% 
  bind_rows(internet_table) %>%
  arrange(state, year)

age <- over_sixty %>%
  bind_rows(census_2000_table) %>%
  arrange(state, year)

full_table <- internet %>%
  left_join(age) %>%
  group_by(state) %>%
  mutate(percent_over_60 = zoo::na.approx(percent_over_60))

### write data ###
write_csv(full_table, here('data', 'internet_age_covariates.csv'))

saveRDS(full_table, here('data', 'internet_age_covariates.RDS'))

