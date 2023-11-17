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


all_laws <- read_csv(here('data', 'all_laws.csv'))

donor_states <- all_laws %>%
  mutate(ag_valid_donor = case_when(
    ag_effective_date == 0 ~ 1,
    as.Date(ag_effective_date,
            format = '%m/%d/%Y') >= as.Date('01-01-2016', 
                                 format = '%m-%d-%Y') ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(cra_valid_donor = case_when(
    cra_notice_effective_date == 0 ~ 1,
    as.Date(cra_notice_effective_date,
            format = '%m/%d/%Y') >= as.Date('01-01-2016', 
                                            format = '%m-%d-%Y') ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(cause_of_action_valid_donor = case_when(
    cause_of_action_effective_date == 0 ~ 1,
    as.Date(cause_of_action_effective_date,
            format = '%m/%d/%Y') >= as.Date('01-01-2016', 
                                            format = '%m-%d-%Y') ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(encryption_valid_donor = case_when(
    encryption_effective_date == 0 ~ 1,
    as.Date(encryption_effective_date,
            format = '%m/%d/%Y') >= as.Date('01-01-2016', 
                                            format = '%m-%d-%Y') ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(harm_valid_donor = case_when(
    harm_required_effective_date == 0 ~ 1,
    as.Date(harm_required_effective_date,
            format = '%m/%d/%Y') >= as.Date('01-01-2016', 
                                            format = '%m-%d-%Y') ~ 1,
    TRUE ~ 0
  )) %>%
  group_by(state) %>%
  mutate(provisions = sum(ag_valid_donor,
                          cra_valid_donor,
                          encryption_valid_donor,
                          harm_valid_donor,
                          cause_of_action_valid_donor)) %>%
  filter(!state %in% c('Puerto Rico', 'U.S. Virgin Islands',
                     'Guam')) %>%
  filter(provisions == 5 | state == 'Hawaii')

## panel data

# load panel data

consumer_sentinel <- read_csv(here('data', 'panel_data_ext.csv'))

consumer_sentinel <- consumer_sentinel %>%
  mutate(state = ifelse(state == 'Ilinois', 'Illinois', state),
         state = ifelse(state == 'Massachusets', 'Massachusetts', state),
         state = ifelse(state == 'Hawai', "Hawaii", state))

# covariate data

covariate_table <- read_csv(here('data', 'internet_age_covariates.csv'))

hawaii <- donor_states %>%
  left_join(consumer_sentinel) %>%
  left_join(covariate_table) %>%
  #drop_na() %>%
  filter(!state %in% c('Puerto Rico', 
                       'District of Columbia')) %>%
  select(state, 
         year, 
         id_theft_per_capita, 
         percent_broadband, 
         percent_internet_connection,
         percent_over_60) %>%
  mutate(bundle = ifelse(state == 'Hawaii' &
                           year >= 2007,
                         1,
                         0)) %>%
  ungroup() 

hawaii <- plm::make.pbalanced(hawaii, index = c("state", "year"))


# synthetic control
ppool_syn <- augsynth(id_theft_per_capita ~ bundle | 
                        percent_internet_connection + percent_broadband +
                        percent_over_60,
                      state,
                      year, 
                      #t_int = as.Date("01/01/2009",
                      #                format = "%m/%d/%Y"),
                      #                 as.Date("01/01/2016",
                      #                         format = "%m/%d/%Y"), 
                      hawaii, 
                      progfunc = "Ridge",
                      fixedeff = T,
                      scm = T)

asyn_summ <- summary(ppool_syn)

hi_synhi <- hawaii %>%
  filter(state == "Hawaii") %>%
  #filter(regionno == 4) %>%
  bind_cols(difference = asyn_summ$att$Estimate) %>%
  bind_cols(upper_ci = asyn_summ$att$upper_bound) %>%
  bind_cols(lower_ci = asyn_summ$att$lower_bound) %>%
  mutate(synthetic_hawaii = 
           id_theft_per_capita - difference) %>%
  mutate(ci_midpoint = (upper_ci + lower_ci)/2) %>%
  mutate(synthetic_ca_upper_ci = synthetic_hawaii + ci_midpoint) %>%
  mutate(synthetic_ca_lower_ci = synthetic_hawaii - ci_midpoint)

hi_bundle_plot <- hi_synhi %>%
  filter(year <= 2017) %>%
  ggplot() +
  geom_point(aes(x = year, 
                 y = id_theft_per_capita, 
                 color = 'hawaii')) +
  geom_line(aes(x = year, 
                y = id_theft_per_capita, 
                color = 'hawaii')) +
  geom_point(aes(x = year, 
                 y = synthetic_hawaii, 
                 color = 'synthetic hawaii')) +
  geom_line(aes(x = year, 
                y = synthetic_hawaii, 
                color = 'synthetic hawaii')) +
  #eom_ribbon(aes(x = year,
  #                ymin =  synthetic_ca_lower_ci * 1.1,
  #                ymax = synthetic_ca_upper_ci * 1.1,
  #                color = "Synthetic hawaii"),
  #            fill = 'darkblue',
  #            alpha = .2) +
  scale_color_manual(values = c('hawaii' = 'darkblue', 'synthetic hawaii' = 'darkred')) +
  geom_vline(aes(xintercept = 2007),
             linetype = "dashed") +
  theme_classic() +
  theme(axis.title = element_text()) +
  theme(strip.text.x = element_text(size = 15)) + 
  theme(legend.text=element_text(size= 15),
        legend.position = 'bottom') +
  theme(axis.title = element_text(size = 20)) +
  theme(plot.title=element_text(hjust = .5, size = 20),
        axis.text = element_text(size = 20),
        axis.title.x = element_text(vjust = -.5)) +
  ggtitle('2007 Hawaii Policy Bundle') +
  xlab('Year') +
  ylab('ID Theft/100,000')
