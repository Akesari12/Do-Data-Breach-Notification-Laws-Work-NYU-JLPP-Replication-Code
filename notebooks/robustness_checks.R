pacman::p_load(augsynth, # augmented synthetic control
               tidyverse, # for tidyverse 
               here, # for reproducibility
               ggthemes)  

## Attorney General Notification ##

source(here('notebooks', 'ag_notice.R'))

# time placebo check

consumer_ag_notice <- consumer_ag_notice %>%
  #(state != "New York") %>%
  mutate(ag_notice_required = 1 * (year >= (as.numeric(effective_year) - 3)))

ppool_syn <- multisynth(id_theft_per_capita ~ ag_notice_required | 
                          percent_broadband +  
                          percent_over_60 + year_lag + two_year_lag + 
                          three_year_lag + four_year_lag,  
                        state, 
                        year, 
                        consumer_ag_notice,
                        fixedeff = F,
                        #n_lags = 5,
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

ag_notice_placebo_plot <- ppool_syn_summ$att %>%
  filter(Time >= -5) %>%
  filter(Level == 'Average') %>%
  ggplot(aes(x = Time, y = Estimate, color = Level)) +
  geom_point() +
  geom_line() + 
  geom_vline(xintercept = 0) + 
  theme_classic() +
  theme(axis.title = element_text(),
        legend.position = "bottom") +
  ggtitle('3-Year Placebo Check for Required Notification To Regulator') +
  xlab('Time Before Placebo Treatment') +
  ylab('ATT (Identity Theft Per 100K)') +
  theme(legend.position = "none") +
  #facet_wrap(~Level) +
  theme(strip.text.x = element_text(size = 15)) +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 20)) +
  theme(plot.title=element_text(hjust = .5, size = 20))

ggsave(filename = here('images', 'ag_notice_placebo_plot.png'),
       plot = ag_notice_placebo_plot,
       device = 'png',
       scale = 1,
       width = 4512,
       height = 2072,
       units = 'px')


# values of v

source(here('notebooks', 'ag_notice.R'))


nus <- seq(.01, 1, .01)

hyper_search_multi_synth <- function(nu_val){
  ppool_syn <- multisynth(id_theft_per_capita ~ ag_notice_required | 
                            percent_broadband +  
                            percent_over_60 + year_lag + two_year_lag + 
                            three_year_lag + four_year_lag,  
                          state, 
                          year,
                          nu = nu_val,
                          consumer_ag_notice,
                          fixedeff = T,
                          n_lags = 5,
                          n_leads = 3) 
  
  ppool_syn_summ <- summary(ppool_syn)
  
  att <- ppool_syn_summ$att %>%
    filter(Time >= 0) %>%
    summarise(att = mean(Estimate)) %>%
    pull(att)
  
  return(att)
}

atts <- lapply(nus, hyper_search_multi_synth)

robust_df <- data.frame(nus, unlist(atts))
robust_df <- robust_df %>% 
  mutate(atts = as.numeric(atts)) %>%
  mutate(ideal = ifelse(nus == round(ppool_syn$nu, 2),
                        'black',
                        'white'))

#saveRDS(robust_df, here('data', 'robustness_nu_ag_notice.rds'))
robust_df <- readRDS(here('data', 'robustness_nu_ag_notice.rds'))

nu_plot <- ggplot(data = robust_df,
       aes(x = nus, y = atts)) +
  geom_point(size = 7, color = robust_df$ideal) +
  theme_classic() +
  theme(axis.title = element_text(),
        legend.position = "bottom") +
  ggtitle('ATTs as Different Levels of Pooling Parameter, nu') +
  xlab('Nu (0 to 1)') +
  ylab('ATT (Identity Theft Per 100K)') +
  theme(legend.position = "none") +
  #facet_wrap(~Level) +
  theme(strip.text.x = element_text(size = 15)) +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 20)) +
  theme(plot.title=element_text(hjust = .5, size = 20))

robust_df <- robust_df %>% 
  arrange(desc(ideal == "black"))

nu_plot_bw <- ggplot(data = robust_df, aes(x = nus, y = atts, color = ideal, fill = ideal)) +
  geom_point(size = 7, shape = 21) +  # shape = 21 for filled circles
  scale_color_manual(values = c("black" = "black", "red" = "black")) +  # Outline colors
  scale_fill_manual(values = c("black" = "white", "red" = "black")) +  # Fill colors
  theme_classic() +
  ggtitle('ATTs as Different Levels of Pooling Parameter, nu') +
  xlab('Nu (0 to 1)') +
  ylab('ATT (Identity Theft Per 100K)') +
  theme(legend.position = "none",
        strip.text.x = element_text(size = 15),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 20),
        plot.title = element_text(hjust = 0.5, size = 20))

ggsave(filename = here('images', 'values_of_nu_plot.png'),
       plot = nu_plot,
       device = 'png',
       scale = 1,
       width = 4512,
       height = 2072,
       units = 'px')

ggsave(filename = here('images/bw_images_paper/', 'values_of_nu_plot_bw.png'),
       plot = nu_plot_bw,
       device = 'png',
       scale = 1,
       width = 4512,
       height = 2072,
       units = 'px')

# state placebo


source(here('notebooks', 'ag_notice.R'))

base_att <- ppool_syn_summ$att %>%
  filter(Time >= 0) %>%
  summarise(att = mean(Estimate)) %>%
  pull(att)

years <- unique(consumer_ag_notice$effective_year)

remove <- c(2005, 2017, 2018)

years <- as.numeric(years[!years %in% remove])

placebo_atts <- c()

placebo_run <- function(){
  placebo_state_treat <- consumer_ag_notice %>%
    group_by(state) %>%
    mutate(placebo_effective_year = sample(years, 1)) %>%
    ungroup() %>%
    mutate(ag_notice_required = 1 * (year >= placebo_effective_year)) 
  
  ppool_syn <- multisynth(id_theft_per_capita ~ ag_notice_required | 
                            percent_broadband +  
                            percent_over_60 + year_lag + two_year_lag + 
                            three_year_lag + four_year_lag,  
                          state, 
                          year, 
                          placebo_state_treat,
                          fixedeff = T,
                          n_lags = 5,
                          n_leads = 3)         
  
  ppool_syn_summ <- summary(ppool_syn)
  
  att <- ppool_syn_summ$att %>%
    filter(Time >= 0) %>%
    summarise(att = mean(Estimate)) %>%
    pull(att)
  
  return(att)
}

for (i in 1:50) {
  
  skip_to_next <- FALSE
  
  # Note that print(b) fails since b doesn't exist
  
  tryCatch(placebo_atts[i] <- placebo_run(), error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
}

placebo_atts_df <- data.frame(na.omit(placebo_atts))

saveRDS(placebo_atts_df, here('data', 'state_placebo_ag_notice.rds'))

colnames(placebo_atts_df) <- c('atts')

state_placebo_plot <- placebo_atts_df %>%
  ggplot() +
  geom_density(aes(x = atts),
               fill = 'blue', 
               color = 'blue',
               alpha = .1) +
  geom_vline(xintercept = base_att, color = 'red') +
  theme_classic() +
  theme(axis.title = element_text(),
        legend.position = "bottom") +
  ggtitle('ATTs for Randomly Assigned Treatments') +
  xlab('ATTs (Identity Theft Per 100k)') +
  ylab('Density') +
  theme(legend.position = "none") +
  #facet_wrap(~Level) +
  theme(strip.text.x = element_text(size = 15)) +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 20)) +
  theme(plot.title=element_text(hjust = .5, size = 20))

ggsave(filename = here('images', 'state_placebo_plot.png'),
       plot = state_placebo_plot,
       device = 'png',
       scale = 1,
       width = 4512,
       height = 2072,
       units = 'px')

## California Encryption Spillover

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

ggsave(filename = here('images', 'encryption_spillover_check.png'),
       plot = texas_spillover,
       device = 'png',
       scale = 1,
       width = 4512,
       height = 2072,
       units = 'px')
