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

ag_notice <- read_csv(here('data', 'ag_notice_effective_date.csv'))

cause_of_action <- read_csv(here('data', 'cause_of_action_effective_date.csv'))

cra_notice <- read_csv(here('data', 'cra_notice_effective_date.csv'))

encryption <- read_csv(here('data', 'encryption_effective_date.csv'))

harm_required <- read_csv(here('data', 'harm_analysis_effective_date.csv'))

timeline <- read_csv(here('data', 'notification_days.csv'))

timeline <- timeline %>%
  select(Jurisdictions, AG_notification_days, Consumer_notification_days, Consumer_notification_days_effective)

ag_notice %>%
  rename(state = `Jurisdictions`,
         ag_effective_date = Effective_date) %>%
  mutate(across(everything(),
                ~ str_remove_all(., '\\n'))) %>%
  select(state, ag_effective_date)

process_df <- function(df, colname_effective_date = 'effective_date') {
  colnames(df) <- c('state', 'citation', 'treat', 'Effective_date')
  
  new_df <- df %>%
    mutate(across(everything(),
                  ~ str_remove_all(., '\\n'))) %>%
    select(state, Effective_date)
  
  colnames(new_df) <- c('state', colname_effective_date)
  
  return(new_df)
}

ag_notice <- process_df(ag_notice, 'ag_effective_date')
cause_of_action <- process_df(cause_of_action, 'cause_of_action_effective_date')
cra_notice <- process_df(cra_notice, 'cra_notice_effective_date')
encryption <- process_df(encryption, 'encryption_effective_date')
harm_required <- process_df(harm_required, 'harm_required_effective_date')
timeline <- process_df(timeline, 'timeline_effective_date')

full_df <- ag_notice %>%
  left_join(cause_of_action) %>%
  left_join(cra_notice) %>%
  left_join(encryption) %>%
  left_join(harm_required) %>%
  left_join(timeline)

write.table(full_df, 
            file = here('data',
                        "all_laws.txt"), sep = ",", quote = FALSE, row.names = F)

write_csv(full_df,
          here('data',
               'all_laws.csv'))
