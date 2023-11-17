pacman::p_load(augsynth, # augmented synthetic control
               tidyverse, # for tidyverse 
               here, # for reproducibility
               ggthemes,
               anytime,
               choroplethr,
               choroplethrMaps,
               Hmisc,
               png)  

## Diff in Diffs ##

source(here('notebooks', 'prep_first_breach_data.R'))

good_fit <- consumer_breach_law %>%
  filter(state %in% c('Colorado', 'New Mexico')) %>%
  filter(year < 2014) %>%
  ggplot() + 
  geom_point(aes(x = year,
                 y = id_theft_per_capita,
                 color = state)) +
  geom_line(aes(x = year,
                y = id_theft_per_capita,
                color = state)) +
  geom_vline(aes(xintercept = 2006)) +
  scale_color_grey(start = 0.8, end = 0.2, name = "State") +  # Grayscale and change legend title
  theme_classic() +
  theme(axis.title = element_text()) +
  ggtitle('Colorado and New Mexico ID theft \nbefore/after Colorado \nbreach notification law') +
  xlab('Year') +
  ylab('Identity Theft Per 100K') + 
  theme(legend.text = element_text(size = 20),  # Increase legend font size
        legend.title = element_text(size = 20)) +
  theme(axis.text = element_text(size = 20)) +
  theme(axis.title = element_text(size = 20)) +
  theme(plot.title = element_text(hjust = 0.5, size = 20))

bad_fit <- consumer_breach_law %>%
  filter(state %in% c('Arizona', 'South Dakota')) %>%
  filter(year < 2018) %>%
  ggplot() + 
  geom_point(aes(x = year,
                 y = id_theft_per_capita,
                 color = state)) +
  geom_line(aes(x = year,
                y = id_theft_per_capita,
                color = state)) +
  geom_vline(aes(xintercept = 2006)) +
  scale_color_grey(start = 0.8, end = 0.2, name = "State") +  # Grayscale and change legend title
  theme_classic() +
  ggtitle('Arizona and South Dakota ID theft \nbefore/after Arizona \nbreach notification law') +
  xlab('Year') +
  ylab('Identity Theft Per 100K') + 
  theme(legend.text = element_text(size = 20),  # Increase legend font size
        legend.title = element_text(size = 20)) +
  theme(axis.text = element_text(size = 20)) +
  theme(axis.title = element_text(size = 20)) +
  theme(plot.title = element_text(hjust = 0.5, size = 20))


## Synthetic Control for 1 Unit ##

massachusetts <- consumer_breach_law %>%
  filter(state != "California") %>%
  filter(year < 2017) %>%
  # filter(!state %in% c('Arizona') %>%
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
#                     'Nebraska',
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
mutate(breach_law = ifelse(state != 'Massachusetts', 
                           0,
                           breach_law))

syn <- augsynth(id_theft_per_capita ~ breach_law, 
                state, 
                year, 
                massachusetts,
                progfunc = "none", 
                scm = T)

syn_sum <- summary(syn)

mass_synmass <- massachusetts %>%
  filter(state == 'Massachusetts') %>%
  bind_cols(difference = syn_sum$att$Estimate) %>%
  mutate(synthetic_massachusetts = id_theft_per_capita + difference)

mass_syn_example <- mass_synmass %>%
  ggplot() +
  geom_point(aes(x = year, 
                 y = id_theft_per_capita, 
                 color = 'Massachusetts')) +
  geom_line(aes(x = year, 
                y = id_theft_per_capita, 
                color = 'Massachusetts')) +
  geom_point(aes(x = year, 
                 y = synthetic_massachusetts, 
                 color = 'Synthetic Massachusetts')) +
  geom_line(aes(x = year, 
                y = synthetic_massachusetts, 
                color = 'Synthetic Massachusetts')) +
  #scale_color_manual(values = c('Massachusetts' = 'black', 'Synthetic Massachusetts' = 'grey', name = 'State')) +  # Adjusted to grayscale
  scale_color_grey(start = 0.8, end = 0.2, name = "State") +  # Grayscale and change legend title
  geom_vline(aes(xintercept = 2007)) +
  theme_classic() +
  ggtitle('Massachusetts and Synthetic \nMassachusetts \nBreach Notification Law') +
  xlab('Year') +
  ylab('ID Theft Per 100K') + 
  theme(legend.text = element_text(size = 20),  # Keeping legend text size 20
        legend.title = element_text(size = 20),  # Removing legend title
        axis.text = element_text(size = 20),  # Keeping axis text size 20
        axis.title = element_text(size = 20),  # Keeping axis title size 20
        plot.title = element_text(hjust = 0.5, size = 20))  # Keeping plot title size 20


## MultiSynth ##

source(here('notebooks', 'first_breach_law.R'))

multi_syn_example <- ppool_syn_summ$att %>%
  ggplot(aes(x = Time, y = Estimate, color = Level)) +
  geom_point() +
  geom_line() + 
  geom_vline(xintercept = 0) + 
  theme_classic() +
  theme(axis.title = element_text(),
        legend.position = "bottom") +
  ggtitle('Staggered Synthetic Controls \nfor First Breach Notification Law') +
  xlab('Time') +
  ylab('Identity Theft Per 100K') +
  theme(legend.position = "none") +
  facet_wrap(~Level) +
  theme(strip.text.x = element_text(size = 15)) + 
  theme(legend.text=element_text(size= 15)) +
  theme(axis.text = element_text(size = 8)) +
  theme(axis.title = element_text(size = 20)) +
  theme(plot.title=element_text(hjust = .5, size = 20))

# Maps

## first law
df_breach_law_state <- breach_notification_laws %>%
  mutate(region = tolower(state),
         effective_date = as.Date(effective_date, format = '%m/%d/%Y'),
         value = as.integer(format(effective_date, format = '%Y'))) %>%
  filter(!region %in% c('guam', 'puerto rico', 'u.s. virgin islands')) %>%
  select(region, value)

df_breach_law_state$value = cut2(df_breach_law_state$value, cuts=c(seq(2003, 2020, by = 2)))

choro <- StateChoropleth$new(df_breach_law_state)
choro$title <- 'Adoption of Data Breach Notification Statutes'
choro$ggplot_scale = scale_fill_brewer(name = 'Year', palette = "Greys", drop = FALSE)
first_law_map <- choro$render() + theme(legend.text = element_text(size = 17),
                                        plot.background = element_rect(fill = "white", color = NA),  # Add background color
                                        panel.background = element_rect(fill = "white", color = NA)) 

## Choropleths

plot_map <- function(notebook, dataset, cut_min, cut_max, step, title, palette){
  source(here('notebooks', notebook))
  
  df_breach_law_state <- dataset %>%
    mutate(region = tolower(state),
           effective_date = as.Date(effective_date, format = '%m/%d/%Y'),
           value = as.integer(format(effective_date, format = '%Y'))) %>%
    filter(!region %in% c('guam', 'puerto rico', 'u.s. virgin islands')) %>%
    select(region, value)
  
  df_breach_law_state$value = cut2(df_breach_law_state$value, cuts=c(seq(cut_min, cut_max, by = step)))
  
  choro <- StateChoropleth$new(df_breach_law_state)
  choro$title <- title
  choro$ggplot_scale = scale_fill_brewer(name = 'Year', palette = palette, drop = FALSE)
  choro$render() + theme(legend.text = element_text(size = 17),
                         plot.background = element_rect(fill = "white", color = NA),  # Add background color
                         panel.background = element_rect(fill = "white", color = NA)) 
}

private_cause_map <- plot_map('private_cause.R', private_cause,2006, 2023, 3,'Adoption of Private Cause of Action', "Greys")

ag_notice_map <- plot_map('ag_notice.R', ag_notice, 2005, 2020, 2, 'Adoption of Notification to Regulators', "Greys")

cra_notice_map <- plot_map('cra_notice.R', cra_notice, 2005, 2020, 2, 'Adoption of Notification to Credit Reporting Agencies', "Greys")

harm_analysis_map <- plot_map('harm_analysis.R', harm_analysis, 2003, 2018, 2, 'Adoption of Exemption for Low Risk of Harm', "Greys")

#online_posting_map <- plot_map('online_posting.R', online_posting, 2007, 2020, 2, 'Adoption of Requirement to Post Breach Notices Online', "Greys")

encryption_exception_map <- plot_map('encryption_exception.R', encryption_exception, 2006, 2018, 2, 'Adoption of Application of Breach Notification Requirement to Encrypted Data', "Greys")


# Adjust the file paths and names
ggsave(filename = here('images/bw_images', 'did_example_bw.png'),
       plot = good_fit,
       device = 'png',
       scale = 1,
       width = 4512,
       height = 2072,
       units = 'px')

ggsave(filename = here('images/bw_images', 'bad_did_example_bw.png'),
       plot = bad_fit,
       device = 'png',
       scale = 1,
       width = 4512,
       height = 2072,
       units = 'px')

ggsave(filename = here('images/bw_images', 'syn_example_bw.png'),
       plot = mass_syn_example,
       device = 'png',
       scale = 1,
       width = 4512,
       height = 2072,
       units = 'px')

ggsave(filename = here('images/bw_images', 'multi_syn_example_bw.png'),
       plot = multi_syn_example,
       device = 'png',
       scale = 1,
       width = 4512,
       height = 2072,
       units = 'px')

ggsave(filename = here('images/bw_images', 'first_law_map_bw.png'),
       plot = first_law_map,
       device = 'png',
       scale = 1,
       width = 4512,
       height = 2072,
       units = 'px')

ggsave(filename = here('images/bw_images', 'private_cause_map_bw.png'),
       plot = private_cause_map,
       device = 'png',
       scale = 1,
       width = 4512,
       height = 2072,
       units = 'px')

ggsave(filename = here('images/bw_images', 'ag_notice_map_bw.png'),
       plot = ag_notice_map,
       device = 'png',
       scale = 1,
       width = 4512,
       height = 2072,
       units = 'px')

ggsave(filename = here('images/bw_images', 'cra_notice_map_bw.png'),
       plot = cra_notice_map,
       device = 'png',
       scale = 1,
       width = 4512,
       height = 2072,
       units = 'px')

ggsave(filename = here('images/bw_images', 'harm_analysis_map_bw.png'),
       plot = harm_analysis_map,
       device = 'png',
       scale = 1,
       width = 4512,
       height = 2072,
       units = 'px')

ggsave(filename = here('images/bw_images', 'encryption_exception_map_bw.png'),
       plot = encryption_exception_map,
       device = 'png',
       scale = 1,
       width = 4512,
       height = 2072,
       units = 'px')


### Convert Originals ###

convert_to_grayscale <- function(input_folder, output_folder) {
  dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)
  
  file_list <- list.files(input_folder, pattern = "\\.png$", full.names = TRUE)
  
  for (filename in file_list) {
    tryCatch({
      img <- readPNG(filename)
      
      if (dim(img)[3] == 3) {  # Check if the image is RGB
        gray_img <- 0.2989 * img[, , 1] + 0.5870 * img[, , 2] + 0.1140 * img[, , 3]
      } else {
        gray_img <- img  # Already grayscale
      }
      
      # Add "_bw" to the new image names
      new_filename <- sub("\\.png$", "_bw.png", basename(filename))
      
      output_path <- file.path(output_folder, new_filename)
      writePNG(gray_img, output_path)
    }, error = function(e) {
      cat("Error processing:", filename, "\n")
    })
  }
}

input_folder <- here('images')
output_folder <- here('images', 'bw_images_paper')

convert_to_grayscale(input_folder, output_folder)


