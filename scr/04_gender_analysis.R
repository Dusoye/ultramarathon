data <- data_clean(raw_data)
rm(raw_data)


#####################
# overall participation
#####################

data %>% 
  filter(Year_of_event>2000) %>% 
  group_by(Year_of_event) %>% 
  summarise(events = length(unique(Event_name)), runners = n()) -> stats_year

data %>%
  group_by(Year_of_event, race_type) %>%
  distinct(Event_name, race_type) %>% 
  count(Year_of_event, race_type) %>%
  ggplot(aes(x = Year_of_event, y = n, colour = race_type)) +
  theme_minimal() +
  geom_point() +
  scale_y_continuous(labels = comma) +
  ggtitle('Number of events per year') -> events.p

data %>%
  count(Year_of_event, race_type) %>%
  ggplot(aes(x = Year_of_event, y = n, colour = race_type)) +
  theme_minimal() +
  geom_point() +
  scale_y_continuous(labels = comma) +
  ggtitle('Number of participants per year') -> athletes.p

grid.arrange(events.p, athletes.p, ncol = 1)

#####################
# gender distribution plots
#####################

# female participation by year 
female_perc <- data %>%
  filter(Athlete_gender %in% c('M','F'),
         Year_of_event >= 1950) %>%
  count(Year_of_event, Athlete_gender) %>%
  group_by(Year_of_event) %>%
  mutate(percent = n/sum(n)) %>% 
  filter(Athlete_gender == 'F')

# segment the linear model
lin.mod <- lm(percent ~ Year_of_event, data = female_perc)
seg.mod <- segmented::segmented(lin.mod, seg.Z = ~Year_of_event, psi = c(1980, 2000))

future_years <- data.frame(Year_of_event = seq(max(female_perc$Year_of_event) + 1, max(female_perc$Year_of_event) + 90))

# Predict proportions for future years
future_years$percent <- predict(seg.mod, newdata = future_years, type = "response")
future_years$source <- 'predicted'

# Combine past data with future predictions
data %>%
  filter(Year_of_event >= 1950,
         Athlete_gender %in% c('M','F')) %>%
  count(Year_of_event, Athlete_gender) %>%
  group_by(Year_of_event) %>%
  mutate(percent = n/sum(n)) %>% 
  filter(Athlete_gender == 'F') %>%
  mutate(source = 'observed') %>%
  dplyr::select(Year_of_event, percent, source) %>% 
  rbind(., future_years) %>%
  mutate(Male = 1-percent,
         Female = percent) %>% 
  dplyr::select(-percent) %>%
  pivot_longer(cols = c('Male', 'Female'), names_to = 'Gender', values_to = 'percent') %>% 
  mutate(gender_source = paste0(substr(Gender,1,1),'_', source)) %>%
  ggplot(aes(x = Year_of_event, y = percent, fill = gender_source)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("F_observed" = "#F8766D", "M_observed" = "#00BFC4", 
                               "F_predicted" = "#FFCCCC", "M_predicted" = "#33FFFF")) +
  theme_minimal() +
  ggtitle('Gender divide') +
  scale_y_continuous(labels = percent) -> genderpercpredict.p

# plot observed
data %>%
  filter(Year_of_event >= 1950,
         Athlete_gender %in% c('M','F')) %>%
  count(Year_of_event, Athlete_gender) %>%
  group_by(Year_of_event) %>%
  mutate(percent = n/sum(n)) %>%
  ggplot(aes(x = Year_of_event, y = percent, fill = Athlete_gender)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'year', y = 'percentage', fill = 'gender') +
  ggtitle('Gender divide') +
  scale_y_continuous(labels = percent) -> genderperc.p

# participation rates by country
race_locations <- c('FRA','ESP','USA','GBR','GER','AUS','RSA','NZL')
data %>%
  filter(Year_of_event >= 1950,
         Athlete_gender %in% c('M','F'),
         race_location %in% race_locations) %>%
  count(Year_of_event, Athlete_gender, race_location) %>%
  group_by(Year_of_event, race_location) %>%
  mutate(percent = n/sum(n)) %>% 
  filter(Athlete_gender == 'F') %>% 
  ggplot(aes(x = Year_of_event, y = percent, colour = race_location)) +
  geom_line() +
  theme_minimal() +
  labs(x = 'year', y = 'percentage female') +
  ggtitle('Gender divide') +
  scale_y_continuous(labels = percent) -> genderperccountry.p

## geospatial gender percentage gif
race_count <- data %>%
  filter(Year_of_event >= 1990,
         Athlete_gender %in% c('M','F')) %>%
  distinct(Year_of_event, race_location, Event_name) %>%
  count(Year_of_event, race_location) %>%
  rename(event_count = n)

female_perc_country <- data %>%
  filter(Year_of_event >= 1990,
         Athlete_gender %in% c('M','F')) %>%
  count(Year_of_event, race_location, Athlete_gender) %>%
  group_by(Year_of_event, race_location) %>%
  mutate(percent = n/sum(n)) %>% 
  filter(Athlete_gender == 'F') %>%
  merge(., race_count, by = c('Year_of_event', 'race_location'), all = TRUE) %>%
  merge(., ioc_codes, by.x = 'race_location', by.y = 'code') %>% #join ioc names to more accurately join to geospatial data
  filter(event_count > 2) %>%
  arrange(Year_of_event, race_location)

#filtered_data <- female_perc_country %>% filter(Year_of_event == 2022 & Athlete_gender == 'F')

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>% filter(sovereignt != 'Antarctica')

years <- unique(female_perc_country$Year_of_event)

for(i in years){
  filtered_data <- female_perc_country %>% filter(Year_of_event == i & Athlete_gender == 'F')
  
  # Merge the data based on ISO country codes if exists, if not join on country name
  iso_merged_data = left_join(world, filtered_data, by = c("iso_a3" = "race_location"))
  country_merged_data <- left_join(world[is.na(iso_merged_data$percent), ], filtered_data, by = c("name" = "country"))
  merged_data <- bind_rows(iso_merged_data, country_merged_data)
  
  ggplot(data = merged_data) +
    geom_sf(aes(fill = percent)) +
    scale_fill_gradientn(colors = rev(viridis::inferno(7)), limits = c(0, 0.5), name = "Percentage") +
    theme_minimal() +
    ggtitle(paste0("Female participant Heatmap ", i)) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          legend.title=element_text(size=10),
          legend.text=element_text(size=5),
          legend.position="right")-> p
  
  ggsave(paste("./output/maps/", i, ".png", sep=""), plot = p, width=12, height=5)
}

img_files <- paste0("./output/maps/", years, ".png")
animation <- image_read(img_files)
frames = c()

for (i in length(img_files):1) {
  x = image_read(img_files[i])
  c(x, frames) -> frames
}

animation = image_animate(frames, fps = 2, dispose = "previous")
image_write(animation, "./output/participation_map.gif")


#####################
# performance
#####################

data %>%
  filter(Year_of_event >= 1980) %>%
  mutate(race_distance = paste0(race_distance, race_unit)) %>%
  filter(race_distance %in% c('50km','100km','50m','100m')) %>%
  group_by(Year_of_event, race_distance) %>%
  summarise(quickest_time = as.duration(min(athlete_duration)),
            average_time = mean(athlete_duration)) %>% 
  ggplot(aes(x = Year_of_event, y = average_time/(60*60), colour = race_distance)) +
  geom_line() +
  theme_minimal() +
  ylab('time (h)') +
  ggtitle('average time') -> avg.time.p

gender_pace <- data %>%
  filter(Year_of_event >= 1980, race_type == 'distance') %>%
  mutate(race_distance = paste0(race_distance, race_unit),
         event = paste0(Year_of_event, '_', Event_name)) %>% 
  filter(race_distance %in% c('50km','100km','50m','100m'),
         Athlete_gender %in% c('F','M')) %>% 
  group_by(Year_of_event, race_distance, event, Athlete_gender) %>%
  summarise(fastest_time = min(athlete_duration),
            average_time = mean(athlete_duration)) %>%
  pivot_wider(names_from = Athlete_gender, values_from = c(fastest_time, average_time)) %>%
  mutate(event = str_replace(event, "^[^_]*_", "")) %>%
  ungroup() %>% group_by(event) %>%
  mutate(average_diff_f = (average_time_F - first(average_time_F))/average_time_F,
         average_diff_m = (average_time_M - first(average_time_M))/average_time_M) %>%
  ungroup()

gender_pace %>%
  select(Year_of_event, race_distance, female = average_diff_f, male = average_diff_m) %>%
  pivot_longer(cols = c(female,male), names_to ='gender', values_to = 'difference') %>% 
  group_by(Year_of_event, gender) %>%
  summarise(percent_diff = mean(difference, na.rm = TRUE)) %>%
  #mutate(gender_dist = paste0(race_distance, '_', gender)) %>%
  ggplot(aes(x=Year_of_event, y = percent_diff, colour = gender)) +
  geom_line() +
  theme_minimal()

