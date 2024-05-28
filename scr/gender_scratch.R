country.regression <- function(data, country){
  tmpdata <- filter(data, race_location == country)
  
  percdata <- tmpdata %>%
    filter(Year_of_event >= 1950,
           Athlete_gender %in% c('M','F')) %>%
    count(Year_of_event, Athlete_gender) %>%
    group_by(Year_of_event) %>%
    mutate(percent = n/sum(n)) %>% 
    filter(Athlete_gender == 'F') 
  
  lin.mod <- lm(percent ~ Year_of_event, data = percdata)
  seg.mod <- segmented::segmented(lin.mod, seg.Z = ~Year_of_event, npsi = 2)
  
  newdata <- data.frame(Year_of_event = seq(min(percdata$Year_of_event), max(percdata$Year_of_event), length.out = 100))
  
  # Generate predictions
  newdata$pred <- predict(seg.mod, newdata = newdata)
 
  ggplot(percdata, aes(x = Year_of_event, y = percent)) +
    geom_point() +  # plot the data points
    geom_line(data = newdata, aes(y = pred), colour = 'red') +  # plot the regression line
    theme_minimal() +
    ggtitle(paste0('Segmented linear regression of female participation for ',country)) -> p
  
  print(p)
}

country.regression(data_clean, 'NZL')

# Show the summary of the segmented model
summary(seg.mod)

newdata <- data.frame(Year_of_event = seq(min(female_perc$Year_of_event), max(female_perc$Year_of_event), length.out = 100))

# Generate predictions
newdata$pred <- predict(seg.mod, newdata = newdata)

# Plot the data and the regression line
ggplot(female_perc, aes(x = Year_of_event, y = percent)) +
  geom_point() +  # plot the data points
  geom_line(data = newdata, aes(y = pred), colour = 'red') +  # plot the regression line
  theme_minimal() +
  ggtitle('Segmented linear regression of female participation')



data_clean %>%
  filter(Year_of_event >= 2000,
         Athlete_gender %in% c('M','F')) %>%
  count(Event_name, race_location, Athlete_gender) %>%
  group_by(Event_name) %>%
  mutate(percent = n/sum(n)) %>%
  filter(Athlete_gender == 'F', sum(n)>100) %>%
  ungroup() %>% group_by(race_location, Event_name) %>%
  summarise(Event_count = n(),
            avg_female = mean(percent),
            count_female = sum(n)) %>%
  View()


data_clean %>%
  filter(Year_of_event >= 2000, race_type == 'distance') %>%
  mutate(race_distance = distance_km) %>%
  select(-c(Athlete_average_speed, Athlete_performance, race_unit, Athlete_age_category, Athlete_year_of_birth,
            Event_number_of_finishers, Event_dates, athlete_units, athlete_distance, speed)) %>%
  count(Year_of_event, race_distance, Athlete_gender) %>%
  group_by(Year_of_event, race_distance) %>%
  mutate(percent = n/sum(n)) %>% 
  filter(Athlete_gender == 'F') %>%
  arrange(Year_of_event, desc(n)) %>% 
  ungroup() %>% group_by(Year_of_event) %>%
  slice_head(n = 4) %>%
  ggplot(aes(x = Year_of_event, y = percent, fill = as.factor(race_distance), group = race_distance)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_minimal()

data_clean %>%
  filter(Year_of_event >= 2000, race_type == 'distance') %>%
  select(-c(Athlete_average_speed, Athlete_performance, race_distance, race_unit, Athlete_age_category, Athlete_year_of_birth,
            Event_number_of_finishers, Event_dates, athlete_units, Year_of_event, athlete_distance, speed)) %>%
  fwrite(., './output/racedata.csv', sep = ',', row.names = FALSE)




######

library(dplyr)
library(tidyverse)
library(sp)
library(raster)
library(ggplot2)
library(ggthemes)

race_count <- data_clean %>%
  filter(Year_of_event >= 1990,
         Athlete_gender %in% c('M','F')) %>%
  distinct(Year_of_event, race_location, Event_name) %>%
  count(Year_of_event, race_location) %>%
  rename(event_count = n)

female_perc <- data_clean %>%
  filter(Year_of_event >= 1990,
         Athlete_gender %in% c('M','F')) %>%
  count(Year_of_event, race_location, Athlete_gender) %>%
  group_by(Year_of_event, race_location) %>%
  mutate(percent = n/sum(n)) %>% 
  filter(Athlete_gender == 'F') %>%
  merge(., race_count, by = c('Year_of_event', 'race_location'), all = TRUE) %>%
  filter(event_count > 5)

# Load the world map data using the `rnaturalearth` package
world <- ne_countries(scale = "medium", returnclass = "sf")

# Filter for the year 2022 and gender 'F'
filtered_data <- female_perc %>% filter(Year_of_event == 2022 & Athlete_gender == 'F')

# Merge the data based on ISO country codes
merged_data <- left_join(world, filtered_data, by = c("iso_a3" = "race_location"))

# Plot the geospatial heatmap
ggplot(data = merged_data) +
  geom_sf(aes(fill = percent)) +
  scale_fill_gradientn(colors = rev(viridis::inferno(7)), name = "Percentage") +
  theme_minimal() +
  labs(title = "Female participant Heatmap for Year")

years <- unique(female_perc$Year_of_event)

for(i in years){
  filtered_data <- female_perc %>% filter(Year_of_event == i & Athlete_gender == 'F')
  
  # Merge the data based on ISO country codes
  merged_data <- left_join(world, filtered_data, by = c("iso_a3" = "race_location"))

  ggplot(data = merged_data) +
    geom_sf(aes(fill = percent)) +
    scale_fill_continuous(limits = c(0, 0.5)) +
    scale_fill_gradientn(colors = rev(viridis::inferno(7)), name = "Percentage") +
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
  
  ggsave(paste("./output/maps/", i, ".png", sep=""), plot = p)
}

library(magick)
img_files <- paste0("./output/maps/", years, ".png")
animation <- image_read(img_files)
frames = c()

for (i in length(img_files):1) {
  x = image_read(img_files[i])
  c(x, frames) -> frames
}

animation = image_animate(frames, fps = 2)
image_write(animation, "./output/tmp.gif")

image_animate(animation, "animation.gif", delay = 100) 

system("convert -delay 80 ./output/maps/*.png ./output/maps/maps_over_time.gif")

file.remove(list.files(pattern="./output/maps/*"))

p <- ggplot(data = merged_data) +
  geom_sf(aes(fill = percent)) +
  scale_fill_gradientn(colors = rev(viridis::inferno(7)), name = "Percentage") +
  theme_minimal() +
  transition_states(Year_of_event, transition_length = 2, state_length = 1) +
  enter_fade() +
  exit_fade()

anim <- animate(p, duration = 12, fps = 1, rewind = TRUE)
anim

install.packages("highcharter")
library(highcharter)

race_locations <- c('FRA','ESP','USA','GBR','GER','AUS','RSA','NZL')
data_clean %>%
  filter(Year_of_event >= 1950,
         Athlete_gender %in% c('M','F'),
         race_location %in% race_locations) %>%
  count(Year_of_event, Athlete_gender, race_location) %>%
  group_by(Year_of_event, race_location) %>%
  mutate(percent = n/sum(n)) %>% 
  filter(Athlete_gender == 'F') %>% 
  hchart(., "line", hcaes(x = Year_of_event, y = percent, group = race_location)) %>%
  hc_plotOptions(
    series = list(
      marker = list(
        enabled = FALSE  # This disables the point markers
      )
    )
  )


