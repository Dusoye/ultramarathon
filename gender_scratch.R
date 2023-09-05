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
  filter(Athlete_gender == 'F') %>%
  ungroup() %>% group_by(race_location, Event_name) %>%
  summarise(Event_count = n(),
            avg_female = mean(percent),
            count_female = sum(n)) %>%
  filter(count_female>=5) %>%
  View()



