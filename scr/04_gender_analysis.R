data_clean <- data.clean(raw_data)
rm(raw_data)

#######
# gender distribution plots
#######
female_perc <- data_clean %>%
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
data_clean %>%
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
  theme_tufte() +
  ggtitle('Gender divide') +
  scale_y_continuous(labels = percent) -> genderpercpredict.p

data_clean %>%
  filter(Year_of_event >= 1950,
         Athlete_gender %in% c('M','F')) %>%
  count(Year_of_event, Athlete_gender) %>%
  group_by(Year_of_event) %>%
  mutate(percent = n/sum(n)) %>%
  ggplot(aes(x = Year_of_event, y = percent, fill = Athlete_gender)) +
  geom_bar(stat = 'identity') +
  theme_tufte() +
  labs(x = 'year', y = 'percentage', fill = 'gender') +
  ggtitle('Gender divide') +
  scale_y_continuous(labels = percent) -> genderperc.p


race_locations <- c('FRA','ESP','USA','GBR','GER','ITA','RSA')
data_clean %>%
  filter(Year_of_event >= 1950,
         Athlete_gender %in% c('M','F'),
         race_location %in% race_locations) %>%
  count(Year_of_event, Athlete_gender, race_location) %>%
  group_by(Year_of_event, race_location) %>%
  mutate(percent = n/sum(n)) %>% 
  filter(Athlete_gender == 'F') %>% View()
  ggplot(aes(x = Year_of_event, y = percent, colour = race_location)) +
  geom_line() +
  theme_minimal() +
  labs(x = 'year', y = 'percentage female') +
  ggtitle('Gender divide') +
  scale_y_continuous(labels = percent) -> genderperccountry.p


female_perc <- data_clean %>%
  filter(Year_of_event >= 1990,
         Athlete_gender %in% c('M','F')) %>%
  count(Year_of_event, race_location, Athlete_gender) %>%
  group_by(Year_of_event, race_location) %>%
  mutate(percent = n/sum(n)) %>% 
  filter(Athlete_gender == 'F') %>%
  merge(., race_count, by = c('Year_of_event', 'race_location'), all = TRUE) %>%
  filter(event_count > 5)

# Filter for the year 2022 and gender 'F'
filtered_data <- female_perc %>% filter(Year_of_event == 2019 & Athlete_gender == 'F')