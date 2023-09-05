data_clean <- data.clean(raw_data)
rm(raw_data)

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
  filter(Athlete_gender == 'F') %>%
  ggplot(aes(x = Year_of_event, y = percent, colour = race_location)) +
  geom_line() +
  theme_minimal() +
  labs(x = 'year', y = 'percentage female') +
  ggtitle('Gender divide') +
  scale_y_continuous(labels = percent) -> genderperccountry.p