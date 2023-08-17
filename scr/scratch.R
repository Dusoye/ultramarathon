############
# data prep
############

data <- fread("./data/TWO_CENTURIES_OF_UM_RACES.csv")
names(data) <- gsub(" ", "_", names(data))

(sapply(data, function(x) mean(is.na(x) | x == '')*100)) %>% as.data.frame(.)

data %>%
  filter(!`Event_distance/length` %like% '/') %>% #filter out stage races
  mutate(race_location = str_extract(Event_name, "\\(([^()]*)\\)(?![^()]*\\()"), 
         race_location = str_replace_all(race_location, "[()]", ""), #location of race from event name
         race_distance = as.numeric(str_extract(`Event_distance/length`, "[0-9.]+")), #distance or time of event
         race_unit = tolower(str_extract(`Event_distance/length`, "[a-zA-Z]+")), 
         race_distance = as.numeric(race_distance),
         race_type = if_else(race_unit %in% c('h', 'd'), 'time', 'distance'), #split out distance races from timed
         race_unit = if_else(race_type == 'time', race_unit, 
                        if_else(str_starts(race_unit, 'k'), 'km',
                        if_else(str_starts(race_unit, 'm'), 'm', 'other'))),
         distance_km = if_else(race_unit == 'km', race_distance, if_else(race_unit == 'm', race_distance*1.62, NA)), #convert miles into km
         athlete_age = as.numeric(Year_of_event) - as.numeric(Athlete_year_of_birth)) %>%
  mutate( #standardise athlete performance values
         numeric_string = str_extract(Athlete_performance, "\\d+[:d.]+\\d+[:.]*\\d*"),
         athlete_units = str_extract(Athlete_performance, "[a-zA-Z ]+"),
         days = if_else(str_detect(Athlete_performance, "d"), 
                        as.numeric(str_extract(Athlete_performance, "\\d+(?=d)")), 
                        0),
         time = if_else(str_detect(Athlete_performance, ":"), 
                        as.duration(hms(str_extract(Athlete_performance, "(?<=d |^)\\d+:\\d+:\\d+"))), 
                        as.duration(0)),
         athlete_duration = as.duration(days(days)) + time,
         athlete_distance = case_when(
           str_detect(athlete_units, "km") ~ as.numeric(str_replace(numeric_string, "km", "")),
           TRUE ~ NA_real_
         ),
         days = NULL,
         time = NULL,
         numeric_string = NULL
  ) %>% 
  select(-c(Event_dates, `Event_distance/length`, Athlete_club)) -> data_clean

### participation

data_clean %>%
  group_by(Year_of_event, race_type) %>%
  distinct(Event_name, race_type) %>% 
  count(Year_of_event, race_type) %>%
  ggplot(aes(x = Year_of_event, y = n, colour = race_type)) +
  theme_minimal() +
  geom_point() +
  scale_y_continuous(labels = comma) +
  ggtitle('Number of events per year') -> events.p

data_clean %>%
  count(Year_of_event, race_type) %>%
  ggplot(aes(x = Year_of_event, y = n, colour = race_type)) +
  theme_minimal() +
  geom_point() +
  scale_y_continuous(labels = comma) +
  ggtitle('Number of participants per year') -> athletes.p

data_clean %>%
  count(Year_of_event, race_type, Athlete_ID) %>% 
  group_by(Year_of_event, race_type) %>%
  summarise(average_races = mean(n)) %>%
  ggplot(aes(x = Year_of_event, y = average_races, colour = race_type)) +
  theme_minimal() +
  geom_point() +
  scale_y_continuous(labels = comma) +
  ggtitle('Average number of races per athlete') -> avgrace.p

data_clean %>%
  distinct(Year_of_event, race_type, Event_name, Event_number_of_finishers) %>% 
  group_by(Year_of_event, race_type) %>%
  summarise(average_athletes = mean(Event_number_of_finishers)) %>%
  ggplot(aes(x = Year_of_event, y = average_athletes, colour = race_type)) +
  theme_minimal() +
  geom_point() +
  scale_y_continuous(labels = comma) +
  ggtitle('Average number of athletes per race') -> avgathlete.p

grid.arrange(events.p, athletes.p, ncol = 2)
grid.arrange(avgathlete.p, avgrace.p, ncol = 1)

data_clean %>%
  distinct(Year_of_event, Event_name, race_distance, race_unit, race_type) %>%
  mutate(race_distance = paste0(race_distance, " ", race_unit)) %>%
  count(race_distance) %>%
  arrange(desc(n)) %>%
  head(n = 10)
#gender shift
data_clean %>%
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
  geom_line(data = newdata, aes(y = 1-pred, x = Year_of_event, fill = NULL), colour = 'red') +
  scale_y_continuous(labels = percent)
  
# highest & lowest female participation
data_clean %>%
  filter(Year_of_event >= 2017,
         Athlete_gender %in% c('M','F')) %>%
  group_by(Athlete_gender, race_location, Event_name) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(race_location) %>%
  summarise(event_count = n_distinct(Event_name),
            female_percent = sum(if_else(Athlete_gender == 'F', n, 0)) / sum(n)) %>% 
  ungroup() %>%
  filter(event_count > 1) %>%
  arrange(desc(female_percent)) %>%
  mutate(n = row_number()) %>% 
  filter(n < 6 | n > (nrow(.) - 5)) %>%
  dplyr::select(-n)
  
race_locations <- c('FRA','ESP','USA','GBR','GER','ITA','RSA')
data_clean_1950 %>%
  filter(Athlete_gender %in% c('M','F'),
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
  scale_y_continuous(labels = percent)


gender_pace <- data_clean_1950 %>%
  mutate(race_distance = paste0(race_distance, race_unit),
         event = paste0(Year_of_event, '_', Event_name)) %>% 
  filter(race_distance %in% c('50km','100km','50m','100m'),
         Athlete_gender %in% c('F','M')) %>% 
  group_by(Year_of_event, race_distance, event, Athlete_gender) %>%
  summarise(fastest_time = min(athlete_duration),
            average_time = mean(athlete_duration)) %>%
  pivot_wider(names_from = Athlete_gender, values_from = c(fastest_time, average_time)) %>%
  mutate(fastest_diff = (fastest_time_F - fastest_time_M)/fastest_time_M,
         average_diff = (average_time_F - average_time_M)/average_time_M) %>%
  ungroup()

gender_pace %>% 
  mutate(female_winner = if_else(fastest_time_F < fastest_time_M,1,0)) %>% View()
  group_by(Year_of_event) %>%
  summarise(female_winners = sum(female_winner, na.rm = TRUE),
            female_winners_perc = female_winners/n()) %>% View()
  ggplot(aes(x = Year_of_event, y = female_winners_perc)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  scale_y_continuous(labels = percent) +
  ggtitle('percent of races with female winner')

gender_pace %>%
  group_by(Year_of_event, race_distance) %>%
  summarise(fastest_diff = mean(fastest_diff, na.rm = TRUE),
            average_diff = mean(average_diff, na.rm = TRUE)) %>% 
  ggplot(aes(x = Year_of_event, y = fastest_diff, colour = race_distance)) +
  geom_line() +
  theme_minimal() +
  ylab('percentage') +
  scale_y_continuous(labels = percent) +
  ggtitle('percentage difference of winning female to male finishing time') -> fast.female.p

gender_pace %>%
  group_by(Year_of_event, race_distance) %>%
  summarise(fastest_diff = mean(fastest_diff, na.rm = TRUE),
            average_diff = mean(average_diff, na.rm = TRUE)) %>% 
  ggplot(aes(x = Year_of_event, y = average_diff, colour = race_distance)) +
  geom_line() +
  theme_minimal() +
  ylab('percentage') +
  scale_y_continuous(labels = percent) +
  ggtitle('percentage difference of average female to male finishing time') -> avg.female.p

data_clean %>%
  filter(Year_of_event >= 1950,
         Athlete_gender %in% c('M','F')) %>%
  count(Year_of_event, Athlete_gender) %>%
  group_by(Year_of_event) %>%
  mutate(percent = n/sum(n)) %>% 
  filter(Athlete_gender == 'F') -> female_perc

#lm(percent ~ Year_of_event, data = female_perc, family = binomial) -> model

#install.packages("segmented") 
library(segmented)

lin.mod <- lm(percent ~ Year_of_event, data = female_perc)
seg.mod <- segmented::segmented(lin.mod, seg.Z = ~Year_of_event, psi = c(1980, 2000))

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
  geom_line(data = newdata, aes(y = 1-pred, x = Year_of_event, fill = NULL), colour = 'red')

# Extend dataframe with future years
future_years <- data.frame(Year_of_event = seq(max(female_perc$Year_of_event) + 1, max(female_perc$Year_of_event) + 80))

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
  theme_minimal() 

###

data_clean %>%
  distinct(Year_of_event, Event_name) %>%
  count(Event_name) %>%
  arrange(desc(n)) %>%
  filter(n >= 10) -> multiple_events

data_clean %>%
  filter(Year_of_event >= 1950) %>% 
  filter(Event_name %in% multiple_events$Event_name) %>%
  group_by(Year_of_event, Event_name, Athlete_gender) %>%
  summarise(average_speed = mean(athlete_duration, na.rm = TRUE)) %>% 
  filter(Event_name == 'Comrades Marathon - Down Run (RSA)') %>%
  ggplot(aes(x = Year_of_event, y = average_speed/(60*60), colour = Athlete_gender)) +
  geom_point() +
  theme_minimal()


data_clean %>%
  filter(Year_of_event >= 1950) %>% filter(Event_name == 'Comrades Marathon - Down Run (RSA)') %>% 
  group_by(Year_of_event) %>%
  head(10) %>% View()

age_groups = data.frame(age_breaks = c(0,seq.int(20,70, by = 5), Inf))

data_clean_1950 %>%
  mutate(age_group = cut(athlete_age, age_groups$age_breaks)) %>%
  count(Year_of_event, age_group) %>%
  filter(complete.cases(.)) %>% 
  ungroup() %>% group_by(Year_of_event) %>%
  mutate(percent = n/sum(n)) %>%
  ggplot(aes(x = Year_of_event, y = percent, fill = age_group)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  ggtitle('athlete ages')


data_clean_1950 %>%
  filter(race_location %in% race_locations) %>%
  group_by(Year_of_event, race_location) %>%
  distinct(Athlete_country) %>%
  count(Year_of_event, race_location) %>%
  ggplot(aes(x = Year_of_event, y = n, fill = race_location)) +
  theme_minimal() +
  geom_bar(stat = 'identity', position = position_dodge(width = 0)) +
  ggtitle('nationalities per race')

data_clean_1950 %>%
  filter(race_location %in% race_locations) %>%
  group_by(Year_of_event, race_location) %>%
  distinct(Athlete_country) %>%
  count(Year_of_event, race_location) %>% View()
  ggplot(aes(x = Year_of_event, y = n, colour = race_location)) +
  theme_minimal() +
  geom_point() +
  ggtitle('participant nationalities per race location')


data_clean_1950 %>%
  filter(race_location %in% race_locations) %>%
  mutate(foreign_race = if_else(Athlete_country == race_location, 0, 1)) %>%
  group_by(Year_of_event, race_location) %>% 
  summarise(foreign_participation = sum(foreign_race)/n()) %>% 
  ggplot(aes(x = Year_of_event, y = foreign_participation, colour = race_location)) +
  theme_minimal() +
  geom_point() +
  ggtitle('foreign participation per race')
  
data_clean %>%
  filter(Year_of_event == 2019) %>%
  group_by(Year_of_event, Event_name, race_location) %>%
  distinct(Athlete_country) %>%
  count(Year_of_event, Event_name, race_location) %>%
  arrange(desc(n)) %>% head(n = 20)

utmb <- distinct(data_clean_1950, Event_name) %>% filter(Event_name %like% c('(CCC)','(UTMB)','(TDS)','(OCC)'))

data_clean %>%
  filter(Event_name %in% utmb$Event_name,
         Year_of_event >= 1950) %>%
  mutate(foreign_race = if_else(Athlete_country %in% c('FRA', 'ITA', 'SUI'), 0, 1)) %>%
  group_by(Year_of_event, Event_name) %>% 
  summarise(foreign_participation = sum(foreign_race)/n()) %>% 
  ggplot(aes(x = Year_of_event, y = foreign_participation, colour = Event_name)) +
  theme_minimal() +
  geom_point() +
  ggtitle('foreign participation per race utmb')

data_clean %>%
  mutate(location = if_else(race_location == 'GER', 'GER','RoW')) %>%
  count(location, Athlete_age_category) %>%
  pivot_wider(names_from = location, values_from = n) %>%
  head(n = 10)

data %>%
  group_by(Year_of_event) %>%
  summarise(missing = sum(if_else((is.na(Athlete_year_of_birth) | Athlete_year_of_birth == ""), 1, 0)),
            perc_missing = missing/n()) %>% 
  arrange(desc(missing)) %>%
  head(n = 20)


data_clean %>%
  select(Year_of_event, Event_name) %>%
  group_by(Event_name) %>%
  summarise(first_event = min(Year_of_event),
            last_event = max(Year_of_event),
            event_count = length(unique(paste0(Year_of_event,Event_name))),
            athlete_count = n(),
            average_athletes = athlete_count/event_count) %>%
  View()

data_clean %>%
  distinct(Year_of_event, Event_name, Event_number_of_finishers) %>%
  arrange(desc(Event_number_of_finishers)) %>%
  head(n = 10)

data_clean %>%
  distinct(Year_of_event, Event_name, race_location, Event_number_of_finishers) %>%
  group_by(race_location) %>%
  summarise(first_event = min(Year_of_event),
            different_events = length(unique(Event_name)),
            total_events = n(),
            total_athletes = sum(Event_number_of_finishers)) %>%
  arrange(desc(total_athletes)) %>%
  head(n = 10)


# athletes

data_clean %>%
  filter(Year_of_event >= 1950) %>%
  group_by(Athlete_country) %>%
  summarise(athlete_count = length(unique(Athlete_ID)),
            total_finishers = n()) %>%
  add_row(Athlete_country = 'Total', 
          athlete_count = sum(.$athlete_count),
          total_finishers = sum(.$total_finishers)) %>%
  mutate(avg_races = total_finishers/athlete_count) %>%
  arrange(desc(athlete_count)) %>%
  head(n = 11)

data_clean_1950 %>%
  group_by(Athlete_ID) %>%
  summarise(race_count = n()) %>%
  arrange(desc(race_count))

data_clean_1950 %>%
  filter(Athlete_ID == 236) %>%
  mutate(race_distance = paste0(race_distance, "_", race_unit)) %>%
  count(Year_of_event, race_location) %>%
  arrange(desc(Year_of_event))


data_clean_1950 %>%
  filter(Athlete_ID == 236) %>%
  mutate(race_distance = paste0(race_distance, " ", race_unit)) %>%
  count(race_distance) %>%
  arrange(desc(n))

data_clean_1950 %>%
  mutate(race_distance = paste0(race_distance, race_unit)) %>%
  filter(race_distance %in% c('50km','100km','50m','100m')) %>%
  group_by(Year_of_event, race_distance) %>%
  summarise(quickest_time = as.duration(min(athlete_duration)),
            average_time = mean(athlete_duration)) %>% 
  ggplot(aes(x = Year_of_event, y = quickest_time/(60*60), colour = race_distance)) +
  geom_line() +
  theme_minimal() +
  ylab('time (h)') +
  ggtitle('fastest time')
  

data_clean_1950 %>%
  mutate(race_distance = paste0(race_distance, race_unit)) %>%
  filter(race_distance %in% c('50km','100km','50m','100m')) %>%
  group_by(Year_of_event, race_distance) %>%
  summarise(quickest_time = as.duration(min(athlete_duration)),
            average_time = mean(athlete_duration)) %>% 
  ggplot(aes(x = Year_of_event, y = average_time/(60*60), colour = race_distance)) +
  geom_line() +
  theme_minimal() +
  ylab('time (h)') +
  ggtitle('average time')

library(finalfit)
ff_glimpse(data)
missing_plot(data)
missing_pattern(data_clean)

data_clean %>%
  mutate(speed = if_else(race_type == 'distance', distance_km/(as.numeric(athlete_duration) / (60 * 60)),
                         athlete_distance * if_else(athlete_units == 'km',1,1.62) / (race_distance * if_else(race_unit == 'd', 24, 1))),
         speed_error = if_else(speed >= 20 | athlete_duration == 0, TRUE, FALSE)) %>%
  arrange(desc(speed)) %>%
  View()
