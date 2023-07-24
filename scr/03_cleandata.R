data.prep <- function(data){
  names(data) <- gsub(" ", "_", names(data)) #replace spaces from column names
  
  dataout <- data %>%
    filter(!`Event_distance/length` %like% '/') %>% #filter out stage races
    select(-c(Event_dates,  Athlete_club)) #columns not required
}

race.details <- function(data){
  dataout <- data %>%
    mutate(race_location = str_extract(Event_name, "\\(([^()]*)\\)(?![^()]*\\()"), 
           race_location = str_replace_all(race_location, "[()]", ""),
           race_distance = as.numeric(str_extract(`Event_distance/length`, "[0-9.]+")),
           race_unit = tolower(str_extract(`Event_distance/length`, "[a-zA-Z]+")),
           race_distance = as.numeric(race_distance),
           race_type = if_else(race_unit %in% c('h', 'd'), 'time', 'distance'),
           race_unit = if_else(race_type == 'time', race_unit, 
                               if_else(str_starts(race_unit, 'k'), 'km',
                                       if_else(str_starts(race_unit, 'm'), 'm', 'other'))),
           distance_km = if_else(race_unit == 'km', race_distance, if_else(race_unit == 'm', race_distance*1.62, NA)),
           `Event_distance/length` = NULL)
  
  return(dataout)
}

athlete.details <- function(data){
  dataout <- data %>%
    mutate(numeric_string = str_extract(Athlete_performance, "\\d+[:d.]+\\d+[:.]*\\d*"),
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
          athlete_age = as.numeric(Year_of_event) - as.numeric(Athlete_year_of_birth),
          days = NULL,
          time = NULL,
          numeric_string = NULL
    ) 
  
  return(dataout)
}


data.clean <- function(data){
  dataout <- data.prep(data) 
  dataout <- race.details(dataout)
  dataout <- athlete.details(dataout)
  
  return(dataout)
}