library(lubridate)
library(tidyverse)

###Find age based on HMIS reporting logic
Client_Entry_Data <- Client_Entry_Data %>%
  mutate(ClientAge = ifelse(is.na(DOB), 
                            NA,
                            ifelse(EntryDate < Export_Data$ExportStartDate,
                                   as.numeric(floor(as.period(interval(DOB, Export_Data$ExportStartDate)) / years(1))),
                                   as.numeric(floor(as.period(interval(DOB, MostRecentEntry)) / years(1))))))



###Find age group
Client_Entry_Data <- Client_Entry_Data %>%
  mutate(AgeGroup = case_when(
    is.na(ClientAge) ~ NA_character_,
    ClientAge < 5 ~ "Under 5",
    ClientAge >= 5 & ClientAge <= 12 ~ "5 to 12",
    ClientAge >= 13 & ClientAge <= 17 ~ "13 to 17",
    ClientAge >= 18 & ClientAge <= 24 ~ "18 to 24",
    ClientAge >= 25 & ClientAge <= 34 ~ "25 to 34",
    ClientAge >= 35 & ClientAge <= 44 ~ "35 to 44",
    ClientAge >= 45 & ClientAge <= 54 ~ "45 to 54",
    ClientAge >= 55 & ClientAge <= 64 ~ "55 to 64",
    ClientAge >= 65 ~ "65 and over",
    TRUE ~ NA_character_
  ))

###Age Group data frame
age_group_counts <- Client_Entry_Data %>%
  group_by(AgeGroup) %>%
  summarise(DistinctPersonalIDCount = n_distinct(PersonalID))

