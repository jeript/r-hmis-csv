library(lubridate)
library(tidyverse)

###Date Functions

###Calculate Days Homeless between Approximate Date and Entry Date
Client_Entry_Data <- Client_Entry_Data %>%
  mutate(DaysHomelessBeforeEntry = ifelse(is.na(DateToStreetESSH), 
                                          as.numeric(EntryDate), 
                                          as.numeric(difftime(EntryDate, DateToStreetESSH, units = "days"))))


###Calculate Days Spent Homeless in Project
Client_Entry_Data <- Client_Entry_Data %>%
  mutate(DaysHomelessInProject = difftime(ymd(ExitAdjust),
                                          ymd(EntryDate),
                                          units = "days"))



###Find the Most Recent Entry Date
Client_Entry_Data <- Client_Entry_Data %>%
  group_by(PersonalID) %>%
  mutate(MostRecentEntry = max(EntryDate, na.rm = TRUE)) %>%
  ungroup()

###Find age based on HMIS reporting logic
Client_Entry_Data <- Client_Entry_Data %>%
  mutate(ClientAge = ifelse(is.na(DOB), 
                            NA,
                            ifelse(EntryDate < Export_Data$ExportStartDate,
                                   as.numeric(floor(as.period(interval(DOB, Export_Data$ExportStartDate)) / years(1))),
                                   as.numeric(floor(as.period(interval(DOB, MostRecentEntry)) / years(1))))))


