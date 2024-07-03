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




