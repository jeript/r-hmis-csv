library(lubridate)
library(dplyr)

###Date Functions

###Add export start and end
Client_Entry_Data <- Client_Entry_Data %>%
  mutate(ReportStartDate = Export_Data$ExportStartDate)

Client_Entry_Data <- Client_Entry_Data %>%
  mutate(ReportEndDate = Export_Data$ExportEndDate)

###Find the Most Recent Entry Date
Client_Entry_Data <- Client_Entry_Data %>%
  group_by(PersonalID) %>%
  mutate(MostRecentEntry = max(EntryDate, na.rm = TRUE)) %>%
  ungroup()
