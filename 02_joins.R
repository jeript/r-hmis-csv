library(dplyr)
library(lubridate)

############################################################
### This will join Client, Entry, Exit, and Project data ###
############################################################

### Join Client and Enrollment Data
Client_Entry_Data <- Client_Data %>%
  left_join(Enrollment_Data, by = "PersonalID")

### Join Client, Enrollment, and Exit Data
Client_Entry_Data <- Client_Entry_Data %>%
  left_join(Exit_Data, by = c("PersonalID", "EnrollmentID")) %>%
  mutate(ExitAdjust = coalesce(ExitDate, no_end_date),
         EnrollmentDateRange = interval(EntryDate, ExitAdjust))

### Join Client, Enrollment, Exit, and Organization Data
Project_Join <- Organization_Data %>%
  left_join(Project_Data, by = "OrganizationID")

### Join Client, Enrollment, Exit, Organization, and Project Data
Client_Entry_Data <- Client_Entry_Data %>%
  left_join(Project_Join, by = "ProjectID")

#########################################################
### This will join just Project and Organization Data ###
#########################################################

### Join Organization and Project Data
Project_Join <- Organization_Data %>%
  left_join(Project_Data, by = "OrganizationID")

###Inventory filter. This will show rows where the InventoryEndDate is null
###or is before the ExportStartDate.
Inventory_Data_filtered <- Inventory_Data %>%
  filter(is.na(InventoryEndDate) | InventoryEndDate < Export_Data$ExportStartDate)

###This will join Project and Inventory data in a new data frame
###that will differentiate from the previous Project_Join data
ProjectOnly_Data <- Project_Join %>%
  left_join(Inventory_Data_filtered, by = "ProjectID")

###Open funding sources filter
Funder_Data_filtered <- Funder_Data %>%
  filter(is.na(EndDate))

###This will add Funder data to ProjectOnly_Data
ProjectOnly_Data <- Project_Join %>%
  left_join(Funder_Data_filtered, by = "ProjectID")

#########################################
### This will Join Client and CE Data ###
#########################################

### Join Client and Assessment Data
Client_CE_Data <- Client_Data %>%
  left_join(Assessment_Data, by = "PersonalID")


### Join Client, Assessment, and Event Data
Client_CE_Data <- Client_CE_Data %>%
  left_join(Event_Data, by = "PersonalID", relationship = "many-to-many")


