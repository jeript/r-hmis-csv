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

### Add Living Situation Descriptions
Client_Entry_Data <- Client_Entry_Data %>%
  mutate(LivingSituationDescription = living_situation(LivingSituation),
         DestinationDescription = living_situation(Destination))

### Add Extended Living Situation Descriptions
Client_Entry_Data <- Client_Entry_Data %>%
  mutate(ExtLivingSituationDescription = extended_living_situation(LivingSituation),
         ExtDestinationDescription = extended_living_situation(Destination))

### Add Project Type Descriptions
Client_Entry_Data <- Client_Entry_Data %>%
  mutate(ProjectTypeDescription = project_type(ProjectType))

### Add simplified Project Type Descriptions
Client_Entry_Data <- Client_Entry_Data %>%
  mutate(ProjectTypeDescriptionExt = project_type_ext(ProjectType))
