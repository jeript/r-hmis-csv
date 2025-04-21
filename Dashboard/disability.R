library(dplyr)
library(lubridate)


### Add Disability Descriptions
Disabilities_Data <- Disabilities_Data %>%
  mutate(DisabilityDescription = disability_type(DisabilityType))

Disabilities_Data <- Disabilities_Data %>%
  rename(DisabilityInfoDate = InformationDate)
  

###Remove columns
Disabilities_Data <- Disabilities_Data %>%
  select(-DisabilitiesID, -DisabilityType, -DisabilityResponse, -DataCollectionStage)


Disabilities_Data <- merge(Client_Entry_Data[, c("EnrollmentID", "EntryDate", "ExitDate")], 
                     Disabilities_Data, 
                     by = "EnrollmentID", 
                     all.y = TRUE)


convert_csv_to_rds <- function(data, output_path) {
  saveRDS(data, output_path)
  message("Saved RDS to ", output_path)
}

convert_csv_to_rds(Disabilities_Data, "Dashboard/Disabilities_Data.rds")
