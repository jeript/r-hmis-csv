library(lubridate)
library(tidyverse)



###This will classify a person's type and is dependent on age only and includes
###youth
Client_Entry_Data$PersonType <- with(Client_Entry_Data, ifelse(is.na(ClientAge), 
                                "Unknown",
                                ifelse(ClientAge < 18, 
                                "Child",
                                ifelse(ClientAge >= 18 & ClientAge <= 24, 
                                "Youth", 
                                "Adult"))))



###Determines household type based on PersonType and household composition
###and separates youth and parenting youth from adults
determine_HHType <- function(df) {
  df %>%
    group_by(HouseholdID) %>%
    mutate(
      HHType = case_when(
        all(PersonType == "Adult") & any(RelationshipToHoH == 1) ~ "Adult",
        any(PersonType == "Adult") & any(PersonType == "Youth") & 
          !any(PersonType == "Child") & any(RelationshipToHoH == 1) ~ "Adult",
        any(PersonType == "Adult") & any(RelationshipToHoH == 1) & 
          any(PersonType == "Child") ~ "Adult Child",
        any(PersonType == "Youth") & any(RelationshipToHoH == 1) & 
          any(PersonType == "Child") & !any(PersonType == "Adult") ~ "Parenting Youth",
        all(PersonType == "Youth") ~ "Youth",
        all(PersonType == "Child") ~ "Child Only",
        TRUE ~ "Unknown"
      )
    ) %>%
    ungroup()
}


###Household Type Count Data Frame
HouseholdDF <- Client_Entry_Data %>%
  group_by(HHType) %>%
  summarise(DistinctPersonalIDCount = n_distinct(PersonalID))

###Determines household type based on PersonType and household composition
###and includes youth and parenting youth in the adult count
determine_APR_HHType <- function(df) {
  df %>%
    group_by(HouseholdID) %>%
    mutate(
      APR_HHType = case_when(
        all(PersonType == "Adult") & any(RelationshipToHoH == 1) ~ "Adult",
        any(PersonType == "Adult") & any(PersonType == "Youth") & 
          !any(PersonType == "Child") & any(RelationshipToHoH == 1) ~ "Adult",
        any(PersonType == "Adult") & any(RelationshipToHoH == 1) & 
          any(PersonType == "Child") ~ "Adult Child",
        any(PersonType == "Youth") & any(RelationshipToHoH == 1) & 
          any(PersonType == "Child") & !any(PersonType == "Adult") ~ "Adult",
        all(PersonType == "Youth") ~ "Adult",
        all(PersonType == "Child") ~ "Child Only",
        TRUE ~ "Unknown"
      )
    ) %>%
    ungroup()
}



###APR Household Type Count Data Frame
HouseholdDF_APR <- Client_Entry_Data %>%
  group_by(APR_HHType) %>%
  summarise(DistinctPersonalIDCount = n_distinct(PersonalID))
