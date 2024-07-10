library(lubridate)
library(dplyr)



###This will classify a person's type and is dependent on age only and includes
###youth
Client_Entry_Data$APRPersonType <- with(Client_Entry_Data, ifelse(is.na(ClientAge), 
                                "Unknown",
                                ifelse(ClientAge < 12, 
                                "Child Under 12",
                                ifelse(ClientAge >= 12 & ClientAge <= 17, 
                                "Youth 12-17",
                                ifelse(ClientAge >= 18 & ClientAge <= 24,
                                "Adult-age Youth 18-24",       
                                "Adult 18 and over")))))

Person_counts_APR <- Client_Entry_Data %>%
  group_by(APRPersonType) %>%
  summarise(DistinctPersonalIDCount = n_distinct(PersonalID))

Client_Entry_Data$PersonType <- with(Client_Entry_Data, ifelse(is.na(ClientAge), 
                                                               "Unknown",
                                                               ifelse(ClientAge < 17, 
                                                                      "Child",
                                                                      ifelse(ClientAge >= 18 & ClientAge <= 24, 
                                                                             "Youth", 
                                                                             "Adult"))))

Person_counts <- Client_Entry_Data %>%
  group_by(PersonType) %>%
  summarise(DistinctPersonalIDCount = n_distinct(PersonalID))


###Determines household type based on PersonType and household composition
determine_APR_HHType <- function(df) {
  df %>%
    group_by(HouseholdID) %>%
    mutate(
      APR_HHType = case_when(
        all(PersonType == "Adult") ~ "Adult",
        any(PersonType == "Adult") & any(PersonType == "Youth") & 
          !any(PersonType == "Child") & any(RelationshipToHoH == 1) ~ "Adult",
        any(PersonType == "Adult") & any(RelationshipToHoH == 1) & 
          any(PersonType == "Child") ~ "Adult Child",
        any(PersonType == "Youth") & any(RelationshipToHoH == 1) & 
          any(PersonType == "Child") & !any(PersonType == "Adult") ~ "Adult Child",
        all(PersonType == "Youth") ~ "Adult",
        all(PersonType == "Child") ~ "Child Only",
        TRUE ~ "Unknown"
      )
    ) %>%
    ungroup()
}


Client_Entry_Data <- determine_APR_HHType(Client_Entry_Data)

###APR Household Type Count Data Frame
HouseholdDF_APR <- Client_Entry_Data %>%
  group_by(APR_HHType) %>%
  summarise(DistinctPersonalIDCount = n_distinct(PersonalID))

