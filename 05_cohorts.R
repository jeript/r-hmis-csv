library(lubridate)
library(dplyr)

Client_Entry_Data$PersonType <- with(Client_Entry_Data, ifelse(is.na(ClientAge), 
                                "Unknown",
                                ifelse(ClientAge < 12, 
                                "Child Under 12",
                                ifelse(ClientAge >= 12 & ClientAge <= 17, 
                                "Youth 12-17",
                                ifelse(ClientAge >= 18 & ClientAge <= 24,
                                "Adult-age Youth 18-24",       
                                "Adult 18 and over")))))


Person_counts <- Client_Entry_Data %>%
  group_by(PersonType) %>%
  summarise(DistinctPersonalIDCount = n_distinct(PersonalID))

determine_HHType <- function(df) {
  df %>% 
    group_by(HouseholdID) %>%
    mutate(HHType = case_when(
      all(PersonType == "Adult 18 and over") ~ "Adult",
      any(PersonType == "Adult 18 and over") & 
        any(PersonType == "Adult-age Youth 18-24") ~ "Adult",
      any(PersonType == "Adult 18 and over") & 
        (any(PersonType == "Child Under 12") | any(PersonType == "Youth 12-17")) ~ "Adult Child",
      (any(PersonType == "Youth 12-17") | any(PersonType == "Adult-age Youth 18-24")) &
        any(RelationshipToHoH == 1) & any(PersonType == "Child Under 12") ~ "Parenting Youth",
      all(PersonType %in% c("Youth 12-17", "Adult-age Youth 18-24")) ~ "Youth",
      all(PersonType == "Child Under 12") ~ "Child",
      TRUE ~ NA_character_
    )) %>%
    ungroup()
}


Client_Entry_Data <- determine_HHType(Client_Entry_Data)

HouseholdDF <- Client_Entry_Data %>%
  group_by(HHType) %>%
  summarise(DistinctPersonalIDCount = n_distinct(PersonalID))



Client_Entry_Data$PersonTypeAPR <- with(Client_Entry_Data, ifelse(is.na(ClientAge), 
                                   "Unknown",
                                    ifelse(ClientAge < 18, 
                                    "Child",
                                    "Adult")))



Person_counts_APR <- Client_Entry_Data %>%
  group_by(PersonTypeAPR) %>%
  summarise(DistinctPersonalIDCount = n_distinct(PersonalID))


###Determines household type based on PersonType and household composition
determine_APR_HHType <- function(df) {
  df %>%
    group_by(HouseholdID) %>%
    mutate(
      APR_HHType = case_when(
        all(PersonTypeAPR == "Adult") ~ "Adult",
        any(PersonTypeAPR == "Adult") & any(PersonTypeAPR == "Youth") & 
          !any(PersonTypeAPR == "Child") & any(RelationshipToHoH == 1) ~ "Adult",
        any(PersonTypeAPR == "Adult") & any(RelationshipToHoH == 1) & 
          any(PersonTypeAPR == "Child") ~ "Adult Child",
        any(PersonTypeAPR == "Youth") & any(RelationshipToHoH == 1) & 
          any(PersonTypeAPR == "Child") & !any(PersonTypeAPR == "Adult") ~ "Adult Child",
        all(PersonTypeAPR == "Youth") ~ "Adult",
        all(PersonTypeAPR == "Child") ~ "Child Only",
        TRUE ~ "Unknown"
      )
    ) %>%
    ungroup()
}


Client_Entry_Data <- determine_APR_HHType(Client_Entry_Data)


###Propogate CoC Data to HH
Client_Entry_Data <- Client_Entry_Data %>%
  group_by(HouseholdID) %>%
  mutate(EnrollmentCoC = EnrollmentCoC[RelationshipToHoH == 1][1],
         ) %>%
  ungroup()

###Propogate Living Situation to HH
Client_Entry_Data <- Client_Entry_Data %>%
  group_by(HouseholdID) %>%
  mutate(
    HoH_LivingSituation = LivingSituation[RelationshipToHoH == 1][1],
    HoH_LivingSituationDescription = LivingSituationDescription[RelationshipToHoH == 1][1],
    HoH_ExtLivingSituationDescription = ExtLivingSituationDescription[RelationshipToHoH == 1][1],
    
    LivingSituation = ifelse(ClientAge < 18 | (ClientAge >= 18 & is.na(LivingSituation)), HoH_LivingSituation, LivingSituation),
    LivingSituationDescription = ifelse(ClientAge < 18 | (ClientAge >= 18 & is.na(LivingSituationDescription)), HoH_LivingSituationDescription, LivingSituationDescription),
    ExtLivingSituationDescription = ifelse(ClientAge < 18 | (ClientAge >= 18 & is.na(ExtLivingSituationDescription)), HoH_ExtLivingSituationDescription, ExtLivingSituationDescription)
  ) %>%
  select(-HoH_LivingSituation, -HoH_LivingSituationDescription, -HoH_ExtLivingSituationDescription) %>%
  ungroup()

###Filter out non CoC codes
Client_Entry_Data <- Client_Entry_Data %>%
  filter(EnrollmentCoC == "AL-500")

