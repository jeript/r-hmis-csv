library(dplyr)

race_columns <- c("AmIndAKNative", "Asian", "BlackAfAmerican", "HispanicLatinaeo", "MidEastNAfrican", "NativeHIPacific", "White", "RaceNone")

gender_columns <- c("Woman", "Man", "NonBinary", "CulturallySpecific", "Transgender", "Questioning", "DifferentIdentity", "GenderNone")

### Merge Races

Client_Entry_Data$MergedRace <- apply(Client_Entry_Data[race_columns], 1, function(row) {
  identified_races <- names(row[row == 1])
  identified_races <- identified_races[!is.na(identified_races)]
  if (length(identified_races) > 0) {
    return(paste(identified_races, collapse = ", "))
  } else {
    return("")
  }
})

### Add Merged Race To the Joined file
Client_Entry_Data$MergedRace[Client_Entry_Data$MergedRace == ""] <- NA

race_counts <- Client_Entry_Data %>%
  group_by(MergedRace) %>%
  summarise(Distinct_PersonalID_Count = n_distinct(PersonalID))

###Merge Gender
Client_Entry_Data$MergedGender <- apply(Client_Entry_Data[gender_columns], 1, function(row) {
  identified_genders <- names(row[row == 1])
  identified_genders <- identified_genders[!is.na(identified_genders)]
  if (length(identified_genders) > 0) {
    return(paste(identified_genders, collapse = ", "))
  } else {
    return(NA)
  }
})

###Add Merged Gender to the Joined file
Client_Entry_Data$MergedGender[Client_Entry_Data$MergedGender == ""] <- NA

###Remove Columns
Client_Entry_Data <- Client_Entry_Data %>%
  select(-c(
    "AmIndAKNative", "Asian", "BlackAfAmerican", "HispanicLatinaeo", "MidEastNAfrican", 
    "NativeHIPacific", "White", "RaceNone", "Woman", "Man", "NonBinary", "CulturallySpecific", 
    "Transgender", "Questioning", "DifferentIdentity", "GenderNone"
  ))



gender_counts <- Client_Entry_Data %>%
  group_by(MergedGender) %>%
  summarise(Distinct_PersonalID_Count = n_distinct(PersonalID))
