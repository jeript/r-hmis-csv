library(tidyverse)

###Import relevant csv files manually and choose columns below

###Coordinated Entry Files
Assessment_Data <- read.csv("Assessment.csv")
Event_Data <- read.csv("Event.csv")

###Client Data
Client_Data <- read.csv("Client.csv")

###Entry Exit Files
Enrollment_Data <- read.csv("Enrollment.csv")
Exit_Data <- read.csv("Exit.csv")
Disabilities_Data <- read.csv("Disabilities.csv")
IncomeBenefits_Data <- read.csv("IncomeBenefits.csv")

###Project Files
Organization_Data <- read.csv("Organization.csv")
Project_Data <- read.csv("Project.csv")
Funder_Data <- read.csv("Funder.csv")
CEParticipation_Data <- read.csv("CEParticipation.csv")
HMISParticipation_Data <- read.csv("HMISParticipation.csv")
Inventory_Data <- read.csv("Inventory.csv")


#Export Data
Export_Data <- read.csv("Export.csv")


### Choose Columns

assessment_columns <- c("AssessmentID", "EnrollmentID", "PersonalID", "AssessmentDate", "AssessmentLocation",
                        "AssessmentType", "AssessmentLevel", "PrioritizationStatus")

event_columns <- c("EventID", "EnrollmentID", "PersonalID", "EventDate", "Event")

client_columns <- c("PersonalID", "DOB", "AmIndAKNative", "Asian", "BlackAfAmerican",
                    "HispanicLatinaeo", "MidEastNAfrican", "NativeHIPacific", "White",
                    "RaceNone", "Woman", "Man", "NonBinary", "CulturallySpecific",
                    "Transgender", "Questioning", "DifferentIdentity", "GenderNone", "VeteranStatus")

disabilitites_columns <- c("DisabilitiesID", "EnrollmentID", "PersonalID", "InformationDate", "DisabilityType")

income_columns <- c("IncomeBenefitsID", "EnrollmentID", "PersonalID", "InformationDate", "IncomeFromAnySource",
                    "TotalMonthlyIncome")

enrollment_columns <- c("EnrollmentID", "PersonalID", "ProjectID", "EntryDate",
                        "HouseholdID", "RelationshipToHoH", "EnrollmentCoC",
                        "LivingSituation", "RentalSubsidyType", "LengthOfStay",
                        "LOSUnderThreshold", "PreviousStreetESSH", "DateToStreetESSH",
                        "TimesHomelessPastThreeYears", "MonthsHomelessPastThreeYears",
                        "DisablingCondition", "DateOfEngagement", "MoveInDate")

exit_columns <- c("PersonalID", "EnrollmentID", "ExitDate", "Destination", "DestinationSubsidyType")

organization_columns <- c("OrganizationID", "OrganizationName", "VictimServiceProvider")

project_columns <- c("ProjectID", "OrganizationID", "ProjectName", "ProjectType")

ceparticipation_data <- c("CEParticipationID", "ProjectID", "AccessPoint", "PreventionAssessment", "CrisisAssessment",
                          "HousingAssessment", "DirectServices", "ReceivesReferrals", "CEParticipationStatusStartDate",
                          "CEParticipationStatusEndDate")

funder_columns <- c("FunderID", "ProjectID", "Funder", "StartDate", "EndDate")

hmisparticipation_columns <- c("HMISParticipationID", "ProjectID", "HMISParticipationType", "HMISParticipationStatusStartDate",
                               "HMISParticipationStatusEndDate")

inventory_columns <- c("InventoryID", "ProjectID", "CoCCode", "HouseholdType", "Availability", "BedInventory", "CHVetBedInventory",
                       "YouthVetBedInventory", "VetBedInventory", "CHYouthBedInventory", "YouthBedInventory", "CHBedInventory",
                       "OtherBedInventory", "ESBedType", "InventoryStartDate", "InventoryEndDate")


export_columns <- c("ExportStartDate", "ExportEndDate")
