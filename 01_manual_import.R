library(tidyverse)
library(lubridate)

Client_Data <- read_csv("Client.csv", col_types = cols_only(
  PersonalID = col_character(),
  DOB = col_date(),
  AmIndAKNative = col_character(),
  Asian = col_character(),
  BlackAfAmerican = col_character(),
  HispanicLatinaeo = col_character(),
  MidEastNAfrican = col_character(),
  NativeHIPacific = col_character(),
  White = col_character(),
  RaceNone = col_character(),
  Woman = col_character(),
  Man = col_character(),
  NonBinary = col_character(),
  CulturallySpecific = col_character(),
  Transgender = col_character(),
  Questioning = col_character(),
  DifferentIdentity = col_character(),
  GenderNone = col_character(),
  VeteranStatus = col_character()
))

Enrollment_Data <- read_csv("Enrollment.csv", col_types = cols_only(
  EnrollmentID = col_character(),
  PersonalID = col_character(),
  ProjectID = col_character(),
  EntryDate = col_date(),
  HouseholdID = col_character(),
  RelationshipToHoH = col_character(),
  EnrollmentCoC = col_character(),
  LivingSituation = col_character(),
  RentalSubsidyType = col_character(),
  LengthOfStay = col_character(),
  LOSUnderThreshold = col_character(),
  PreviousStreetESSH = col_character(),
  DateToStreetESSH = col_date(),
  TimesHomelessPastThreeYears = col_character(),
  MonthsHomelessPastThreeYears = col_character(),
  DisablingCondition = col_character(),
  DateOfEngagement = col_date(),
  MoveInDate = col_date()
))

Exit_Data <- read_csv("Exit.csv", col_types = cols_only(
  PersonalID = col_character(),
  EnrollmentID = col_character(),
  ExitDate = col_date(),
  Destination = col_character(),
  DestinationSubsidyType = col_character()
))

Organization_Data <- read_csv("Organization.csv", col_types = cols_only(
  OrganizationID = col_character(),
  OrganizationName = col_character(),
  VictimServiceProvider = col_character()
))

Project_Data <- read_csv("Project.csv", col_types = cols_only(
  ProjectID = col_character(),
  OrganizationID = col_character(),
  ProjectName = col_character(),
  ProjectCommonName = col_character(),
  ProjectType = col_character(),
  OperatingStartDate = col_date(),
  OperatingEndDate = col_date(),
  HousingType = col_character(),
  RRHSubType = col_character()
))

ProjectCoC_Data <- read_csv("ProjectCoC.csv", col_types = cols_only(
  ProjectID = col_character(),
  CoCCode = col_character()
))

Export_Data <- read_csv("Export.csv", col_types = cols_only(
  ExportStartDate = col_date(),
  ExportEndDate = col_date()
))

Disabilities_Data <- read_csv("Disabilities.csv", col_types = cols_only(
  DisabilitiesID = col_character(),
  EnrollmentID = col_character(),
  PersonalID = col_character(),
  InformationDate = col_date(),
  DisabilityType = col_character(),
  DisabilityResponse = col_character(),
  DataCollectionStage = col_character()
))

IncomeBenefits_Data <- read_csv("IncomeBenefits.csv", col_types = cols_only(
  IncomeBenefitsID = col_character(),
  EnrollmentID = col_character(),
  PersonalID = col_character(),
  IncomeFromAnySource = col_character(),
  TotalMonthlyIncome = col_double(),
  InformationDate = col_date(),
  Earned = col_character(),
  EarnedAmount = col_double(),
  Unemployment = col_character(),
  UnemploymentAmount = col_double(),
  SSI = col_character(),
  SSIAmount = col_double(),
  SSDI = col_character(),
  SSDIAmount = col_double(),
  VADisabilityService = col_character(),
  VADisabilityServiceAmount = col_double(),
  VADisabilityNonService = col_character(),
  VADisabilityNonServiceAmount = col_double(),
  PrivateDisability = col_character(),
  PrivateDisabilityAmount = col_double(),
  WorkersComp = col_character(),
  WorkersCompAmount = col_double(),
  TANF = col_character(),
  TANFAmount = col_double(),
  GA = col_character(),
  GAAmount = col_double(),
  SocSecRetirement = col_character(),
  SocSecRetirementAmount = col_double(),
  Pension = col_character(),
  PensionAmount = col_double(),
  ChildSupport = col_character(),
  ChildSupportAmount = col_double(),
  Alimony = col_character(),
  AlimonyAmount = col_double(),
  OtherIncomeSource = col_character(),
  OtherIncomeAmount = col_double(),
  DataCollectionStage = col_character()
))


Funder_Data <- read_csv("Funder.csv", col_types = cols_only(
  FunderID = col_character(),
  ProjectID = col_character(),
  Funder = col_character(),
  StartDate = col_date(),
  EndDate = col_date()
))

CEParticipation_Data <- read_csv("CEParticipation.csv", col_types = cols_only(
  CEParticipationID = col_character(),
  ProjectID = col_character(),
  AccessPoint = col_character(),
  PreventionAssessment = col_character(),
  CrisisAssessment = col_character(),
  HousingAssessment = col_character(),
  DirectServices = col_character(),
  ReceivesReferrals = col_character(),
  CEParticipationStatusStartDate = col_date(),
  CEParticipationStatusEndDate = col_date()
))

HMISParticipation_Data <- read_csv("HMISParticipation.csv", col_types = cols_only(
  HMISParticipationID = col_character(),
  ProjectID = col_character(),
  HMISParticipationType = col_character(),
  HMISParticipationStatusStartDate = col_date(),
  HMISParticipationStatusEndDate = col_date()
))

Inventory_Data <- read_csv("Inventory.csv", col_types = cols_only(
  InventoryID = col_character(),
  ProjectID = col_character(),
  CoCCode = col_character(),
  HouseholdType = col_character(),
  Availability = col_character(),
  BedInventory = col_double(),
  CHVetBedInventory = col_double(),
  YouthVetBedInventory = col_double(),
  VetBedInventory = col_double(),
  CHYouthBedInventory = col_double(),
  YouthBedInventory = col_double(),
  CHBedInventory = col_double(),
  OtherBedInventory = col_double(),
  ESBedType = col_character(),
  InventoryStartDate = col_date(),
  InventoryEndDate = col_date()
))

Assessment_Data <- read_csv("Assessment.csv", col_types = cols_only(
  AssessmentID = col_character(),
  EnrollmentID = col_character(),
  PersonalID = col_character(),
  AssessmentDate = col_date(),
  AssessmentLocation = col_character(),
  AssessmentType = col_character(),
  AssessmentLevel = col_character(),
  PrioritizationStatus = col_character()
))

Event_Data <- read_csv("Event.csv", col_types = cols_only(
  EventID = col_character(),
  EnrollmentID = col_character(),
  
  EventDate = col_date(),
  Event = col_character(),
 
  LocationCrisisOrPHHousing = col_character(),
  ReferralResult = col_character(),
  ResultDate = col_date()
))

HealthDV_Data <- read_csv("HealthAndDV.csv", col_types = cols_only(
  EnrollmentID = col_character(),
  PersonalID = col_character(),
  DomesticViolenceSurvivor = col_character(),
  InformationDate = col_date(),
  DataCollectionStage = col_character()
  ))
