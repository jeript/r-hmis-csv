library(dplyr)
library(lubridate)

###Income Prep

IncomeBenefits_Data$TotalIncome <- rowSums(IncomeBenefits_Data[, c("EarnedAmount", "UnemploymentAmount", "SSIAmount", 
                                                                   "SSDIAmount", "VADisabilityServiceAmount", "VADisabilityNonServiceAmount",
                                                                   "PrivateDisabilityAmount", "WorkersCompAmount", 
                                                                   "TANFAmount", "GAAmount", "SocSecRetirementAmount", 
                                                                   "PensionAmount", "ChildSupportAmount", 
                                                                   "AlimonyAmount", "OtherIncomeAmount")], 
                                           na.rm = TRUE)

###Remove columns
IncomeBenefits_Data <- IncomeBenefits_Data %>%
  select(-IncomeBenefitsID, -Earned, -Unemployment, 
         -SSI, -SSDI, -VADisabilityService,
         -VADisabilityNonService, -PrivateDisability,
         -WorkersComp, -TANF, -GA, -SocSecRetirement,
         -Pension, -ChildSupport,
         -Alimony, -OtherIncomeSource)

###Add Income Descriptions
IncomeBenefits_Data <- IncomeBenefits_Data %>%
  mutate(IncomeDescription = case_when(
    !is.na(EarnedAmount) & EarnedAmount != "" ~ "Earned",
    !is.na(UnemploymentAmount) & UnemploymentAmount != "" ~ "Unemployment",
    !is.na(SSIAmount) & SSIAmount != "" ~ "SSI",
    !is.na(SSDIAmount) & SSDIAmount != "" ~ "SSDI",
    !is.na(VADisabilityServiceAmount) & VADisabilityServiceAmount != "" ~ "VA Service",
    !is.na(VADisabilityNonServiceAmount) & VADisabilityNonServiceAmount != "" ~ "VA Non-Service",
    !is.na(PrivateDisabilityAmount) & PrivateDisabilityAmount != "" ~ "Private Disability",
    !is.na(WorkersCompAmount) & WorkersCompAmount != "" ~ "Worker's Compensation",
    !is.na(TANFAmount) & TANFAmount != "" ~ "TANF",
    !is.na(GAAmount) & GAAmount != "" ~ "General Assistance",
    !is.na(SocSecRetirementAmount) & SocSecRetirementAmount != "" ~ "Social Security Retirement",
    !is.na(PensionAmount) & PensionAmount != "" ~ "Pension",
    !is.na(ChildSupportAmount) & ChildSupportAmount != "" ~ "Child Support",
    !is.na(AlimonyAmount) & AlimonyAmount != "" ~ "Alimony",
    !is.na(OtherIncomeAmount) & OtherIncomeAmount != "" ~ "Other Income",
    TRUE ~ "No Income"
  ))


IncomeBenefits_Data <- IncomeBenefits_Data %>%
  # Group by EnrollmentID
  group_by(EnrollmentID) %>%
  # Filter rows where DataCollectionStage is 2 or 5 and keep the most recent InformationDate
  mutate(is_recent = ifelse(DataCollectionStage %in% c(2, 5), 
                            InformationDate == max(InformationDate[DataCollectionStage %in% c(2, 5)], na.rm = TRUE), 
                            TRUE)) %>%
  # Keep all rows where DataCollectionStage is 1 or 3, and the most recent rows of 2 or 5
  filter(DataCollectionStage %in% c(1, 3) | is_recent) %>%
  # Remove the helper column
  select(-is_recent) %>%
  # Ungroup to finalize the result
  ungroup()


IncomeBenefits_Data <- IncomeBenefits_Data %>%
  mutate(Earned = ifelse(IncomeDescription == "Earned", "Yes", "No"))

IncomeBenefits_Data <- IncomeBenefits_Data %>%
  mutate(IncomeStage = case_when(
    DataCollectionStage == 1 ~ "Income at Start",
    DataCollectionStage == 2 ~ "Income at Update",
    DataCollectionStage == 3 ~ "Income at Exit",
    DataCollectionStage == 5 ~ "Income at Assessment",
    TRUE ~ NA_character_  # Keeps it blank if none of the conditions are met
  ))


###Remove more columns
IncomeBenefits_Data <- IncomeBenefits_Data %>%
  select(-EarnedAmount, -UnemploymentAmount, 
         -SSIAmount, -SSDIAmount, -VADisabilityServiceAmount,
         -VADisabilityNonServiceAmount, -PrivateDisabilityAmount,
         -WorkersCompAmount, -TANFAmount, -GAAmount, -SocSecRetirementAmount,
         -PensionAmount, -ChildSupportAmount,
         -AlimonyAmount, -OtherIncomeAmount, -IncomeFromAnySource)

IncomeBenefits_Data <- IncomeBenefits_Data %>%
  rename(IncomeInfoDate = InformationDate)

Income_Data <- merge(Client_Entry_Data[, c("EnrollmentID", "EntryDate", "ExitDate")], 
                     IncomeBenefits_Data, 
                     by = "EnrollmentID", 
                     all.y = TRUE)



convert_csv_to_rds(Income_Data, "Income_Data.rds")
