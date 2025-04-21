
living_situation <- function(ReferenceNo) {
  case_when(
    ReferenceNo == 8 ~ "Client doesn't know",
    ReferenceNo == 9 ~ "Client prefers not to answer",
    ReferenceNo == 17 ~ "Other",
    ReferenceNo == 24 ~ "Deceased",
    ReferenceNo == 30 ~ "No exit interview completed",
    ReferenceNo == 37 ~ "Worker unable to determine",
    ReferenceNo == 99 ~ "Data not collected",
    ReferenceNo == 101 ~ "Emergency shelter/ h/motel paid for by a third party/Host Home shelter",
    ReferenceNo == 116 ~ "Place not meant for habitation",
    ReferenceNo == 118 ~ "Safe Haven",
    ReferenceNo == 204 ~ "Psychiatric hospital/ other psychiatric facility",
    ReferenceNo == 205 ~ "Substance abuse treatment facility or detox center",
    ReferenceNo == 206 ~ "Hospital or other residential non-psychiatric medical facility",
    ReferenceNo == 207 ~ "Jail/prison/juvenile detention",
    ReferenceNo == 225 ~ "Long-term care facility or nursing home",
    ReferenceNo == 215 ~ "Foster care home of foster care group home",
    ReferenceNo == 327 ~ "Moved from HOPWA funded project to HOPWA TH",
    ReferenceNo == 302 ~ "Transitional housing",
    ReferenceNo == 332 ~ "Host Home (non-crisis)",
    ReferenceNo == 329 ~ "Residential project or halfway house with no homeless criteria",
    ReferenceNo == 314 ~ "H/Motel paid for by household",
    ReferenceNo == 313 ~ "Staying or living with friends, temporary tenure",
    ReferenceNo == 335 ~ "Staying or living with family, temporary tenure",
    ReferenceNo == 336 ~ "Staying or living in a friend's room, apartment or house",
    ReferenceNo == 335 ~ "Staying or living in a family member's room, apartment, or house",
    ReferenceNo == 423 ~ "Staying or living with friends, permanent tenure",
    ReferenceNo == 422 ~ "Staying or living with family, permanent tenure",
    ReferenceNo == 435 ~ "Rental by client, with ongoing housing subsidy",
    ReferenceNo == 410 ~ "Rental by client, no ongoing housing subsidy",
    ReferenceNo == 426 ~ "Moved from one HOPWA funded project to HOPWA PH",
    ReferenceNo == 421 ~ "Owned by client, with ongoing housing subsidy",
    ReferenceNo == 411 ~ "Owned by client, no ongoing housing subsidy"
  )
}



extended_living_situation <- function(ReferenceNo) {
  case_when(
    ReferenceNo %in% c(8, 9, 17, 24, 30, 37, 99) | is.na(ReferenceNo) ~ "Other Living Situations",
    ReferenceNo %in% c(101, 116, 118) ~ "Homeless Situations",
    ReferenceNo %in% c(204, 205, 206, 207, 225, 215) ~ "Institutional Situations",
    ReferenceNo %in% c(327, 302, 332, 329, 314, 313, 335, 336) ~ "Temporary Housing Situations",
    ReferenceNo %in% c(423, 422, 435, 410, 426, 421, 411) ~ "Permanent Housing Situations",
  )
}


rental_subsidy_types <- function(ReferenceNo){
  case_when(
    ReferenceNo == 419 ~ "VASH",
    ReferenceNo == 420 ~ "Other subsidy",
    ReferenceNo == 428 ~ "GPD TIP",
    ReferenceNo == 431 ~ "RRH or equivalent",
    ReferenceNo == 433 ~ "HCV vouncer (tenant or project based) (not dedicated)",
    ReferenceNo == 434 ~ "Public housing unit",
    ReferenceNo == 436 ~ "Emergency Housing Voucher",
    ReferenceNo == 437 ~ "Family Unification Program Voucher (FUP)",
    ReferenceNo == 438 ~ "Foster Youth to Independence Initiative (FYI)",
    ReferenceNo == 439 ~ "Permanent Supportive Housing",
    ReferenceNo == 440 ~ "Other permanent housing dedicated for formerly homeless persons",
  )
}

project_type <- function(ReferenceNo){
  case_when(
    ReferenceNo == 1 ~ "Emergency Shelter (NbN)",
    ReferenceNo == 0 ~ "Emergency Shelter (E/E)",
    ReferenceNo == 2 ~ "Transitional Housing",
    ReferenceNo == 3 ~ "Permanent Supportive Housing",
    ReferenceNo == 4 ~ "Street Outreach",
    ReferenceNo == 6 ~ "Services Only",
    ReferenceNo == 7 ~ "Other",
    ReferenceNo == 8 ~ "Safe Haven",
    ReferenceNo == 9 ~ "PH - Housing Only",
    ReferenceNo == 10 ~ "PH - Housing with Services",
    ReferenceNo == 11 ~ "Day Shelter",
    ReferenceNo == 12 ~ "Prevention",
    ReferenceNo == 13 ~ "Rapid Rehousing",
    ReferenceNo == 14 ~ "Coordinated Entry"
  )
}

project_type_ext <- function(ReferenceNo) {
  case_when(
    ReferenceNo %in% c(0, 1) ~ "Emergency Shelter",
    ReferenceNo == 2 ~ "Transitional Housing",
    ReferenceNo %in% c(3, 9, 10) ~ "Permanent Housing",
    ReferenceNo == 4 ~ "Street Outreach",
    ReferenceNo == 6 ~ "Services Only",
    ReferenceNo == 7 ~ "Other",
    ReferenceNo == 8 ~ "Safe Haven",
    ReferenceNo == 11 ~ "Day Shelter",
    ReferenceNo == 12 ~ "Homelessness Prevention",
    ReferenceNo == 13 ~ "Rapid Re-Housing",
    ReferenceNo == 14 ~ "Coordinated Entry",
    TRUE ~ "Unknown"
  )
}

project_type_abb <- function(ReferenceNo){
  case_when(
    ReferenceNo == 1 ~ "ES (NbN)",
    ReferenceNo == 0 ~ "ES (E/E)",
    ReferenceNo == 2 ~ "TH",
    ReferenceNo == 3 ~ "PSH",
    ReferenceNo == 4 ~ "OUT",
    ReferenceNo == 6 ~ "SSO",
    ReferenceNo == 7 ~ "Other",
    ReferenceNo == 8 ~ "SH",
    ReferenceNo == 9 ~ "OPH no Svcs",
    ReferenceNo == 10 ~ "OPH w/ Svcs",
    ReferenceNo == 11 ~ "DAY",
    ReferenceNo == 12 ~ "HP",
    ReferenceNo == 13 ~ "RRH",
    ReferenceNo == 14 ~ "CE"
  )
}

rel_to_hoh <- function(ReferenceNo){
  case_when(
    ReferenceNo == 1 ~ "HoH",
    ReferenceNo == 2 ~ "HoHs child",
    ReferenceNo == 3 ~ "HoHs partner/spouse",
    ReferenceNo == 4 ~ "HoHs other relation",
    ReferenceNo == 5 ~ "Non-relation member",
    ReferenceNo == 99 ~ "Data not collected"
  )
}

disability_type <- function(ReferenceNo){
  case_when(
    ReferenceNo == 5 ~ "Physical disability",
    ReferenceNo == 6 ~ "Developmental disability",
    ReferenceNo == 7 ~ "Chronic health condition",
    ReferenceNo == 8 ~ "HIV/AIDS",
    ReferenceNo == 9 ~ "Mental health disorder",
    ReferenceNo == 10 ~ "Substance use disorder"
  )
}

rrh_subtype <- function(ReferenceNo){
  case_when(
    ReferenceNo == 1 ~ "RRH: Services only",
    ReferenceNo == 2 ~ "RRH: Housing with or without services"
  )
}

housing_type <- function(ReferenceNo){
  case_when(
    ReferenceNo == 1 ~ "Site-based: single site",
    ReferenceNo == 2 ~ "Site-based: clustered/multiple sites",
    ReferenceNo == 3 ~ "Tenant-based: scattered site",
  )
}

household_type <- function(ReferenceNo){
  case_when(
    ReferenceNo == 1 ~ "Households without children",
    ReferenceNo == 3 ~ "Households with at least one adult and one child",
    ReferenceNo == 4 ~ "Households with only children"
  )
}


bed_type <- function(ReferenceNo){
  case_when(
    ReferenceNo == 1 ~ "Facility-based beds",
    ReferenceNo == 2 ~ "Voucher beds",
    ReferenceNo == 3 ~ "Other beds"
  )
}

availability_type <- function(ReferenceNo){
  case_when(
    ReferenceNo == 1 ~ "Year-round",
    ReferenceNo == 2 ~ "Seasonal",
    ReferenceNo == 3 ~ "Overflow"
  )
}

population_type <- function(ReferenceNo){
  case_when(
    ReferenceNo == 1 ~ "DV: Survivor of Domestic Violence",
    ReferenceNo == 3 ~ "HIV: Persons with HIV/AIDS",
    ReferenceNo == 4 ~ "NA: Not applicable"
  )
}

hmisparticipation_type <- function(ReferenceNo){
  case_when(
    ReferenceNo == 0 ~ "Not Participating",
    ReferenceNo == 1 ~ "HMIS Participating",
    ReferenceNo == 2 ~ "Comparable Database Participating"
  )
}

los_type <- function(ReferenceNo){
  case_when(
    ReferenceNo == 2 ~ "One week or more, but less than one month",
    ReferenceNo == 3 ~ "One month or more, but less than 90 days",
    ReferenceNo == 4 ~ "90 days or more but less than one year",
    ReferenceNo == 5 ~ "One year or longer",
    ReferenceNo == 8 ~ "Client doesn’t know",
    ReferenceNo == 9 ~ "Client prefers not to answer",
    ReferenceNo == 10 ~ "One night or less",
    ReferenceNo == 11 ~ "Two to six nights",
    ReferenceNo == 99 ~ "Data not collected"
  )
}

timeshomeless_type <- function(ReferenceNo){
  case_when(
    ReferenceNo == 1 ~ "One time",
    ReferenceNo == 2 ~ "Two times",
    ReferenceNo == 3 ~ "Three times",
    ReferenceNo == 4 ~ "Four or more times",
    ReferenceNo == 8 ~ "Client doesn’t know",
    ReferenceNo == 9 ~ "Client prefers not to answer",
    ReferenceNo == 99 ~ "Data not collected"
  )
}


monthshomeless_type <- function(ReferenceNo){
  case_when(
    ReferenceNo == 101 ~ "1",
    ReferenceNo == 102 ~ "2",
    ReferenceNo == 103 ~ "3",
    ReferenceNo == 104 ~ "4",
    ReferenceNo == 105 ~ "5",
    ReferenceNo == 106 ~ "6",
    ReferenceNo == 107 ~ "7",
    ReferenceNo == 108 ~ "8",
    ReferenceNo == 109 ~ "9",
    ReferenceNo == 110 ~ "10",
    ReferenceNo == 111 ~ "11",
    ReferenceNo == 112 ~ "12",
    ReferenceNo == 113 ~ "More than 12 months",
    ReferenceNo == 8 ~ "Client doesn’t know",
    ReferenceNo == 9 ~ "Client prefers not to answer",
    ReferenceNo == 99 ~ "Data not collected"
  )
}

assessmentlevel_type <- function(ReferenceNo){
  case_when(
    ReferenceNo == 1 ~ "Crisis Needs Assessment",
    ReferenceNo == 2 ~ "Housing Needs Assessment"
  )
}

prioritization_type <- function(ReferenceNo){
  case_when(
    ReferenceNo == 1 ~ "Placed on prioritization list",
    ReferenceNo == 2 ~ "Not placed on prioritization list"
  )
}


event_type <- function(ReferenceNo){
  case_when(
    ReferenceNo == 1 ~ "Referral to Prevention Assistance project",
    ReferenceNo == 2 ~ "Problem Solving/Diversion/Rapid Resolution intervention or service",
    ReferenceNo == 3 ~ "Referral to scheduled Coordinated Entry Crisis Needs Assessment",
    ReferenceNo == 4 ~ "Referral to scheduled Coordinated Entry Housing Needs Assessment",
    ReferenceNo == 5 ~ "Referral to Post-placement/ follow-up case management",
    ReferenceNo == 6 ~ "Referral to Street Outreach project or services",
    ReferenceNo == 7 ~ "Referral to Housing Navigation project or services",
    ReferenceNo == 8 ~ "Referral to Non-continuum services: Ineligible for continuum services",
    ReferenceNo == 9 ~ "Referral to Non-continuum services: No availability in continuum services",
    ReferenceNo == 10 ~ "Referral to Emergency Shelter bed opening",
    ReferenceNo == 11 ~ "Referral to Transitional Housing bed/unit opening",
    ReferenceNo == 12 ~ "Referral to Joint TH-RRH project/unit/resource opening",
    ReferenceNo == 13 ~ "Referral to RRH project resource opening",
    ReferenceNo == 14 ~ "Referral to PSH project resource opening",
    ReferenceNo == 15	~ "Referral to Other PH project/unit/resource opening",
    ReferenceNo == 16	~ "Referral to emergency assistance/flex fund/furniture assistance",
    ReferenceNo == 18	~ "Referral to a Housing Stability Voucher"
  )
}

result_type <- function(ReferenceNo){
  case_when(
    ReferenceNo == 1 ~ "Successful referral: client accepted",
    ReferenceNo == 2 ~ "Unsuccessful referral: client rejected",
    ReferenceNo == 3 ~ "Unsuccessful referral: provider rejected"
  )
}

funding_type <- function(ReferenceNo){
  case_when(
    ReferenceNo == 1 ~ "HUD: CoC – Homelessness Prevention (High Performing Comm. Only)",
    ReferenceNo == 2 ~ "HUD: CoC – Permanent Supportive Housing",
    ReferenceNo == 3 ~ "HUD: CoC – Rapid Re-Housing",
    ReferenceNo == 4 ~ "HUD: CoC – Supportive Services Only",
    ReferenceNo == 5 ~ "HUD: CoC – Transitional Housing",
    ReferenceNo == 6 ~ "HUD: CoC – Safe Haven",
    ReferenceNo == 7 ~ "HUD: CoC – Single Room Occupancy (SRO)",
    ReferenceNo == 8 ~ "HUD: ESG – Emergency Shelter (operating and/or essential services)",
    ReferenceNo == 9 ~ "HUD: ESG – Homelessness Prevention",
    ReferenceNo == 10 ~ "HUD: ESG – Rapid Rehousing",
    ReferenceNo == 11 ~ "HUD: ESG – Street Outreach",
    ReferenceNo == 13 ~ "HUD: HOPWA – Hotel/Motel Vouchers",
    ReferenceNo == 14 ~ "HUD: HOPWA – Housing Information",
    ReferenceNo == 15	~ "HUD: HOPWA – Permanent Housing (facility based or TBRA)",
    ReferenceNo == 16	~ "HUD: HOPWA – Permanent Housing Placement",
    ReferenceNo == 17	~ "HUD: HOPWA – Short-Term Rent, Mortgage, Utility assistance",
    ReferenceNo == 18	~ "HUD: HOPWA – Short-Term Supportive Facility",
    ReferenceNo == 19	~ "HUD: HOPWA – Transitional Housing (facility based or TBRA)",
    ReferenceNo == 20	~ "HUD: HUD/VASH",
    ReferenceNo == 21	~ "HHS: PATH – Street Outreach & Supportive Services Only",
    ReferenceNo == 22	~ "HHS: RHY – Basic Center Program (prevention and shelter)",
    ReferenceNo == 23	~ "HHS: RHY – Maternity Group Home for Pregnant and Parenting Youth",
    ReferenceNo == 24	~ "HHS: RHY – Transitional Living Program",
    ReferenceNo == 25	~ "HHS: RHY – Street Outreach Project",
    ReferenceNo == 26	~ "HHS: RHY – Demonstration Project",
    ReferenceNo == 27	~ "VA: CRS Contract Residential Services",
    ReferenceNo == 30	~ "VA: Community Contract Safe Haven Program",
    ReferenceNo == 33	~ "VA: Supportive Services for Veteran Families",
    ReferenceNo == 34	~ "N/A",
    ReferenceNo == 35	~ "HUD: Pay for Success",
    ReferenceNo == 36	~ "HUD: Public and Indian Housing (PIH) Programs",
    ReferenceNo == 37	~ "VA: Grant Per Diem – Bridge Housing",
    ReferenceNo == 38	~ "VA: Grant Per Diem – Low Demand",
    ReferenceNo == 39	~ "VA: Grant Per Diem – Hospital to Housing",
    ReferenceNo == 40	~ "VA: Grant Per Diem – Clinical Treatment",
    ReferenceNo == 41	~ "VA: Grant Per Diem – Service Intensive Transitional Housing",
    ReferenceNo == 42	~ "VA: Grant Per Diem – Transition in Place",
    ReferenceNo == 43	~ "HUD: CoC – Youth Homeless Demonstration Program (YHDP)",
    ReferenceNo == 44	~ "HUD: CoC – Joint Component TH/RRH",
    ReferenceNo == 45	~ "VA: Grant Per Diem – Case Management/Housing Retention",
    ReferenceNo == 46	~ "Local or Other Funding Source (Please Specify)",
    ReferenceNo == 47	~ "HUD: ESG – CV",
    ReferenceNo == 48	~ "HUD: HOPWA – CV",
    ReferenceNo == 50	~ "HUD: HOME",
    ReferenceNo == 51	~ "HUD: HOME (ARP)",
    ReferenceNo == 52	~ "HUD: PIH (Emergency Housing Voucher)",
    ReferenceNo == 53	~ "HUD: ESG – RUSH",
    ReferenceNo == 54	~ "HUD: Unsheltered Special NOFO",
    ReferenceNo == 55	~ "HUD: Rural Special NOFO"
  )
}

enhanced_yes_no_translator <- function(ReferenceNo) {
  case_when(
    ReferenceNo == 0 ~ "No",
    ReferenceNo == 1 ~ "Yes",
    ReferenceNo == 8 ~ "Client doesn't know",
    ReferenceNo == 9 ~ "Client declined",
    ReferenceNo == 99 ~ "Data not collected",
    TRUE ~ "something's wrong"
  )
}
