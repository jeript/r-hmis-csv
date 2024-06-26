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
