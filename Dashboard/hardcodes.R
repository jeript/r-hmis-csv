library(lubridate)
library(dplyr)

###Hard codes from Eva

# Living Situations Groups (includes PLS, CLS, and destinations) 
#(Updated to match FY2024 DS) ---------------------------------------------
# For reference, these come from the HMIS CSV Export specs, pgs 41-43

allowed_prior_living_sit <- 
  c(116, 101, 118, 215, 206, 207, 225, 204, 205, 302, 329, 314, 332, 335, 336,
    410, 435, 421, 411, 8, 9, 99)

allowed_current_living_sit <- 
  c(116, 101, 118, 215, 206, 207, 225, 204, 205, 302, 329, 314, 332, 335, 336,
    410, 435, 421, 411, 17, 37, 8, 9, 99)

allowed_destinations <- 
  c(116, 101, 118, 215, 206, 207, 225, 204, 205, 302, 329, 314, 332, 312, 313,
    327, 422, 423, 426, 410, 435, 421, 411, 30, 17, 24, 8, 9, 99)

allowed_living_situations <- 
  c(allowed_prior_living_sit, allowed_current_living_sit, allowed_destinations) %>%
  unique() %>%
  sort()

perm_livingsituation <- c(336, 410, 411, 421, 422, 423, 426, 435)

lh_livingsituation <- c(101, 116, 118)

homeless_livingsituation <- c(101, 302, 116, 118)

temp_livingsituation <- c(101, 302, 312, 313, 314, 116, 118, 327, 332, 335)

institutional_livingsituation <- c(204, 205, 206, 207, 215, 225, 327, 329)

other_livingsituation <- c(8, 9, 17, 24, 30, 37, 99)

not_homeless_livingsituation <- 
  c(204, 205, 206, 207, 215, 225, 302, 314, 329, 332, 335, 336, 410, 411, 421, 435)

# Chronic Groupings -------------------------------------------------------

###Length of stay less than 7 days for temporary situations
ch_temp_7_los <- c(10, 11)

###Length of stay less than 90 days for institutional situations
ch_temp_90_los <- c(2, 3, 10, 11)

###Months spent homeless
ch_months <- c(112, 113)

###Number of times homeless
ch_times <- 4

###Homeless living situations
ch_living <- c(116, 101, 118)

###Institutional situations
ch_ins_living <- c(215, 206, 207, 225, 204, 205)

###Temporary situations
ch_temp_living <- c(8, 9, 99, 302, 329, 314, 332, 312, 313, 327, 336, 335)

###Homeless night before temporary or institutional situation
ch_prior_ins_temp <- 1



# Project Type Groupings --------------------------------------------------

es_nbn_project_type <- 1

es_ee_project_type <- 0

th_project_type <- 2

psh_project_type <- 3

out_project_type <- 4

sso_project_type <- 6

other_project_project_type <- 7

sh_project_type <- 8

hp_project_type <- 12

rrh_project_type <- 13

ce_project_type <- 14

lh_residential_project_types <- c(0, 1, 2, 8)

lh_project_types <- c(0, 1, 2, 4, 8)

psh_project_types <- c(3, 9, 10)

ph_project_types <- c(3, 9, 10, 13)

ph_other_project_types <- c(9, 10)

lh_ph_hp_project_types <- c(0, 1, 2, 3, 4, 8, 9, 12, 13)

coc_funded_project_types <- c(2, 3, 13)

project_types_w_beds <- c(0, 1, 2, 3, 8, 9, 10, 13)

project_types_wo_beds <- c(4, 6, 7, 12, 14)

project_types_w_cls <- c(1, 4, 6, 14)

long_stayer_98_percentile_project_types <- c(0, 2, 8, 12, 13)

long_stayer_percentile_project_types <- c(0, 2, 3, 8, 9, 10, 12, 13)

all_project_types <- c(0, 1, 2, 3, 4, 6, 8, 9, 10, 11, 12, 13, 14) # minus Other

# Funding Source Groupings -------------------------------------------------

ssvf_fund_sources <- 33


# Race Values --------------------------------------------------------------

yes_no_enhanced <- c(0, 1, 8, 9, 99)
yes_no <- c(0, 1, 99)
dkr_dnc <- c(8, 9, 99)
dkr <- c(8, 9)


# Allowed Subsidy Types ---------------------------------------------------

subsidy_types <- c(419, 420, 428, 431, 433, 434, 436, 437, 438, 439, 440)
