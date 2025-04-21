library(shiny)
library(tidyverse)
library(lubridate)

# Load pre-processed data
Client_Entry_Data <- readRDS("Client_Entry_Data.rds")
Disabilities_Data <- readRDS("Disabilities_Data.rds")
Income_Data <- readRDS("Income_Data.rds")

# Date range limits ---------------------------------------------------------
min_start_date <- as.Date("2021-01-01")
max_end_date <- max(Client_Entry_Data$ExportEndDate, na.rm = TRUE)
