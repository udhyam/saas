# Setup Work Environment
# -----------------------------------------------------------------------------

# Clean the worskspace
rm(list = ls())

# Loading Packages
library(KoboconnectR)
library(tidyverse)
library(janitor)
library(lubridate)
library(googlesheets4)
library(ggplot2)

# Load Credentials
# -----------------------------------------------------------------------------
# Login into Google
gs4_auth()

# Authorise Google Account
2

# Data Analysis Prep
# -----------------------------------------------------------------------------

# Download the Cleaned data
data_master_vyapaaris <- read_sheet("https://docs.google.com/spreadsheets/d/1O4CEb3Fq-OjMrqzZuJaPPmD88sgBvBMqsu7Bf8hE10Y/", 
                                            sheet = "fc-clean-final")

# Export backup for Reporting
write_csv(data_master_vyapaaris, "data/processed/first-contact-master.csv")


# Create a field "New Final Status" and drop extra entries
data_master_vyapaaris <- data_master_vyapaaris |>
  mutate(new_final_status = if_else(is.na(final_status), "new", final_status)) |>
  filter(new_final_status != "dropped")

# Status Suummary
# ----------------------------------------------------------------------------
# Overall Funnel
status_summary <- data_master_vyapaaris |>
  group_by(new_final_status) |>
  summarise(count = n()) |>
  mutate(count = if_else(new_final_status == "active" | new_final_status == "new", count, count*-1)) |>
  add_row(new_final_status = "original", count = 197) |>
  filter(new_final_status != "active") |>
  mutate(new_final_status =  factor(new_final_status, 
                                    levels = c("original", "inactive", "duplicate",
                                               "missing", "not located", "new"))) |>
  select(final_status = new_final_status, count) |>
  arrange(final_status)

# Export Status For Reference
write_sheet(status_summary, 
            ss = "https://docs.google.com/spreadsheets/d/1bcSvo_9ONt0gt8NNNNCT6W0pFBqtjzYBHjcO0YcWrEE/",
            sheet = "fc-status-summary")

# Match Errors
status_errors <- data_master_vyapaaris  |>
  group_by(sambhav_flag = final_status,
           new_flag = vyapaari_data_status,
           new_status = vyapaari_final_status) |> 
  summarise(count = n())

# Export Match Errors
write_sheet(status_errors, 
            ss = "https://docs.google.com/spreadsheets/d/1bcSvo_9ONt0gt8NNNNCT6W0pFBqtjzYBHjcO0YcWrEE/",
            sheet = "fc-status-errors")

# Monthly Reporting
# -----------------------------------------------------------------------------

# Data filtering
data_master_vyapaaris_analysis <- data_master_vyapaaris |>
  mutate(new_final_status = if_else(is.na(final_status) & !is.na(start), "new", 
                                    if_else(is.na(final_status) & is.na(start), "missing", final_status))) |>
  filter(new_final_status != "dropped") |>
  mutate(start_date = as.Date(start, "%Y-%m-%d")) |>
  filter(start_date < "2024-10-01")

# Vyapaari Suummary
status_type <- data_master_vyapaaris_analysis |>
  group_by(final_status = new_final_status, vyapaari_gender, shop_ownership) |>
  summarise(count = n()) |>
  filter(final_status != "inactive")

# Export Match Errors
write_sheet(status_type, 
            ss = "https://docs.google.com/spreadsheets/d/1bcSvo_9ONt0gt8NNNNCT6W0pFBqtjzYBHjcO0YcWrEE/",
            sheet = "fc-status-type")

# Quick review summaries
# ----------------------------------------------------------------------------
data_master_vyapaaris |>
  group_by(phone_number, vyapaari_name) |>
  summarise(count = n()) |>
  arrange(desc(count))

data_master_vyapaaris |>
  group_by(x_uuid) |>
  summarise(count = n()) |>
  arrange(desc(count))

data_master_vyapaaris |>
  group_by(shop_business_type_clean) |>
  summarise(count = n())
