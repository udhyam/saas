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

# Load the KoboToolbox credentials
USER_ID <- Sys.getenv(c("USER_ID"))
PASSWORD <- Sys.getenv(c("PASSWORD"))

# Login into Kobo
kobo_token <- get_kobo_token(url = "kf.kobotoolbox.org", uname = USER_ID, pwd = PASSWORD)

# Login into Google
gs4_auth()

# Authorise Google Account
2

# Download Data
# -----------------------------------------------------------------------------

# List of Kobo forms data to download
# Select forms to download
id_contact_vyapaaris <- Sys.getenv(c("ID_CONTACT_FORM"))

# Baseline - Vyapaaris
data_contact_vyapaaris_dwnld <- 
  kobo_df_download(
    url = "kf.kobotoolbox.org",
    uname = USER_ID,
    pwd = PASSWORD,
    assetid = id_contact_vyapaaris,
    all = "false",
    lang = "_xml",
    sleep = 5
  )

# Download the Sambhav data
data_original_vyapaaris_dwnld <- read_sheet("https://docs.google.com/spreadsheets/d/1O4CEb3Fq-OjMrqzZuJaPPmD88sgBvBMqsu7Bf8hE10Y/", 
                                      sheet = "sambhav-original")

# Secure the workspace
rm(USER_ID)
rm(PASSWORD)
rm(kobo_token)
rm(id_contact_vyapaaris)

# Clean the data
# -----------------------------------------------------------------------------

# Normalize field names
data_contact_vyapaaris <- clean_names(data_contact_vyapaaris_dwnld)
data_original_vyapaaris <- clean_names(data_original_vyapaaris_dwnld)

# Backup the Kobo Data
write_sheet(data_contact_vyapaaris, 
            ss = "https://docs.google.com/spreadsheets/d/1O4CEb3Fq-OjMrqzZuJaPPmD88sgBvBMqsu7Bf8hE10Y/",
            sheet = "fc-kobo-original")

# Remove extra files
rm(data_contact_vyapaaris_dwnld)
rm(data_original_vyapaaris_dwnld)

# Transform the data
# -----------------------------------------------------------------------------
# Normalise the status flags for active/inactive vyapaaris in the original data
data_original_vyapaaris <- data_original_vyapaaris |>
  mutate(final_status = str_to_lower(final_status))

# Normalise the status flags for active/inactive vyapaaris in the first contact data
data_contact_vyapaaris <- data_contact_vyapaaris |>
  mutate(vyapaari_data_status = ifelse(vyapaari_data_status == "", "existing", vyapaari_data_status)) %>%
  mutate(vyapaari_final_status = ifelse(vyapaari_located == "yes", "active",
                                        ifelse(vyapaari_located == "no", "missing", vyapaari_located)))

# Combine the data original and first contact data
# -----------------------------------------------------------------------------
# Create sub-set 1 for vyapaaris with phone numbers
data_original_vyapaaris_numbers <- data_original_vyapaaris |>
  filter(!is.na(phone_number))

# Create sub-set 2 for vyapaaris without phone numbers
data_original_vyapaaris_no_numbers <- data_original_vyapaaris |>
  filter(is.na(phone_number))

# Outer join the original and first contact data to create a master dataset
data_master_vyapaaris_numbers <- data_original_vyapaaris_numbers |>
  full_join(data_contact_vyapaaris, by = c("phone_number" = "vyapaari_phone_number"))

# Append the no phone number vyapaaris at the bottom of the master
data_master_vyapaaris <- bind_rows(data_master_vyapaaris_numbers,
                                   data_original_vyapaaris_no_numbers)

# Remove extra files
rm(data_master_vyapaaris_numbers)
rm(data_original_vyapaaris_numbers)
rm(data_original_vyapaaris_no_numbers)

# Clean the phone number data for vyapaaris
# -----------------------------------------------------------------------------
# List All Duplicate Phone Numbers
dups_phone_list <- data_contact_vyapaaris |>
  group_by(vyapaari_phone_number) |>
  filter(!is.na(vyapaari_phone_number)) |>
  summarise(count = n()) |>
  arrange(desc(count)) |>
  filter(count > 1) |>
  select(-count) |>
  mutate(ph_no_flag = "duplicate")

# Export List For Reference
write_sheet(dups_phone_list, 
            ss = "https://docs.google.com/spreadsheets/d/1bcSvo_9ONt0gt8NNNNCT6W0pFBqtjzYBHjcO0YcWrEE/",
            sheet = "fc-dups-phone-number")

# Identify duplicates phone numbers
dups_phone_num <- data_master_vyapaaris |>
  group_by(phone_number) |>
  filter(!is.na(phone_number)) |>
  summarise(count = n()) |>
  arrange(desc(count)) |>
  filter(count > 1) |>
  select(-count) |>
  mutate(ph_no_flag = "shared")

# Extract duplicate phone number vyapaaris from master data
data_master_dups <- data_master_vyapaaris |>
  left_join(dups_phone_num, by = c("phone_number")) |>
  mutate(name_match = if_else(vyapaari_name == q7_name_of_candidate_beneficiary, "match", "no-match")) |>
  filter(ph_no_flag == "shared" & name_match == "match") |>
  select(-ph_no_flag, -name_match)

# Create a de-duplicated database of vyapaaris
data_master_de_dups <- data_master_vyapaaris |>
  left_join(dups_phone_num, by = c("phone_number")) |>
  filter(is.na(ph_no_flag)) |>
  select(-ph_no_flag)

# Add back to master data
data_master_vyapaaris <- bind_rows(data_master_dups, data_master_de_dups)

# Remove extra files
rm(dups_phone_list)
rm(dups_phone_num)
rm(data_master_dups)
rm(data_master_de_dups)

# Vyapaari ID
# -----------------------------------------------------------------------------
data_master_vyapaaris <- data_master_vyapaaris |>
  mutate(vyapaari_id = str_c("UVS", str_pad(x_index, width = 3, pad = "0")))

# Data Keys
# -----------------------------------------------------------------------------
# Import Master Lists of Clean Data
# Clean Vyapaari Business
vyapaari_business <- read_sheet("https://docs.google.com/spreadsheets/d/1jH6J_Umeqyyu1gbabpI7e36797Js8YCLPm0UqveitWI/", 
                                sheet = "business-type-clean")

# Clean Vyapaari Locality
vyapaari_locality <- read_sheet("https://docs.google.com/spreadsheets/d/1jH6J_Umeqyyu1gbabpI7e36797Js8YCLPm0UqveitWI/", 
                                sheet = "locality-clean")

# Clean Vyapaari City
vyapaari_origin_city <- read_sheet("https://docs.google.com/spreadsheets/d/1jH6J_Umeqyyu1gbabpI7e36797Js8YCLPm0UqveitWI/", 
                                   sheet = "origin-city-clean")

# Clean Vyapaari Education
vyapaari_edu_level <- read_sheet("https://docs.google.com/spreadsheets/d/1jH6J_Umeqyyu1gbabpI7e36797Js8YCLPm0UqveitWI/", 
                                 sheet = "education-level-clean")

# Join all the clean classfications to the master data
data_master_vyapaaris <- data_master_vyapaaris |>
  mutate(shop_business_type = str_replace(str_trim(shop_business_type), "  ", " "),
         origin_city = str_replace(str_trim(origin_city), "  ", " "),
         shop_locality = str_replace(str_trim(shop_locality), "  ", " "),
         education_level = str_replace(str_trim(education_level), "  ", " ")) |>
  left_join(vyapaari_business, by = "shop_business_type", multiple = "first") |>
  left_join(vyapaari_origin_city, by = "origin_city", multiple = "first") |>
  left_join(vyapaari_locality, by = "shop_locality", multiple = "first") |>
  left_join(vyapaari_edu_level, by = "education_level", multiple = "first")

# Export all the clean classifications for reuse
# Business Type
vyapaari_business_new <- data_master_vyapaaris |>
  group_by(shop_business_type, shop_business_type_clean) |>
  filter(is.na(shop_business_type_clean)) |>
  summarise(1) |>
  select(-`1`)

vyapaari_business <- bind_rows(vyapaari_business, vyapaari_business_new) |>
  filter(shop_business_type != "") |>
  arrange(shop_business_type) |>
  distinct()

write_sheet(vyapaari_business, 
            ss = "https://docs.google.com/spreadsheets/d/1jH6J_Umeqyyu1gbabpI7e36797Js8YCLPm0UqveitWI/",
            sheet = "business-type-clean")

# Vyapaari Locality
vyapaari_locality_new <- data_master_vyapaaris |>
  group_by(shop_locality, shop_locality_clean) |>
  filter(is.na(shop_locality_clean)) |>
  summarise(1) |>
  select(-`1`)

vyapaari_locality <- bind_rows(vyapaari_locality, vyapaari_locality_new) |>
  filter(shop_locality != "") |>
  arrange(shop_locality) |>
  distinct()

write_sheet(vyapaari_locality, 
            ss = "https://docs.google.com/spreadsheets/d/1jH6J_Umeqyyu1gbabpI7e36797Js8YCLPm0UqveitWI/",
            sheet = "locality-clean")

# Origin City
vyapaari_origin_city_new <- data_master_vyapaaris |>
  group_by(origin_city, origin_city_clean) |>
  filter(is.na(origin_city_clean)) |>
  summarise(1) |>
  select(-`1`)

vyapaari_origin_city <- bind_rows(vyapaari_origin_city, vyapaari_origin_city_new) |>
  filter(origin_city != "") |>
  arrange(origin_city) |>
  distinct()

write_sheet(vyapaari_origin_city, 
            ss = "https://docs.google.com/spreadsheets/d/1jH6J_Umeqyyu1gbabpI7e36797Js8YCLPm0UqveitWI/",
            sheet = "origin-city-clean")

# Education Level
vyapaari_edu_level_new <- data_master_vyapaaris |>
  group_by(education_level, education_level_clean) |>
  filter(is.na(education_level_clean)) |>
  summarise(1) |>
  select(-`1`) |>
  distinct()

vyapaari_edu_level <- bind_rows(vyapaari_edu_level, vyapaari_edu_level_new) |>
  filter(education_level != "") |>
  arrange(education_level)

write_sheet(vyapaari_edu_level, 
            ss = "https://docs.google.com/spreadsheets/d/1jH6J_Umeqyyu1gbabpI7e36797Js8YCLPm0UqveitWI/",
            sheet = "education-level-clean")

# Remove extra files
rm(vyapaari_business)
rm(vyapaari_business_new)
rm(vyapaari_locality)
rm(vyapaari_locality_new)
rm(vyapaari_origin_city)
rm(vyapaari_origin_city_new)
rm(vyapaari_edu_level)
rm(vyapaari_edu_level_new)

# Master Dataset
# -----------------------------------------------------------------------------

# Backup the Mater Data
write_sheet(data_master_vyapaaris, 
            ss = "https://docs.google.com/spreadsheets/d/1O4CEb3Fq-OjMrqzZuJaPPmD88sgBvBMqsu7Bf8hE10Y/",
            sheet = "fc-clean-final")

# Create Exports
# -----------------------------------------------------------------------------

# For Sattva
export_sattva <- data_master_vyapaaris |>
  filter(vyapaari_final_status == "active") |>
  select(vyapaari_id, start, end, username, enumerator_name, vyapaari_name, vyapaari_data_status, vyapaari_located,
         vyapaari_gender, vyapaari_age, vyapaari_photograph, vyapaari_photograph_url, shop_name, shop_location,
         x_shop_location_latitude, x_shop_location_longitude, x_shop_location_altitude, x_shop_location_precision,
         shop_locality, business_photograph, business_photograph_url, shop_ownership, shop_business_type,
         business_baseline_details, shop_facility_type, shop_facility_type_other,
         working_days, daily_income, daily_expenses, daily_profits, monthly_income, monthly_expenses, monthly_profits,
         smartphone_access, smartphone_usability, bank_account, digital_payments,
         total_employees, total_family_employees, household_baseline_details,
         total_family_size, total_children, total_earning_members, education_level, origin_city, vyapaari_other_details,
         x_id, x_uuid, x_submission_time, x_validation_status, x_notes, x_status, x_submitted_by, x_version, x_tags, x_index,
         vyapaari_final_status, shop_business_type_clean, origin_city_clean,
         origin_region_clean, shop_locality_clean, education_level_clean)

write_sheet(export_sattva, 
            ss = "https://docs.google.com/spreadsheets/d/1bcSvo_9ONt0gt8NNNNCT6W0pFBqtjzYBHjcO0YcWrEE/",
            sheet = "fc-sattva-vyapaaris")

# For Ops Team
export_ops <- data_master_vyapaaris |>
  filter(vyapaari_final_status == "active") |>
  select(vyapaari_id, enumerator_name, vyapaari_name, phone_number, vyapaari_data_status,
         vyapaari_located, vyapaari_gender, vyapaari_age, vyapaari_photograph,
         shop_location, shop_locality, business_photograph, shop_ownership, shop_business_type,
         business_baseline_details, shop_facility_type, working_days,
         daily_income, daily_expenses, daily_profits, monthly_income, monthly_expenses, monthly_profits,
         smartphone_access, smartphone_usability, bank_account, digital_payments,
         total_employees, total_family_employees, household_baseline_details,
         total_family_size, total_children, total_earning_members,
         education_level, origin_city, vyapaari_other_details, x_index)

write_sheet(export_ops, 
            ss = "https://docs.google.com/spreadsheets/d/1bcSvo_9ONt0gt8NNNNCT6W0pFBqtjzYBHjcO0YcWrEE/",
            sheet = "fc-ops-vyapaaris")

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

data_master_vyapaaris  |>
  group_by(final_status, vyapaari_data_status, vyapaari_final_status) |> 
  summarise(count = n())
