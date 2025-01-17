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
id_baseline_vyapaaris <- Sys.getenv(c("ID_BASELINE_FORM"))

# Baseline - Vyapaaris
data_baseline_vyapaaris <- 
  kobo_df_download(
    url = "kf.kobotoolbox.org",
    uname = USER_ID,
    pwd = PASSWORD,
    assetid = id_baseline_vyapaaris,
    all = "false",
    lang = "_xml",
    sleep = 5
  )

# Save original backup
write_sheet(data_baseline_vyapaaris, 
            ss = "https://docs.google.com/spreadsheets/d/1O4CEb3Fq-OjMrqzZuJaPPmD88sgBvBMqsu7Bf8hE10Y/",
            sheet = "bl-kobo-original")

# Read the First Contact - Ops
data_first_contact_vyapaaris_ops <- 
  read_sheet(ss = "https://docs.google.com/spreadsheets/d/1bcSvo_9ONt0gt8NNNNCT6W0pFBqtjzYBHjcO0YcWrEE/",
             sheet = "fc-ops-vyapaaris")

# Read the First Contact - Sattva
data_first_contact_vyapaaris <- 
  read_sheet(ss = "https://docs.google.com/spreadsheets/d/1bcSvo_9ONt0gt8NNNNCT6W0pFBqtjzYBHjcO0YcWrEE/",
             sheet = "fc-sattva-vyapaaris")

# Secure the workspace
rm(USER_ID)
rm(PASSWORD)
rm(kobo_token)
rm(id_baseline_vyapaaris)

# Clean Data
# -----------------------------------------------------------------------------
# Clean up the group field labels
data_baseline_vyapaaris_clean <- data_baseline_vyapaaris |>
  select(-matches("header"), -matches("note"), -matches("info")) |>
  rename_with(~ str_replace_all(., c("group_gn69x50" = "b1_rev",
                                     "group_cj4oj86" = "b1_exp",
                                     "group_wb4eu10" = "b2_rev",
                                     "group_ub0yl99" = "b2_exp",
                                     "group_ff57r52" = "b3_rev",
                                     "group_xp4ro84" = "b3_exp")))

# Save cleaned backup
# For Analysis
write_sheet(data_baseline_vyapaaris_clean, 
            ss = "https://docs.google.com/spreadsheets/d/1O4CEb3Fq-OjMrqzZuJaPPmD88sgBvBMqsu7Bf8hE10Y/",
            sheet = "bl-tidy-data")

# For Sattva
write_sheet(data_baseline_vyapaaris_clean, 
            ss = "https://docs.google.com/spreadsheets/d/1bcSvo_9ONt0gt8NNNNCT6W0pFBqtjzYBHjcO0YcWrEE/",
            sheet = "bl-sattva-vyapaaris-raw")

# Add suffixes to all field names
data_baseline_vyapaaris_clean <- data_baseline_vyapaaris_clean |>
  rename_with( .fn = function(.x){paste0(.x, "_bl")}) |>
  mutate(bl_flag = "YES")
data_first_contact_vyapaaris_ops <- data_first_contact_vyapaaris_ops |>
  rename_with( .fn = function(.x){paste0(.x, "_fc")}) |>
  mutate(fc_flag = "YES")
data_first_contact_vyapaaris <- data_first_contact_vyapaaris |>
  rename_with( .fn = function(.x){paste0(.x, "_fc")}) |>
  mutate(fc_flag = "YES")

# Clean the workspace
rm(data_baseline_vyapaaris)

# Pending Data
# -----------------------------------------------------------------------------
# Missing First Contact Entries
data_fc_missing <-  anti_join(data_baseline_vyapaaris_clean,
                              data_first_contact_vyapaaris_ops,
                              by = join_by(vyapaari_id_bl == vyapaari_id_fc)) |>
  filter(!(is.na(vyapaari_status_bl) | is.null(vyapaari_status_bl))) |>
  select(vyapaari_id = vyapaari_id_bl,
         vyapaari_name = vyapaari_name_bl,
         vyapaari_phone_number = vyapaari_phone_number_bl)

# Missing Baseline Entries
data_bl_missing <- anti_join(data_first_contact_vyapaaris_ops,
                             data_baseline_vyapaaris_clean,
                             by = join_by(vyapaari_id_fc == vyapaari_id_bl)) |>
  select(vyapaari_id = vyapaari_id_fc,
         vyapaari_name = vyapaari_name_fc,
         vyapaari_phone_number = phone_number_fc)

# Matching Two Datasets
data_fc_bl_matching <- full_join(data_first_contact_vyapaaris_ops,
                                 data_baseline_vyapaaris_clean,
                                 by = join_by(vyapaari_id_fc == vyapaari_id_bl)) |>
  filter(!(is.na(vyapaari_status_bl) | is.null(vyapaari_status_bl))) |>
  select(fc_phone_number = phone_number_fc,
         fc_name = vyapaari_name_fc,
         vyapaari_id = vyapaari_id_fc,
         bl_phone_number = vyapaari_phone_number_bl,
         bl_name = vyapaari_name_bl) |>
  mutate(number_match = if_else(fc_phone_number == bl_phone_number, "YES", "NO"),
         name_match = if_else(fc_name == bl_name, "YES", "NO"))

# Export both the files for ops
write_sheet(data_fc_missing, 
            ss = "https://docs.google.com/spreadsheets/d/1bcSvo_9ONt0gt8NNNNCT6W0pFBqtjzYBHjcO0YcWrEE/",
            sheet = "fc-missing-data")

write_sheet(data_bl_missing, 
            ss = "https://docs.google.com/spreadsheets/d/1bcSvo_9ONt0gt8NNNNCT6W0pFBqtjzYBHjcO0YcWrEE/",
            sheet = "bl-missing-data")

write_sheet(data_fc_bl_matching, 
            ss = "https://docs.google.com/spreadsheets/d/1bcSvo_9ONt0gt8NNNNCT6W0pFBqtjzYBHjcO0YcWrEE/",
            sheet = "fc-bl-matching")

# Clean the workspace
rm(data_fc_missing)
rm(data_bl_missing)
rm(data_fc_bl_matching)

# Ingest the cleaned data
# -----------------------------------------------------------------------------
# Read the Google sheet
data_baseline_vyapaaris_cleaned <- read_sheet("https://docs.google.com/spreadsheets/d/1O4CEb3Fq-OjMrqzZuJaPPmD88sgBvBMqsu7Bf8hE10Y/",
                                              sheet = "bl-transform-data")

# Clean Vyapaari Business
vyapaari_business <- read_sheet("https://docs.google.com/spreadsheets/d/1jH6J_Umeqyyu1gbabpI7e36797Js8YCLPm0UqveitWI/", 
                                sheet = "business-type-clean")

# Join cleaned business info
data_baseline_vyapaaris_cleaned <- data_baseline_vyapaaris_cleaned |>
  mutate(type_b1 = str_replace(str_trim(type_b1), "  ", " "),
         type_b2 = str_replace(str_trim(type_b2), "  ", " "),
         type_b3 = str_replace(str_trim(type_b3), "  ", " ")) |>
  left_join(vyapaari_business, by = join_by(type_b1 == shop_business_type), multiple = "first") |>
  left_join(vyapaari_business, by = join_by(type_b2 == shop_business_type), multiple = "first") |>
  left_join(vyapaari_business, by = join_by(type_b3 == shop_business_type), multiple = "first") |>
  rename(type_b1_clean = shop_business_type_clean.x,
         type_b2_clean = shop_business_type_clean.y,
         type_b3_clean = shop_business_type_clean)

# Extract clean businesses list
# Business #1
list_bt_1 <- data_baseline_vyapaaris_cleaned |>
  group_by(shop_business_type = type_b1, shop_business_type_clean = type_b1_clean) |>
  filter(!is.na(shop_business_type_clean)) |>
  summarise(1) |>
  select(-`1`)

# Business #2
list_bt_2 <- data_baseline_vyapaaris_cleaned |>
  group_by(shop_business_type = type_b2, shop_business_type_clean = type_b2_clean) |>
  filter(!is.na(shop_business_type_clean)) |>
  summarise(1) |>
  select(-`1`)

# Business #3
list_bt_3 <- data_baseline_vyapaaris_cleaned |>
  group_by(shop_business_type = type_b3, shop_business_type_clean = type_b3_clean) |>
  filter(!is.na(shop_business_type_clean)) |>
  summarise(1) |>
  select(-`1`)

# Final list of business names
vyapaari_business_new <- bind_rows(list_bt_1, list_bt_2, list_bt_3)

# Create clean Vyapaari Business masterlist
vyapaari_business <- bind_rows(vyapaari_business, vyapaari_business_new) |>
  filter(shop_business_type != "") |>
  arrange(shop_business_type) |>
  distinct()

# Export data
write_sheet(vyapaari_business, 
            ss = "https://docs.google.com/spreadsheets/d/1jH6J_Umeqyyu1gbabpI7e36797Js8YCLPm0UqveitWI/",
            sheet = "business-type-clean")

# Clean the workspace
rm(vyapaari_business)
rm(vyapaari_business_new)
rm(list_bt_1)
rm(list_bt_2)
rm(list_bt_3)

# Import manually reviewed Baseline data
# -----------------------------------------------------------------------------
# Create total revenue and expenses fields
data_baseline_vyapaaris_cleaned <- data_baseline_vyapaaris_cleaned |>
  mutate(all_buinesses = paste0(type_b1_clean, ", ", type_b2_clean, ", ", type_b3_clean),
         total_rev = rowSums(data_baseline_vyapaaris_cleaned[, c("b1_total_rev", "b2_total_rev", "b3_total_rev")], na.rm = TRUE),
         total_exp = rowSums(data_baseline_vyapaaris_cleaned[, c("b1_total_exp", "b2_total_exp", "b3_total_exp")], na.rm = TRUE),
         total_prof = rowSums(data_baseline_vyapaaris_cleaned[, c("b1_total_prof", "b2_total_prof", "b3_total_prof")], na.rm = TRUE))

# Create a subset for analysis
data_baseline_vyapaaris_final <- data_baseline_vyapaaris_cleaned |>
  select(vyapaari_id, enumerator_name, all_buinesses, total_rev, total_exp, total_prof, current_loan_count,
         b1_type = type_b1_clean, b1_total_rev, b1_total_exp, b1_total_prof, b1_months = time_share_b1,
         second_business, b2_type = type_b2_clean, b2_total_rev, b2_total_exp, b2_total_prof, b2_months = time_share_b2,
         third_business, b3_type = type_b3_clean, b3_total_rev, b3_total_exp, b3_total_prof, b3_months = time_share_b3,
         house_ownership, house_type, vehicle_type, total_children, total_children_school)

write_sheet(data_baseline_vyapaaris_final, 
            ss = "https://docs.google.com/spreadsheets/d/1O4CEb3Fq-OjMrqzZuJaPPmD88sgBvBMqsu7Bf8hE10Y/",
            sheet = "bl-final")
