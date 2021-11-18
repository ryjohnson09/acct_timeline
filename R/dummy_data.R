# Create dummy data
library(tidyverse)
library(lubridate)
library(odbc)
library(DBI)
library(dbplyr)

ex_data_raw <- tribble(
  ~event_type, ~event_date, ~event_label, ~event_notes, ~x_axis,
  "sales", "2021-8-1", "Opp Renewal", "notes go here", 0,
  "sales", "2021-8-1", "Expansion opp", "notes go here",0,
  "CS", "2021-8-3", "Demo", "demo notes go here",0,
  "CS", "2021-9-12", "Q checkin", "Some notes here",0,
  "sales", "2021-10-3", "Opp Renewal", "notes go here",0,
  "CS", "2021-11-26", "Q checkin", "Q checkin notes go here", 0
)

ex_data <- ex_data_raw %>% 
  mutate(across(event_date, as_date))


# Month label df
month_date_range <- seq(lubridate::today() - months(12),
                        lubridate::today() + months(2), by='month')
month_data <- tibble(floor_date(month_date_range, unit = "month")) %>% 
  mutate(month_label = month(month_date_range, label = TRUE, abbr = TRUE)) %>% 
  mutate(year_label = year(month_date_range))


# Salesforce stuff
# Setup connections
con <- DBI::dbConnect(odbc::odbc(),
                      database = "marketing", 
                      Driver = "Redshift",
                      host = "redhouse.cyii7eabibhu.us-east-1.redshift.amazonaws.com", 
                      port = 5439, 
                      UID = Sys.getenv("WAREHOUSE_USER"), 
                      PWD = Sys.getenv("WAREHOUSE_PASSWORD"))

# Get Account names
account_info <- tbl(con, in_schema("salesforce", "account")) %>%
  filter(pod_team_c == "Ent LSH") %>% 
  filter(is_deleted == "0") %>% 
  filter(type == "Customer") %>% 
  filter(customer_success_team_member_c == "0050L000009iXh7QAE") %>% 
  collect() %>% 
  select(acct_id = id, acct_name = name,
         acct_cs_owner_id = customer_success_team_member_c,
         acct_owner_id = owner_id)


# Get Opps
opps <- tbl(con, in_schema("salesforce", "opportunity")) %>% 
  filter(is_deleted == "0") %>%
  filter(!stage_name %in% c("Opportunity Disqualified")) %>% 
  collect() %>% 
  select(opp_id = id, 
         acct_id = account_id, 
         opp_name = name, 
         opp_stage = stage_name,
         opp_amount = amount,
         opp_close_date = close_date,
         opp_type = type,
         opp_products = product_interest_c,
         license_start_date = license_start_date_c,
         license_end_date = license_end_date_c) %>%
  filter(acct_id %in% account_info$acct_id)

opp_final <- opps %>% 
  select(event_subtype = opp_stage,
         event_date = opp_close_date,
         event_label = opp_type,
         event_notes = opp_name,
         event_acct_id = acct_id) %>%
  mutate(event_type = "Sales") %>% 
  select(event_type, event_subtype, everything())




# Get call notes
calls <- dplyr::tbl(con, in_schema("salesforce", "task")) %>%
  filter(account_id %in% !!account_info$acct_id) %>%
  filter(owner_id %in% c("0050L000009iXh7QAE")) %>%
  select(
    call_id = id,
    call_subject = subject,
    call_what_id = what_id,
    call_date = activity_date,
    call_status = status,
    call_description = description,
    call_subtype = task_subtype,
    call_category = meeting_category_c,
    call_type = type,
    call_acct_id = account_id,
    call_who_id = who_id,
    call_owner_id = owner_id,
    call_created_date = created_date,
    call_created_by_id = created_by_id
  ) %>%
  collect() %>%
  mutate(label = case_when(
    (call_subtype == 'Call' & call_type == 'Call') ~ 'Call',!is.na(call_subtype) ~ 'Email'
  )) %>%
  # Filter for just calls
  filter(label == "Call")

calls_final <- calls %>% 
  select(event_subtype = call_category,
         event_date = call_date,
         event_label = call_category,
         event_notes = call_description,
         event_acct_id = call_acct_id) %>% 
  mutate(event_type = "CS") %>% 
  select(event_type, everything())
  

# Final merge
final_data <- full_join(opp_final, calls_final) %>% 
  # add in acct name
  left_join(select(account_info, acct_id, acct_name), by = c("event_acct_id" = "acct_id")) %>% 
  mutate(x_axis = 0) %>% 
  filter(acct_name == "Merck") %>% 
  filter(between(event_date, min(month_date_range), max(month_date_range)))
