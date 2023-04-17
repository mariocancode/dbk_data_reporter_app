library(dplyr)
library(tidyr)
library(lubridate)

dbk_cleaning <- read.csv("application_data.csv") %>%
  mutate_all(~case_when(. == "------" | . == "N/A" ~ NA, TRUE ~ .)) %>%
  mutate(Inspection_date = mdy_hm(Inspection_date))


dbk_inspections_umcp <- dbk_cleaning %>% 
  filter(str_detect(Name,"UMCP"))

dbk_inspections_non_umcp <- dbk_cleaning %>% 
  filter(!case_when(str_detect(Name, "UMCP") ~ TRUE, TRUE ~ FALSE))

# POTENTIAL STORY: Hotels in College Park are consecutively receiving food safety violations.
dbk_violations_hotels <- dbk_inspections_non_umcp %>% # Looking specifically at hotels in the college park area
  filter(str_detect(Category,"Hotel"))

dbk_violations_cphotel <- dbk_inspections_non_umcp %>% # Looking @ College Park Hotel and Suites
  filter(str_detect(Name,"COLLEGE PARK HOTEL & SUITES"))

dbk_violations_bestwestern <- dbk_inspections_non_umcp %>% # Looking @ Best Western Plus in CP
  filter(str_detect(Name,"BEST WESTERN PLUS COLLEGE PARK HOTEL"))

dbk_hotel_common_violations <- dbk_violations_hotels %>% # What are the most common violations among CP hotels?
  select(Food_from_approved_source,Food_protected_from_contamination,Food_contact_surfaces_and_equipment, Ill_workers_restricted, Proper_hand_washing, Proper_sewage_disposal, Cooling_time_and_temperature,Cooking_time_and_temperature,Cold_holding_temperature,Hot_holding_temperature,Hot_and_cold_running_water_provided,Reheating_time_and_temperature,No_bare_hand_contact,Adequate_hand_washing_facilities,Rodent_and_insects) %>%
  pivot_longer(cols = everything(), names_to = "violation_type", values_to = "compliance_status") %>%
  filter(compliance_status %in% c("In Compliance", "Out of Compliance")) %>%
  group_by(violation_type) %>%
  summarize(count = sum(compliance_status == "Out of Compliance")) %>%
  arrange(desc(count))