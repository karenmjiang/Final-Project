# Data Preparation for ACO Data

library(readr)
library(janitor)
library(tidyverse)
library(stringr)


# ACO Dataset -------------------------------------------------------------

aco <-
  
  # Read in CSV data
  read.csv(
    "data/2018_Shared_Savings_Program__SSP__Accountable_Care_Organizations__ACO__PUF.csv"
  ) %>%
  
  # Selected Columns
  select(
    ACO_ID,
    Initial_Start_Date,
    Agree_Type,
    ACO_State,
    QualScore,
    Current_Track_1,
    Current_Track_1_Plus,
    Current_Track_2,
    Current_Track_3,
    Initial_Track_1,
    Initial_Track_1_Plus,
    Initial_Track_2,
    Initial_Track_3,
    N_AB,
    N_AB_Year_PY,
    N_AB_Year_ESRD_PY,
    N_AB_Year_DIS_PY,
    N_AB_Year_AGED_Dual_PY,
    N_AB_Year_AGED_NonDual_PY,
    contains("N_Ben_"),
    BnchmkMinExp,
    N_PCP,
    N_Spec,
    N_NP,
    N_PA,
    N_CNS
  ) %>%
  
  # Turning ACO_State into iterable lists
  mutate(ACO_State = map(ACO_State,
                         ~ unlist(str_split(.x, ", ")))) %>%
  
  # Counting the number of States each ACO operates within
  mutate(num_states = map(ACO_State, length),
         num_states = as.integer(num_states)) %>%
  
  # Collapsing Dummy Variables around Current Track
  gather(current, flag, Current_Track_1:Current_Track_3) %>%
  filter(flag == 1) %>%
  select(-flag) %>%
  mutate(current = str_replace_all(current, "_", " "),
         current = str_replace(current, "Current ", "")) %>%
  
  # Collapsing Dummy Variables around Initial Track
  gather(initial, flag2, Initial_Track_1:Initial_Track_3) %>%
  filter(flag2 == 1) %>%
  select(-flag2) %>%
  mutate(initial = str_replace_all(initial, "_", " "),
         initial = str_replace(initial, "Initial ", "")) %>%
  
  # Changing Start Date into date type, and extracting year
  mutate(
    Initial_Start_Date = as.Date(Initial_Start_Date, format = "%m/%d/%Y"),
    Initial_Start_Year = as.factor(format(Initial_Start_Date, "%Y"))
  ) %>%
  
  # Calculating total providers
  mutate(
    total_providers = N_PCP + N_Spec + N_NP + N_PA + N_CNS
  )

saveRDS(aco, file = "clean_data/aco.RDS")
saveRDS(aco, file = "about/data/aco.RDS")


# County Dataset ----------------------------------------------------------

county <-
  
  # Read in CSV data
  read.csv("data/2018_County-level_FFS_Data_for_Shared_Savings_Program_Benchmark_PUF.csv") %>%
  
  # Clean column headers
  clean_names() %>%
  
  # Tidy data into longer format
  pivot_longer(
    cols = per_capita_esrd_expenditures:aged_non_dual_person_years,
    names_to = "Names",
    values_to = "Values"
  ) %>%
  
  # Create new columns in more readable format
  mutate(
    Beneficiary = case_when(
      str_detect(Names, "disabled") ~ "Disabled",
      str_detect(Names, "esrd") ~ "End Stage Renal Disease",
      str_detect(Names, "aged_dual") ~ "Aged Dual",
      str_detect(Names, "aged_non_dual") ~ "Aged Non-Dual"
    ),
    Category = case_when(
      str_detect(Names, "per_capita") ~ "Per Capita Expenditures",
      str_detect(Names, "risk_score") ~ "Average HCC Risk Score",
      str_detect(Names, "person_years") ~ "Person Years"
    )
  ) %>%
  
  # Re-order column names
  select(-Names)

saveRDS(county, file = "clean_data/county.RDS")
saveRDS(county, file = "about/data/county.RDS")

