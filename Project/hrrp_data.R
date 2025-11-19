install.packages('janitor')
library('janitor')
library('dplyr')
library('tidyverse')
hrrp <- read.csv("source_data/FY_2025_Hospital_Readmissions_Reduction_Program_Hospital.csv") |>
  clean_names()

names(hrrp)

target_col <- "excess_readmission_ratio"

hrrp_target <- hrrp |>
  mutate(
    excess_readmission_ratio = as.numeric(excess_readmission_ratio)
  ) |>
  select(
    facility_id,
    target = excess_readmission_ratio
  ) |>
  filter(!is.na(target))

hrrp_target <- hrrp_target |>
  mutate(facility_id = str_pad(as.character(facility_id), width = 6, pad = "0"))

timely_with_groups <- timely_with_groups |>
  mutate(facility_id = str_pad(as.character(facility_id), width = 6, pad = "0"))


model_df <- hrrp_target |>
  inner_join(timely_with_groups, by = "facility_id")

write.csv(model_df, "manufactured_data/model_df.csv", row.names = FALSE)