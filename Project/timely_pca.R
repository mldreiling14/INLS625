library(stats)

ed_vars <- c("OP_18b", "OP_18c", "OP_22", "EDV")
prev_vars <- c("SAFE_USE_OF_OPIOIDS", "IMM_3", "HCP_COVID_19")


timely_grouped <- timely_wide |>
  mutate(
    ed_index = rowMeans(across(all_of(ed_vars)), na.rm = TRUE),
    prevention_index = rowMeans(across(all_of(prev_vars)), na.rm = TRUE)
  )

library(stats)

## ED PCA
timely_ed_imp <- timely_wide |>
  mutate(across(all_of(ed_vars),
                ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) |>
  select(all_of(ed_vars))

pca_ed <- prcomp(timely_ed_imp, center = TRUE, scale. = TRUE)
pca_ed

ed_scores <- as.data.frame(pca_ed$x) |>
  select(PC1) |>
  mutate(facility_id = timely_wide$facility_id) |>
  rename(ed_flow_pc1 = PC1)
ed_scores
## Prevention PCA
timely_prev_imp <- timely_wide |>
  mutate(across(all_of(prev_vars),
                ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) |>
  select(all_of(prev_vars))

pca_prev <- prcomp(timely_prev_imp, center = TRUE, scale. = TRUE)

prev_scores <- as.data.frame(pca_prev$x) |>
  select(PC1) |>
  mutate(facility_id = timely_wide$facility_id) |>
  rename(prevention_pc1 = PC1)

## Join back
timely_with_groups <- timely_wide |>
  left_join(ed_scores, by = "facility_id") |>
  left_join(prev_scores, by = "facility_id")

timely_with_groups
