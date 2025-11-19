timecare <- read.csv("source_data/Timely_and_Effective_Care-Hospital.csv")

timecare <- timecare |> clean_names()

clean_score <- timecare |> mutate(
  score = if_else(score == 0, NA, score),
  score = na_if(score, "Not Available")
)

missing_data <- clean_score |> group_by(measure_id) |> 
  summarise(missing_count = sum(is.na(score))) |>
  arrange(desc(missing_count))

ggplot(missing_data, aes(x=reorder(measure_id, -missing_count), y = missing_count)) + 
  geom_bar(stat = "identity") + 
  theme(
    axis.text.x = element_text(angle = 90)
  ) 

percent_missing <- clean_score |> group_by(measure_id) |>
  summarise(
    total_obs = n(),
    missing_obs = sum(is.na(score)),
    percent_missing = missing_obs / total_obs
  ) |>
  arrange(desc(percent_missing))

percent_missing |> print(n=26)


timely_clean <- timecare |>
  mutate(
    score_chr = as.character(score),
    score_chr = str_trim(score_chr),
    score_chr = case_when(
      str_to_lower(score_chr) %in% c("not available","n/a","na","--","-","*","data suppressed") ~ NA_character_,
      str_to_lower(score_chr) == "low" ~ "1",
      str_to_lower(score_chr) == "medium" ~ "2",
      str_to_lower(score_chr) == "high" ~ "3",
      str_to_lower(score_chr) == "very high" ~ "4",
      TRUE ~ str_remove_all(score_chr, "%")
    ),
    score_num = suppressWarnings(as.numeric(score_chr))
  )


keep_ids <- percent_missing |>
  filter(percent_missing < 0.40) |>
  pull(measure_id)

keep_ids
timely_kept <- timely_clean |>
  filter(measure_id %in% keep_ids)

timely_wide <- timely_kept |>
  select(facility_id, measure_id, score_num) |>
  distinct() |>
  pivot_wider(
    id_cols = facility_id,
    names_from = measure_id,
    values_from = score_num
  )
timely_wide

dim(timely_wide)
colSums(is.na(timely_wide)) / nrow(timely_wide)

write.csv(timely_wide, "manufactured_data/timely_wide.csv", row.names = FALSE)

