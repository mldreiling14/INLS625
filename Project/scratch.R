library('tidyverse')
library('dplyr')
library('stringr')
library('janitor')

timecare <- read.csv("source_data/Timely_and_Effective_Care-Hospital.csv")
HRR <- read.csv("source_data/FY_2025_Hospital_Readmissions_Reduction_Program_Hospital.csv")

wider <- timecare |> pivot_wider(names_from = Measure.ID, values_from = Score)
wider |> glimpse()

names(timecare)

clean_score <- timecare |> mutate(
  Score = if_else(Score == 0, NA, Score),
  Score = na_if(Score, "Not Available")
)
clean_score

a <- clean_score |> group_by(Measure.ID) |> 
  summarise(Missing_count = sum(is.na(Score))) |>
  arrange(desc(Missing_count))
a

ggplot(a, aes(x=reorder(Measure.ID, -Missing_count), y = Missing_count)) + 
  geom_bar(stat = "identity") + 
  theme(
    axis.text.x = element_text(angle = 90)
  )


percent_missing <- clean_score |> group_by(Measure.ID) |>
  summarise(
    total_obs = n(),
    missing_obs = sum(is.na(Score)),
    percent_missing = missing_obs / total_obs
  ) |>
  arrange(desc(percent_missing))

percent_missing |> print(n=26)

score_count <- clean_score |> group_by(Measure.ID) |> sum()
score_missing <- clean_score |> group_by(Measure.ID) |> sum(is.na(Score))
  
mutate(percent_missing = score_count / score_missing)


score_table <- function(x){
  clean_score |>
      filter(Measure.ID == x) |>
      mutate(
        Score_numeric = suppressWarnings(as.numeric(str_remove_all(Score, "%")))
      ) |>
      select(Measure.ID, Score, Score_numeric)
}

measure_ids <- unique(clean_score$Measure.ID)

for (id in measure_ids) {
  temp_table <- score_table(id)
  safe_id <- gsub("[^A-Za-z0-9_-]", "_", id)
  file_name <- paste0("measure_", safe_id, "_scores.csv")
  write.csv(temp_table, file_name, row.names = FALSE)
}

 



