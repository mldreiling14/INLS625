library('tidyverse')
library('dplyr')

timecare <- read.csv("source_data/Timely_and_Effective_Care-Hospital.csv")
HRR <- read.csv("source_data/FY_2025_Hospital_Readmissions_Reduction_Program_Hospital.csv")

wider <- timecare |> pivot_wider(names_from = Measure.ID, values_from = Score)

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


