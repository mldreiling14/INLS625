#set wd
# setwd("/home/rstudio/projects")

#Read in the data
price_data <- read.csv("ProductPriceIndex.csv")

#Load tidyverse package
library('tidyverse')

#Observe Data
glimpse(price_data)

#Unique Product name list to identify which produce is listed
unique(price_data$productname) 

#Averagespread and prices need to be converted to numeric data from characters
price_data$averagespread <- as.numeric(gsub(",", "", gsub("%", "", price_data$averagespread)))

price_data <- price_data |> mutate(across(
  c(farmprice, losangelesretail, chicagoretail, newyorkretail, atlantaretail),
  ~ as.numeric(str_remove_all(., "\\$"))
))

summary(price_data$averagespread)

# Analyze the distribution of produce prices
ggplot(data = price_data, aes(x = productname, y =averagespread)) +
                      geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, hjust =1))

#There are some clear outliers in the data. Lets identify which product had the most extreme spread.
price_data |> 
  arrange(desc(averagespread)) |>
  select(productname, averagespread) |>
  head()

Q1 <- quantile(price_data$averagespread, 0.25, na.rm = T)
Q3 <- quantile(price_data$averagespread, 0.75, na.rm = T)

IQR <- Q3 - Q1

upper_limit <- Q3 + 1.5 * IQR
lower_limit <- Q1 - 1.5 * IQR
outliers <- price_data |> filter(averagespread > upper_limit)

summary(outliers$averagespread) 

#Lets clean the data to eliminate the extreme outliers and observe 0/missing values. 

price_data |>
  summarise(
    Atlanta_zeros    = sum(atlantaretail == 0, na.rm = TRUE),
    Chicago_zeros    = sum(chicagoretail == 0, na.rm = TRUE),
    LosAngeles_zeros = sum(losangelesretail == 0, na.rm = TRUE),
    NewYork_zeros    = sum(newyorkretail == 0, na.rm = TRUE)
  )

price_data |>
  filter(atlantaretail == 0 | chicagoretail == 0 | losangelesretail == 0 | newyorkretail == 0) |>
  select(date, productname, atlantaretail, chicagoretail, losangelesretail, newyorkretail) |>
  arrange(date)

price_data <- price_data |>
  mutate(across(c(atlantaretail, chicagoretail, losangelesretail, newyorkretail),
                ~ na_if(., 0)))

price_data_clean <- price_data |>
  filter(is.finite(averagespread),
         !is.na(farmprice), farmprice > 0,
         averagespread >= lower_limit, averagespread <= upper_limit)

city_spreads <-  price_data_clean|>
    mutate(
      atlanta_spread = (atlantaretail - farmprice) / farmprice * 100, 
      chicago_spread = (chicagoretail - farmprice) / farmprice * 100, 
      la_spread = (losangelesretail - farmprice) / farmprice * 100, 
      ny_spread = (newyorkretail - farmprice) / farmprice * 100, 
    ) |>
  pivot_longer(
    cols = c(atlanta_spread, chicago_spread, la_spread, ny_spread), 
    names_to = "city",
    values_to = "markup"
  ) |>
  mutate(city = recode(city,
                       atlanta_spread = "Atlanta",
                       chicago_spread = "Chicago",
                       la_spread = "Los Angeles",
                       ny_spread = "New York"))

#Lets create a function to extract outliers in case we need to do it again 
find_outliers <- function(x) {
  Q1f <- quantile(x, 0.25, na.rm = T)
  Q3f <- quantile(x, 0.75, na.rm = T)
  IQRf <- Q3f - Q1f
  
  upper_bound <- Q3f + 1.5 * IQRf
  
  lower_bound <- Q1f - 1.5 * IQRf
  upper_bound <- Q3f + 1.5 * IQRf
  
  outlier_index <- which(x < lower_bound | x > upper_bound)
  return(list(
    outlier_values = x[outlier_index],
    indices = outlier_index,
    lower_bound = lower_bound,
    upper_bound = upper_bound
  ))
}

# Run function to get bounds
outlier_info <- find_outliers(city_spreads$markup)

# Extract outlier rows from city_spreads
outlier_points <- city_spreads |>
  filter(markup < outlier_info$lower_bound | markup > outlier_info$upper_bound)


ggplot(city_spreads, aes(x=city, y=markup)) + 
  geom_boxplot() +
  geom_hline(yintercept = 289.3, color = "red", linetype = "dashed") + 
  geom_jitter(data = outlier_points, height = 0, width = 0.05)


# Now I want to run a time series analysis to observe the produce price increase over time compared between each city. 
# First I need to adjust the data set, so the date is formatted correctly. 

avg_produce_price <- price_data_clean |> 
  mutate(date = as.Date(date, format = "%Y-%m-%d"),
         year = year(date))  |>
  group_by(year) |>
  summarize(
    Atlanta = mean(atlantaretail, na.rm = T),
    Chicago = mean(chicagoretail, na.rm = T),
    LosAngeles = mean(losangelesretail, na.rm = T),
    NewYork = mean(newyorkretail, na.rm =T)
  ) |>
  mutate(across(-year, ~  ifelse(is.nan(.x), NA_real_, .x)))

avg_produce_price |> head()

avg_produce_price_long <- avg_produce_price |>
    pivot_longer(-year, names_to = "city", values_to = "mean_price")

ggplot(avg_produce_price_long, aes(x = year, y=mean_price, color = city)) + 
    geom_line(size = 0.5) +
    labs(
      title = "Average Produce Price Over Time by City", 
      x= "Year",
      y = "Average Price ($)" 
    ) +
    theme_minimal()



