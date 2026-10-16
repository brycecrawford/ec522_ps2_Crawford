pacman::p_load(tidyquant, tidyverse)

# unemp_raw <- tq_get(
#   c("UNRATE", "ORUR", "CAUR", "TXUR", "NYUR"),
#   get = "economic.data",
#   from = "1976-01-01"
# )
# 
# head(unemp_raw)
# 
# dir.create("data", showWarnings = FALSE)
# 
# write_rds(unemp_raw, "data/unemp_raw.rds")

unemp_raw = read_rds("data/unemp_raw.rds")

unemp <- unemp_raw |> 
  rename(unemployment_rate = price) |> 
  mutate(
    region = case_when(
    symbol == "UNRATE" ~ "United States",
    symbol == "ORUR" ~ "Oregon",
    symbol == "CAUR" ~ "California",
    symbol == "TXUR" ~ "Texas",
    symbol == "NYUR" ~ "New York"))

unemp <- unemp |> 
  arrange(region, date) |> 
  group_by(region) |> 
  mutate(
    unemployment_rate = replace(
      unemployment_rate,
      date == as.Date("2025-10-01"),
      unemployment_rate[date == as.Date("2025-09-01")]
    )
  ) |> 
  ungroup()

# Creating change in unemployment rate variable
unemp <- unemp |> 
  arrange(region, date) |> 
  group_by(region) |> 
  mutate(unemployment_rate_change = unemployment_rate - lag(unemployment_rate)) |> 
  ungroup()

# Creating tibble of observations from Jan 2015
unemp_recent <- unemp |> 
  filter(date >= "2015-01-01")

# Computing the mean, maximum, and minimum of unemployment rate from all observations
unemp_summary <- unemp |> 
  group_by(region) |> 
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate = min(unemployment_rate, na.rm = TRUE),
    max_rate = max(unemployment_rate, na.rm = TRUE))

# Computing the mean, maximum, and minimum of unemployment rate for observations from Jan 2015 onward
unemp_recent_summary <- unemp_recent |> 
  group_by(region) |> 
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate = min(unemployment_rate, na.rm = TRUE),
    max_rate = max(unemployment_rate, na.rm = TRUE)
  )

# finding extremes: Largest month-to-month increases
unemp_top_decreases <- unemp |> 
  slice_min(unemployment_rate_change, n = 5)

# finding extremes: Largest month-to-month decreases
unemp_top_increases <- unemp |> 
  slice_max(unemployment_rate_change, n = 5)

# Creating a tibble of observations excluding the year 2020
unemp_no_covid <- unemp |> 
  filter(year(date) != 2020)

# Finding extremes using dataset excluding covid year (2020)
unemp_top_increases_no_covid <- unemp_no_covid |> 
  slice_max(unemployment_rate_change, n = 5)

# Finding extremes using dataset excluding covid year (2020)
unemp_top_decreases_no_covid <- unemp_no_covid |> 
  slice_min(unemployment_rate_change, n = 5)


