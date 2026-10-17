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

# Part 5

# Plot comparing unemployment rate per region over time
ggplot(data = unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() +
  labs(
    x = "Date",
    y = "Unemployment Rate (%)",
    title = "Unemployment Rate by Region") +
  theme_minimal()

# Creating individual graphs of unemployment rate per region over time
ggplot(data = unemp, aes(x = date, y = unemployment_rate)) +
  geom_line() +
  labs(
    x = "Date",
    y = "Unemployment Rate (%)",
    title = "Unemployment Rate Broken Up by Region") +
  facet_wrap(~region) +
  theme_minimal()

# Bar graph comparing mean unemployment rate per region
ggplot(data = unemp_summary, aes(x =fct_reorder(region,mean_rate), y = mean_rate, fill = region)) +
  scale_fill_brewer(palette = "Set1") +
  geom_col() +
  labs(
    x = "Region",
    y = "Unemployment Rate (%)",
    title = "Mean Unemployment Rate by Region"
  )

macro <- unemp |> 
  filter(year(date) >= 2017) |> 
  filter(year(date) <= 2023)

tariff_unemp <- unemp |> 
  filter(date >= as.Date("2024-01-01"), region == "United States")

ggplot(tariff_unemp, aes( x = date, y = unemployment_rate)) +
  geom_line() + 
  geom_vline(xintercept = as.Date("2025-04-01"),linetype = "dashed", color = "red") +
  annotate("text", x = as.Date("2025-04-01"), y = Inf, hjust = -0.05, vjust = 27, label = "2025-04-01") +
  labs(
    x = "Year",
    y = "Unemployment Rate (%)",
    title = "Unemployment Rate After Liberation Day Tariffs (2025-04-02)")
  

# I chose to plot the unemployment rate between 2024 and 2026 to see how it changed after Trump invoked the International Emergency Economic Powers Act (IEEPA) to impose global reciprocal tariffs. From listening to podcasts and reading news during that time, it sounded like companies were laying off employees. The tariffs were implemented in phases so there wasn't as large as a shock as I was expecting. It does seem to show an association since unemployment did rise after Liberation day. 


