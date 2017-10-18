library(dplyr)
library(purrr)
library(lubridate)
library(ggplot2)


first.date <- min(df$date)
current.date <- Sys.Date()
years <- unique(df$year)


##year

map(years, function(x) filter(df, year == x) %>%
      mutate(period = case_when(
        .$date <= as.Date(paste0(x, "-",
                                month(current.date), "-", 
                                 day(current.date))) ~ "Year-to-Date",
        TRUE ~ "Rest-of-Year"))) %>%
  bind_rows() %>%
  filter(year != year(first.date)) %>%
  group_by(year, period) %>%
  summarise(total = sum(minuter)) %>%
  ggplot(aes(x = as.character(year), y = total, fill = period)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=total), vjust = -2)

##month

map(years, function(x) filter(df, year == x & month == month(current.date)) %>%
      mutate(period = case_when(
        .$date <= as.Date(paste0(x, "-",
                                 month(current.date), "-", 
                                 day(current.date))) ~ "Month-to-Date",
        TRUE ~ "Rest-of-Month"))) %>%
  bind_rows() %>%
  filter(year_month != paste0(year(first.date), "-", month(first.date))) %>%
  group_by(year_month, period) %>%
  summarise(total = sum(minuter)) %>%
  ggplot(aes(x = year_month, y = total, fill = period)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=sum(total)), vjust = -2) +
  geom_col(position = position_stack(reverse = TRUE))









