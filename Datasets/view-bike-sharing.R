library(tidyr)
library(dplyr)
library(ggplot2)

# bike sharing data --------------------

bikedata = read.csv("Datasets/bike_sharing/hour.csv")
unique_days = bikedata$dteday |> unique()
unique_days = unique_days[1:365]
gridobj = expand.grid(hr = 1:24, dteday = unique_days[1:365])
bikedata = data.frame(dteday = gridobj$dteday, hr = gridobj$hr) |>
  left_join(bikedata, by = c("dteday", "hr"))

hr = 1:24
cnt = sapply(unique_days, \(x) bikedata$cnt[bikedata$dteday == x])
temp = sapply(unique_days, \(x) bikedata$atemp[bikedata$dteday == x])
colnames(cnt) = colnames(temp) = unique_days

tidy_data = left_join(
  as.tibble(cnt) |>
    mutate(Hour = hr) |>
    pivot_longer(cols = unique_days,
      names_to = "Date", values_to = "Count"),
  as.tibble(temp * 50) |>
    mutate(Hour = hr) |>
    pivot_longer(cols = unique_days,
      names_to = "Date", values_to = "Temperature"),
  by = c("Date", "Hour")
)

tidy_data |>
  mutate(Date = as.Date.character(Date),
    Days = as.integer(Date - min(Date))) |>
  pivot_longer(cols = c("Count", "Temperature"),
    names_to = "Variable", values_to = "Value") |>
  ggplot(aes(x = Hour, y = Value, group = Date, color = Days)) +
  geom_line(alpha = 0.2) +
  facet_wrap(~Variable, scales = "free") +
  scale_color_gradientn(colors = rainbow(10)) +
  theme_bw()

