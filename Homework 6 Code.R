# Lance Allan
# Homework 6

library(tidyverse)
library(data.table)
library(ggplot2)
library(extraDistr)
library(lubridate)

# data setup
num_households <- 1200
df <- fread("charging_data.csv")
df$DateTime <- as.POSIXct(df$DateTime, format = "%Y-%m-%d %H:%M:%S")

# extract DateTime parts
df$Month <- month(df$DateTime)
df$Day <- day(df$DateTime)
df$Hour <- hour(df$DateTime)
df$Minute <- minute(df$DateTime)

# set treatment and pre/post
treatment_month <- 6

df$Treated <- ifelse(df$Household <= 600, "Control", "Treatment")
# set 15-minute periods, you can change this to a length of time you think is relevant
df$Period <- (df$Hour * 4) + (df$Minute / 15) + 1
df$Post <- ifelse(df$Month >= treatment_month, 1, 0)

#number of days in pre-treatment period (set correctly)
pre_treat_days <- 100

#save pre-treatment data
df_pre <- df %>%
  filter(Month < treatment_month) %>%
  group_by(Treated, Period) %>%
  summarise(Tot_kWh = sum(ChargingPower)) %>%
  ungroup()
df_pre <- df_pre %>%
  mutate(Avg_kWh = Tot_kWh / ( pre_treat_days* (num_households / 2)))

# fill in missing periods with 0 kWh
periods <- 1:96
df_fill <- data.frame(Treated = c("Control", "Treatment"), Period = rep(periods, 2))
pre <- merge(df_fill, df_pre, by = c("Treated", "Period"), all.x = TRUE)
pre$Avg_kWh <- ifelse(is.na(pre$Avg_kWh), 0, pre$Avg_kWh)

# perform similar process to make df_post
post_treat_days <- 100

df_post <- df %>%
  filter(Month >= treatment_month) %>%
  group_by(Treated, Period) %>%
  summarise(Tot_kWh = sum(ChargingPower)) %>%
  ungroup()

df_post <- df_post %>%
  mutate(Avg_kWh = Tot_kWh / ( post_treat_days* (num_households / 2)))

# fill in missing periods with 0 kWh
post <- merge(df_fill, df_post, by = c("Treated", "Period"), all.x = TRUE)
post$Avg_kWh <- ifelse(is.na(post$Avg_kWh), 0, post$Avg_kWh)

df %>%
  filter(Period >= 73 & Period <= 85) %>%
  group_by(Month, Treated) %>%
  summarise(avg_charging_power = mean(ChargingPower)) %>%
  ggplot(aes(x = Month, y = avg_charging_power, color = factor(Treated))) +
  geom_line() +
  labs(title = "Average Charging Power by Month for Treatment and Control Groups (6-9pm)",
       x = "Month",
       y = "Average Charging Power (kW)")

df %>%
  filter(Period >= 73 & Period <= 85) %>%
  group_by(Month) %>%
  summarise(diff_avg_charging_power = mean(ChargingPower[Treated == 'Treatment']) - mean(ChargingPower[Treated == 'Control'])) %>%
  ggplot(aes(x = Month, y = diff_avg_charging_power)) +
  geom_line() +
  labs(title = "Treatment-Control Difference in Average Charging Power by Month (6-9pm)",
       x = "Month",
       y = "Treatment-Control Difference (kW)")

# Merge dataframes
df_merge <- bind_rows(mutate(pre, Post = 0), mutate(post, Post = 1))
df_merge <- mutate(df_merge, Group = paste(Treated, Post, sep = "_"))

# plot average load shape from control/treatment before and after
df_merge %>%
  ggplot(aes(x = Period, y = Avg_kWh, color = factor(Group))) +
  geom_line() +
  labs(title = "Average Charging Load Shape by Time of Day",
       x = "Time of Day (15-minute increments)",
       y = "Average Charging Power (kWh)")

# diff-in-diff estimate
df_did <- df_merge %>%
  reframe(diff_in_diff = (Avg_kWh[Group == "Treatment_1"] - Avg_kWh[Group == "Treatment_0"]) - (Avg_kWh[Group == "Control_1"] - Avg_kWh[Group == "Control_0"]))

df_did %>%
  ggplot(aes(x = periods, y = diff_in_diff)) +
  geom_line() +
  labs(title = "Diff-in-Diff Estimate of the Effect of Managed Charging",
    x = "Time of Day (15-minute increments)",
    y = "Diff-in-Diff Estimate (kWh)")