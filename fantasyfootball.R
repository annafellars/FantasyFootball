## Add libraries
library(ffopportunity)
library(tidyverse)
library(ggplot2)
library(dplyr)

## Get data from ffoportunity
load_data <- ep_load()
build_data <-ep_build()
 
player_summary <- as.data.frame(build_data[["ep_weekly"]])
pass_plays <- as.data.frame(build_data[["ep_pbp_pass"]]) 
rush_plays <- as.data.frame(build_data[["ep_pbp_rush"]])


## First scatterplot
first_plot <- ggplot(pass_plays, aes(x = air_yards, y = ep, color = factor(complete_pass))) +
  geom_point()
first_plot

## Group by quarterback to look at individual players
QB_sum <- pass_plays |>
  mutate(complete_pass = as.numeric(as.character(complete_pass))) |>
  group_by(passer_full_name) |>
  summarise(complete_pass = sum(complete_pass, na.rm = TRUE),
            ep = sum(ep, na.rm = TRUE),
            air_yards = sum(air_yards, na.rm = TRUE)) |>
  mutate(pass_group = case_when(
    complete_pass <= 25 & complete_pass >= 0 ~ "low",
    complete_pass >= 26 & complete_pass <= 50 ~ "mid", 
    complete_pass >= 51 & complete_pass <= 75 ~ "high",
    complete_pass > 75 ~ "very high"))

## Plotting Expected Points by Air Yards and Complete Passes
qb_plot <- ggplot(QB_sum, aes(x = air_yards, y = ep, color = factor(pass_group))) +
  geom_point() +
  scale_color_manual(values = c("low" = "red", "mid" = "orange", "high" = "skyblue", "very high" = "darkgreen"),
                                labels = c("low" = "low (0-25)", "mid" = "mid (25-50) ", "high" = "high (50-75)", "very high" = "very high (75+)"),
                     limits = c("low", "mid", "high", "very high")) +
  labs(x = "Air Yards", y = "Expected Points", color = "Number of Complete Passes")
qb_plot

## Grouping by individual games (I know there are different quarterbacks that can play in a game but the majority of the time there is just the starter)
game_sum <- pass_plays |>
  mutate(complete_pass = as.numeric(as.character(complete_pass))) |>
  group_by(game_id, posteam) |>
  summarise(complete_pass = sum(complete_pass, na.rm = TRUE),
            ep = sum(ep, na.rm = TRUE),
            air_yards = sum(air_yards, na.rm = TRUE)) |>
  mutate(pass_group = case_when(
    complete_pass <= 13 & complete_pass >= 0 ~ "low",
    complete_pass >= 14 & complete_pass <= 19 ~ "mid", 
    complete_pass >= 20 & complete_pass <= 28 ~ "high",
    complete_pass > 28 ~ "very high"))

## Plotting Expected Points by Air Yards and Complete Passes
game_plot <- ggplot(game_sum, aes(x = air_yards, y = ep, color = factor(pass_group))) +
  geom_point() +
  scale_color_manual(values = c("low" = "red", "mid" = "orange", "high" = "skyblue", "very high" = "darkgreen"),
                     labels = c("low" = "low (0-13)", "mid" = "mid (14-19) ", "high" = "high (20-28)", "very high" = "very high (28+)"),
                     limits = c("low", "mid", "high", "very high")) +
  labs(x = "Air Yards", y = "Expected Points", color = "Number of Complete Passes")
game_plot
  
## Getting numbers to back the graphs

#checking for normality to see what method to use 
shapiro.test(game_sum$ep)
shapiro.test(game_sum$air_yards)
shapiro.test(game_sum$complete_pass)

#Correlation tests for air yards and complete passes 
ay_ep_corr <- cor.test(game_sum$air_yards, game_sum$ep, method = "kendall")
cp_ep_corr <- cor.test(game_sum$complete_pass, game_sum$ep, method = "kendall")

ay_ep_corr
cp_ep_corr
