require(tidyverse)
library(dplyr)
library(fmsb)
library(scales)
options(scipen = 999)

deliveries <- read_csv("deliveries.csv")
matches <- read_csv("matches.csv")

matches_2016 <- matches %>%
  filter(season == "2016")

kohli_deliveries <- deliveries %>%
  filter(match_id %in% matches_2016$id,
         batter == "V Kohli")

kohli_matches <- kohli_deliveries %>%
  group_by(match_id) %>%
  summarise(
    runs = sum(batsman_runs),
    inning = min(inning)  # Assume first inning he batted is what matters
  ) %>%
  left_join(matches_2016, by = c("match_id" = "id")) %>%
  mutate(
    result = ifelse(winner == "Royal Challengers Bangalore", "Win", "Loss"),
    innings_type = ifelse(inning == 1, "1st", "2nd")
  ) %>%
  arrange(match_id) %>%
  mutate(match_number = row_number()) %>%
  select(match_id, match_number, runs, inning, innings_type, result, team1, team2, winner)


chart_1 <- ggplot(kohli_matches, aes(x = match_number, y = runs)) +
  geom_col(fill = "gray", width = 0.7) +  # Bars
  geom_line(aes(group = 1), color = "black", linewidth = 1,
            linetype = "longdash") +  # Shorter dashes
  geom_point(aes(color = result), size = 3) + 
  geom_text(aes(label = runs), vjust = -1, size = 3) +
  geom_hline(yintercept = 50, linetype = "dotted", color = "gray40") +
  geom_hline(yintercept = 100, linetype = "dotted", color = "gray40") +
  scale_color_manual(values = c("Win" = "green", "Loss" = "red")) +
  scale_x_continuous(
    breaks = kohli_matches$match_number,
    labels = kohli_matches$match_number
  ) +
  scale_y_continuous(
    breaks = c(0, 25, 50, 75, 100),
    labels = c(0, 25, 50, 75, 100)
  ) +
  labs(
    x = "Match Number",
    y = "Runs Scored",
    title = "Virat Kohli's IPL 2016 Performance Match-by-match"
  ) +
  theme_minimal()
chart_1

kohliIPL <- deliveries %>%
  filter(batter == "V Kohli") %>%
  left_join(matches %>% select(id, season), by = c("match_id" = "id")) %>%
  group_by(season) %>%
  summarise(runs = sum(batsman_runs))

kohliIPL$season <- sub(".*/", "", kohliIPL$season)
kohliIPL$season <- ifelse(nchar(kohliIPL$season) == 2,
                                   paste0("20", kohliIPL$season),
                                   kohliIPL$season)
kohliIPL$season <- as.numeric(kohliIPL$season)
kohliIPL <- kohliIPL %>%
  mutate(season = ifelse(season == 2021 & runs == 471, 2020, season))
kohliIPL$season <- factor(kohliIPL$season, levels = rev(sort(unique(kohliIPL$season))))
kohliIPL <- kohliIPL %>% arrange(-season)


chart_2 <- ggplot(kohliIPL, aes(y = season, x = runs)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = runs), hjust = -0.2, size = 3) +  # Labels on bars
  labs(
    x = "Runs Scored",
    y = "IPL Season",
    title = "Virat Kohli's IPL Runs by Season"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 10)
  )
chart_2
  
target_batsmen <- c("V Kohli", "JC Buttler", "Shubman Gill", "CH Gayle")

# STEP 1: Calculate per-match performance
batting_stats <- deliveries %>%
  left_join(matches %>% select(id, season), by = c("match_id" = "id")) %>%
  filter(
    batter %in% target_batsmen,
    (batter == "V Kohli" & season == 2016) |
      (batter == "JC Buttler" & season == 2022) |
      (batter == "Shubman Gill" & season == 2023) |
      (batter == "CH Gayle" & season == 2011)
  ) %>%
  group_by(batter, season, match_id) %>%
  summarise(
    match_runs = sum(batsman_runs),
    boundaries = sum(batsman_runs %in% c(4, 6)),
    .groups = "drop"
  )

# STEP 2: Count legal balls faced
balls_per_delivery <- deliveries %>%
  left_join(matches %>% select(id, season), by = c("match_id" = "id")) %>%
  filter(
    batter %in% target_batsmen,
    (batter == "V Kohli" & season == 2016) |
      (batter == "JC Buttler" & season == 2022) |
      (batter == "Shubman Gill" & season == 2023) |
      (batter == "CH Gayle" & season == 2011),
    is.na(extras_type) | extras_type != "wides"
  ) %>%
  group_by(batter, season, match_id) %>%
  summarise(
    legal_balls = n(),
    .groups = "drop"
  )

# STEP 3: Count dismissals
dismissals <- deliveries %>%
  left_join(matches %>% select(id, season), by = c("match_id" = "id")) %>%
  filter(
    player_dismissed %in% target_batsmen,
    (player_dismissed == "V Kohli" & season == 2016) |
      (player_dismissed == "JC Buttler" & season == 2022) |
      (player_dismissed == "Shubman Gill" & season == 2023) |
      (player_dismissed == "CH Gayle" & season == 2011)
  ) %>%
  group_by(player_dismissed, season) %>%
  summarise(
    dismissals = n(),
    .groups = "drop"
  ) %>%
  rename(batter = player_dismissed)

# STEP 4: Build summary table
batting_summary <- batting_stats %>%
  left_join(balls_per_delivery, by = c("batter", "season", "match_id")) %>%
  group_by(batter, season) %>%
  summarise(
    total_runs = sum(match_runs),
    total_balls = sum(legal_balls),
    fifties = sum(match_runs >= 50),
    total_boundaries = sum(boundaries),
    .groups = "drop"
  ) %>%
  left_join(dismissals, by = c("batter", "season")) %>%
  mutate(
    batting_average = round(total_runs / dismissals, 2),
    strike_rate = round((total_runs / total_balls) * 100, 2)
  )

# STEP 5: Normalize all 5 metrics
batting_radar_data <- batting_summary %>%
  select(batter, total_runs, fifties, strike_rate, total_boundaries, batting_average) %>%
  mutate(across(where(is.numeric), rescale))

# STEP 6: Plotting function with 5 metrics
plot_radar_for_player <- function(player_name, fill_color = "red") {
  
  player_data <- batting_radar_data %>%
    filter(batter == player_name) %>%
    select(total_runs, fifties, strike_rate, total_boundaries, batting_average) %>%
    as.data.frame()
  
  # Add required MAX and MIN rows
  player_radar <- rbind(
    rep(1, ncol(player_data)),  # Max values
    rep(0, ncol(player_data)),  # Min values
    player_data                 # Player's normalized data
  )
  
  colnames(player_radar) <- c("Runs", "50+ Scores", "Strike Rate", "Boundaries", "Batting Avg")
  
  radarchart(
    player_radar,
    axistype = 1,
    pcol = fill_color,
    pfcol = scales::alpha(fill_color, 0.4),
    plwd = 2,
    cglcol = "grey70",
    cglty = 1,
    axislabcol = "grey50",
    caxislabels = c("0", "0.25", "0.5", "0.75", "1"),
    vlcex = 0.9,
    title = player_name
  )
}
plot_radar_for_player("V Kohli", fill_color = "red")
plot_radar_for_player("JC Buttler", fill_color = "blue")
plot_radar_for_player("Shubman Gill", fill_color = "green")
plot_radar_for_player("CH Gayle", fill_color = "purple")