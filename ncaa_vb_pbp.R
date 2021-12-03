library(rvest)
library(tidyverse)
library(lubridate)
library(glue)
library(httr)
library(jsonlite)
library(stringr)
library(logging)
library(dplyr)
# game_id = 5168298
ncaa_vb_pbp <- function(game_id) {
  base_url <- "https://stats.ncaa.org/game/play_by_play"

  full_url <- paste(base_url, game_id, sep="/")

  base <- rvest::read_html(full_url)

  play_nodes <- base %>%
    html_nodes(".mytbl + .mytable") %>%
    html_nodes("tr:not(.grey_heading)")

  play_away_actions <- play_nodes %>%
    html_nodes("td:nth_child(1):not(:only-child)") %>%
    html_text() %>%
    str_trim("both")

  play_score <- play_nodes %>%
    html_nodes("td:nth_child(2)") %>%
    html_text() %>%
    str_trim("both")

  play_home_actions <- play_nodes %>%
    html_nodes("td:nth_child(3)") %>%
    html_text() %>%
    str_trim("both")

  max_length <- max(c(length(play_home_actions), length(play_away_actions), length(play_score)))
  play_away_actions <- c(play_away_actions, rep(NA, max_length - length(play_away_actions)))
  play_home_actions <- c(play_home_actions, rep(NA, max_length - length(play_home_actions)))
  play_score <- c(play_score, rep(NA, max_length - length(play_score)))

  base_plays <- data.frame(
    away_action = play_away_actions,
    score = play_score,
    home_action = play_home_actions,
    stringsAsFactors = FALSE
  )

  plays <- base_plays %>%
    mutate(
      match_action_number = row_number(),
      action = case_when(
        (nchar(home_action) > 0 & nchar(away_action) == 0) ~ home_action,
        (nchar(home_action) == 0 & nchar(away_action) > 0) ~ away_action,
        TRUE ~ ""
      ),
      action_team = case_when(
        (nchar(home_action) > 0 & nchar(away_action) == 0) ~ "home",
        (nchar(home_action) == 0 & nchar(away_action) > 0) ~ "away",
        TRUE ~ ""
      ),
      action_type = case_when(
        grepl("Match started", action) == TRUE ~ "Match Start",
        grepl("Match ended", action) == TRUE ~ "Match End",
        grepl("Set started", action) == TRUE ~ "Set Start",
        grepl("Set ended", action) == TRUE ~ "Set End",
        grepl("timeout|Timeout", action) == TRUE ~ "Timeout",
        grepl("Challenge|challenge", action) == TRUE ~ "Challenge",
        grepl("Dig", action) == TRUE ~ "Dig",
        grepl("Reception", action) == TRUE ~ "Reception",
        grepl("serve|service", action) == TRUE ~ "Serve",
        grepl("Set", action) == TRUE ~ "Set",
        grepl("Attack", action) == TRUE ~ "Attack",
        grepl("Sub|sub in", action) == TRUE ~ "Sub In",
        grepl("Sub|sub out", action) == TRUE ~ "Sub Out",
        grepl("error|Error|violation|Violation", action) == TRUE ~ "Error",
        TRUE ~ "None"
      ),
      error = (action_type == "Error"),
      error_type = case_when(
        error & grepl("set|Set", action) ~ "Set",
        error & grepl("service|Service", action) ~ "Service",
        error & grepl("attack|Attack", action) ~ "Attack",
        error & grepl("set|Set", action) ~ "Set",
        error & grepl("block|Block", action) ~ "Block",
        error & grepl("footfall|Footfall", action) ~ "Footfall Violation",
        error & grepl("net|net", action) ~ "Net Violation",
        TRUE ~ ""
      ),
      lag_action_type = lag(action_type, default = NA),
      lead_action_type = lead(action_type, default = NA),
      scoring_play = nchar(score) > 0,
      score = case_when(
        action_type == "Match Start" ~ "0-0",
        action_type == "Set Start" ~ "0-0",
        (nchar(score) == 0) ~ as.character(NA),
        TRUE ~ score
      ),
      set = cumsum(action_type == "Set Start"),
      set = ifelse(set == 0, 1, set),
      rally_number = cumsum(action_type == "Serve"),
      rally_number = case_when(
        grepl("Sub", action_type) == TRUE ~ as.integer(NA),
        grepl("Start", action_type) == TRUE ~ as.integer(NA),
        grepl("Timeout", action_type) == TRUE ~ as.integer(NA),
        TRUE ~ rally_number
      )
    ) %>%
    fill(score, .direction = "down") %>%
    separate(score, into = c("away_score", "home_score"), extra = "merge") %>%
    group_by(rally_number) %>%
    mutate(
      rally_play_number = row_number(),
      rally_start = (min(rally_play_number) == rally_play_number),
      rally_end = (max(rally_play_number) == rally_play_number)
    ) %>%
    ungroup() %>%
    group_by(set) %>%
    mutate(
      set_play_number = row_number(),
    ) %>%
    ungroup() %>%
    mutate(
      rally_play_number = case_when(
        grepl("Sub", action_type) == TRUE ~ as.integer(NA),
        grepl("Start", action_type) == TRUE ~ as.integer(NA),
        grepl("Timeout", action_type) == TRUE ~ as.integer(NA),
        TRUE ~ rally_play_number
      )
    )

  plays <- plays %>%
    select(-home_action, -away_action) %>%
    select(
      match_action_number,
      set,
      set_play_number,
      rally_number,
      rally_play_number,
      action_type,
      action,
      away_score,
      home_score,
      scoring_play,
      error,
      error_type,
      lag_action_type,
      lead_action_type,
      rally_start,
      rally_end
    )

  return(plays)
}
ncaa_vb_pbp(5168298)
