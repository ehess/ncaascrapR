library(rvest)
library(tidyverse)
library(stringr)
library(dplyr)

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
      game_id = game_id,
      match_action_number = row_number(),
      action = case_when(
        (nchar(home_action) > 0 & nchar(away_action) == 0) ~ home_action,
        (nchar(home_action) == 0 & nchar(away_action) > 0) ~ away_action,
        TRUE ~ ""
      ),
      action = gsub("\\+","", action),
      action = str_trim(action, "both"),
      action_team = case_when(
        (nchar(home_action) > 0 & nchar(away_action) == 0) ~ "home",
        (nchar(home_action) == 0 & nchar(away_action) > 0) ~ "away",
        TRUE ~ ""
      ),
      action_type = case_when(
        grepl("Match started", action) ~ "Match Start",
        grepl("Match ended", action) ~ "Match End",
        grepl("Set started", action) ~ "Set Start",
        grepl("Set ended", action) ~ "Set End",
        grepl("timeout|Timeout", action) ~ "Timeout",
        grepl("Challenge|challenge", action) ~ "Challenge",
        grepl("Dig", action) ~ "Dig",
        grepl("Reception", action) ~ "Reception",
        grepl("serve|service", action) ~ "Serve",
        grepl("Set", action) ~ "Set",
        grepl("Attack", action) ~ "Attack",
        grepl("Sub in", action) ~ "Sub In",
        grepl("Sub out", action) ~ "Sub Out",
        grepl("error|Error|violation|Violation", action) ~ "Error",
        TRUE ~ "None"
      ),
      error = (action_type == "Error"),
      error_type = case_when(
        error & grepl("set|Set", action) ~ "Set",
        error & grepl("service|Service", action) ~ "Service",
        error & grepl("attack|Attack", action) ~ "Attack",
        error & grepl("Ball|ball handling|Handling", action) ~ "Ball Handling",
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
        grepl("Sub", action_type) ~ NA_integer_,
        grepl("Start", action_type) ~ NA_integer_,
        grepl("Timeout", action_type) ~ NA_integer_,
        TRUE ~ rally_number
      )
    ) %>%
    fill(score, .direction = "down") %>%
    separate(score, into = c("away_score", "home_score"), extra = "merge") %>%
    separate(action, sep = "\n", into = c("primary_action","other_actions"), extra = "merge", remove = FALSE) %>%
    mutate(
      primary_action = str_trim(primary_action, "both"),
      other_actions = str_trim(other_actions, "both"),
      involved_player = case_when(
        action_type == "Serve" ~ str_extract(primary_action, "(.*)\\s+serves"),
        action_type == "Sub In" ~ str_extract(primary_action, "in\\s+(.*)"),
        action_type == "Sub Out" ~ str_extract(primary_action, "out\\s+(.*)"),
        TRUE ~ str_extract(primary_action, "by\\s+(.*)")
      ),
      involved_player = case_when(
        action_type == "Serve" ~ sub("\\s+serves", "", involved_player),
        action_type == "Sub In" ~ sub("in\\s+", "", involved_player),
        action_type == "Sub Out" ~ sub("out\\s+", "", involved_player),
        TRUE ~ sub("by\\s+", "", involved_player)
      ),
      involved_player = str_trim(involved_player, "both")
    ) %>%
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
        grepl("Sub", action_type) ~ NA_integer_,
        grepl("Start", action_type) ~ NA_integer_,
        grepl("Timeout", action_type) ~ NA_integer_,
        TRUE ~ rally_play_number
      )
    )

  plays <- plays %>%
    select(
      game_id,
      match_action_number,
      set,
      set_play_number,
      rally_number,
      rally_play_number,
      action_type,
      action,
      primary_action,
      other_actions,
      involved_player,
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
