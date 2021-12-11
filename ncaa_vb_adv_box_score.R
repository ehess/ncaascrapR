#' ncaa_vb_adv_box_score()
#' Generates a simple advanced box score from volleyball play by play data.
#' Features:
#' - service point %
#' - serve %
#' - sideout %
#' - hurtful error %
#' - block win %
#' - forfeited points
#' - total points generated
#'
#' How to get a game ID:
#' 1. find the game you want to view data for via stats.ncaa.org's scoreboard
#' 2. open up its box score page
#' 3. Click the play-by-play tab
#' 4. Copy the string of numbers at the end of the URL from your browser's address bar.
#'
#' @param game_id the ID for the game's play_by_play data from the box score page
#' @returns a data frame of play_by_play data for the specified game


library(tidyverse)
library(stringr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(glue)

ncaa_vb_adv_box_score <- function(game_id) {
    pbp <- ncaa_vb_pbp(game_id)

    team_hit_box <- pbp %>%
        filter(
            action_type == "Kill" | action_type == "Dig" | error
        ) %>%
        group_by(action_team) %>%
        summarize(
            kills = sum(action_type == "Kill", na.rm = TRUE),
            errors = sum(!is.na(error_type), na.rm = TRUE),
            attempts = n(),
            hit_pct = (kills - errors) / attempts
        ) %>%
        rename(team = action_team)


    team_error_box <- pbp %>%
        mutate(
            lag_home_score = lag(home_score),
            lag_away_score = lag(away_score),
            lead_home_score = lead(home_score),
            lead_away_score = lead(away_score),
            action_team_scored = (action_team == scoring_team),
            block_conversion = case_when(
                (action_type == "Block") & action_team_scored ~ TRUE,
                (action_type == "Block") & !action_team_scored ~ FALSE,
                (action_type == "Error") & (error_type == "Block") ~ FALSE,
                TRUE ~ NA
            ),
            forfeit_point = case_when(
                (action_type == "Error") & (!action_team_scored) ~ TRUE,
                TRUE ~ NA
            ),
            attempted_serve = (error_type == "Service") | (action_type == "Serve"),
            successful_serve = (action_type == "Serve"),
        ) %>%
        group_by(action_team) %>%
        summarise(
            hurtful_error_pct = sum(forfeit_point, na.rm = TRUE) / length(unique(rally_number)),
            serve_pct = sum(successful_serve, na.rm = TRUE) / sum(attempted_serve, na.rm = TRUE),
            forfeited_points = sum(forfeit_point, na.rm = TRUE),
            block_win_pct = mean(block_conversion, na.rm = TRUE),
        ) %>%
        rename(team = action_team)

    team_service_box <- pbp %>%
        filter(grepl("Sub", action_type) == FALSE) %>%
        filter(grepl("Match", action_type) == FALSE) %>%
        filter(grepl("Set Start", action_type) == FALSE) %>%
        filter(grepl("Set End", action_type) == FALSE) %>%
        filter(nchar(action_team) > 0 & !is.na(action_team)) %>%
        mutate(
            action_team_scored = (action_team == scoring_team),
            lead_rally_starting_team = lead(rally_starting_team)
        ) %>%
        group_by(rally_number) %>%
        filter(
            rally_play_number == last(rally_play_number)
        ) %>%
        ungroup() %>%
        mutate(
            ends_in_sideout = (action_team_scored) & (scoring_team != rally_starting_team),
            service_point = (action_team_scored) & (scoring_team == rally_starting_team)
        ) %>%
        group_by(rally_starting_team) %>%
        summarize(
            service_point_pct = sum(service_point, na.rm = TRUE) / length(unique(rally_number)),
            sideout_pct = sum(ends_in_sideout, na.rm = TRUE) / length(unique(rally_number)),
        ) %>%
        rename(team = rally_starting_team)

    team_score_box = pbp %>%
        group_by(scoring_team) %>%
        summarize(
            points = sum(scoring_play, na.rm = TRUE)
        ) %>%
        rename(team = scoring_team)

    final_box = left_join(team_hit_box,  team_error_box, by = c("team"))
    final_box = left_join(final_box,  team_service_box, by = c("team"))
    final_box = left_join(final_box,  team_score_box, by = c("team"))
    final_box <- final_box %>%
        filter(!is.na(team))
    return(final_box)
}
