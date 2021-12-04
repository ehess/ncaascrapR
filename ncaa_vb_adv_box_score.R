library(tidyverse)
library(stringr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(glue)

# https://stackoverflow.com/a/53290748
Mode <- function(x) {
    if ( length(x) <= 2 ) return(x[1])
    if ( anyNA(x) ) x = x[!is.na(x)]
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

# ncaa_vb_adv_box_score <- function(game_id, away = "Away", home = "Home") {
away = "Away"
home = "Home"
    game_id = 5168298
    pbp <- ncaa_vb_pbp(game_id)

    custom_box <- pbp %>%
        mutate(
            action_team = case_when(
                action_team == "home" ~ home,
                action_team == "away" ~ away,
                TRUE ~ action_team
            ),
            lag_home_score = lag(home_score),
            lag_away_score = lag(away_score),
            lead_home_score = lead(home_score),
            lead_away_score = lead(away_score),
            action_team_scored = case_when(
                (action_type == "Error" & error_type %in% c("Ball Handling", "Set", "Block")) & (action_team == home) & (lead_home_score > home_score) ~ TRUE,
                (action_type == "Error" & error_type %in% c("Ball Handling", "Set", "Block")) & (action_team == away) & (lead_home_score > home_score) ~ FALSE,
                (action_type == "Error" & error_type %in% c("Ball Handling", "Set", "Block")) & (action_team == away) & (lead_away_score > away_score) ~ TRUE,
                (action_type == "Error" & error_type %in% c("Ball Handling", "Set", "Block")) & (action_team == home) & (lead_away_score > away_score) ~ FALSE,
                (action_team == home) & (lag_home_score < home_score) ~ TRUE,
                (action_team == away) & (lag_home_score < home_score) ~ FALSE,
                (action_team == away) & (lag_away_score < away_score) ~ TRUE,
                (action_team == home) & (lag_away_score < away_score) ~ FALSE,
                TRUE ~ NA,
            ),
            block_conversion = case_when(
                (action_type == "Block") & action_team_scored ~ TRUE,
                (action_type == "Block") & !action_team_scored ~ FALSE,
                (action_type == "Error") & (error_type == "Block") ~ FALSE,
                TRUE ~ NA
            ),
            is_kill = (action_type == "Kill"),
            is_attack_error = (action_type == "Error" & error_type %in% c("Ball Handling", "Block", "Set", "Attack", "Service") & !action_team_scored),
            is_non_kill_attack = (action_type == "Attack" & lead_action_type != "Kill"),
            is_attempt = case_when(
                is_kill ~ TRUE,
                is_attack_error ~ TRUE,
                is_non_kill_attack ~ TRUE,
                TRUE ~ NA
            ),
            forfeit_point = case_when(
                (action_type == "Error") & !action_team_scored ~ TRUE,
                TRUE ~ NA
            )
        )


    base_box <- custom_box %>%
        filter(grepl("Sub", action_type) == FALSE) %>%
        filter(grepl("Match", action_type) == FALSE) %>%
        filter(grepl("Set Start", action_type) == FALSE) %>%
        filter(grepl("Set End", action_type) == FALSE) %>%
        filter(nchar(action_team) > 0 & !is.na(action_team)) %>%
        mutate(
            lead_rally_starting_team = lead(rally_starting_team)
        ) %>%
        group_by(rally_number) %>%
        mutate(
            ends_in_sideout = (action_team_scored) & (rally_starting_team != lead_rally_starting_team)
        ) %>%
        ungroup()

    final_box = base_box %>%
        group_by(action_team) %>%
        summarize(
            kills = sum(is_kill, na.rm = TRUE),
            nonkill_attacks = sum(is_non_kill_attack, na.rm = TRUE),
            hitting_errors = sum(is_attack_error, na.rm = TRUE),
            hits = (kills - hitting_errors),
            attempts = sum(is_attempt, na.rm = TRUE),
            hit_pct = hits / attempts, # does not match box score
            sideout_pct = sum(ends_in_sideout, na.rm = TRUE) / length(unique(rally_number)),
            hurtful_error_pct = sum(forfeit_point, na.rm = TRUE) / length(unique(rally_number)),
            block_win_pct = mean(block_conversion, na.rm = TRUE),
            forfeited_points = sum(forfeit_point, na.rm = TRUE),
            points = sum(action_team_scored, na.rm = TRUE)
        ) %>%
        select(-kills, -nonkill_attacks, -hitting_errors, -hits, -attempts)
    View(final_box)
    # return(final_box)
# }
