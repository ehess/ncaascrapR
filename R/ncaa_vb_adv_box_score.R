#' @title ncaa_vb_adv_box_score
#' @description Generates a simple advanced box score from volleyball play by play data.
#' @return a data frame with the following columns:
#' - service point %
#' - serve %
#' - sideout %
#' - hurtful error %
#' - block win %
#' - forfeited points
#' - total points generated
#'
#' @details How to get a game ID:
#' 1. find the game you want to view data for via stats.ncaa.org's scoreboard
#' 2. open up its box score page
#' 3. Click the play-by-play tab
#' 4. Copy the string of numbers at the end of the URL from your browser's address bar.
#'
#' @param game_id the ID for the game's play_by_play data from the box score page
#' @param away Away Team
#' @param home Home Team
#' @returns a data frame of play_by_play data for the specified game
#' @importFrom dplyr lag lead case_when filter mutate group_by ungroup summarize
#'

ncaa_vb_adv_box_score <- function(game_id, away = "Away", home = "Home") {
  # game_id = 5168298
  pbp <- ncaa_vb_pbp(game_id)

  custom_box <- pbp %>%
    dplyr::mutate(
      lag_home_score = dplyr::lag(home_score),
      lag_away_score = dplyr::lag(away_score),
      lead_home_score = dplyr::lead(home_score),
      lead_away_score = dplyr::lead(away_score),
      action_team_scored = dplyr::case_when(
        (action_type == "Error" & error_type %in% c("Ball Handling", "Set", "Block")) & (action_team == "home") & (lead_home_score > home_score) ~ TRUE,
        (action_type == "Error" & error_type %in% c("Ball Handling", "Set", "Block")) & (action_team == "away") & (lead_home_score > home_score) ~ FALSE,
        (action_type == "Error" & error_type %in% c("Ball Handling", "Set", "Block")) & (action_team == "away") & (lead_away_score > away_score) ~ TRUE,
        (action_type == "Error" & error_type %in% c("Ball Handling", "Set", "Block")) & (action_team == "home") & (lead_away_score > away_score) ~ FALSE,
        (action_team == "home") & (lag_home_score < home_score) ~ TRUE,
        (action_team == "away") & (lag_home_score < home_score) ~ FALSE,
        (action_team == "away") & (lag_away_score < away_score) ~ TRUE,
        (action_team == "home") & (lag_away_score < away_score) ~ FALSE,
        TRUE ~ NA),
      scoring_team = dplyr::case_when(
        action_team_scored ~ action_team,
        !action_team_scored & (action_team == "home") ~ "away",
        !action_team_scored & (action_team == "away") ~ "home",
        TRUE ~ NA_character_),
      block_conversion = dplyr::case_when(
        (action_type == "Block") & action_team_scored ~ TRUE,
        (action_type == "Block") & !action_team_scored ~ FALSE,
        (action_type == "Error") & (error_type == "Block") ~ FALSE,
        TRUE ~ NA),
      forfeit_point = dplyr::case_when(
        (action_type == "Error") & (scoring_team != action_team) ~ TRUE,
        TRUE ~ NA),
      attempted_serve = (error_type == "Service") | (action_type == "Serve"),
      successful_serve = (action_type == "Serve")
    )

  base_box <- custom_box %>%
    dplyr::filter(
      grepl("Sub", action_type) == FALSE,
                  grepl("Match", action_type) == FALSE,
                  grepl("Set Start", action_type) == FALSE,
                  grepl("Set End", action_type) == FALSE,
                  (nchar(action_team) > 0 & !is.na(action_team))) %>%
    dplyr::mutate(
      lead_rally_starting_team = dplyr::lead(rally_starting_team)) %>%
    dplyr::group_by(rally_number) %>%
    dplyr::mutate(
      ends_in_sideout = (action_team_scored) & (scoring_team != rally_starting_team),
      service_point = (action_team_scored) & (scoring_team == rally_starting_team)) %>%
    dplyr::ungroup()

  final_box = base_box %>%
    dplyr::group_by(rally_starting_team) %>%
    dplyr::summarize(
      # hit_pct = # hit_pct is something we should have but it is actually pretty hard to do?
      serve_pct = sum(successful_serve, na.rm = TRUE) / sum(attempted_serve, na.rm = TRUE),
      service_point_pct = sum(service_point, na.rm = TRUE) / length(unique(rally_number)),
      sideout_pct = sum(ends_in_sideout, na.rm = TRUE) / length(unique(rally_number)),
      hurtful_error_pct = sum(forfeit_point, na.rm = TRUE) / length(unique(rally_number)),
      block_win_pct = mean(block_conversion, na.rm = TRUE),
      forfeited_points = sum(forfeit_point, na.rm = TRUE), # sometimes scoring errors are in the opponent's side of the table
      points = sum(action_team_scored, na.rm = TRUE)) %>%
    dplyr::mutate(
      rally_starting_team = dplyr::case_when(
        rally_starting_team == "home" ~ home,
        rally_starting_team == "away" ~ away,
        TRUE ~ NA_character_)) %>%
    dplyr::filter(!is.na(rally_starting_team))
  return(final_box)
}
