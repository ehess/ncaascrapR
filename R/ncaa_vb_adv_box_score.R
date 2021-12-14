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
#' @importFrom dplyr lag lead case_when filter mutate group_by ungroup summarize left_join first last n
#'

ncaa_vb_adv_box_score <- function(game_id, away = "Away", home = "Home") {
  # game_id = 5168298
  pbp <- ncaa_vb_pbp(game_id)

  team_hit_box <- pbp %>%
    dplyr::filter(action_type == "Kill" | action_type == "Dig" | error) %>%
    dplyr::group_by(action_team) %>%
    dplyr::summarize(
      kills = sum(action_type == "Kill", na.rm = TRUE),
      errors = sum(!is.na(error_type), na.rm = TRUE),
      attempts = dplyr::n(),
      hit_pct = (kills - errors) / attempts) %>%
    dplyr::rename(team = action_team)

  team_error_box <- pbp %>%
    dplyr::mutate(
      lag_home_score = dplyr::lag(home_score),
      lag_away_score = dplyr::lag(away_score),
      lead_home_score = dplyr::lead(home_score),
      lead_away_score = dplyr::lead(away_score),
      action_team_scored = (action_team == scoring_team),
      block_conversion = dplyr::case_when(
        (action_type == "Block") & action_team_scored ~ TRUE,
        (action_type == "Block") & !action_team_scored ~ FALSE,
        (action_type == "Error") & (error_type == "Block") ~ FALSE,
        TRUE ~ NA
      ),
      forfeit_point = dplyr::case_when(
        (action_type == "Error") & (!action_team_scored) ~ TRUE,
        TRUE ~ NA
      ),
      attempted_serve = (error_type == "Service") | (action_type == "Serve"),
      successful_serve = (action_type == "Serve")) %>%
    dplyr::group_by(action_team) %>%
    dplyr::summarise(
      hurtful_error_pct = sum(forfeit_point, na.rm = TRUE) / length(unique(rally_number)),
      serve_pct = sum(successful_serve, na.rm = TRUE) / sum(attempted_serve, na.rm = TRUE),
      forfeited_points = sum(forfeit_point, na.rm = TRUE),
      block_win_pct = mean(block_conversion, na.rm = TRUE)) %>%
    dplyr::rename(team = action_team)

  team_rally_box <- pbp %>%
    dplyr::filter(grepl("Sub", action_type) == FALSE) %>%
    dplyr::filter(grepl("Match", action_type) == FALSE) %>%
    dplyr::filter(grepl("Set Start", action_type) == FALSE) %>%
    dplyr::filter(grepl("Set End", action_type) == FALSE) %>%
    dplyr::filter(nchar(action_team) > 0 & !is.na(action_team)) %>%
    dplyr::mutate(
      action_team_scored = (action_team == scoring_team),
      lead_rally_starting_team = lead(rally_starting_team),
      ends_in_sideout = (action_team_scored) & (scoring_team != rally_starting_team),
      service_point = (action_team_scored) & (scoring_team == rally_starting_team)) %>%
    dplyr::group_by(rally_number) %>%
    dplyr::filter(rally_play_number == last(rally_play_number)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(scoring_team) %>%
    dplyr::summarize(
      avg_ttk = mean(rally_total_plays, na.rm = TRUE),
      service_point_pct = mean(service_point, na.rm = TRUE),
      sideout_pct = mean(ends_in_sideout, na.rm = TRUE)) %>%
    dplyr::rename(team = scoring_team)

  team_score_box = pbp %>%
    dplyr::group_by(scoring_team) %>%
    dplyr::summarize(points = sum(scoring_play, na.rm = TRUE)) %>%
    dplyr::rename(team = scoring_team)

  final_box = dplyr::left_join(team_hit_box, team_rally_box, by = c("team"))
  final_box = dplyr::left_join(final_box, team_error_box, by = c("team"))
  final_box = dplyr::left_join(final_box, team_score_box, by = c("team"))
  final_box <- final_box %>%
    dplyr::filter(!is.na(team))
  return(final_box)
}
