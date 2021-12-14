#' @title ncaa_bb_player_box_score
#'
#' @description Obtains basketball game player box scores from the NCAA API.
#'
#' @param game_id the game_id of a given NCAA BB game from the NCAA API
#' @return a data frame with NCAA BB player box score data for the given game
#' @importFrom rlang .data
#' @export
#' @examples \donttest{
#'   ncaa_bb_player_box_score(5897924)
#' }
ncaa_bb_player_box_score <- function(game_id) {
  "https://data.ncaa.com/casablanca/game/" |>
    paste(game_id, "boxscore.json", sep="/") |>
    jsonlite::fromJSON(flatten = TRUE) |>
    (\(x){dplyr::inner_join(x$teams |>
                              dplyr::mutate(teamId = as.character(teamId)), x$meta$teams, by=c('teamId'='id'))})() |>
    as.data.frame() |>
    purrr::map_if(is.data.frame, list) |>
    dplyr::as_tibble() |>
    tidyr::unnest(playerStats) |>
    dplyr::mutate(team_loc = dplyr::case_when(homeTeam == 'false' ~ 'away',
                                              T ~ 'home'),
                  team_slug = seoName,
                  team_abbr = sixCharAbbr,
                  team_short = shortName,
                  team_nick = nickName,
                  name = paste(firstName, lastName),
                  pos = position,
                  mp = as.numeric(minutesPlayed),
                  fgm = as.integer(gsub('-','',stringi::stri_extract_all_regex(fieldGoalsMade,'([0-9]*)-'))),
                  fga = as.integer(gsub('-','',stringi::stri_extract_all_regex(fieldGoalsMade,'-([0-9]*)'))),
                  fgpct = fgm / fga,
                  tpm = as.integer(gsub('-','',stringi::stri_extract_all_regex(threePointsMade,'([0-9]*)-'))),
                  tpa = as.integer(gsub('-','',stringi::stri_extract_all_regex(threePointsMade,'-([0-9]*)'))),
                  tppct = tpm / tpa,
                  ftm = as.integer(gsub('-','',stringi::stri_extract_all_regex(freeThrowsMade,'([0-9]*)-'))),
                  fta = as.integer(gsub('-','',stringi::stri_extract_all_regex(freeThrowsMade,'-([0-9]*)'))),
                  ftpct = ftm / fta,
                  treb = as.integer(totalRebounds),
                  oreb = as.integer(offensiveRebounds),
                  dreb = treb - oreb,
                  ast = as.integer(assists),
                  pf = as.integer(personalFouls),
                  stl = as.integer(steals),
                  to = as.integer(turnovers),
                  blk = as.integer(blockedShots),
                  pts = as.integer(.data$points)) |>
    dplyr::select(team_loc:pts) |>
    as.data.frame()

}
