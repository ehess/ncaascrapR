#' @title ncaa_bb_team_box_score
#' @description Obtains basketball game team box scores from the NCAA API.
#' @param game_id the game_id of a given NCAA BB game from the NCAA API
#' @return a data frame with NCAA BB team box score data for the given game
#' @export
#' @examples \donttest{
#'   ncaa_bb_team_box_score(5897924)
#' }
ncaa_bb_team_box_score <- function(game_id) {
  "https://data.ncaa.com/casablanca/game/" |>
    paste(game_id, "boxscore.json", sep="/") |>
    jsonlite::fromJSON(flatten = TRUE) |>
    (\(x){dplyr::inner_join(x$teams |>
                              dplyr::mutate(teamId = as.character(teamId)), x$meta$teams, by=c('teamId'='id'))})() |>
    as.data.frame() |>
    purrr::map_if(is.data.frame, list) |>
    dplyr::as_tibble() |>
    dplyr::select(playerTotals.fieldGoalsMade:color) |>
    dplyr::mutate(team_loc = dplyr::case_when(homeTeam == 'false' ~ 'away',
                                              T ~ 'home'),
                  team_slug = seoName,
                  team_abbr = sixCharAbbr,
                  team_short = shortName,
                  team_nick = nickName,
                  fgm = as.integer(gsub('-','',stringi::stri_extract_all_regex(playerTotals.fieldGoalsMade,'([0-9]*)-'))),
                  fga = as.integer(gsub('-','',stringi::stri_extract_all_regex(playerTotals.fieldGoalsMade,'-([0-9]*)'))),
                  fgpct = fgm / fga,
                  tpm = as.integer(gsub('-','',stringi::stri_extract_all_regex(playerTotals.threePointsMade,'([0-9]*)-'))),
                  tpa = as.integer(gsub('-','',stringi::stri_extract_all_regex(playerTotals.threePointsMade,'-([0-9]*)'))),
                  tppct = tpm / tpa,
                  ftm = as.integer(gsub('-','',stringi::stri_extract_all_regex(playerTotals.freeThrowsMade,'([0-9]*)-'))),
                  fta = as.integer(gsub('-','',stringi::stri_extract_all_regex(playerTotals.freeThrowsMade,'-([0-9]*)'))),
                  ftpct = ftm / fta,
                  treb = as.integer(playerTotals.totalRebounds),
                  oreb = as.integer(playerTotals.offensiveRebounds),
                  dreb = treb - oreb,
                  ast = as.integer(playerTotals.assists),
                  pf = as.integer(playerTotals.personalFouls),
                  stl = as.integer(playerTotals.steals),
                  to = as.integer(playerTotals.turnovers),
                  blk = as.integer(playerTotals.blockedShots),
                  pts = as.integer(playerTotals.points)
    ) |>
    dplyr::select(team_loc:pts) |>
    as.data.frame()
}
