#' get_bb_player_box
#'
#' Obtains player box score data for a given basketball game_id from stats.ncaa.org.
#'
#' @param game_id the game_id for a given game on stats.ncaa.org
#' @return a dataframe for box score data for that game
get_bb_player_box <- function(game_id){
  html <- paste0('https://stats.ncaa.org/game/box_score/',game_id) |>
    rvest::read_html()
  html |>
    rvest::html_node(xpath = '/html/body/div[2]/table[5]') |>
    rvest::html_table() |>
    dplyr::filter(X1 != X2 & !(X1 %in% c('Player','TEAM','Totals'))) |>
    dplyr::mutate(player = X1,
                  player_id = html |>
                    rvest::html_node(xpath = '/html/body/div[2]/table[5]') |>
                    rvest::html_nodes('a') |>
                    rvest::html_attr('href') |>
                    (\(x) gsub('_seq=','', stringi::stri_extract_all_regex(x,'_seq=([0-9]*)$')))(),
                  team = html |>
                    rvest::html_node(xpath = '/html/body/div[2]/table[5]') |>
                    rvest::html_node('.heading') |>
                    rvest::html_text() |>
                    trimws(),
                  team_id = html |>
                    rvest::html_nodes(xpath = '/html/body/div[2]/table[1]/tr/td/a') |>
                    rvest::html_attr('href') |>
                    (\(x) gsub('/teams/','',x))() |>
                    dplyr::nth(1),
                  pos = X2,
                  g = as.integer(X3),
                  min = lubridate::ms(X4),
                  fgm = as.integer(X5),
                  fga = as.integer(X6),
                  fgpct = fgm / fga,
                  tpm = as.integer(X7),
                  tpa = as.integer(X8),
                  tppct = tpm / tpa,
                  ftm = as.integer(X9),
                  fta = as.integer(X10),
                  ftpct = ftm / fta,
                  pts = as.integer(X11),
                  oreb = as.integer(X12),
                  dreb = as.integer(X13),
                  treb = as.integer(X14),
                  ast = as.integer(X15),
                  to = as.integer(X16),
                  stl = as.integer(X17),
                  blk = as.integer(X18),
                  personal_fouls = as.integer(X19),
                  dq = as.integer(X20),
                  technical_fouls = as.integer(X21),
                  dplyr::across(where(is.integer), ~ dplyr::case_when(is.na(.x) ~ as.integer(0),
                                                                    T~.x)),
                  dplyr::across(dplyr::ends_with('pct'), ~ dplyr::case_when(is.na(.x) ~ NaN,
                                                                            T~.x))) |>
    dplyr::select(player:technical_fouls) |>
    dplyr::bind_rows(html |>
                       rvest::html_node(xpath = '/html/body/div[2]/table[6]') |>
                       rvest::html_table() |>
                       dplyr::filter(X1 != X2 & !(X1 %in% c('Player','TEAM','Totals'))) |>
                       dplyr::mutate(player = X1,
                                     player_id = html |>
                                       rvest::html_node(xpath = '/html/body/div[2]/table[6]') |>
                                       rvest::html_nodes('a') |>
                                       rvest::html_attr('href') |>
                                       (\(x) gsub('_seq=','', stringi::stri_extract_all_regex(x,'_seq=([0-9]*)$')))(),
                                     team = html |>
                                       rvest::html_node(xpath = '/html/body/div[2]/table[6]') |>
                                       rvest::html_node('.heading') |>
                                       rvest::html_text() |>
                                       trimws(),
                                     team_id = html |>
                                       rvest::html_nodes(xpath = '/html/body/div[2]/table[1]/tr/td/a') |>
                                       rvest::html_attr('href') |>
                                       (\(x) gsub('/teams/','',x))() |>
                                       dplyr::nth(2),
                                     pos = X2,
                                     g = as.integer(X3),
                                     min = lubridate::ms(X4),
                                     fgm = as.integer(X5),
                                     fga = as.integer(X6),
                                     fgpct = fgm / fga,
                                     tpm = as.integer(X7),
                                     tpa = as.integer(X8),
                                     tppct = tpm / tpa,
                                     ftm = as.integer(X9),
                                     fta = as.integer(X10),
                                     ftpct = ftm / fta,
                                     pts = as.integer(X11),
                                     oreb = as.integer(X12),
                                     dreb = as.integer(X13),
                                     treb = as.integer(X14),
                                     ast = as.integer(X15),
                                     to = as.integer(X16),
                                     stl = as.integer(X17),
                                     blk = as.integer(X18),
                                     personal_fouls = as.integer(X19),
                                     dq = as.integer(X20),
                                     technical_fouls = as.integer(X21),
                                     dplyr::across(where(is.integer), ~ dplyr::case_when(is.na(.x) ~ as.integer(0),
                                                                                         T~.x)),
                                     dplyr::across(dplyr::ends_with('pct'), ~ dplyr::case_when(is.na(.x) ~ NaN,
                                                                                               T~.x))) |>
                       dplyr::select(player:technical_fouls))
}

#' get_bb_team_box
#'
#' Obtains team box score data for a given basketball game_id from stats.ncaa.org.
#'
#' @param game_id the game_id for a given game on stats.ncaa.org
#' @return a dataframe for box score data for that game
get_bb_team_box <- function(game_id){
  html <- paste0('https://stats.ncaa.org/game/box_score/',game_id) |>
    rvest::read_html()
  html |>
    rvest::html_node(xpath = '/html/body/div[2]/table[5]') |>
    rvest::html_table() |>
    dplyr::filter(X1 == 'Totals') |>
    dplyr::mutate(team = html |>
                    rvest::html_node(xpath = '/html/body/div[2]/table[5]') |>
                    rvest::html_node('.heading') |>
                    rvest::html_text() |>
                    trimws(),
                  team_id = html |>
                    rvest::html_nodes(xpath = '/html/body/div[2]/table[1]/tr/td/a') |>
                    rvest::html_attr('href') |>
                    (\(x) gsub('/teams/','',x))() |>
                    dplyr::nth(1),
                  g = as.integer(X3),
                  min = lubridate::ms(X4),
                  fgm = as.integer(X5),
                  fga = as.integer(X6),
                  fgpct = fgm / fga,
                  tpm = as.integer(X7),
                  tpa = as.integer(X8),
                  tppct = tpm / tpa,
                  ftm = as.integer(X9),
                  fta = as.integer(X10),
                  ftpct = ftm / fta,
                  pts = as.integer(X11),
                  oreb = as.integer(X12),
                  dreb = as.integer(X13),
                  treb = as.integer(X14),
                  ast = as.integer(X15),
                  to = as.integer(X16),
                  stl = as.integer(X17),
                  blk = as.integer(X18),
                  personal_fouls = as.integer(X19),
                  dq = as.integer(X20),
                  technical_fouls = as.integer(X21),
                  dplyr::across(where(is.integer), ~ dplyr::case_when(is.na(.x) ~ as.integer(0),
                                                                      T~.x)),
                  dplyr::across(dplyr::ends_with('pct'), ~ dplyr::case_when(is.na(.x) ~ NaN,
                                                                            T~.x))) |>
    dplyr::select(team:technical_fouls) |>
    dplyr::bind_rows(html |>
                       rvest::html_node(xpath = '/html/body/div[2]/table[6]') |>
                       rvest::html_table() |>
                       dplyr::filter(X1 == 'Totals') |>
                       dplyr::mutate(team = html |>
                                       rvest::html_node(xpath = '/html/body/div[2]/table[6]') |>
                                       rvest::html_node('.heading') |>
                                       rvest::html_text() |>
                                       trimws(),
                                     team_id = html |>
                                       rvest::html_nodes(xpath = '/html/body/div[2]/table[1]/tr/td/a') |>
                                       rvest::html_attr('href') |>
                                       (\(x) gsub('/teams/','',x))() |>
                                       dplyr::nth(2),
                                     g = as.integer(X3),
                                     min = lubridate::ms(X4),
                                     fgm = as.integer(X5),
                                     fga = as.integer(X6),
                                     fgpct = fgm / fga,
                                     tpm = as.integer(X7),
                                     tpa = as.integer(X8),
                                     tppct = tpm / tpa,
                                     ftm = as.integer(X9),
                                     fta = as.integer(X10),
                                     ftpct = ftm / fta,
                                     pts = as.integer(X11),
                                     oreb = as.integer(X12),
                                     dreb = as.integer(X13),
                                     treb = as.integer(X14),
                                     ast = as.integer(X15),
                                     to = as.integer(X16),
                                     stl = as.integer(X17),
                                     blk = as.integer(X18),
                                     personal_fouls = as.integer(X19),
                                     dq = as.integer(X20),
                                     technical_fouls = as.integer(X21),
                                     dplyr::across(where(is.integer), ~ dplyr::case_when(is.na(.x) ~ as.integer(0),
                                                                                         T~.x)),
                                     dplyr::across(dplyr::ends_with('pct'), ~ dplyr::case_when(is.na(.x) ~ NaN,
                                                                                               T~.x))) |>
                       dplyr::select(team:technical_fouls))
}
