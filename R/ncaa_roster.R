#' @title ncaa_rosters
#' @description a generalized function for obtaining NCAA rosters for individual
#' teams
#'
#' @param team_id a unique team identifier for a given NCAA team from any sport
#' @return a dataframe of roster data with the `team`, `team_id`, `player`,
#' `player_id`, `pos`, `year`, `games_played`, and `games_started` values for
#' each player on a given team. Where available, the `height` of players is
#' returned, in inches.
#'
#' @examples
#' \donttest{
#' ncaa_rosters('505669')
#' }
#'
#' @export


ncaa_rosters <- function(team_id) {
  # we actually have to send two requests to get the rosters, annoyingly enough
  roster_url <-
    rvest::read_html(glue::glue('https://stats.ncaa.org/teams/{team_id}')) |>
    rvest::html_node(xpath = '/html/body/div[2]/a[1]') |>
    rvest::html_attr('href')
  roster_html <-  rvest::read_html(glue::glue('https://stats.ncaa.org{roster_url}'))
  id_table <- data.frame(player = roster_html |>
                           rvest::html_nodes(xpath='/html/body/div[2]/table') |>
                           rvest::html_nodes('a') |>
                           rvest::html_text(),
                         player_id = roster_html |>
                           rvest::html_nodes(xpath='/html/body/div[2]/table') |>
                           rvest::html_nodes('a') |>
                           rvest::html_attr('href') |>
                           (\(x) gsub('stats_player_seq=','',stringi::stri_extract_all_regex(x,'stats_player_seq=[0-9]*$')))() |>
                           as.integer())

  roster <- roster_html |>
    rvest::html_elements("#stat_grid") |>
    rvest::html_table() |>
    dplyr::first() |>
    janitor::row_to_names(1) |>
    dplyr::mutate(
      jersey = as.integer(Jersey),
      player = trimws(Player),
      pos = Pos,
      year = Yr,
      games_played = as.integer(GP),
      games_started = as.integer(GS),
      dplyr::across(.cols = any_of("Ht"), ~ as.integer(gsub(
        '-',
        '',
        stringi::stri_extract_all_regex(.x, '[0-9]*-', simplify = T)
      )) * as.integer(12) + as.integer(gsub(
        '-',
        '',
        stringi::stri_extract_all_regex(.x, '-[0-9]*', simplify = T)
      )), .names = 'height')) |>
    dplyr::select(player, pos, year, games_played, games_started, any_of('height')) |>
    dplyr::left_join(id_table,by=c('player')) |>
    dplyr::mutate(team_id = team_id,
                  team = roster_html |>
                    rvest::html_nodes(xpath='/html/body/div[2]/fieldset[1]/legend/a') |>
                    rvest::html_text()
                  ) |>
    dplyr::select(team, team_id, player, player_id, pos, year, any_of('height'), games_played, games_started)
  return(roster)
}
