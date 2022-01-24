#' @title ncaa_bb_player_box
#'
#' @description Obtains player box score data for a given basketball game_id 
#' from stats.ncaa.org.
#'
#' @param game_id the game_id for a given game on stats.ncaa.org
#' @return a dataframe for box score data for that game
ncaa_bb_player_box <- function(game_id) {
  html <- paste0('https://stats.ncaa.org/game/box_score/', game_id) |>
    rvest::read_html()
  html |>
    rvest::html_node(xpath = '/html/body/div[2]/table[5]') |>
    rvest::html_table() |>
    janitor::row_to_names(2) |>
    dplyr::filter(Player != 'Totals') |>
    dplyr::mutate(
      player = gsub(',','',sub("(^.*),\\s(.*$)","\\2 \\1", Player)),
      player_id = html |>
        rvest::html_node(xpath = '/html/body/div[2]/table[5]') |>
        rvest::html_nodes('a') |>
        rvest::html_attr('href') |>
        (\(x) gsub(
          '_seq=',
          '',
          stringi::stri_extract_all_regex(x, '_seq=([0-9]*)$')
        ))() |>
        (\(x) c(x, ''))(),
      team = html |>
        rvest::html_node(xpath = '/html/body/div[2]/table[5]') |>
        rvest::html_node('.heading') |>
        rvest::html_text() |>
        trimws(),
      team_id = html |>
        rvest::html_nodes(xpath = '/html/body/div[2]/table[1]/tr/td/a') |>
        rvest::html_attr('href') |>
        (\(x) gsub('/teams/', '', x))() |>
        dplyr::nth(1),
      pos = Pos,
      g = as.integer(G),
      min = lubridate::ms(MP, roll = T, quiet = T),
      fgm = as.integer(FGM),
      fga = as.integer(FGA),
      fgpct = dplyr::case_when(is.na(fga) ~ NaN, is.na(fgm) ~ 0, T ~ fgm / fga),
      tpm = as.integer(`3FG`),
      tpa = as.integer(`3FGA`),
      tppct = dplyr::case_when(is.na(tpa) ~ NaN, is.na(tpm) ~ 0, T ~ tpm / tpa),
      ftm = as.integer(FT),
      fta = as.integer(FTA),
      ftpct = dplyr::case_when(is.na(fta) ~ NaN, is.na(ftm) ~ 0, T ~ ftm / fta),
      pts = as.integer(PTS),
      oreb = as.integer(ORebs),
      dreb = as.integer(DRebs),
      treb = as.integer(`Tot Reb`),
      ast = as.integer(AST),
      to = as.integer(TO),
      stl = as.integer(STL),
      blk = as.integer(BLK),
      personal_fouls = as.integer(gsub('/', '', Fouls)),
      dq = as.integer(gsub('/', '', DQ)),
      technical_fouls = as.integer(gsub('/', '', `Tech Fouls`)),
      dplyr::across(
        where(is.integer),
        ~ dplyr::case_when(is.na(.x) ~ as.integer(0),
                           T ~
                             .x)
      )
    ) |>
    dplyr::select(player:technical_fouls) |>
    dplyr::bind_rows(
      html |>
        rvest::html_node(xpath = '/html/body/div[2]/table[6]') |>
        rvest::html_table() |>
        janitor::row_to_names(2) |>
        dplyr::filter(Player != 'Totals') |>
        dplyr::mutate(
          player = gsub(',','',sub("(^.*),\\s(.*$)","\\2 \\1", Player)),
          player_id = html |>
            rvest::html_node(xpath = '/html/body/div[2]/table[6]') |>
            rvest::html_nodes('a') |>
            rvest::html_attr('href') |>
            (\(x) gsub(
              '_seq=',
              '',
              stringi::stri_extract_all_regex(x, '_seq=([0-9]*)$')
            ))()  |>
            (\(x) c(x, ''))(),
          team = html |>
            rvest::html_node(xpath = '/html/body/div[2]/table[6]') |>
            rvest::html_node('.heading') |>
            rvest::html_text() |>
            trimws(),
          team_id = html |>
            rvest::html_nodes(xpath = '/html/body/div[2]/table[1]/tr/td/a') |>
            rvest::html_attr('href') |>
            (\(x) gsub('/teams/', '', x))() |>
            dplyr::nth(2),
          pos = Pos,
          g = as.integer(G),
          min = lubridate::ms(MP, roll = T, quiet =
                                T),
          fgm = as.integer(FGM),
          fga = as.integer(FGA),
          fgpct = dplyr::case_when(is.na(fga) ~ NaN, is.na(fgm) ~ 0, T ~ fgm / fga),
          tpm = as.integer(`3FG`),
          tpa = as.integer(`3FGA`),
          tppct = dplyr::case_when(is.na(tpa) ~ NaN, is.na(tpm) ~ 0, T ~ tpm / tpa),
          ftm = as.integer(FT),
          fta = as.integer(FTA),
          ftpct = dplyr::case_when(is.na(fta) ~ NaN, is.na(ftm) ~ 0, T ~ ftm / fta),
          pts = as.integer(PTS),
          oreb = as.integer(ORebs),
          dreb = as.integer(DRebs),
          treb = as.integer(`Tot Reb`),
          ast = as.integer(AST),
          to = as.integer(TO),
          stl = as.integer(STL),
          blk = as.integer(BLK),
          personal_fouls = as.integer(gsub('/', '', Fouls)),
          dq = as.integer(gsub('/', '', DQ)),
          technical_fouls = as.integer(gsub('/', '', `Tech Fouls`)),
          dplyr::across(
            where(is.integer),
            ~ dplyr::case_when(is.na(.x) ~ as.integer(0),
                               T ~
                                 .x)
          ),
          dplyr::across(
            dplyr::ends_with('pct'),
            ~ dplyr::case_when(is.na(.x) ~ NaN,
                               T ~
                                 .x)
          )
        ) |>
        dplyr::select(player:technical_fouls)
    )
}

#' @title get_bb_team_box
#'
#' @description Obtains team box score data for a given basketball game_id from 
#' stats.ncaa.org.
#'
#' @param game_id the game_id for a given game on stats.ncaa.org
#' @return a dataframe for box score data for that game
ncaa_bb_team_box <- function(game_id) {
  html <- paste0('https://stats.ncaa.org/game/box_score/', game_id) |>
    rvest::read_html()
  html |>
    rvest::html_node(xpath = '/html/body/div[2]/table[5]') |>
    rvest::html_table() |>
    janitor::row_to_names(2) |>
    dplyr::filter(Player == 'Totals') |>
    dplyr::mutate(
      team = html |>
        rvest::html_node(xpath = '/html/body/div[2]/table[5]') |>
        rvest::html_node('.heading') |>
        rvest::html_text() |>
        trimws(),
      team_id = html |>
        rvest::html_nodes(xpath = '/html/body/div[2]/table[1]/tr/td/a') |>
        rvest::html_attr('href') |>
        (\(x) gsub('/teams/', '', x))() |>
        dplyr::nth(1),
      min = lubridate::ms(MP, roll = T, quiet = T),
      fgm = as.integer(FGM),
      fga = as.integer(FGA),
      fgpct = dplyr::case_when(is.na(fga) ~ NaN, is.na(fgm) ~ 0, T ~ fgm / fga),
      tpm = as.integer(`3FG`),
      tpa = as.integer(`3FGA`),
      tppct = dplyr::case_when(is.na(tpa) ~ NaN, is.na(tpm) ~ 0, T ~ tpm / tpa),
      ftm = as.integer(FT),
      fta = as.integer(FTA),
      ftpct = dplyr::case_when(is.na(fta) ~ NaN, is.na(ftm) ~ 0, T ~ ftm / fta),
      pts = as.integer(PTS),
      oreb = as.integer(ORebs),
      dreb = as.integer(DRebs),
      treb = as.integer(`Tot Reb`),
      ast = as.integer(AST),
      to = as.integer(TO),
      stl = as.integer(STL),
      blk = as.integer(BLK),
      personal_fouls = as.integer(gsub('/', '', Fouls)),
      dq = as.integer(gsub('/', '', DQ)),
      technical_fouls = as.integer(gsub('/', '', `Tech Fouls`)),
      dplyr::across(
        where(is.integer),
        ~ dplyr::case_when(is.na(.x) ~ as.integer(0),
                           T ~
                             .x)
      ),
      dplyr::across(
        dplyr::ends_with('pct'),
        ~ dplyr::case_when(is.na(.x) ~ NaN,
                           T ~
                             .x)
      )
    ) |>
    dplyr::select(team:technical_fouls) |>
    dplyr::bind_rows(
      html |>
        rvest::html_node(xpath = '/html/body/div[2]/table[6]') |>
        rvest::html_table() |>
        janitor::row_to_names(2) |>
        dplyr::filter(Player == 'Totals') |>
        dplyr::mutate(
          team = html |>
            rvest::html_node(xpath = '/html/body/div[2]/table[5]') |>
            rvest::html_node('.heading') |>
            rvest::html_text() |>
            trimws(),
          team_id = html |>
            rvest::html_nodes(xpath = '/html/body/div[2]/table[1]/tr/td/a') |>
            rvest::html_attr('href') |>
            (\(x) gsub('/teams/', '', x))() |>
            dplyr::nth(1),
          min = lubridate::ms(MP, roll = T, quiet =
                                T),
          fgm = as.integer(FGM),
          fga = as.integer(FGA),
          fgpct = dplyr::case_when(is.na(fga) ~ NaN, is.na(fgm) ~ 0, T ~ fgm / fga),
          tpm = as.integer(`3FG`),
          tpa = as.integer(`3FGA`),
          tppct = dplyr::case_when(is.na(tpa) ~ NaN, is.na(tpm) ~ 0, T ~ tpm / tpa),
          ftm = as.integer(FT),
          fta = as.integer(FTA),
          ftpct = dplyr::case_when(is.na(fta) ~ NaN, is.na(ftm) ~ 0, T ~ ftm / fta),
          pts = as.integer(PTS),
          oreb = as.integer(ORebs),
          dreb = as.integer(DRebs),
          treb = as.integer(`Tot Reb`),
          ast = as.integer(AST),
          to = as.integer(TO),
          stl = as.integer(STL),
          blk = as.integer(BLK),
          personal_fouls = as.integer(gsub('/', '', Fouls)),
          dq = as.integer(gsub('/', '', DQ)),
          technical_fouls = as.integer(gsub('/', '', `Tech Fouls`)),
          dplyr::across(
            where(is.integer),
            ~ dplyr::case_when(is.na(.x) ~ as.integer(0),
                               T ~
                                 .x)
          ),
          dplyr::across(
            dplyr::ends_with('pct'),
            ~ dplyr::case_when(is.na(.x) ~ NaN,
                               T ~
                                 .x)
          )
        ) |>
        dplyr::select(team:technical_fouls)
    )
}
