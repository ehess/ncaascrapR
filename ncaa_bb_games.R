#' ncaa_bb_games
#'
#' Obtains basketball game data by date from the ncaa stats API. Some notes:
#'
#' * When values are missing (such as a team rank, or a bracket round), they are recorded as NA
#' * dates and times are recorded in ET as is returned by the ET
#' * If seeking to group by a team identifier, it is recommended to use slugs
#' * game_id can be used to obtain pbp and box score data for the game
#' *
#'
#'
#' @param date An R Date object. Defaults to `Sys.Date()`
#'
#' @return a data.frame with NCAA MBB game data
#'
#' @example ncaa_bb_games(as.Date('2021-12-03'))
#' @export
ncaa_bb_games <-
  function(date = Sys.Date(),
           type = c('men', 'women')) {
    type |>
      (\(x) {
        if (length(x) != 1) {
          cli::cli_abort(
            c(
              '{.var type} takes one argument, {.field "men"} or {.field "women"}',
              'x' = 'You must specify one of either "men" or "women" to obtain game data for each sport.'
            )
          )
        } else if (!(x %in% c('men', 'women'))) {
          cli::cli_abort(
            c(
              '{.var type} must be either {.field "men"} or {.field "women"}',
              'x' = 'You have specified an invalid sport type.'
            )
          )
        } else {
          x
        }
      })() |>
      (\(x) {
        paste0(
          "https://data.ncaa.com/casablanca/scoreboard/basketball-",
          x,
          "/d1/",
          gsub('-', '/', date),
          "/scoreboard.json"
        )
      })() |> # build URL
      jsonlite::fromJSON(flatten = T) |> # fetch data
      (\(x) {
        x$games
      })() |> # extract games dataframe
      tidyr::unnest(
        cols = c(game.away.conferences, game.home.conferences),
        names_repair = tidyr::tidyr_legacy
      ) |>
      dplyr::mutate(
        game_id = gsub('/game/', '', game.url),
        start_time = as.POSIXct(as.numeric(game.startTimeEpoch), origin =
                                  "1970-01-01"),
        # seems to return ET regardless of computer locale
        date = as.Date(start_time, tz = 'America/New_York'),
        bracket_round = gsub(
          '&',
          '',
          stringi::stri_extract_all_regex(game.bracketRound, '^(.*)&')
        ),
        bracket_id = game.bracketId,
        bracket_region = game.bracketRegion,
        game_network = game.network,
        game_status = game.gameState,
        current_game_period = game.currentPeriod,
        current_game_clock = game.contestClock,
        away_score = as.integer(game.away.score),
        away_abbrev = game.away.names.char6,
        away_short = game.away.names.short,
        away_slug = game.away.names.seo,
        away_full = game.away.names.full,
        away_rank = game.away.rank,
        away_seed = game.away.seed,
        away_conference = conferenceName,
        home_score = as.integer(game.home.score),
        home_abbrev = game.home.names.char6,
        home_short = game.home.names.short,
        home_slug = game.home.names.seo,
        home_full = game.home.names.full,
        home_conference = conferenceName1,
        home_rank = game.home.rank,
        home_seed = game.home.seed,
      ) |>
      dplyr::filter(away_conference != 'Top 25' &
                      home_conference != 'Top 25') |> # some weirdness where it lists top 25 as a conference unto itself
      dplyr::select(c(game_id:home_conference)) |>
      dplyr::mutate(dplyr::across(
        where(is.character),
        ~ dplyr::case_when(.x == '' ~ NA_character_,
                           T ~ .x)
      )) |>
      as.data.frame()
  }