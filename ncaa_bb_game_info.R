ncaa_bb_game_info <- function(game_id) {
  "https://data.ncaa.com/casablanca/game/" |>
    paste(game_id, "gameInfo.json", sep="/") |>
    jsonlite::fromJSON(flatten = TRUE) |>
    unlist() |>
    (\(x){data.frame(value = x)})() |>
    tibble::rownames_to_column() |>
    tidyr::pivot_wider(names_from = 'rowname', values_from =  'value') # |>
    # dplyr::select(venue_city = venue.city, # most of this data is redundant with the exception of these cols -- worth filtering down?
    #               venue_name = venue.name, # also should tidy all this but not sure of the real utility here
    #               venue_state = venue.state,
    #               division = championship.division,
    #               season = championship.year)
    
}