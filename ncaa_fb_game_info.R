#' ncaa_fb_game_info
#'
#' Obtains football game team box scores from the NCAA API.
#'
#' @param game_id the game_id of a given NCAA FB game from the NCAA API
#'
#' @return a data.frame with NCAA FB game info with a single row
#'
#' @example ncaa_fb_game_info(5931773)
#' @export
ncaa_fb_game_info <- function(game_id) {
  game_info <- ncaa_game_info(game_id)

  return(game_info)
}
