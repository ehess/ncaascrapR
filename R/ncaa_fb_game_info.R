#' @title ncaa_fb_game_info
#' @description Obtains football game team box scores from the NCAA API
#' @param game_id the game_id of a given NCAA FB game from the NCAA API
#' @return a data frame with NCAA FB game info with a single row
#' @export
#' @examples \donttest{
#'   ncaa_fb_game_info(5931773)
#' }
ncaa_fb_game_info <- function(game_id) {
  game_info <- ncaa_game_info(game_id)

  return(game_info)
}
