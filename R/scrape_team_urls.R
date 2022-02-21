#' @title scrape_team_urls
#'
#' @description scrapes team_urls for a given year, sport code, and division
#' @param year the year of the season
#' @param division the division of nCAA competition. Defaults to 1, can be 2 or
#' 3.
#' @param sport_code a code indicating which sport to scrape
#' @return a dataframe of team_ids and school names for the provided params.
scrape_team_urls <- function(year, sport_code, division = 1){
  teams_page <- rvest::read_html(glue::glue("http://stats.ncaa.org/team/inst_team_list?sport_code={sport_code}&academic_year={year}&division={division}&conf_id=-1&schedule_date="))
  data.frame(year = year,
             division = division,
             sport_code = sport_code,
             school_name = rvest::read_html(glue::glue("http://stats.ncaa.org/team/inst_team_list?sport_code={sport_code}&academic_year={year}&division={division}&conf_id=-1&schedule_date=")) |>
               rvest::html_nodes(xpath='//*[@id="contentarea"]/div[4]/div/table') |>
               rvest::html_children() |>
               rvest::html_nodes("a") |>
               rvest::html_text(),
             team_url = rvest::read_html(glue::glue("http://stats.ncaa.org/team/inst_team_list?sport_code={sport_code}&academic_year={year}&division={division}&conf_id=-1&schedule_date=")) |>
               rvest::html_nodes(xpath='//*[@id="contentarea"]/div[4]/div/table') |>
               rvest::html_children() |>
               rvest::html_nodes("a") |>
               rvest::html_attr('href'))
}
