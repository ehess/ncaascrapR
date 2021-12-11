#' ncaa_vb_roster()
#' Scrape roster information from the stats.ncaa.org website
#'
#' How to get a school_id and team_id:
#' 1. find the team you want to view data for on the stats.ncaa.org website
#' 2. select the "roster" tab from the team's page
#' 3. note the two sets of numbers in the URL
#' 4. the first number in the URL is the school_id, the second number is the team_id
#'
#'
#' @param school_id the id for the school across all seasons
#' @param team_id the id for the roster for that school's season
#' @returns a data frame of the roster for the team

library(rvest)
library(tidyverse)
library(stringr)
library(dplyr)

ncaa_vb_roster <- function(school_id, team_id) {
  base_url <- "https://stats.ncaa.org/team"

  full_url <- paste(base_url, school_id, "roster", team_id, sep="/")

  base <- rvest::read_html(full_url)

  roster_nodes <- base %>%
    html_nodes("#stat_grid") %>%
    html_nodes("tr:not(.grey_heading)")

  roster_table <- base %>% html_table()
  data <- roster_table[[1]]

  colnames(data) <- data[1,]

  roster <- data %>% filter(!row_number() == 1)

  return(data)
}
