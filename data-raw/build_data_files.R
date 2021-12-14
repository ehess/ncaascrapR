#' scrape team ids
#' grabs team ids using RSelenium over entire history of NCAA
#'
#' this function obtains the NCAA team codes for all of the teams for a given
#' sport. The potential sports include:
#'
#' * Men's Baseball - `MBA`
#' * Men's Basketball - `MBB`
#' * Men's Football - `MFB`
#' * Men's Ice Hockey - `MIH`
#' * Men's Lacrosse - `MLA`
#' * Men's Soccer - `MSO`
#' * Men's Tennis - `MTE`
#' * Men's Volleyball - `MVB`
#' * Men's Water Polo - `MWP`
#' * Women's Basketball - `WBB`
#' * Women's Bowling - `WBW`
#' * Women's Field Hockey - `WFH`
#' * Women's Ice Hockey - `WIH`
#' * Women's Lacrosse - `WLA`
#' * Women's Softball - `WSB`
#' * Women's Soccer - `WSO`
#' * Women's Beach Volleyball - `WSV`
#' * Women's Tennis - `WTE`
#' * Women's Volleyball - `WVB`
#' * Women's Water Polo - `WWP`
#'
#' @param sport_code the sport code for the desired sport to obtain team names
#' @returns a data frame with the team, team_id, season, division, conference, wins, losses, ties, and games for all teams in the sport in NCAA history
scrape_teams <- function(sport_code) {
  system("taskkill /im java.exe /f",
         intern = FALSE,
         ignore.stdout = FALSE)
  url <- glue::glue('https://stats.ncaa.org/teams/history/{sport_code}/0')
  rD <- RSelenium::rsDriver(browser = "firefox")
  remDr <- rD$client
  remDr$navigate(url)
  dropdown <-
    remDr$findElement(using = 'xpath', '/html/body/div[2]/h1/div[1]/a/div')
  dropdown$clickElement()
  page_source <- remDr$getPageSource()[[1]]
  children <- rvest::read_html(page_source) |>
    rvest::html_node(xpath = '/html/body/div[2]/h1/div[1]/div') |>
    rvest::html_nodes('li') |>
    rvest::html_text()
  dropdown$clickElement()
  try({
    table <- purrr::map_dfr(2:length(children), \(x) {
      Sys.sleep(0.2)
      # table <- purrr::map_dfr(2:10, \(x) { # debugging
      dropdown <-
        remDr$findElement(using = 'xpath', '/html/body/div[2]/h1/div[1]/a/div')
      dropdown$clickElement()
      webElem <-
        remDr$findElement(using = 'xpath',
                          glue::glue('/html/body/div[2]/h1/div[1]/div/ul/li[{x}]'))
      page_source <- remDr$getPageSource()[[1]]
      if (rvest::read_html(page_source) |>
          rvest::html_node(xpath = glue::glue('/html/body/div[2]/h1/div[1]/div/ul/li[{x}]')) |>
          rvest::html_text() != 'CAREER') {
        webElem$clickElement()
        pages <-
          remDr$findElement(using = 'xpath',
                            '/html/body/div[2]/div[3]/div[1]/label/select/option[4]')
        pages$clickElement()
        page_source <- remDr$getPageSource()[[1]]
        paginate_buttons <- rvest::read_html(page_source) |>
          rvest::html_node(xpath = '/html/body/div[2]/div[3]/div[5]/span') |>
          rvest::html_nodes('a') |>
          rvest::html_text()
        if (length(paginate_buttons)) {
          teams <- purrr::map_dfr(1:length(paginate_buttons), \(x) {
            paginate_button <- remDr$findElement(using = 'xpath',
                                                 glue::glue('/html/body/div[2]/div[3]/div[5]/span/a[{x}]'))
            paginate_button$clickElement()
            # dropping coaches atm -- potential to re-add later? commented out code is a good starting spot
            # rvest::read_html(remDr$getPageSource()[[1]]) |>
            #   rvest::html_node(xpath = '//*[@id="team_history_data_table"]') |>
            #   rvest::html_table() |>
            #   dplyr::mutate(`Head Coaches` = strsplit(`Head Coaches`, "\\(([0-9]*-[0-9]*)\\)")) |>
            #   tidyr::unnest(`Head Coaches`) |>
            #   dplyr::mutate(`Head Coaches` = trimws(`Head Coaches`),
            #                 TeamID = rvest::read_html(remDr$getPageSource()[[1]]) |>
            #                   rvest::html_node(xpath = '//*[@id="team_history_data_table"]') |>
            #                   rvest::html_nodes('tr') |>
            #                   (\(x) {x[2:length(x)]})() |>
            #                   (\(x) {x[1:length(x)-1]})() |>
            #                   purrr::map(\(x){
            #                     tmp <- x |>
            #                       rvest::html_nodes('a') |>
            #                       rvest::html_attr('href') |>
            #                       (\(x) {x[stringr::str_detect(x, 'team')]})() |>
            #                       stringr::str_extract('([0-9]*)$')
            #                     if(!length(tmp)){
            #                       tmp <- NA_character_
            #                     }
            #                     return(tmp)
            #                   }) |>
            #                   unlist(),
            #                 CoachID = rvest::read_html(remDr$getPageSource()[[1]]) |>
            #                   rvest::html_node(xpath = '//*[@id="team_history_data_table"]') |>
            #                   rvest::html_nodes('tr') |>
            #                   (\(x) {x[2:length(x)]})() |>
            #                   (\(x) {x[1:length(x)-1]})() |>
            #                   purrr::map(\(x){
            #                     tmp <- x |>
            #                       rvest::html_nodes('a') |>
            #                       rvest::html_attr('href') |>
            #                       (\(x) {x[stringr::str_detect(x, 'people')]})() |>
            #                       stringr::str_extract('([0-9]*)$')
            #                     if(!length(tmp)){
            #                       tmp <- NA_character_
            #                     }
            #                     return(tmp)
            #                   })  |>
            #                   unlist(),
            #                 Team = children |> dplyr::nth(x),
            #                 Year = as.integer(substring(Year,1,4)),
            #                 `Head Coaches` = as.character(`Head Coaches`),
            #                 Division = as.character(Division),
            #                 Conference = as.character(Conference),
            #                 Conference = dplyr::case_when(Conference == '-' ~ NA_character_,
            #                                               T~Conference),
            #                 Wins = dplyr::case_when(is.na(Wins) ~ as.integer(0),
            #                                         T ~ as.integer(Wins)),
            #                 Losses = as.integer(dplyr::case_when(is.na(Losses) ~ as.integer(0),
            #                                                      T ~ as.integer(Losses))),
            #                 Ties = as.integer(dplyr::case_when(is.na(Ties) ~ as.integer(0),
            #                                                    T~ as.integer(Ties))),
            #                 Games = Wins + Losses + Ties)
            page_source <- remDr$getPageSource()[[1]]
            team_name <- rvest::read_html(page_source) |>
              rvest::html_node(xpath = '/html/body/div[2]/h1/div[1]/a/span') |>
              rvest::html_text()
            cli::cli_alert('Scraping {team_name}') # debugging
            df <- rvest::read_html(page_source) |>
              rvest::html_node(xpath = '//*[@id="team_history_data_table"]') |>
              rvest::html_table() |>
              dplyr::filter(Year != '') |>
              dplyr::mutate(
                TeamID = rvest::read_html(page_source) |>
                  rvest::html_node(xpath = '//*[@id="team_history_data_table"]') |>
                  rvest::html_nodes('tr') |>
                  rvest::html_nodes('a') |>
                  rvest::html_attr('href') |>
                  (\(x) {
                    x[stringi::stri_detect(x, regex='teams')]
                  })() |>
                  stringi::stri_extract(regex = '([0-9]*)$'),
                Team = team_name,
                Year = as.integer(substring(Year, 1, 4)),
                Division = as.character(Division),
                Conference = as.character(Conference),
                Conference = dplyr::case_when(Conference == '-' ~ NA_character_,
                                              T ~ Conference),
                Wins = dplyr::case_when(is.na(Wins) ~ as.integer(0),
                                        T ~ as.integer(Wins)),
                Losses = as.integer(
                  dplyr::case_when(is.na(Losses) ~ as.integer(0),
                                   T ~ as.integer(Losses))
                ),
                Ties = as.integer(
                  dplyr::case_when(is.na(Ties) ~ as.integer(0),
                                   T ~ as.integer(Ties))
                ),
                Games = Wins + Losses + Ties,
                Team = rvest::read_html(page_source) |>
                  rvest::html_node(xpath = '/html/body/div[2]/h1/div[1]/a/span') |>
                  rvest::html_text()
              ) |>
              dplyr::select(
                team = Team,
                team_id = TeamID,
                year = Year,
                division = Division,
                conference = Conference,
                wins = Wins,
                lossses = Losses,
                ties = Ties,
                games = Games
              )
            return(df)


          })

        } else {
          return(data.frame())
        }
      } else {
        return(data.frame())
      }
    })
  })
  remDr$close()
  system("taskkill /im java.exe /f",
         intern = FALSE,
         ignore.stdout = FALSE)
  if(exists('table')){
    return(table)
  } else {
    cli::cli_alert_info('Error occurred in scraping.')
  }

}
