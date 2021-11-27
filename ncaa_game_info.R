
ncaa_game_info <- function(game_id) {
  base_url <- "https://data.ncaa.com/casablanca/game/"
  
  full_url <- paste(base_url, game_id, "gameInfo.json", sep="/")
  
  # Check for internet
  #check_internet()
  
  # Create the GET request and set response as res
  res <- httr::GET(full_url)
  
  # Check the result
  #check_status(res)
  

  info.json <- jsonlite::fromJSON(full_url, flatten = TRUE)
  info.df <- as.data.frame(unlist(info.json)) %>% tibble::rownames_to_column(var = "name") %>% 
    dplyr::rename(value = .data$`unlist(info.json)`) %>%
    tidyr::pivot_wider(names_from = .data$name, values_from = .data$value) %>% janitor::clean_names()
  
  return(info.df)
}
