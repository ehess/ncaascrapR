#' @title ncaa_vb_pbp
#' @description Scrape about plays during a volleyball game from the NCAA website.
#'
#' @details How to get a game ID:
#' 1. find the game you want to view data for via stats.ncaa.org's scoreboard
#' 2. open up its box score page
#' 3. Click the play-by-play tab
#' 4. Copy the string of numbers at the end of the URL from your browser's address bar.
#'
#' Note: there are at least two HTML table types available from the NCAA website,
#' and the code below handles each differently (see if statements checking for `html_columns`).
#'
#'   - HTML table type 1 (three columns): Ohio State vs Georgia Tech, play-by-play ID: 5174968
#'   - HTML table type 2 (four columns): Miami vs Florida, play-by-play ID: 5172096
#'
#' @param game_id the ID for the game's play_by_play data from the box score page
#' @returns a data frame of play_by_play data for the specified game

ncaa_vb_pbp <- function(game_id) {
  base_url <- "https://stats.ncaa.org/game/play_by_play"

  full_url <- paste(base_url, game_id, sep="/")

  base <- read_html(full_url)

  play_tables <- base %>% html_nodes(".mytbl + .mytable") %>% html_table(fill=TRUE)

  away_team_name = "away"
  home_team_name = "home"
  base_plays <- bind_rows(play_tables)
  html_columns = 3
  if (ncol(base_plays) == 4) {
    html_columns = 4
    base_plays <- subset(base_plays, select = -c(X3, X4))
    colnames(base_plays) <- c("score","action")
    base_score = base_plays$score[1]
    split_result = str_split_fixed(base_score, "\\s*-\\s*", 2)
    away_team_name = split_result[1]
    home_team_name = split_result[2]

    base_plays <- rbind(c("0-0","Set started"), base_plays)
    base_plays <- rbind(c("0-0","Match started"), base_plays)
    base_plays <- rbind(base_plays, c(NA_character_, "Match ended"))

    base_plays <- base_plays %>%
      filter(
        !grepl("starters", action)
      ) %>%
      mutate(
        action = case_when(
          grepl("End of \\d\\w+ Set", score) ~ "Set ended",
          TRUE ~ action
        ),
        score = case_when(
          grepl("End of \\d\\w+ Set", score) ~ lag(score),
          TRUE ~ score
        ),
        home_team = home_team_name,
        away_team = away_team_name,
        mentioned_team = case_when(
          grepl(home_team, action) ~ home_team,
          grepl(away_team, action) ~ away_team,
          TRUE ~ NA_character_
        ),
        action_team = case_when(
          mentioned_team == home_team & grepl(" error", action) ~ away_team,
          mentioned_team == away_team & grepl(" error", action) ~ home_team,
          mentioned_team == home_team ~ home_team,
          mentioned_team == away_team ~ away_team,
          TRUE ~ NA_character_
        )
      ) %>%
      fill(score, .direction = "down") %>%
      mutate(
        html_columns = 4
      ) %>%
      select(-mentioned_team)
  } else { # 3-column
    html_columns = 3
    colnames(base_plays) <- c("away_action","score","home_action")
    # get away/home team name from first row
    away_team_name = base_plays$away_action[1]
    home_team_name = base_plays$home_action[1]

    # strip out set headers
    base_plays <- base_plays %>%
      mutate(
        home_team = home_team_name,
        away_team = away_team_name,
        action = case_when(
          (nchar(home_action) > 0 & nchar(away_action) == 0) ~ home_action,
          (nchar(home_action) == 0 & nchar(away_action) > 0) ~ away_action,
          TRUE ~ ""
        ),
        action_team = case_when(
          (nchar(home_action) > 0 & nchar(away_action) == 0) ~ home_team,
          (nchar(home_action) == 0 & nchar(away_action) > 0) ~ away_team,
          TRUE ~ ""
        )
      ) %>%
      mutate(
        html_columns = 3
      ) %>%
      select(-home_action, -away_action)
  }

  base_plays <- base_plays %>%
    filter(
      score != "Score"
      & !grepl("End of \\d\\w+ Set", action)
      & score != "Media Timeout"
      & nchar(action) > 0
      & action != "Play"
    ) %>%
    mutate(
      score = case_when(
        grepl("Match started", action) ~ "0-0",
        grepl("Set started", action) ~ "0-0",
        (nchar(score) == 0) ~ NA_character_,
        TRUE ~ score
      ),
      score = str_trim(score, "both")
    ) %>%
    fill(score, .direction = "down") %>%
    separate(score, into = c("away_score", "home_score"), extra = "merge") %>%
    mutate(
      game_id = as.integer(game_id),
      home_score = as.integer(home_score),
      away_score = as.integer(away_score),

      lag_home_score = lag(home_score),
      lag_away_score = lag(away_score),
      lead_home_score = lead(home_score),
      lead_away_score = lead(away_score),

      scoring_team = case_when(
        lag_home_score < home_score ~ home_team,
        lag_away_score < away_score ~ away_team,
        TRUE ~ NA_character_
      ),
      scoring_play = !is.na(scoring_team),
      action = gsub("\\+","", action),
      action = str_trim(action, "both"),
      lead_action = lead(action)
    ) %>%
    filter(
      !(grepl(" serves", action) & grepl(" serves an ace", lead_action))
    )


  plays <- base_plays %>%
    separate(action, sep = "\n", into = c("primary_action","other_actions"), extra = "merge", remove = FALSE) %>%
    mutate(
      primary_action = str_trim(primary_action, "both"),
      primary_action = sub("  ", " ", primary_action),
      other_actions = str_trim(other_actions, "both"),
      action_type = case_when(
        grepl("error", tolower(primary_action)) ~ "Error",
        grepl("violation", tolower(primary_action)) ~ "Error",
        grepl("match started", tolower(primary_action)) ~ "Match Start",
        grepl("match ended", tolower(primary_action)) ~ "Match End",
        grepl("set started", tolower(primary_action)) ~ "Set Start",
        grepl("set ended", tolower(primary_action)) ~ "Set End",
        grepl("timeout", tolower(primary_action)) ~ "Timeout",
        grepl("challenge", tolower(primary_action)) ~ "Challenge",
        grepl("challange", tolower(primary_action)) ~ "Challenge",
        grepl("dig", tolower(primary_action)) ~ "Dig",
        grepl("reception", tolower(primary_action)) ~ "Reception",
        grepl("block", tolower(primary_action)) ~ "Block",
        grepl("serve", tolower(primary_action)) ~ "Serve",
        grepl("set by", tolower(primary_action)) ~ "Set",
        grepl("attack", tolower(primary_action)) ~ "Attack",
        grepl("subs:", tolower(primary_action)) ~ "Sub",
        grepl("sub in", tolower(primary_action)) ~ "Sub In",
        grepl("sub out", tolower(primary_action)) ~ "Sub Out",
        grepl("kill", tolower(primary_action)) ~ "Kill",
        TRUE ~ "None"
      ),
      action_team = case_when(
        action_type == "Match Start" ~ NA_character_,
        action_type == "Set Start" ~ NA_character_,
        action_type == "Match End" ~ NA_character_,
        action_type == "Set End" ~ NA_character_,
        TRUE ~ action_team
      ),
      error = (action_type == "Error"),
      error_type = case_when(
        error & grepl("attack", tolower(primary_action)) ~ "Attack",
        error & grepl("ball handling", tolower(primary_action)) ~ "Ball Handling",
        (error & grepl("set", tolower(primary_action))) ~ "Set",
        (error & grepl("service", tolower(primary_action))) ~ "Service",
        error & grepl("block", tolower(primary_action)) ~ "Block",
        error & grepl("footfall", tolower(primary_action)) ~ "Footfall Violation",
        error & grepl("net", tolower(primary_action)) ~ "Net Violation",
        !error ~ NA_character_,
        TRUE ~ ""
      ),
      involved_players = case_when(
        action_type == "Serve" ~ str_extract(primary_action, "(.*)\\s+serves"),
        (action_type == "Error" & error_type == "Service" & html_columns == 4) ~ str_extract(primary_action, "(.*)\\s+service error"),
        (action_type == "Error" & error_type == "Attack" & html_columns == 4) ~ str_extract(primary_action, "(.*)\\s+attack error"),
        action_type == "Sub" ~ str_extract(primary_action, ":\\s+(.*)"),
        action_type == "Sub In" ~ str_extract(primary_action, "in\\s+(.*)"),
        action_type == "Sub Out" ~ str_extract(primary_action, "out\\s+(.*)"),
        TRUE ~ str_extract(primary_action, "by\\s+(.*)")
      ),
      involved_players = case_when(
        action_type == "Serve" ~ sub("\\s+serves", "", involved_players),
        (action_type == "Error" & error_type == "Service" & html_columns == 4) ~ sub("\\s+service error", "", involved_players),
        (action_type == "Error" & error_type == "Attack" & html_columns == 4) ~ sub("\\s+attack error", "", involved_players),
        action_type == "Sub" ~ sub(":\\s+", "", involved_players),
        action_type == "Sub In" ~ sub("in\\s+", "", involved_players),
        action_type == "Sub Out" ~ sub("out\\s+", "", involved_players),
        TRUE ~ sub("by\\s+", "", involved_players)
      ),
      involved_players = str_trim(involved_players, "both"),
      lead_primary_action = lead(primary_action),
      lead_action_team = lead(action_team),
      lag_action_type = lag(action_type, default = NA),
      lead_action_type = lead(action_type, default = NA),
      set = cumsum(action_type == "Set Start"),
      set = ifelse(set == 0, 1, set),
      rally_number = cumsum(action_type == "Serve"),
      rally_number = case_when(
        grepl("Sub", action_type) ~ NA_integer_,
        grepl("Start", action_type) ~ NA_integer_,
        grepl("Timeout", action_type) ~ NA_integer_,
        TRUE ~ rally_number
      )
    ) %>%
    mutate(
      # we need to filter out duplicate plays that have different action_team values. This typically happens on some types of errors where the table row that holds the entire rally is in the scoring team's column BUT the error actually should be assessed to their opponent.
      lead_error_type = lead(error_type),
      duplicate_situation = (lead_primary_action == primary_action) & (lead_action_team != action_team) & lead_error_type == error_type & (error_type %in% c("Ball Handling", "Set") & !scoring_play & is.na(other_actions)),
      # we also need to update the second dupe's action_team to properly account for the error. this can be done safely here because we're not counting anything here, the scoring team is already properly set, and nothing below is dependent on the action_team value
      lag_duplicate_situation = lag(duplicate_situation),
      lag_action_team = lag(action_team),
      action_team = case_when(
        lag_duplicate_situation ~ lag_action_team,
        TRUE ~ action_team
      )
    ) %>%
    filter(
      !duplicate_situation
    ) %>%
    group_by(set) %>%
    mutate(
      set_play_number = row_number()
    ) %>%
    ungroup() %>%
    mutate(
      game_id = as.integer(game_id),
      home_score = as.integer(home_score),
      away_score = as.integer(away_score),
      match_action_number = row_number()
    )

  if (html_columns == 3) {
    plays <- plays %>%
      group_by(rally_number) %>%
      mutate(
        rally_play_number = row_number(),
        rally_total_plays = n(),
        rally_start = (min(rally_play_number) == rally_play_number),
        rally_end = (max(rally_play_number) == rally_play_number),
        rally_end_action_type = last(action_type),
        rally_starting_team = first(action_team)
      ) %>%
      ungroup() %>%
      mutate(
        rally_play_number = case_when(
          grepl("Sub", action_type) ~ NA_integer_,
          grepl("Start", action_type) ~ NA_integer_,
          grepl("End", action_type) ~ NA_integer_,
          grepl("Timeout", action_type) ~ NA_integer_,
          TRUE ~ rally_play_number
        ),
        rally_total_plays = case_when(
          grepl("Sub", action_type) ~ NA_integer_,
          grepl("Start", action_type) ~ NA_integer_,
          grepl("End", action_type) ~ NA_integer_,
          grepl("Timeout", action_type) ~ NA_integer_,
          TRUE ~ rally_total_plays
        ),
        rally_start = case_when(
          grepl("Sub", action_type) ~ NA,
          grepl("Start", action_type) ~ NA,
          grepl("End", action_type) ~ NA,
          grepl("Timeout", action_type) ~ NA,
          TRUE ~ rally_start
        ),
        rally_end = case_when(
          grepl("Sub", action_type) ~ NA,
          grepl("Start", action_type) ~ NA,
          grepl("End", action_type) ~ NA,
          grepl("Timeout", action_type) ~ NA,
          TRUE ~ rally_end
        ),
        rally_end_action_type = case_when(
          grepl("Sub", action_type) ~ NA_character_,
          grepl("Start", action_type) ~ NA_character_,
          grepl("End", action_type) ~ NA_character_,
          grepl("Timeout", action_type) ~ NA_character_,
          TRUE ~ rally_end_action_type
        ),
        rally_starting_team = case_when(
          grepl("Sub", action_type) ~ NA_character_,
          grepl("Start", action_type) ~ NA_character_,
          grepl("End", action_type) ~ NA_character_,
          grepl("Timeout", action_type) ~ NA_character_,
          TRUE ~ rally_starting_team
        )
      ) %>%
      select(
        game_id,
        away_team,
        home_team,
        match_action_number,
        set,
        set_play_number,
        rally_number,
        rally_play_number,
        action_type,
        action_team,
        action,
        primary_action,
        other_actions,
        involved_players,
        away_score,
        home_score,
        scoring_play,
        scoring_team,
        error,
        error_type,
        lag_action_type,
        lead_action_type,
        rally_start,
        rally_end,
        rally_total_plays,
        rally_end_action_type,
        rally_starting_team,
        # duplicate_situation,
        # lag_duplicate_situation
      )
  } else {
    plays <- plays %>%
      select(
        game_id,
        away_team,
        home_team,
        match_action_number,
        set,
        set_play_number,
        action_type,
        action_team,
        action,
        primary_action,
        other_actions,
        involved_players,
        away_score,
        home_score,
        scoring_play,
        scoring_team,
        error,
        error_type,
        lag_action_type,
        lead_action_type,
        # duplicate_situation,
        # lag_duplicate_situation
      )
  }

  return(plays)
}
