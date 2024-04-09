### ---- Load packages ---- 

  library(tidyverse)
  library(magrittr)
  library(lubridate) # for mdy() function

### ---- Read in starting data ----

  # read in dataset
  # -> data was scraped from web and is player level info for every game from 2008-09 to 2012-13 season
  data_raw <- read_csv("data-basketball-start.csv", col_names = TRUE)
  
### ---- Data manipulation: Entire process ----
  
  # GOAL: create game level data

  # create player-level data
  
  # edit season variable
  # add date variable
  # -> combine month, day and determine year based on what season and if in beginning of season (end of same year) or beginning of next year
  # -> change month to be numeric as opposed to short abbreviations
  # -> add year variable based on the month in which the game took place
  # -> create a string for the entire date and convert to a date variable 
  # -> keep just the new date and not the variables used
  # calculate additional standard stats and place in a good order
  # -> set percentages to NA if non attempted and round calculated ones
  # -> clean up team and opp strings -> get rid of all spaces and special characters
  data_player <- data_raw %>% 
    mutate(
      Season = case_when(
        Season == 809 ~ "2008-09",
        Season == 910 ~ "2009-10",
        Season == 1011 ~ "2010-11",
        Season == 1112 ~ "2011-12",
        Season == 1213 ~ "2012-13"
      ) %>% ordered(levels = c("2008-09", "2009-10", "2010-11", "2011-12", "2012-13")),
      Date = paste(Month,
                   Day, 
                   ifelse(match(Month, month.abb) > 6,
                                       substr(Season, 1, 4),
                                       as.numeric(substr(Season, 1, 4)) + 1),
                   sep = "-") %>% mdy,
                    
    .keep = "unused",
    .after = Season) %>% 
    mutate(TwoPercent = ifelse(is.na(TwoPM / TwoPA), NA, TwoPM / TwoPA),
           ThreePercent = ifelse(is.na(ThreePM / ThreePA), NA, ThreePM / ThreePA),
           FTPercent = ifelse(is.na(FTM / FTA), NA, FTM / FTA),
           across(ends_with("Percent"), ~ round(.x, 2)),
           DefReb = Reb - OffReb,
           across(c(Team, Opp), ~ gsub("[^a-zA-Z]", "", .x)),
           .after = FTA)
    
  # create game-level data
  # aggregate player stats to team stats per game for each team and season (exclude shooting percentages)
  # remove games with no data
  # -> these rows are likely from when a team played a Division 2 team or something
  # add new and edit variables
  # -> calculate some additional statistics (traditional ones) and also some advanced metrics (from KenPom)
  data_game <- data_player %>%   
    summarize(.by = c(Season, Team, Date, Opp, Location, Outcome), # also grouping by the other character vectors that need to keep (all the same values across rows obviously)
              across(c(TwoPM:FTA, DefReb:Points), sum)) %>% 
    filter(complete.cases(.)) %>% 
    mutate(TwoPercent = ifelse(is.na(TwoPM / TwoPA), NA, TwoPM / TwoPA),
           ThreePercent = ifelse(is.na(ThreePM / ThreePA), NA, ThreePM / ThreePA),
           FTPercent = ifelse(is.na(FTM / FTA), NA, FTM / FTA),
           across(ends_with("Percent"), ~ round(.x, 2)),
           .after = FTA) %>% 
    mutate(GameNumber = row_number(),
           .by = c(Team, Season),
           .after = Date)
  
  # now data is game level
  # -> but there is repeat rows for each game (one from each teams' perspective)
  
  # calculate KenPom stats
  data_kenpom <- data_game %>% 
    mutate(Poss = (TwoPA + ThreePA) - OffReb + Turnovers + 0.475 * FTA,
           eFGP = (TwoPM + 1.5 * ThreePM) / (TwoPA + ThreePA),
           ORP = OffReb / (OffReb + DefReb),
           TP = Turnovers / Poss,
           FTR = FTA / (TwoPA + ThreePA),
           OE = (Points * 100) / Poss,
           AR = Assists / (TwoPM + ThreePM),
           across(Poss:AR, ~ round(.x, 2)))
  
  # save final dataset as a csv and an RData file
  # -> csv is for backup purposes and the RData file will be used from now on for easier loading
  write.csv(data_game, file = "data-basketball-final.csv")
  save(data_game, file = "data-basketball-final.RData")
  
  # create BSU datasets for applications / labs
  bsu_player <- data_player %>% 
    filter(Team == "BallState")
  bsu_game <- data_game %>% 
    filter(Team == "BallState")
  save(bsu_player, file = "data-bsu-player.RData") # not going to use
  save(bsu_game, file = "data-bsu-game.RData")
  
  
    
