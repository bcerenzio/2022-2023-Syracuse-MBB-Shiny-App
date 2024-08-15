library(tidyverse)
theme_set(theme_bw())
library(rvest)
library(glue)

#question 1
url <- str_c("https://www.espn.com/mens-college-basketball/team/schedule/_/id/183/season/2023")
syr_xml <- read_html(url)
game_ids <- syr_xml %>% 
  html_nodes(".AnchorLink") %>% 
  html_attr("href") %>% 
  str_extract("\\b\\d{9}\\b") %>% na.omit()
game_ids <- as.vector(game_ids)
game_ids
#question 2
box_scores <- function(x) {
  url1 <- glue("https://www.espn.com/mens-college-basketball/boxscore/_/gameId/{x}")
  xml <- read_html(url1)
  d <- xml %>% html_nodes("table") %>% html_table(header = TRUE)
  boxscore_away <- bind_cols(d[[2]],d[[3]]) %>% filter(!row_number() %in% c(6))
  #removing row number 6 gets rid of the bench header on the espn table
  boxscore_home <- bind_cols(d[[4]],d[[5]]) %>% filter(!row_number() %in% c(6))
  #if statement checks if the name of the home team in the box score's header says Syracuse Orange
  #if it does, it returns the home team box score first, if not, it returns the away team box score first
  #it then puts these boxscores in a list accordingly
  if(xml %>% html_node(".Wrapper+ .Wrapper .h5") %>% html_text() == "Syracuse Orange"){
    return(list(boxscore_home,boxscore_away))
  } else {
    return(list(boxscore_away,boxscore_home))
  }
}

box_scores(401482974)


#question 3
all_box_scores <- map(game_ids, box_scores)
all_box_scores[[1]]

#question 4
syracuse <- function(x){
  boxscore <- as.data.frame(all_box_scores[[x]][[1]])
  boxscore <- boxscore %>% mutate(game_number = x)
  return(boxscore)
}
syracuse_box_scores <- map(1:32,syracuse)

syracuse_box_scores <- bind_rows(syracuse_box_scores)
colnames(syracuse_box_scores) <- c("Player_Name","MIN","FG","3PT","FT","OREB","DREB","REB","AST","STL","BLK","TO","PF","PTS","game_number")
visitors <- function(x){
  boxscore <- as.data.frame(all_box_scores[[x]][[2]])
  boxscore <- boxscore %>% mutate(game_number = x)
  return(boxscore)
}
visitors_box_scores <- map(1:32,visitors)
visitors_box_scores <- bind_rows(visitors_box_scores)
colnames(visitors_box_scores) <- c("Player_Name","MIN","FG","3PT","FT","OREB","DREB","REB","AST","STL","BLK","TO","PF","PTS","game_number")

