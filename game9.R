### We had this code for each game in the season.
### Here's an example from the 9th game of the season. 
library(tidyverse)
library(glue)
df <- read_csv("pbp Games 9-16.csv")
#removes unwanted column
df <- df %>% select(-...7)
#Gives each play a play number
df <- df %>% group_by(game_number) %>% mutate(playID = 1:n())
game9 <- df %>% filter(game_number == 9)
# column for every player on the roster. If he's on the court, he'll be assigned 1, if not, 0
# This next bit of code assigns pre-assigns 1 for those who are starting and 0 for those coming off the bench
game9 <- game9 %>% mutate(`GIRARD III,JOSEPH` = ifelse(playID == 1,1,NA),
                            `MINTZ,JUDAH` = ifelse(playID == 1,1,NA),
                            `BELL,CHRIS` = ifelse(playID == 1,1,NA),
                            `WILLIAMS,BENNY` = ifelse(playID == 1,1,NA),
                            `EDWARDS,JESSE` = ifelse(playID == 1,1,NA),
                            `AJAK,JOHN BOL` = ifelse(playID == 1,0,NA),
                            `BROWN,MALIQ` = ifelse(playID == 1,0,NA),
                            `CAREY,PETER` = ifelse(playID == 1,0,NA),
                            `CLAYTON,ANTHONY` = ifelse(playID == 1,0,NA),
                            `COPELAND,QUADIR` = ifelse(playID == 1,0,NA),
                            `FELDMAN,SHANE` = ifelse(playID == 1,0,NA),
                            `HIMA,MOUNIR` = ifelse(playID == 1,0,NA),
                            `KEATING,STEPHEN` = ifelse(playID == 1,0,NA),
                            `RUFFIN,NIKO` = ifelse(playID == 1,0,NA),
                            `TAYLOR,JUSTIN` = ifelse(playID == 1,0,NA),
                            `TORRENCE,SYMIR` = ifelse(playID == 1,0,NA),
)
#duplicates the dataset. Make sure you change the name based on what game you're doing
#at the end, We'll reassing the game dataset with the game9 dataset
game <- game9


#sets Joe Girard to the attribute player to be used in the for loop
player <- "GIRARD III,JOSEPH"
#For loop to fill out the column for Joe Girard to identify when he's subbed out/in
#There is a for loop under every player. It is the same for loop as the one below.
#Although it makes the code longer, it's easier to run all at once.
for (i in 2:nrow(game)) {
  current_value <- game[i, `player`]
  previous_value <- game[i - 1, `player`]
  next_value <- game[i + 1, `player`]
  x <- player
  status <- game[i, "Syracuse"]
  if(is.na(status)){
    game[i,`player`] <- previous_value
  }else if(str_detect(status,glue("SUB OUT by {player}")) && !is.na(status)){
    game[i,`player`] <- 0
  }else if (stringr::str_detect(status,glue("SUB IN by {player}")) && !is.na(status)) {
    game[i, player] <- 1
  }else{
    game[i,player] <- previous_value
  }
}
player <- "MINTZ,JUDAH"
for (i in 2:nrow(game)) {
  current_value <- game[i, `player`]
  previous_value <- game[i - 1, `player`]
  next_value <- game[i + 1, `player`]
  x <- player
  status <- game[i, "Syracuse"]
  if(is.na(status)){
    game[i,`player`] <- previous_value
  }else if(str_detect(status,glue("SUB OUT by {player}")) && !is.na(status)){
    game[i,`player`] <- 0
  }else if (stringr::str_detect(status,glue("SUB IN by {player}")) && !is.na(status)) {
    game[i, player] <- 1
  }else{
    game[i,player] <- previous_value
  }
}
player <- "BELL,CHRIS"
for (i in 2:nrow(game)) {
  current_value <- game[i, `player`]
  previous_value <- game[i - 1, `player`]
  next_value <- game[i + 1, `player`]
  x <- player
  status <- game[i, "Syracuse"]
  if(is.na(status)){
    game[i,`player`] <- previous_value
  }else if(str_detect(status,glue("SUB OUT by {player}")) && !is.na(status)){
    game[i,`player`] <- 0
  }else if (stringr::str_detect(status,glue("SUB IN by {player}")) && !is.na(status)) {
    game[i, player] <- 1
  }else{
    game[i,player] <- previous_value
  }
}
player <- "WILLIAMS,BENNY"
for (i in 2:nrow(game)) {
  current_value <- game[i, `player`]
  previous_value <- game[i - 1, `player`]
  next_value <- game[i + 1, `player`]
  x <- player
  status <- game[i, "Syracuse"]
  if(is.na(status)){
    game[i,`player`] <- previous_value
  }else if(str_detect(status,glue("SUB OUT by {player}")) && !is.na(status)){
    game[i,`player`] <- 0
  }else if (stringr::str_detect(status,glue("SUB IN by {player}")) && !is.na(status)) {
    game[i, player] <- 1
  }else{
    game[i,player] <- previous_value
  }
}
player <- "EDWARDS,JESSE"
for (i in 2:nrow(game)) {
  current_value <- game[i, `player`]
  previous_value <- game[i - 1, `player`]
  next_value <- game[i + 1, `player`]
  x <- player
  status <- game[i, "Syracuse"]
  if(is.na(status)){
    game[i,`player`] <- previous_value
  }else if(str_detect(status,glue("SUB OUT by {player}")) && !is.na(status)){
    game[i,`player`] <- 0
  }else if (stringr::str_detect(status,glue("SUB IN by {player}")) && !is.na(status)) {
    game[i, player] <- 1
  }else{
    game[i,player] <- previous_value
  }
}
player <- "AJAK,JOHN BOL"
for (i in 2:nrow(game)) {
  current_value <- game[i, `player`]
  previous_value <- game[i - 1, `player`]
  next_value <- game[i + 1, `player`]
  x <- player
  status <- game[i, "Syracuse"]
  if(is.na(status)){
    game[i,`player`] <- previous_value
  }else if(str_detect(status,glue("SUB OUT by {player}")) && !is.na(status)){
    game[i,`player`] <- 0
  }else if (stringr::str_detect(status,glue("SUB IN by {player}")) && !is.na(status)) {
    game[i, player] <- 1
  }else{
    game[i,player] <- previous_value
  }
}
player <- "BROWN,MALIQ"
for (i in 2:nrow(game)) {
  current_value <- game[i, `player`]
  previous_value <- game[i - 1, `player`]
  next_value <- game[i + 1, `player`]
  x <- player
  status <- game[i, "Syracuse"]
  if(is.na(status)){
    game[i,`player`] <- previous_value
  }else if(str_detect(status,glue("SUB OUT by {player}")) && !is.na(status)){
    game[i,`player`] <- 0
  }else if (stringr::str_detect(status,glue("SUB IN by {player}")) && !is.na(status)) {
    game[i, player] <- 1
  }else{
    game[i,player] <- previous_value
  }
}
player <- "CAREY,PETER"
for (i in 2:nrow(game)) {
  current_value <- game[i, `player`]
  previous_value <- game[i - 1, `player`]
  next_value <- game[i + 1, `player`]
  x <- player
  status <- game[i, "Syracuse"]
  if(is.na(status)){
    game[i,`player`] <- previous_value
  }else if(str_detect(status,glue("SUB OUT by {player}")) && !is.na(status)){
    game[i,`player`] <- 0
  }else if (stringr::str_detect(status,glue("SUB IN by {player}")) && !is.na(status)) {
    game[i, player] <- 1
  }else{
    game[i,player] <- previous_value
  }
}
player <- "CLAYTON,ANTHONY"
for (i in 2:nrow(game)) {
  current_value <- game[i, `player`]
  previous_value <- game[i - 1, `player`]
  next_value <- game[i + 1, `player`]
  x <- player
  status <- game[i, "Syracuse"]
  if(is.na(status)){
    game[i,`player`] <- previous_value
  }else if(str_detect(status,glue("SUB OUT by {player}")) && !is.na(status)){
    game[i,`player`] <- 0
  }else if (stringr::str_detect(status,glue("SUB IN by {player}")) && !is.na(status)) {
    game[i, player] <- 1
  }else{
    game[i,player] <- previous_value
  }
}
player <- "COPELAND,QUADIR"
for (i in 2:nrow(game)) {
  current_value <- game[i, `player`]
  previous_value <- game[i - 1, `player`]
  next_value <- game[i + 1, `player`]
  x <- player
  status <- game[i, "Syracuse"]
  if(is.na(status)){
    game[i,`player`] <- previous_value
  }else if(str_detect(status,glue("SUB OUT by {player}")) && !is.na(status)){
    game[i,`player`] <- 0
  }else if (stringr::str_detect(status,glue("SUB IN by {player}")) && !is.na(status)) {
    game[i, player] <- 1
  }else{
    game[i,player] <- previous_value
  }
}
player <- "FELDMAN,SHANE"
for (i in 2:nrow(game)) {
  current_value <- game[i, `player`]
  previous_value <- game[i - 1, `player`]
  next_value <- game[i + 1, `player`]
  x <- player
  status <- game[i, "Syracuse"]
  if(is.na(status)){
    game[i,`player`] <- previous_value
  }else if(str_detect(status,glue("SUB OUT by {player}")) && !is.na(status)){
    game[i,`player`] <- 0
  }else if (stringr::str_detect(status,glue("SUB IN by {player}")) && !is.na(status)) {
    game[i, player] <- 1
  }else{
    game[i,player] <- previous_value
  }
}
player <- "HIMA,MOUNIR"
for (i in 2:nrow(game)) {
  current_value <- game[i, `player`]
  previous_value <- game[i - 1, `player`]
  next_value <- game[i + 1, `player`]
  x <- player
  status <- game[i, "Syracuse"]
  if(is.na(status)){
    game[i,`player`] <- previous_value
  }else if(str_detect(status,glue("SUB OUT by {player}")) && !is.na(status)){
    game[i,`player`] <- 0
  }else if (stringr::str_detect(status,glue("SUB IN by {player}")) && !is.na(status)) {
    game[i, player] <- 1
  }else{
    game[i,player] <- previous_value
  }
}
player <- "KEATING,STEPHEN"
for (i in 2:nrow(game)) {
  current_value <- game[i, `player`]
  previous_value <- game[i - 1, `player`]
  next_value <- game[i + 1, `player`]
  x <- player
  status <- game[i, "Syracuse"]
  if(is.na(status)){
    game[i,`player`] <- previous_value
  }else if(str_detect(status,glue("SUB OUT by {player}")) && !is.na(status)){
    game[i,`player`] <- 0
  }else if (stringr::str_detect(status,glue("SUB IN by {player}")) && !is.na(status)) {
    game[i, player] <- 1
  }else{
    game[i,player] <- previous_value
  }
}
player <- "RUFFIN,NIKO"
for (i in 2:nrow(game)) {
  current_value <- game[i, `player`]
  previous_value <- game[i - 1, `player`]
  next_value <- game[i + 1, `player`]
  x <- player
  status <- game[i, "Syracuse"]
  if(is.na(status)){
    game[i,`player`] <- previous_value
  }else if(str_detect(status,glue("SUB OUT by {player}")) && !is.na(status)){
    game[i,`player`] <- 0
  }else if (stringr::str_detect(status,glue("SUB IN by {player}")) && !is.na(status)) {
    game[i, player] <- 1
  }else{
    game[i,player] <- previous_value
  }
}
player <- "TAYLOR,JUSTIN"
for (i in 2:nrow(game)) {
  current_value <- game[i, `player`]
  previous_value <- game[i - 1, `player`]
  next_value <- game[i + 1, `player`]
  x <- player
  status <- game[i, "Syracuse"]
  if(is.na(status)){
    game[i,`player`] <- previous_value
  }else if(str_detect(status,glue("SUB OUT by {player}")) && !is.na(status)){
    game[i,`player`] <- 0
  }else if (stringr::str_detect(status,glue("SUB IN by {player}")) && !is.na(status)) {
    game[i, player] <- 1
  }else{
    game[i,player] <- previous_value
  }
}
player <- "TORRENCE,SYMIR"
for (i in 2:nrow(game)) {
  current_value <- game[i, `player`]
  previous_value <- game[i - 1, `player`]
  next_value <- game[i + 1, `player`]
  x <- player
  status <- game[i, "Syracuse"]
  if(is.na(status)){
    game[i,`player`] <- previous_value
  }else if(str_detect(status,glue("SUB OUT by {player}")) && !is.na(status)){
    game[i,`player`] <- 0
  }else if (stringr::str_detect(status,glue("SUB IN by {player}")) && !is.na(status)) {
    game[i, player] <- 1
  }else{
    game[i,player] <- previous_value
  }
}

#makes sure that there is at most 5 players on the court at all times
game <- game %>% mutate(players_on_court = rowSums(game[,c("GIRARD III,JOSEPH","MINTZ,JUDAH","BELL,CHRIS","WILLIAMS,BENNY","EDWARDS,JESSE","AJAK,JOHN BOL","BROWN,MALIQ","CAREY,PETER","CLAYTON,ANTHONY","COPELAND,QUADIR","FELDMAN,SHANE","HIMA,MOUNIR","KEATING,STEPHEN","RUFFIN,NIKO","TAYLOR,JUSTIN","TORRENCE,SYMIR")]))

#starters in court
#might need to change based on who's starting that game
game <- game %>% mutate(starters = ifelse(`GIRARD III,JOSEPH` == 1 &
                                            `MINTZ,JUDAH` == 1 &
                                            `BELL,CHRIS` == 1 &
                                            `WILLIAMS,BENNY` == 1 &
                                            `EDWARDS,JESSE` == 1 & 
                                            players_on_court == 5, 1,0))
#number of starters on the court
#might need to change based on who's starting that game
game <- game %>% mutate(starters_on_court = ifelse(players_on_court == 5,rowSums(game[,c("GIRARD III,JOSEPH","MINTZ,JUDAH","BELL,CHRIS","WILLIAMS,BENNY","EDWARDS,JESSE")]),NA))

#Checks for majority bench players
game <- game %>% mutate(majority_bench_players = ifelse((starters_on_court < 3) &
                                                          (players_on_court == 5), 1,0))
# 2pt Made
game <- game %>% mutate(two_pt_made = ifelse(str_detect(game$Syracuse, "GOOD LAYUP") |
                                               str_detect(game$Syracuse, "GOOD JUMPER")|
                                               str_detect(game$Syracuse, "GOOD DUNK"),1,0))
#2pt Attempts
game <- game %>% mutate(two_pt_attempts = ifelse(str_detect(game$Syracuse, "GOOD LAYUP") | 
                                                   str_detect(game$Syracuse, "MISS LAYUP") |
                                                   str_detect(game$Syracuse, "MISS JUMPER") |
                                                   str_detect(game$Syracuse, "GOOD JUMPER")|
                                                   str_detect(game$Syracuse, "MISS DUNK")|
                                                   str_detect(game$Syracuse, "GOOD DUNK"),1,0))
#3pt Made
game <- game %>% mutate(three_pt_made = ifelse(str_detect(game$Syracuse, "GOOD 3PTR"),1,0))

#3pt Attempts
game <- game %>% mutate(three_pt_attempts = ifelse(str_detect(game$Syracuse, "GOOD 3PTR") | 
                                                     str_detect(game$Syracuse, "MISS 3PTR"),1,0))
#3pt%
game <- game %>% mutate(three_pt_pct = ifelse(three_pt_attempts == 1, three_pt_made/three_pt_attempts,NA))

#FG Made
game <- game %>% mutate(fg_made = two_pt_made + three_pt_made)

#FG Attempts
game <- game %>% mutate(fg_attempts = three_pt_attempts + two_pt_attempts)

#FG%
game <- game %>% mutate(fg_pct = ifelse(fg_attempts == 1,fg_made/fg_attempts,NA))

#effective FG%
game <- game %>% mutate(effective_fg_pct = ifelse(fg_attempts == 1,(fg_made + 0.5*three_pt_made)/fg_attempts,NA))

#identifies made free throws
game <- game %>% mutate(made_free_throw = ifelse(str_detect(game$Syracuse, "GOOD FT"),1,0))

#identifies free throw attempts
game <- game %>% mutate(free_throw_attempt = ifelse(str_detect(game$Syracuse, "GOOD FT") | str_detect(game$Syracuse, "MISS FT"),1,0))

#ft%
game <- game %>% mutate(ft_pct = ifelse(free_throw_attempt == 1,made_free_throw/free_throw_attempt,NA))

#Pts Scored
game <- game %>% mutate(pts = ifelse(made_free_throw == 1, 1, ifelse(
  two_pt_made == 1, 2,ifelse(
    three_pt_made == 1, 3,0  
  ))))

#identifies shots taken in the paint
game <- game %>% mutate(in_the_paint = ifelse(str_detect(game$Syracuse, "(in the paint)"),1,0))

#identifies fast break points
game <- game %>% mutate(fastbreak = ifelse(str_detect(game$Syracuse, "(fastbreak)"),1,0))

#identifies Assists
game <- game %>% mutate(assist = ifelse(str_detect(game$Syracuse, "ASSIST"),1,0))

#identifies offensive rebounds
game <- game %>% mutate(off_reb = ifelse(str_detect(game$Syracuse, "REBOUND OFF"),1,0))

#identifies defensive rebounds
game <- game %>% mutate(def_reb = ifelse(str_detect(game$Syracuse, "REBOUND DEF"),1,0))

#total rebounds
game <- game %>% mutate(total_reb = off_reb + def_reb)

#identifies turnover
game <- game %>% mutate(turnover = ifelse(str_detect(game$Syracuse, "TURNOVER"),1,0))


#identifies steals
game <- game %>% mutate(steal = ifelse(str_detect(game$Syracuse, "STEAL"),1,0))

#identifies blocks
game <- game %>% mutate(block = ifelse(str_detect(game$Syracuse, "BLOCK"),1,0))

#opponents 2pt Made
game <- game %>% mutate(opp_two_pt_made = ifelse(str_detect(game$Visitors, "GOOD LAYUP") |
                                               str_detect(game$Visitors, "GOOD JUMPER")|
                                               str_detect(game$Visitors, "GOOD DUNK"),1,0))
#opponents 2pt Attempts
game <- game %>% mutate(opp_two_pt_attempts = ifelse(str_detect(game$Visitors, "GOOD LAYUP") | 
                                                   str_detect(game$Visitors, "MISS LAYUP") |
                                                   str_detect(game$Visitors, "MISS JUMPER") |
                                                   str_detect(game$Visitors, "GOOD JUMPER")|
                                                   str_detect(game$Visitors, "MISS DUNK")|
                                                   str_detect(game$Visitors, "GOOD DUNK"),1,0))
#opponents 3pt Made
game <- game %>% mutate(opp_three_pt_made = ifelse(str_detect(game$Visitors, "GOOD 3PTR"),1,0))

#opponents 3pt Attempts
game <- game %>% mutate(opp_three_pt_attempts = ifelse(str_detect(game$Visitors, "GOOD 3PTR") | 
                                                     str_detect(game$Visitors, "MISS 3PTR"),1,0))
#opponents 3pt%
game <- game %>% mutate(opp_three_pt_pct = ifelse(opp_three_pt_attempts == 1, opp_three_pt_made/opp_three_pt_attempts,NA))

#opponents FG Made
game <- game %>% mutate(opp_fg_made = opp_two_pt_made + opp_three_pt_made)

#opponents FG Attempts
game <- game %>% mutate(opp_fg_attempts = opp_three_pt_attempts + opp_two_pt_attempts)

#opponents FG%
game <- game %>% mutate(opp_fg_pct = ifelse(opp_fg_attempts == 1,opp_fg_made/opp_fg_attempts,NA))

#opponents effective FG%
game <- game %>% mutate(opp_effective_fg_pct = ifelse(opp_fg_attempts == 1,(opp_fg_made + 0.5*opp_three_pt_made)/opp_fg_attempts,NA))

#identifies opponents made free throws
game <- game %>% mutate(opp_made_free_throw = ifelse(str_detect(game$Visitors, "GOOD FT"),1,0))

#identifies opponents free throw attempts
game <- game %>% mutate(opp_free_throw_attempt = ifelse(str_detect(game$Visitors, "GOOD FT") | str_detect(game$Visitors, "MISS FT"),1,0))

#opp ft%
game <- game %>% mutate(opp_ft_pct = ifelse(opp_free_throw_attempt == 1,opp_made_free_throw/opp_free_throw_attempt,NA))

#Pts Scored Against
game <- game %>% mutate(pts_against = ifelse(opp_made_free_throw == 1, 1, ifelse(
  opp_two_pt_made == 1, 2,ifelse(
    opp_three_pt_made == 1, 3,0  
  ))))

#identifies opponents shots taken in the paint
game <- game %>% mutate(opp_in_the_paint = ifelse(str_detect(game$Visitors, "(in the paint)"),1,0))

#identifies opponents fast break points
game <- game %>% mutate(opp_fastbreak = ifelse(str_detect(game$Visitors, "(fastbreak)"),1,0))

#identifies opponents Assists
game <- game %>% mutate(opp_assist = ifelse(str_detect(game$Visitors, "ASSIST"),1,0))

#identifies opponents offensive rebounds
game <- game %>% mutate(opp_off_reb = ifelse(str_detect(game$Visitors, "REBOUND OFF"),1,0))

#identifies opposing defensive rebounds
game <- game %>% mutate(opp_def_reb = ifelse(str_detect(game$Visitors, "REBOUND DEF"),1,0))

#total opponents rebounds
game <- game %>% mutate(opp_total_reb = opp_off_reb + opp_def_reb)

#identifies opponents turnover
game <- game %>% mutate(opp_turnover = ifelse(str_detect(game$Visitors, "TURNOVER"),1,0))


#identifies opponents steals
game <- game %>% mutate(opp_steal = ifelse(str_detect(game$Visitors, "STEAL"),1,0))

#identifies opponents blocks
game <- game %>% mutate(opp_block = ifelse(str_detect(game$Visitors, "BLOCK"),1,0))
#identifies if Syracuse is the home team
game <- game %>% mutate(Home = ifelse(game_number <= 14,1,0))


game9 <- game
