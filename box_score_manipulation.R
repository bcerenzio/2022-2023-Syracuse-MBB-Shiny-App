library(tidyverse)
View(syracuse_box_scores)
#FG Attempts
syracuse_box_scores <- syracuse_box_scores %>% mutate(FGA = ifelse(str_detect(syracuse_box_scores$FG, "-"),str_extract(syracuse_box_scores$FG,"-\\d+"),NA))
syracuse_box_scores$FGA <- str_remove_all(syracuse_box_scores$FGA,"-")
#Editing FG column to only include made FG
syracuse_box_scores$FG <- str_remove_all(syracuse_box_scores$FG, "-\\d+")
#3PA
syracuse_box_scores <- syracuse_box_scores %>% mutate(`3PA` = ifelse(str_detect(syracuse_box_scores$`3PT`, "-"),str_extract(syracuse_box_scores$`3PT`,"-\\d+"),NA))
syracuse_box_scores$`3PA` <- str_remove_all(syracuse_box_scores$`3PA`,"-")
#Editing 3PT column to only include made 3PT
syracuse_box_scores$`3PT` <- str_remove_all(syracuse_box_scores$`3PT`, "-\\d+")
#FTA
syracuse_box_scores <- syracuse_box_scores %>% mutate(FTA = ifelse(str_detect(syracuse_box_scores$FT, "-"),str_extract(syracuse_box_scores$FT,"-\\d+"),NA))
syracuse_box_scores$FTA <- str_remove_all(syracuse_box_scores$FTA,"-")
#Editing FT column to only include made FT
syracuse_box_scores$FT <- str_remove_all(syracuse_box_scores$FT, "-\\d+")
#filter out rows where Player Name is Empty
syracuse_box_scores <- syracuse_box_scores %>% filter(Player_Name != "")
#FG%
syracuse_box_scores <- syracuse_box_scores %>% mutate(`FG%` = ifelse(as.numeric(FGA) >= 1,round((as.numeric(FG)/as.numeric(FGA)),3)*100,0))
#3PT%
syracuse_box_scores <- syracuse_box_scores %>% mutate(`3P%` = ifelse(as.numeric(`3PA`) >= 1,round((as.numeric(`3PT`)/as.numeric(`3PA`)),3)*100,0))
#FT%
syracuse_box_scores <- syracuse_box_scores %>% mutate(`FT%` = ifelse(as.numeric(FTA) >= 1,round((as.numeric(FT)/as.numeric(FTA)),3)*100,0))
#Effective Field Goal %
syracuse_box_scores <- syracuse_box_scores %>% mutate(`eFG%` = ifelse(as.numeric(FGA) >= 1,round(((as.numeric(FG)+0.5*as.numeric(`3PT`))/as.numeric(FGA)),3)*100,0))
#Season Stats
season_stats <- syracuse_box_scores %>%  group_by(Player_Name) %>% summarize(MIN = sum(as.numeric(MIN)),
                                                                                                              FG = sum(as.numeric(FG)), 
                                                                                                              `3PT` = sum(as.numeric(`3PT`)),
                                                                                                              FT = sum(as.numeric(FT)),
                                                                                                              OREB = sum(as.numeric(OREB)),
                                                                                                              DREB = sum(as.numeric(DREB)),
                                                                                                              REB = sum(as.numeric(REB)),
                                                                                                              AST = sum(as.numeric(AST)),
                                                                                                              STL = sum(as.numeric(STL)),
                                                                                                              BLK = sum(as.numeric(BLK)),
                                                                                                              TO = sum(as.numeric(TO)),
                                                                                                              PF = sum(as.numeric(PF)),
                                                                                                              PTS = sum(as.numeric(PTS)),
                                                                                                              game_number = "Season",
                                                                                                              FGA = sum(as.numeric(FGA)),
                                                                                                              `3PA` = sum(as.numeric(`3PA`)),
                                                                                                              FTA = sum(as.numeric(FTA)),
                                                                                                              `FG%` = round(sum(as.numeric(FG))/sum(as.numeric(FGA)),3)*100,
                                                                                                              `3P%` = round(sum(as.numeric(`3PT`))/sum(as.numeric(`3PA`)),3)*100,
                                                                                                              `FT%` = round(sum(as.numeric(FT))/sum(as.numeric(FTA)),3)*100,
                                                                                                              `eFG%` = round((sum(as.numeric(FG)) + 0.5*sum(`3PT`))/sum(as.numeric(FGA)),3)*100)
#making columns in syracuse_box_scores numeric so I can combine the two datasets
syracuse_box_scores$MIN <- as.numeric(syracuse_box_scores$MIN)
syracuse_box_scores$FG <- as.numeric(syracuse_box_scores$FG)
syracuse_box_scores$`3PT` <- as.numeric(syracuse_box_scores$`3PT`)
syracuse_box_scores$FT <- as.numeric(syracuse_box_scores$FT)
syracuse_box_scores$OREB <- as.numeric(syracuse_box_scores$OREB)
syracuse_box_scores$DREB <- as.numeric(syracuse_box_scores$DREB)
syracuse_box_scores$REB <- as.numeric(syracuse_box_scores$REB)
syracuse_box_scores$AST <- as.numeric(syracuse_box_scores$AST)
syracuse_box_scores$STL <- as.numeric(syracuse_box_scores$STL)
syracuse_box_scores$BLK <- as.numeric(syracuse_box_scores$BLK)
syracuse_box_scores$TO <- as.numeric(syracuse_box_scores$TO)
syracuse_box_scores$PF <- as.numeric(syracuse_box_scores$PF)
syracuse_box_scores$PTS <- as.numeric(syracuse_box_scores$PTS)
syracuse_box_scores$FGA <- as.numeric(syracuse_box_scores$FGA)
syracuse_box_scores$`3PA` <- as.numeric(syracuse_box_scores$`3PA`)
syracuse_box_scores$FTA <- as.numeric(syracuse_box_scores$FTA)
#making game_number a character so I can combine the two datasets
syracuse_box_scores$game_number <- as.character(syracuse_box_scores$game_number)

#combining the two datasets
syracuse_box_scores <- bind_rows(syracuse_box_scores,season_stats)

#Creating another dataframe for syracuse team stats to calculate Ast%, REB%, etc.
syracuse_team_stats <- syracuse_box_scores %>% filter(Player_Name == "team")
syracuse_team_stats <- syracuse_team_stats %>% select(-Player_Name,-MIN)
colnames(syracuse_team_stats) <- c("Tm_FG","Tm_3PT","Tm_FT","Tm_OREB","Tm_DREB","Tm_REB","Tm_AST","Tm_STL","Tm_BLK","Tm_TO","Tm_PF","Tm_PTS","game_number","Tm_FGA", "Tm_3PA", "Tm_FTA", "Tm_FG%", "Tm_3P%", "Tm_FT%", "Tm_eFG%")
#adding Team Minutes
#games 4-5 went to overtime, so their values will be larger
#to calculate the total minutes played in a season, I did 200*30 + 225*2
syracuse_team_stats <- syracuse_team_stats %>% mutate(Tm_MIN = ifelse(!(game_number %in% c(4,5,"Season")),200,ifelse(
  game_number %in% c(4,5),225,200*30 + 225*2
)))

#joining box scores with team stats
syracuse_box_scores <- left_join(syracuse_box_scores, syracuse_team_stats, by = c("game_number"))


#Game Score
syracuse_box_scores <- syracuse_box_scores %>% group_by(game_number) %>% mutate(Game_Score = ifelse(game_number != "Season" & Player_Name != "team", round(PTS + FG*0.4 - 0.7*FGA - 0.4*(FTA - FT) + 0.7*OREB + 0.3*DREB + STL + 0.7*AST + 0.7*BLK - 0.4*PF - TO,1), NA)) %>% ungroup()

#True Shooting%
syracuse_box_scores <- syracuse_box_scores %>% mutate(`TS%` = round(PTS / (2*(FGA + 0.475*FTA)),3)*100)

#Turnover Percentage
syracuse_box_scores <- syracuse_box_scores %>% mutate(`TO%` = round(TO/(FGA + 0.475 * FTA + TO),3)*100)


#Opposing FG Attempts
visitors_box_scores <- visitors_box_scores %>% mutate(FGA = ifelse(str_detect(visitors_box_scores$FG, "-"),str_extract(visitors_box_scores$FG,"-\\d+"),NA))
visitors_box_scores$FGA <- str_remove_all(visitors_box_scores$FGA,"-")
#Editing FG column to only include made FG
visitors_box_scores$FG <- str_remove_all(visitors_box_scores$FG, "-\\d+")
#Opposing 3PA
visitors_box_scores <- visitors_box_scores %>% mutate(`3PA` = ifelse(str_detect(visitors_box_scores$`3PT`, "-"),str_extract(visitors_box_scores$`3PT`,"-\\d+"),NA))
visitors_box_scores$`3PA` <- str_remove_all(visitors_box_scores$`3PA`,"-")
#Editing 3PT column to only include made 3PT
visitors_box_scores$`3PT` <- str_remove_all(visitors_box_scores$`3PT`, "-\\d+")
#Opposing FTA
visitors_box_scores <- visitors_box_scores %>% mutate(FTA = ifelse(str_detect(visitors_box_scores$FT, "-"),str_extract(visitors_box_scores$FT,"-\\d+"),NA))
visitors_box_scores$FTA <- str_remove_all(visitors_box_scores$FTA,"-")
#Editing FT column to only include made FT
visitors_box_scores$FT <- str_remove_all(visitors_box_scores$FT, "-\\d+")
#filter out rows where Player Name is Empty
visitors_box_scores <- visitors_box_scores %>% filter(Player_Name != "")

#make values numeric
visitors_box_scores$MIN <- as.numeric(visitors_box_scores$MIN)
visitors_box_scores$FG <- as.numeric(visitors_box_scores$FG)
visitors_box_scores$`3PT` <- as.numeric(visitors_box_scores$`3PT`)
visitors_box_scores$FT <- as.numeric(visitors_box_scores$FT)
visitors_box_scores$OREB <- as.numeric(visitors_box_scores$OREB)
visitors_box_scores$DREB <- as.numeric(visitors_box_scores$DREB)
visitors_box_scores$REB <- as.numeric(visitors_box_scores$REB)
visitors_box_scores$AST <- as.numeric(visitors_box_scores$AST)
visitors_box_scores$STL <- as.numeric(visitors_box_scores$STL)
visitors_box_scores$BLK <- as.numeric(visitors_box_scores$BLK)
visitors_box_scores$TO <- as.numeric(visitors_box_scores$TO)
visitors_box_scores$PF <- as.numeric(visitors_box_scores$PF)
visitors_box_scores$PTS <- as.numeric(visitors_box_scores$PTS)
visitors_box_scores$FGA <- as.numeric(visitors_box_scores$FGA)
visitors_box_scores$`3PA` <- as.numeric(visitors_box_scores$`3PA`)
visitors_box_scores$FTA <- as.numeric(visitors_box_scores$FTA)
visitors_box_scores$game_number <- as.character(visitors_box_scores$game_number)

#Opposing FG%
visitors_box_scores <- visitors_box_scores %>% mutate(`FG%` = ifelse(FGA >= 1,round(FG/FGA,3)*100,0))
#Opposing 3PT%
visitors_box_scores <- visitors_box_scores %>% mutate(`3P%` = ifelse(as.numeric(`3PA`) >= 1,round((as.numeric(`3PT`)/as.numeric(`3PA`)),3)*100,0))
#Opposing FT%
visitors_box_scores <- visitors_box_scores %>% mutate(`FT%` = ifelse(as.numeric(FTA) >= 1,round((as.numeric(FT)/as.numeric(FTA)),3)*100,0))
#Opposing Effective Field Goal %
visitors_box_scores <- visitors_box_scores %>% mutate(`eFG%` = ifelse(as.numeric(FGA) >= 1,round(((as.numeric(FG)+0.5*as.numeric(`3PT`))/as.numeric(FGA)),3)*100,0))
#visitors season stats
visitors_season_stats <- visitors_box_scores %>% filter(Player_Name == "team") %>% group_by(Player_Name)%>% summarize(MIN = NA,
                                                                                                              FG = sum(as.numeric(FG)), 
                                                                                                              `3PT` = sum(as.numeric(`3PT`)),
                                                                                                              FT = sum(as.numeric(FT)),
                                                                                                              OREB = sum(as.numeric(OREB)),
                                                                                                              DREB = sum(as.numeric(DREB)),
                                                                                                              REB = sum(as.numeric(REB)),
                                                                                                              AST = sum(as.numeric(AST)),
                                                                                                              STL = sum(as.numeric(STL)),
                                                                                                              BLK = sum(as.numeric(BLK)),
                                                                                                              TO = sum(as.numeric(TO)),
                                                                                                              PF = sum(as.numeric(PF)),
                                                                                                              PTS = sum(as.numeric(PTS)),
                                                                                                              game_number = "Season",
                                                                                                              FGA = sum(as.numeric(FGA)),
                                                                                                              `3PA` = sum(as.numeric(`3PA`)),
                                                                                                              FTA = sum(as.numeric(FTA)),
                                                                                                              `FG%` = round(sum(as.numeric(FG))/sum(as.numeric(FGA)),3)*100,
                                                                                                              `3P%` = round(sum(as.numeric(`3PT`))/sum(as.numeric(`3PA`)),3)*100,
                                                                                                              `FT%` = round(sum(as.numeric(FT))/sum(as.numeric(FTA)),3)*100,
                                                                                                              `eFG%` = round((sum(as.numeric(FG)) + 0.5*sum(`3PT`))/sum(as.numeric(FGA)),3)*100)
visitors_box_scores <- bind_rows(visitors_box_scores,visitors_season_stats)

#setting column names for visitors boxscores
colnames(visitors_box_scores) <- c("Opp_Player_Name","Opp_MIN","Opp_FG","Opp_3PT","Opp_FT","Opp_OREB","Opp_DREB","Opp_REB","Opp_AST","Opp_STL","Opp_BLK","Opp_TO","Opp_PF","Opp_PTS","game_number","Opp_FGA", "Opp_3PA", "Opp_FTA", "Opp_FG%", "Opp_3P%", "Opp_FT%", "Opp_eFG%")
#filtering such that only team stats show up in visitors box score
visitors_box_scores <- visitors_box_scores %>% filter(Opp_Player_Name == "team")
visitors_box_scores <- visitors_box_scores %>% select(-Opp_Player_Name,-Opp_MIN)
#combining datasets
syracuse_box_scores <- left_join(syracuse_box_scores, visitors_box_scores, by = c("game_number"))


#calculating possessions
syracuse_box_scores <- syracuse_box_scores %>% group_by(game_number) %>% 
  mutate(possessions = ifelse(Player_Name == "team", 
                0.5*(FGA + 0.475*FTA - OREB + TO) + 0.5*(Opp_FGA + 0.475*Opp_FTA - Opp_OREB + Opp_TO),NA)) %>% ungroup()

#offensive ratings per game
syracuse_box_scores <- syracuse_box_scores %>% group_by(game_number) %>% 
  mutate(ORtg = ifelse(Player_Name == "team", round(100*(PTS/possessions),1),NA)) %>% ungroup()

#defensive ratings per game
syracuse_box_scores <- syracuse_box_scores %>% group_by(game_number) %>% 
  mutate(DRtg = ifelse(Player_Name == "team", round(100*(Opp_PTS/possessions),1),NA)) %>% ungroup()
#OREB%
syracuse_box_scores <- syracuse_box_scores %>% group_by(game_number) %>% 
  mutate(`OREB%` = ifelse(Player_Name == "team", round(OREB/(OREB + Opp_DREB),3)*100,round(100*(OREB*(Tm_MIN/5))/(MIN*(Tm_OREB+Opp_DREB)),1))) %>% ungroup()
#DREB%
syracuse_box_scores <- syracuse_box_scores %>% group_by(game_number) %>% 
  mutate(`DREB%` = ifelse(Player_Name == "team", round(DREB/(DREB + Opp_OREB),3)*100,round(100*(DREB*(Tm_MIN/5))/(MIN*(Tm_DREB+Opp_OREB)),1))) %>% ungroup()
#TREB%
syracuse_box_scores <- syracuse_box_scores %>% group_by(game_number) %>% 
  mutate(`REB%` = ifelse(Player_Name == "team", round(REB/(REB + Opp_REB),3)*100,round(100*(REB*(Tm_MIN/5))/(MIN*(Tm_REB+Opp_REB)),1))) %>% ungroup()

#BLK%
syracuse_box_scores <- syracuse_box_scores %>% group_by(game_number) %>% 
  mutate(`BLK%` = ifelse(Player_Name == "team", round(BLK/(Opp_FGA - Opp_3PA),3)*100,round(100*(BLK*(Tm_MIN/5))/(MIN*(Opp_FGA - Opp_3PA)),1))) %>% ungroup()
#AST%
syracuse_box_scores <- syracuse_box_scores %>% group_by(game_number) %>% 
  mutate(`AST%` = ifelse(Player_Name == "team",round(100*AST/FG,1),round(100*AST/(((MIN/(Tm_MIN/5))*Tm_FG)-FG),1))) %>% ungroup()
#STL%
syracuse_box_scores <- syracuse_box_scores %>% group_by(game_number) %>% 
  mutate(`STL%` = ifelse(Player_Name == "team", round(STL/possessions,3)*100,round(100*(STL*(Tm_MIN/5))/(MIN*(0.5*(Tm_FGA + 0.475*Tm_FTA - Tm_OREB + Tm_TO) + 0.5*(Opp_FGA + 0.475*Opp_FTA - Opp_OREB + Opp_TO))),1))) %>% ungroup()
#USG%
syracuse_box_scores <- syracuse_box_scores %>% group_by(game_number) %>% 
  mutate(`USG%` = ifelse(Player_Name != "team", round(100*((FGA+0.44*FTA+TO)*(Tm_MIN/5))/(MIN*(Tm_FGA+0.44*Tm_FTA+Tm_TO)),1),NA)) %>% ungroup()

#offensive four factors
syracuse_box_scores <- syracuse_box_scores %>% group_by(game_number) %>% 
  mutate(off_four_factors = ifelse(Player_Name == "team", 0.4*`eFG%` + 0.25*`TO%` + 0.2*`OREB%` + 0.15 * (FT/FGA),NA)
  )
#pts per 30
syracuse_box_scores <- syracuse_box_scores %>% 
  mutate(pts_per_30 = ifelse(Player_Name != "team", round((PTS/MIN)*30,1),NA))

#assists per 30
syracuse_box_scores <- syracuse_box_scores %>% 
  mutate(ast_per_30 = ifelse(Player_Name != "team", round((AST/MIN)*30,1),NA))

#rebounds per 30
syracuse_box_scores <- syracuse_box_scores %>% 
  mutate(reb_per_30 = ifelse(Player_Name != "team", round((REB/MIN)*30,1),NA))

write_csv(syracuse_box_scores,file = "syracuse_box_scores.csv")


