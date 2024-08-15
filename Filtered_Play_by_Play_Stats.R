library(tidyverse)
#combining all play-by-play data (we each took 8 games)
pbp <- bind_rows(pbp_1_8,pbp_9_16,pbp_17_24,pbp_25_32)

#adding additional columns not added to the orignal pbp data
pbp <- pbp %>% mutate(pts_in_the_paint = ifelse(in_the_paint == 1, pts, 0))
pbp <- pbp %>% mutate(opp_pts_in_the_paint = ifelse(opp_in_the_paint == 1, pts_against, 0))
pbp <- pbp %>% mutate(in_the_paint_fg_pct = ifelse(in_the_paint == 1, fg_pct, NA))
pbp <- pbp %>% mutate(opp_in_the_paint_fg_pct = ifelse(opp_in_the_paint == 1, opp_fg_pct, NA))
pbp <- pbp %>% mutate(in_the_paint_fg_made = ifelse(in_the_paint == 1, fg_made, NA))
pbp <- pbp %>% mutate(in_the_paint_fg_attempts = ifelse(in_the_paint == 1, fg_attempts, NA))
pbp <- pbp %>% mutate(opp_in_the_paint_fg_made = ifelse(opp_in_the_paint == 1, opp_fg_made, NA))
pbp <- pbp %>% mutate(opp_in_the_paint_fg_attempts = ifelse(opp_in_the_paint == 1, opp_fg_attempts, NA))

#groups all possible on_court combinations and summarizes the stats from the play by play data
df23 <- pbp %>% filter(players_on_court == 5) %>% group_by(`GIRARD III,JOSEPH`, `MINTZ,JUDAH`, `BELL,CHRIS`, `WILLIAMS,BENNY`, `EDWARDS,JESSE`,
                              `AJAK,JOHN BOL`, `BROWN,MALIQ`, `CAREY,PETER`, `CLAYTON,ANTHONY`, `COPELAND,QUADIR`, 
                              `FELDMAN,SHANE`, `HIMA,MOUNIR`, `KEATING,STEPHEN`, `RUFFIN,NIKO`, `TAYLOR,JUSTIN`, `TORRENCE,SYMIR`) %>% 
                      summarize(two_pt_made = sum(two_pt_made, na.rm = T),
                                two_pt_attempts = sum(two_pt_attempts, na.rm = T),
                                three_pt_made = sum(three_pt_made, na.rm = T),
                                three_pt_attempts = sum(three_pt_attempts, na.rm = T),
                                fg_made = sum(fg_made, na.rm = T),
                                fg_attempts = sum(fg_attempts, na.rm = T),
                                made_free_throw = sum(made_free_throw, na.rm = T),
                                free_throw_attempt = sum(free_throw_attempt, na.rm = T),
                                in_the_paint_fg_made = sum(in_the_paint_fg_made, na.rm = T),
                                opp_in_the_paint_fg_made = sum(opp_in_the_paint_fg_made, na.rm = T),
                                in_the_paint_fg_attempts = sum(in_the_paint_fg_attempts, na.rm = T),
                                opp_in_the_paint_fg_attempts = sum(opp_in_the_paint_fg_attempts, na.rm = T),
                                opp_two_pt_made = sum(opp_two_pt_made, na.rm = T),
                                opp_two_pt_attempts = sum(opp_two_pt_attempts, na.rm = T),
                                opp_three_pt_made = sum(opp_three_pt_made, na.rm = T),
                                opp_three_pt_attempts = sum(opp_three_pt_attempts, na.rm = T),
                                opp_fg_made = sum(opp_fg_made, na.rm = T),
                                opp_fg_attempts = sum(opp_fg_attempts, na.rm = T),
                                opp_made_free_throw = sum(opp_made_free_throw, na.rm = T),
                                opp_free_throw_attempt = sum(opp_free_throw_attempt, na.rm = T),
                                pts = sum(pts, na.rm = T),
                                pts_in_the_paint = sum(pts_in_the_paint, na.rm = T),
                                fg_pct = round(mean(fg_pct, na.rm = T),3)*100,
                                in_the_paint_fg_pct = round(mean(in_the_paint_fg_pct, na.rm = T),3)*100,
                                three_pt_pct = round(mean(three_pt_pct, na.rm = T),3)*100,
                                ft_pct = round(mean(ft_pct, na.rm = T),3)*100,
                                effective_fg_pct = round(mean(effective_fg_pct, na.rm = T),3)*100,
                                assists = sum(assist, na.rm = T),
                                offensive_rebounds = sum(off_reb, na.rm = T),
                                defensive_rebounds. = sum(def_reb, na.rm = T),
                                total_rebounds = sum(total_reb, na.rm = T),
                                `OREB%` = round((sum(off_reb,na.rm = T)/(sum(off_reb,na.rm = T)+sum(opp_def_reb, na.rm = T))),3)*100,
                                `DREB%` = round((sum(def_reb,na.rm = T)/(sum(def_reb,na.rm = T) + sum(opp_off_reb, na.rm = T))),3)*100,
                                turnovers = sum(turnover, na.rm = T),
                                steals = sum(steal, na.rm = T),
                                block = sum(block, na.rm = T),
                                pts_against = sum(pts_against, na.rm = T),
                                opp_pts_in_the_paint = sum(opp_pts_in_the_paint, na.rm = T),
                                opp_fg_pct = round(mean(opp_fg_pct, na.rm = T),3)*100,
                                opp_in_the_paint_fg_pct = round(mean(opp_in_the_paint_fg_pct, na.rm = T),3)*100,
                                opp_three_pt_pct = round(mean(opp_three_pt_pct, na.rm = T),3)*100,
                                opp_ft_pct = round(mean(opp_ft_pct, na.rm = T),3),
                                opp_effective_fg_pct = round(mean(opp_effective_fg_pct, na.rm = T),3)*100,
                                opp_assists = sum(opp_assist, na.rm = T),
                                opp_offensive_rebounds = sum(opp_off_reb, na.rm = T),
                                opp_defensive_rebounds = sum(opp_def_reb, na.rm = T),
                                opp_total_rebounds = sum(opp_total_reb, na.rm = T),
                                opp_turnovers = sum(opp_turnover, na.rm = T),
                                opp_steals = sum(opp_steal, na.rm = T),
                                opp_blocks = sum(opp_block, na.rm = T),
                                n = n()) %>% 
                    arrange(desc(n)) %>% ungroup()


write_csv(df23, file = "df23.csv")
