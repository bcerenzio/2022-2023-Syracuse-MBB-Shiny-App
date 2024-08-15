library(shiny)
library(tidyverse)
library(DT)
library(glue)
library(grid)
library(RCurl)
library(png)
data <- read_csv("df23.csv")
syracuse_box_scores <- read_csv("syracuse_box_scores.csv")
shot_locations <- read_csv("shotdata.csv") %>% drop_na(loc_x) %>% filter(event_team == "Syracuse")
#adds shot chart image
court <- rasterGrob(readPNG("NCAA-Court.png"),
                    width=unit(1,"npc"), height=unit(1,"npc"))



# Define UI
ui <- navbarPage(
  title = "Syracuse 2022-2023 Stats",
  tabPanel("Boxscores",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 selectInput("gamenumber", label = "Game",
                             choices= c("Season","Game 1: Lehigh","Game 2: Colgate", 
                                        "Game 3: Northeastern", "Game 4: Richmond",
                                        "Game 5: St. John's", "Game 6: Bryant", "Game 7: Illinois",
                                        "Game 8: Notre Dame", "Game 9: Oakland","Game 10: Georgetown",
                                        "Game 11: Monmouth", "Game 12: Cornell", "Game 13: Pittsburgh",
                                        "Game 14: Boston College", "Game 15: Louisville", "Game 16: Virginia",
                                        "Game 17: Virginia Tech", "Game 18: Notre Dame", "Game 19: Miami",
                                        "Game 20: Georgia Tech", "Game 21: North Carolina", "Game 22: Virginia Tech",
                                        "Game 23: Virginia", "Game 24: Boston College", "Game 25: Florida St", 
                                        "Game 26: NC State", "Game 27: Duke", "Game 28: Clemson",
                                        "Game 29: Pittsburgh", "Game 30: Georgia Tech", "Game 31: Wake Forest",
                                        "Game 32: Wake Forest (ACC Tournament)"),
                             selected = "Season")),
               dataTableOutput("table")
             )
           )
  ),
  tabPanel("Shot Location", selectInput(inputId = "name", label = "Player Name",
                                        choices = c("All", unique(shot_locations$shooter)),
                                        selected = "All"), plotOutput("shot_location")
  ),
  tabPanel("Game-by-Game Team Performances",
             fluidPage(
               titlePanel("Game-by-Game Team Performances"),
               sidebarLayout(
                 sidebarPanel(
                   selectInput("yvariable", label = "Statistic",
                               choices = c("ORtg", "DRtg", "eFG%","Opp_eFG%","TS%", "TO%","FG%","Opp_FG%", "3P%", "Opp_3P%","FT%", "PTS", "Opp_PTS", "AST", "AST%","Opp_AST", "OREB%", "DREB%", "REB%","STL", "STL%","Opp_STL", "BLK", "BLK%","Opp_BLK"),
                               selected = "ORtg")),
                 plotOutput("linePlot")
               ))),
  tabPanel("Game-by-Game Player Performances",
           fluidPage(
             titlePanel("Game-by-Game Player Performances"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("Name", label = "Select a Player",
                             choices = c("GIRARD III,JOSEPH", "MINTZ,JUDAH","BELL,CHRIS","WILLIAMS,BENNY",
                                         "EDWARDS,JESSE", "AJAK,JOHN BOL", "BROWN,MALIQ", "CAREY,PETER",
                                         "CLAYTON,ANTHONY", "COPELAND,QUADIR", "FELDMAN,SHANE", "HIMA,MOUNIR", 
                                         "KEATING,STEPHEN","RUFFIN,NIKO", "TAYLOR,JUSTIN", "TORRENCE,SYMIR"),
                             selected = "GIRARD III,JOSEPH"),
                 selectInput("yvariable1", label = "Statistic",
                             choices = c("Game Score", "USG%","eFG%","TS%", "TO%","FG%", "3P%", "FT%", "PTS", "Points per 30","AST", "AST%","Assists per 30","OREB", "OREB%","DREB", "DREB%","REB","REB%","Rebounds per 30","STL", "STL%","BLK","BLK%"),
                             selected = "Game Score")),
               plotOutput("linePlot1")
             ))),
  tabPanel("5 Player Combos",
  fluidPage(
  titlePanel("On Court Production"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("variables", "Select Up to 5 Players:", 
              choices = c("GIRARD III,JOSEPH", "MINTZ,JUDAH","BELL,CHRIS","WILLIAMS,BENNY",
                          "EDWARDS,JESSE", "AJAK,JOHN BOL", "BROWN,MALIQ", "CAREY,PETER",
                          "CLAYTON,ANTHONY", "COPELAND,QUADIR", "FELDMAN,SHANE", "HIMA,MOUNIR", 
                          "KEATING,STEPHEN","RUFFIN,NIKO", "TAYLOR,JUSTIN", "TORRENCE,SYMIR"), 
              selected = c("GIRARD III,JOSEPH", "MINTZ,JUDAH","BELL,CHRIS","WILLIAMS,BENNY",
                           "EDWARDS,JESSE")),
      
      checkboxGroupInput("variables1","Select Stats:",
                         choices = c("Possessions","ORtg","DRtg","Pts", "Pts (in the paint)","FG%","FG% (in the paint)",
                           "3P%","FT%","eFG%","AST%","OREB%",
                           "DREB%","TO%","STL%","BLK%","Points Against",
                           "Points Against (in the paint)","OPP FG%","Opp FG% (in the paint)",
                           "Opp 3P%","Opp FT%","Opp eFG%","Opp AST%",
                           "Opp TO%","Opp STL%","Opp BLK%"),
                         selected = c("ORtg","DRtg","eFG%", "Opp eFG%", "3P%","Opp 3P%","Possessions")),
      
    ),
    mainPanel(
      dataTableOutput("filteredTable"))
  , position = "right")
)
),
tabPanel("On-Off Comparisons",
         fluidPage(
           titlePanel("Team Performance when Player is on the Court (1) Vs. When Player is on the Bench (0)"),
           sidebarLayout(
             sidebarPanel(
               selectInput("player_name", label = "Player: ",
                           choices = c("GIRARD III,JOSEPH", "MINTZ,JUDAH","BELL,CHRIS","WILLIAMS,BENNY",
                                       "EDWARDS,JESSE", "AJAK,JOHN BOL", "BROWN,MALIQ", "CAREY,PETER",
                                       "CLAYTON,ANTHONY", "COPELAND,QUADIR", "FELDMAN,SHANE", "HIMA,MOUNIR", 
                                       "KEATING,STEPHEN","RUFFIN,NIKO", "TAYLOR,JUSTIN", "TORRENCE,SYMIR"),
                           selected = "GIRARD III,JOSEPH"),
               checkboxGroupInput("variables2","Select Stats:",
                                  choices = c("Possessions","ORtg","DRtg","Pts", "Pts (in the paint)","FG%","FG% (in the paint)",
                                              "3P%","FT%","eFG%","AST%","OREB%",
                                              "DREB%","TO%","STL%","BLK%","Points Against",
                                              "Points Against (in the paint)","OPP FG%","Opp FG% (in the paint)",
                                              "Opp 3P%","Opp FT%","Opp eFG%","Opp AST%",
                                              "Opp TO%","Opp STL%","Opp BLK%"),
                                  selected = c("ORtg","DRtg","eFG%", "Opp eFG%", "3P%","Opp 3P%","Possessions")),
               ),
               mainPanel(dataTableOutput("table1"))
             
           , position = "right")
         )
  
)
)


# Define server logic
server <- function(input, output) {
  
  filteredData <- reactive({
    selectedVars <- input$variables
    selectedVars1 <- input$variables1
    
    # Filter data based on selected variables
    if (length(selectedVars) > 0) {
      filtered_data <- data %>%
        #filters to only include rows where selectedVars == 1
        filter(rowSums(select(data, all_of(selectedVars))) == length(selectedVars)) %>% 
        #based on how many players are chosen, it will combine all combinations into one row
        #for instance, if the user only chooses Girard, Mintz, and Edwards, it will take all combinations where that occurs
        #in the dataframe and put the results in one row
        summarize(
          "Possessions" = round(0.5*(sum(fg_attempts,na.rm = T) + 0.475*sum(free_throw_attempt,na.rm = T) - sum(offensive_rebounds,na.rm = T) + sum(turnovers,na.rm = T)) + 0.5*(sum(opp_fg_attempts,na.rm = T) + 0.475*sum(opp_free_throw_attempt,na.rm = T) - sum(opp_offensive_rebounds,na.rm = T) + sum(opp_turnovers,na.rm = T)),0),
          "ORtg" = round((sum(pts,na.rm = T)/round(0.5*(sum(fg_attempts,na.rm = T) + 0.475*sum(free_throw_attempt,na.rm = T) - sum(offensive_rebounds,na.rm = T) + sum(turnovers,na.rm = T)) + 0.5*(sum(opp_fg_attempts,na.rm = T) + 0.475*sum(opp_free_throw_attempt,na.rm = T) - sum(opp_offensive_rebounds,na.rm = T) + sum(opp_turnovers,na.rm = T)),0))*100,1),
          "DRtg" = round((sum(pts_against,na.rm = T)/round(0.5*(sum(fg_attempts,na.rm = T) + 0.475*sum(free_throw_attempt,na.rm = T) - sum(offensive_rebounds,na.rm = T) + sum(turnovers,na.rm = T)) + 0.5*(sum(opp_fg_attempts,na.rm = T) + 0.475*sum(opp_free_throw_attempt,na.rm = T) - sum(opp_offensive_rebounds,na.rm = T) + sum(opp_turnovers,na.rm = T)),0))*100,1),
          "Pts" = sum(pts, na.rm = T),
          "Pts (in the paint)" = sum(pts_in_the_paint, na.rm = T),
          "FG%" = ifelse(sum(fg_attempts,na.rm = T) > 0,round(sum(fg_made, na.rm = T)/sum(fg_attempts,na.rm = T),3)*100,NA),
          "FG% (in the paint)" = ifelse(sum(in_the_paint_fg_attempts, na.rm = T) > 0,round(sum(in_the_paint_fg_made)/sum(in_the_paint_fg_attempts),3)*100,NA),
          "3P%" = ifelse(sum(three_pt_attempts,na.rm = T) > 0,round(sum(three_pt_made, na.rm = T)/sum(three_pt_attempts,na.rm = T),3)*100,NA),
          "FT%" = ifelse(sum(free_throw_attempt,na.rm = T) > 0,round(sum(made_free_throw,na.rm = T)/sum(free_throw_attempt,na.rm = T),3)*100,NA),
          "eFG%" = ifelse(sum(fg_attempts,na.rm = T) > 0,round((sum(two_pt_made, na.rm = T)+0.5*sum(three_pt_made,na.rm = T))/sum(fg_attempts,na.rm = T),3)*100,NA),
          "AST%" = round(sum(assists, na.rm = T)/sum(fg_attempts,na.rm = T),3)*100,
          "OREB%" = round(sum(offensive_rebounds, na.rm = T)/sum(offensive_rebounds,opp_defensive_rebounds,na.rm = T),3)*100,
          "DREB%" = round(sum(defensive_rebounds., na.rm = T)/sum(defensive_rebounds.,opp_offensive_rebounds,na.rm = T),3)*100,
          "TO%" = round(sum(turnovers,na.rm = T)/(sum(fg_attempts,na.rm = T) + 0.475 * sum(free_throw_attempt,na.rm = T) + sum(turnovers,na.rm = T)),3)*100,
          "STL%" = round(sum(steals,na.rm = T)/(0.5*(sum(fg_attempts,na.rm = T) + 0.475*sum(free_throw_attempt,na.rm = T) - sum(offensive_rebounds,na.rm = T) + sum(turnovers,na.rm = T)) + 0.5*(sum(opp_fg_attempts,na.rm = T) + 0.475*sum(opp_free_throw_attempt,na.rm = T) - sum(opp_offensive_rebounds,na.rm = T) + sum(opp_turnovers,na.rm = T))),3)*100,
          "BLK%" =  round(sum(block,na.rm = T)/(sum(opp_fg_attempts,na.rm = T) - sum(opp_three_pt_attempts,na.rm = T)),3)*100,
          "Points Against" = sum(pts_against, na.rm = T),
          "Points Against (in the paint)" = sum(opp_pts_in_the_paint, na.rm = T),
          "OPP FG%" = ifelse(sum(opp_fg_attempts,na.rm = T) > 0,round(sum(opp_fg_made,na.rm = T)/sum(opp_fg_attempts,na.rm = T),3)*100,NA),
          "Opp FG% (in the paint)" = ifelse(sum(opp_in_the_paint_fg_attempts,na.rm = T) > 0,round(sum(opp_in_the_paint_fg_made,na.rm = T)/sum(opp_in_the_paint_fg_attempts,na.rm = T),3)*100,NA),
          "Opp 3P%" = ifelse(sum(opp_three_pt_attempts,na.rm = T) > 0,round(sum(opp_three_pt_made,na.rm = T)/sum(opp_three_pt_attempts,na.rm = T),3)*100,NA),
          "Opp FT%" = ifelse(sum(opp_free_throw_attempt,na.rm = T) > 0,round(sum(opp_made_free_throw,na.rm = T)/sum(opp_free_throw_attempt,na.rm = T),3)*100,NA),
          "Opp eFG%" = ifelse(sum(opp_fg_attempts,na.rm = T) > 0,round((sum(opp_two_pt_made,na.rm = T) + 0.5*sum(opp_three_pt_made,na.rm = T))/sum(opp_fg_attempts,na.rm = T),3)*100,NA),
          "Opp AST%" = round(sum(opp_assists, na.rm = T)/sum(opp_fg_attempts,na.rm = T),3)*100,
          "Opp TO%" = round(sum(opp_turnovers,na.rm = T)/(sum(opp_fg_attempts,na.rm = T) + 0.475 * sum(opp_free_throw_attempt,na.rm = T) + sum(opp_turnovers,na.rm = T)),3)*100,
          "Opp STL%" = round(sum(opp_steals,na.rm = T)/(0.5*(sum(opp_fg_attempts,na.rm = T) + 0.475*sum(opp_free_throw_attempt,na.rm = T) - sum(opp_offensive_rebounds,na.rm = T) + sum(opp_turnovers,na.rm = T)) + 0.5*(sum(fg_attempts,na.rm = T) + 0.475*sum(free_throw_attempt,na.rm = T) - sum(offensive_rebounds,na.rm = T) + sum(turnovers,na.rm = T))),3)*100,
          "Opp BLK%" = round(sum(opp_blocks,na.rm = T)/(sum(fg_attempts,na.rm = T) - sum(three_pt_attempts,na.rm = T)),3)*100
      ) %>% 
        select(all_of(selectedVars1)) 
        
    } else {
      #if no players are selected, it will just return the original dataframe
      filtered_data <- data
    }
    
    return(filtered_data)
  })
 
  output$filteredTable <- renderDataTable({
    datatable(filteredData(),
              options = list(
                scrollX = TRUE
              ),
              height = 30
  )})
  filteredData2 <- reactive({
    selectedPlayer <- switch(
      input$player_name,
      "GIRARD III,JOSEPH" = "GIRARD III,JOSEPH",
      "MINTZ,JUDAH" = "MINTZ,JUDAH",
      "BELL,CHRIS" = "BELL,CHRIS",
      "WILLIAMS,BENNY" = "WILLIAMS,BENNY",
      "EDWARDS,JESSE" = "EDWARDS,JESSE",
      "AJAK,JOHN BOL" = "AJAK,JOHN BOL",
      "BROWN,MALIQ" = "BROWN,MALIQ",
      "CAREY,PETER" = "CAREY,PETER",
      "CLAYTON,ANTHONY" = "CLAYTON,ANTHONY",
      "COPELAND,QUADIR" = "COPELAND,QUADIR",
      "FELDMAN,SHANE" = "FELDMAN,SHANE",
      "HIMA,MOUNIR" = "HIMA,MOUNIR",
      "KEATING,STEPHEN" = "KEATING,STEPHEN",
      "RUFFIN,NIKO" = "RUFFIN,NIKO",
      "TAYLOR,JUSTIN" = "TAYLOR,JUSTIN",
      "TORRENCE,SYMIR"
    )
    selectedVars2 <- input$variables2
    #groups by the player selected and then outputs production when he was on and off the court
    filteredData2 <- data %>% group_by(.data[[selectedPlayer]]) %>% summarize(
      "Possessions" = round(0.5*(sum(fg_attempts,na.rm = T) + 0.475*sum(free_throw_attempt,na.rm = T) - sum(offensive_rebounds,na.rm = T) + sum(turnovers,na.rm = T)) + 0.5*(sum(opp_fg_attempts,na.rm = T) + 0.475*sum(opp_free_throw_attempt,na.rm = T) - sum(opp_offensive_rebounds,na.rm = T) + sum(opp_turnovers,na.rm = T)),0),
      "ORtg" = round((sum(pts,na.rm = T)/round(0.5*(sum(fg_attempts,na.rm = T) + 0.475*sum(free_throw_attempt,na.rm = T) - sum(offensive_rebounds,na.rm = T) + sum(turnovers,na.rm = T)) + 0.5*(sum(opp_fg_attempts,na.rm = T) + 0.475*sum(opp_free_throw_attempt,na.rm = T) - sum(opp_offensive_rebounds,na.rm = T) + sum(opp_turnovers,na.rm = T)),0))*100,1),
      "DRtg" = round((sum(pts_against,na.rm = T)/round(0.5*(sum(fg_attempts,na.rm = T) + 0.475*sum(free_throw_attempt,na.rm = T) - sum(offensive_rebounds,na.rm = T) + sum(turnovers,na.rm = T)) + 0.5*(sum(opp_fg_attempts,na.rm = T) + 0.475*sum(opp_free_throw_attempt,na.rm = T) - sum(opp_offensive_rebounds,na.rm = T) + sum(opp_turnovers,na.rm = T)),0))*100,1),
      "Pts" = sum(pts, na.rm = T),
      "Pts (in the paint)" = sum(pts_in_the_paint, na.rm = T),
      "FG%" = ifelse(sum(fg_attempts,na.rm = T) > 0,round(sum(fg_made, na.rm = T)/sum(fg_attempts,na.rm = T),3)*100,NA),
      "FG% (in the paint)" = ifelse(sum(in_the_paint_fg_attempts, na.rm = T) > 0,round(sum(in_the_paint_fg_made)/sum(in_the_paint_fg_attempts),3)*100,NA),
      "3P%" = ifelse(sum(three_pt_attempts,na.rm = T) > 0,round(sum(three_pt_made, na.rm = T)/sum(three_pt_attempts,na.rm = T),3)*100,NA),
      "FT%" = ifelse(sum(free_throw_attempt,na.rm = T) > 0,round(sum(made_free_throw,na.rm = T)/sum(free_throw_attempt,na.rm = T),3)*100,NA),
      "eFG%" = ifelse(sum(fg_attempts,na.rm = T) > 0,round((sum(two_pt_made, na.rm = T)+0.5*sum(three_pt_made,na.rm = T))/sum(fg_attempts,na.rm = T),3)*100,NA),
      "AST%" = round(sum(assists, na.rm = T)/sum(fg_attempts,na.rm = T),3)*100,
      "OREB%" = round(sum(offensive_rebounds, na.rm = T)/sum(offensive_rebounds,opp_defensive_rebounds,na.rm = T),3)*100,
      "DREB%" = round(sum(defensive_rebounds., na.rm = T)/sum(defensive_rebounds.,opp_offensive_rebounds,na.rm = T),3)*100,
      "TO%" = round(sum(turnovers,na.rm = T)/(sum(fg_attempts,na.rm = T) + 0.475 * sum(free_throw_attempt,na.rm = T) + sum(turnovers,na.rm = T)),3)*100,
      "STL%" = round(sum(steals,na.rm = T)/(0.5*(sum(fg_attempts,na.rm = T) + 0.475*sum(free_throw_attempt,na.rm = T) - sum(offensive_rebounds,na.rm = T) + sum(turnovers,na.rm = T)) + 0.5*(sum(opp_fg_attempts,na.rm = T) + 0.475*sum(opp_free_throw_attempt,na.rm = T) - sum(opp_offensive_rebounds,na.rm = T) + sum(opp_turnovers,na.rm = T))),3)*100,
      "BLK%" =  round(sum(block,na.rm = T)/(sum(opp_fg_attempts,na.rm = T) - sum(opp_three_pt_attempts,na.rm = T)),3)*100,
      "Points Against" = sum(pts_against, na.rm = T),
      "Points Against (in the paint)" = sum(opp_pts_in_the_paint, na.rm = T),
      "OPP FG%" = ifelse(sum(opp_fg_attempts,na.rm = T) > 0,round(sum(opp_fg_made,na.rm = T)/sum(opp_fg_attempts,na.rm = T),3)*100,NA),
      "Opp FG% (in the paint)" = ifelse(sum(opp_in_the_paint_fg_attempts,na.rm = T) > 0,round(sum(opp_in_the_paint_fg_made,na.rm = T)/sum(opp_in_the_paint_fg_attempts,na.rm = T),3)*100,NA),
      "Opp 3P%" = ifelse(sum(opp_three_pt_attempts,na.rm = T) > 0,round(sum(opp_three_pt_made,na.rm = T)/sum(opp_three_pt_attempts,na.rm = T),3)*100,NA),
      "Opp FT%" = ifelse(sum(opp_free_throw_attempt,na.rm = T) > 0,round(sum(opp_made_free_throw,na.rm = T)/sum(opp_free_throw_attempt,na.rm = T),3)*100,NA),
      "Opp eFG%" = ifelse(sum(opp_fg_attempts,na.rm = T) > 0,round((sum(opp_two_pt_made,na.rm = T) + 0.5*sum(opp_three_pt_made,na.rm = T))/sum(opp_fg_attempts,na.rm = T),3)*100,NA),
      "Opp AST%" = round(sum(opp_assists, na.rm = T)/sum(opp_fg_attempts,na.rm = T),3)*100,
      "Opp TO%" = round(sum(opp_turnovers,na.rm = T)/(sum(opp_fg_attempts,na.rm = T) + 0.475 * sum(opp_free_throw_attempt,na.rm = T) + sum(opp_turnovers,na.rm = T)),3)*100,
      "Opp STL%" = round(sum(opp_steals,na.rm = T)/(0.5*(sum(opp_fg_attempts,na.rm = T) + 0.475*sum(opp_free_throw_attempt,na.rm = T) - sum(opp_offensive_rebounds,na.rm = T) + sum(opp_turnovers,na.rm = T)) + 0.5*(sum(fg_attempts,na.rm = T) + 0.475*sum(free_throw_attempt,na.rm = T) - sum(offensive_rebounds,na.rm = T) + sum(turnovers,na.rm = T))),3)*100,
      "Opp BLK%" = round(sum(opp_blocks,na.rm = T)/(sum(fg_attempts,na.rm = T) - sum(three_pt_attempts,na.rm = T)),3)*100
    ) %>% 
      select(.data[[selectedPlayer]],all_of(selectedVars2)) %>% 
      arrange(desc(.data[[selectedPlayer]]))
  })
  output$table1 <- renderDataTable({
    datatable(filteredData2(), options = list(scrollX = TRUE), height = 30)
  })
  # Creates a line plot that showcases the team's performance throughout the season
  output$linePlot <- renderPlot({
    selected_variable <- switch(
      input$yvariable,
      "eFG%" = "eFG%",
      "Opp_eFG%" = "Opp_eFG%",
      "ORtg" = "ORtg",
      "DRtg" = "DRtg",
      "TS%" = "TS%",
      "TO%" = "TO%",
      "FG%" = "FG%",
      "Opp_FG%" = "Opp_FG%",
      "3P%" = "3P%",
      "Opp_3P%" = "Opp_3P%",
      "FT%" = "FT%",
      "PTS" = "PTS",
      "Opp_PTS" = "Opp_PTS",
      "AST" = "AST",
      "AST%" = "AST%",
      "Opp_AST" = "Opp_AST",
      "OREB%" = "OREB%",
      "DREB%" = "DREB%",
      "REB%" = "REB%",
      "STL" = "STL",
      "STL%" = "STL%",
      "Opp_STL" = "Opp_STL",
      "BLK" = "BLK",
      "BLK%" = "BLK%",
      "Opp_BLK" = "Opp_BLK"
    )
    
    if (selected_variable == "ORtg" | selected_variable == "DRtg") {
      syracuse_box_scores %>% filter(Player_Name == "team") %>% filter(game_number != "Season") %>% 
        ggplot(aes(as.numeric(game_number), .data[[selected_variable]])) +
        geom_line(color = "#F76900", size = 2) +
        xlab("Game Number") +
        ylab(input$yvariable) +
        geom_hline(yintercept = 100, linetype = "dashed", color = "black") +
        theme_bw()
    } else {
      syracuse_box_scores %>% filter(Player_Name == "team") %>%  filter(game_number != "Season") %>% 
        ggplot(aes(as.numeric(game_number), .data[[selected_variable]])) +
        geom_line(color = "#F76900", size = 2) +
        xlab("Game Number") +
        ylab(input$yvariable) +
        theme_bw()
    }
  })
  #outputs box scores
  output$table <- renderDataTable({
    selectedBoxscore <- switch(
      input$gamenumber,
      "Game 1: Lehigh" = 1,
      "Game 2: Colgate" = 2,
      "Game 3: Northeastern" = 3,
      "Game 4: Richmond" = 4,
      "Game 5: St. John's" = 5,
      "Game 6: Bryant" = 6,
      "Game 7: Illinois" = 7,
      "Game 8: Notre Dame" = 8,
      "Game 9: Oakland" = 9,
      "Game 10: Georgetown" = 10,
      "Game 11: Monmouth" = 11,
      "Game 12: Cornell" = 12,
      "Game 13: Pittsburgh" = 13,
      "Game 14: Boston College" = 14,
      "Game 15: Louisville" = 15,
      "Game 16: Virginia" = 16,
      "Game 17: Virginia Tech" = 17,
      "Game 18: Notre Dame" = 18,
      "Game 19: Miami" = 19,
      "Game 20: Georgia Tech" = 20,
      "Game 21: North Carolina" = 21,
      "Game 22: Virginia Tech" = 22,
      "Game 23: Virginia" = 23,
      "Game 24: Boston College" = 24,
      "Game 25: Florida St" = 25,
      "Game 26: NC State" = 26,
      "Game 27: Duke" = 27,
      "Game 28: Clemson" = 28,
      "Game 29: Pittsburgh" = 29,
      "Game 30: Georgia Tech" = 30,
      "Game 31: Wake Forest" = 31,
      "Game 32: Wake Forest (ACC Tournament)" = 32,
      "Season" = "Season"
    )
    #if the user selects "Season", the table will show per game stats (for count stats), otherwise, it will show stats from the game selected
    if(selectedBoxscore == "Season"){
      datatable(syracuse_box_scores %>% filter(game_number == selectedBoxscore & Player_Name != "team") %>% 
                  summarize(Player_Name,"MIN Per Game" = round(MIN/32,1), "FG Per Game" = round(FG/32,1), "FGA Per Game" = round(FGA/32,1), `FG%`,"3PT Per Game" = round(`3PT`/32,1), "3PA Per Game" = round(`3PA`/32,1), `3P%`, 
                      "FT Per Game"  = round(FT/32,1),"FTA Per Game" = round(FTA/32,1), `FT%`, `eFG%`, `TS%`,"PTS Per Game" = round(PTS/32,1),"AST Per Game" = round(AST/32,1),`AST%`,"OREB Per Game" = round(OREB/32,1),`OREB%`,"DREB Per Game" = round(DREB/32,1),`DREB%`,"REB Per Game" = round(REB/32,1), `REB%`,"STL Per Game" = round(STL/32,1), `STL%`,"BLK Per Game" = round(BLK/32,1),`BLK%`,"TO Per Game" = round(TO/32,1),`TO%`,"PF Per Game" = round(PF/32,1),
                      `USG%`,"Points Per 30" = pts_per_30, "Assists Per 30" = ast_per_30,"Rebounds Per 30" = reb_per_30) %>% 
                  arrange(Player_Name), 
                options = list(scrollX = TRUE))
    }else{
    datatable(syracuse_box_scores %>% filter(game_number == selectedBoxscore) %>% 
      select(Player_Name, Game_Score,MIN, FG, FGA, `FG%`,`3PT`, `3PA`, `3P%`, 
             FT, FTA, `FT%`, `eFG%`, `TS%`, PTS, AST,`AST%`,OREB,`OREB%` ,DREB,`DREB%`,REB, `REB%`,STL, `STL%`,BLK,`BLK%`, TO,`TO%`, PF,`USG%`,ORtg,DRtg,"Points Per 30" = pts_per_30, "Assists Per 30" = ast_per_30,"Rebounds Per 30" = reb_per_30) %>% 
      arrange(desc(MIN)), 
      options = list(scrollX = TRUE),height = 30)
    }
  })
  #creates a lineplot to showcase each player's performance throughout the season
  output$linePlot1 <- renderPlot({
    selectedPlayer1 <- switch(
      input$Name,
      "GIRARD III,JOSEPH" = "J. Girard III G",
      "MINTZ,JUDAH" = "J. Mintz G",
      "BELL,CHRIS" = "C. Bell F",
      "WILLIAMS,BENNY" = "B. Williams F",
      "EDWARDS,JESSE" = "J. Edwards C",
      "AJAK,JOHN BOL" = "J. Ajak F",
      "BROWN,MALIQ" = "M. Brown F",
      "CAREY,PETER" = "P. Carey C",
      "CLAYTON,ANTHONY" = "A. Clayton G",
      "COPELAND,QUADIR" = "Q. Copeland G",
      "FELDMAN,SHANE" = "S. Feldman G",
      "HIMA,MOUNIR" = "M. Hima C",
      "KEATING,STEPHEN" = "S. Keating F",
      "RUFFIN,NIKO" = "N. Ruffin G",
      "TAYLOR,JUSTIN" = "J. Taylor G",
      "TORRENCE,SYMIR" = "S. Torrence G"
    )
    selected_variable <- switch(
      input$yvariable1,
      "Game Score" = "Game_Score",
      "USG%" = "USG%",
      "eFG%" = "eFG%",
      "TS%" = "TS%",
      "TO%" = "TO%",
      "FG%" = "FG%",
      "3P%" = "3P%",
      "FT%" = "FT%",
      "PTS" = "PTS",
      "Points per 30" = "pts_per_30",
      "AST" = "AST",
      "AST%" = "AST%",
      "Assists per 30" = "ast_per_30",
      "OREB" = "OREB",
      "OREB%" = "OREB%",
      "DREB" = "DREB",
      "DREB%" = "DREB%",
      "REB" = "REB",
      "REB%" = "REB%",
      "Rebounds per 30" = "reb_per_30",
      "STL" = "STL",
      "STL%" = "STL%",
      "BLK" = "BLK",
      "BLK%" = "BLK%"
    )
    if (selected_variable == "Game_Score") {
      #if the user selects "Game Score", then it will place a dashed line where y=10 since 10 to represent the average game score
      syracuse_box_scores %>% filter(Player_Name == selectedPlayer1) %>% filter(game_number != "Season") %>% 
        ggplot(aes(as.numeric(game_number), .data[[selected_variable]])) +
        geom_line(color = "#F76900", size = 2) +
        xlab("Game Number") +
        ylab(input$yvariable1) +
        geom_hline(yintercept = 10, linetype = "dashed", color = "black") +
        theme_bw()
    } else{ syracuse_box_scores %>% filter(Player_Name == selectedPlayer1) %>% filter(game_number != "Season") %>% 
        ggplot(aes(as.numeric(game_number), .data[[selected_variable]])) +
        geom_line(color = "#F76900", size = 2) +
        xlab("Game Number") +
        ylab(input$yvariable1) +
        theme_bw()
      }
  })
  #creates the shot plot
  output$shot_location <- renderPlot({
    if(input$name == "All"){
      ggplot(shot_locations, aes(loc_x, loc_y, shape = three_pt, color = shot_outcome)) +
        #pastes the court image on the graph
        annotation_custom(court, 0, 50, 2, 45) +
        geom_point(size = 2) + ggtitle("All Player's Shot Locations") +
        ylim(2,45) + xlim(0, 50) + xlab("") + ylab("") +
        theme_bw() +
        scale_color_manual(values = c("red", "green"), limits = c('missed', 'made'))  +
        labs(color="Shot Outcome", shape="Three Point Shot")
      
    }else{
      player_shots <- shot_locations %>% filter(shooter == input$name)
      ggplot(player_shots, aes(loc_x, loc_y, color = shot_outcome, shape = three_pt)) +
        annotation_custom(court, 0, 50, 2, 45) +
        geom_point(size = 2) + ggtitle(glue('{input$name} Shot Locations')) + xlim(0, 50) +
        ylim(2, 45) + xlab("") + ylab("") +
        theme_bw() +
        scale_color_manual(values = c("red", "green"), limits = c('missed', 'made')) +
        labs(color="Shot Outcome", shape="Three Point Shot")
    }
  }, height = 800, width = 1000)
}



# Run the application
shinyApp(ui = ui, server = server)

