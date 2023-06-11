library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)
library(plotly)
library(rsconnect)

source("final.R") #loads in your analysis file

#Define ui
ui <- navbarPage(
  title = "Basketball Injury Explorer",
  
  tabPanel(
    title = "Introduction",
    fluidPage(
      h2("Welcome to the Basketball Injury Explorer!"),
      tags$div(
        class = "row",
        tags$div(
          class = "col-md-8",
          p("In today's NBA, we see a lot more players getting injured. Unfortunately, that's just the nature of the sport. The sport has evolved to such a high pace, quick movement, high flying, and physical game. Because of the nature of the game, it demands so much on the bodies of NBA players. And ultimately because of that, NBA players are prone and vulnerable to injury each and every time they step on the court."),
          p("Through this study, we can have a better idea on what makes NBA players vulnerable to certain injuries. Thus, NBA players and sports trainers can take the necessary steps to develop important, effective, and beneficial treatment strategies to ultimately help prevent injuries. In short, by gathering data about the factors that correlate to injury proneness in the NBA, it can ultimately help prevent injuries in the NBA."),
          p("This data in this study is gathered from 2010-2017 via prosportstransactions.com and basketball-reference.com"),
          p("This interactive Shiny application allows you to explore basketball injuries and gain insights into the types of injuries sustained by players.")
        ),
        tags$div(
          class = "col-md-4",
          tags$img(
            src = "https://thumbs.dreamstime.com/b/basketball-players-wheelchair-vector-silhouette-illustration-isolated-basketball-players-wheelchair-vector-silhouette-190643832.jpg",  # Replace with your image URL
            alt = "Basketball Image",
            style = "max-width: 100%; height: auto;"
          )
        )
      )
    )
  ),
  
  tabPanel(
    title = "Injury Visualization",
    sidebarLayout(
      sidebarPanel(
        selectInput("positionInput", "Select Player Position:",
                    choices = c("All", unique(df$Pos))),
        selectInput("injuryInput", "Select Injury Type:",
                    choices = c("All", c("ankle", "knee", "shoulder", "calf", "rest", "concussion", "finger", "hamstring", "Achilles", "wrist", "elbow", "groin", "hip", "hand", "back"))),
      ),
      
      mainPanel(
        plotlyOutput("injuryPlot")
      )
    )
  ),
  
  tabPanel(
    title = "Change Over Time",
    fluidPage(
      h2("Change Over Time"),
      p("According to a recent study on injury patterns in the modern NBA, there has been a rise in lost games due to unusual ailments that were not as frequent in the past. In the early days of the NBA, players saw injuries as minor setbacks or temporary challenges that they could readily overcome. However, as time passed, both the severity of the injuries and the intensity of the game rose. As you can see here, minus the unsual spike of injuries in 2012, there is a clear pattern of the amount of injuries increasing overtime."),
      p("This pattern proves that there is a need for injury prevention in the modern NBA today. As injury increases overtime, so does the need to prevent these injuries. So these visualizations and graphs proves the importance and need for this study."),
      
      plotlyOutput("injuryOverTimePlot"),
      plotlyOutput("injuryChangeOverTimePlot"),
      plotlyOutput("restPlot")
    )
  ),
  
  tabPanel(
    title = "Contrast",
    fluidPage(
      h2("Contrast"),
      p("There are many different angles and ways by going about finding factors that can correlate to injury proneness in the NBA. Since there are endless types of stats and information about NBA players, we can categorize NBA players through either their physical attributes or performance stats. So in this study we will research 4 different factors:"),
      p("- Age"),
      p("- Position"),
      p("- Minutes Per Game"),
      p("- Points Per Game"),
      
      h2("Age"),
      p("We will correlate age with the total amount of injuries recorded for each age"),
      p("As you can see in this first graph as the age increases, so does the amount of injuries, at least for the beggining. But then the amount of injuries start decreasing as the age increases. These patterns and trends can show and explain many things. First, this shows that a large portion of injuries come from the ages between 23-29. But an inconsistency of this graph that needs to be acknowledged is the volume/amount of players for each age group. Since most NBA players are between the ages 23-29, then so will the amount of injuries be. Which explains the sudden incline after age 22, and a sudden decline after the age 30."),
      plotlyOutput("AgePlot"),
      p("So a better way of looking at age is by correlating age with the MEAN of the total aomount of injuries for each player recorded for each age."),
      p("In this second graph, this captures the pattern of injuries increasing as age increases better. In this graph we see that from ages 19-31, for the most part, the mean injury count stayed below the total average. But from age 32 and above, the mean injury count stayed above the total average."),
      p("One outlier is the age 19, which is unusually higher than age groups under 32. This can be explained because it is the youngest age, which means this is the age of NBA rookies and first-year players. Adjusting to the speed and physicality of the NBA can prove quite difficult for these young and new players. The physical demands and physical commitments of the NBA is very different than what they are used to. Which is why in todays modern NBA, we see rookies constantly getting injuried in their first years in the NBA. Another outlier here is the age 40, which is the lowest. This can be explained because there is a very small volume of 40 year old NBA players. And it is common that they don't even get the chance to play in games."),
      plotlyOutput("AgePlot2")
    ),
    
    h2("Position"),
    p("We will correlate the positions with the mean of the total aomount of injuries for each player recorded for each position."),
    p("As you can see, there is no significant difference when it comes to the amount of injuries for each position"),
    plotlyOutput("PositionPlot"),
    p("So a better way of looking at position is by correlating the amount of injuries for each type of injury in correlation with each position"),
    p("There are many things that can be taken in correlation with injuries in this graph. So I will only name a few here. The graph shows that guards(PG's and SG's) are more vunerable to ankle injuries. Which is to be expected since they require much more lateral, quick, ankle movement. The graph also shows that PG's are significantly more vunerable to groin injuries than any other position. The same thing can be said for centers, but for back injuries. The graph shows that centers are significantly more vunerable to back injuries more than any other injuries. Which makes sense because the center position is the tallest position in the sport."),
    sidebarPanel(
      selectInput("InjuriesInput", "Select Injury Type:",
                  choices = c("All", c("ankle", "knee", "shoulder", "calf", "rest", "concussion", "finger", "hamstring", "Achilles", "wrist", "elbow", "groin", "hip", "hand", "back"))),
    ),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    plotlyOutput("PositionsPlot"),
    
    h2("Minutes Per Game"),
    p("We will correlate the minutes played per game with the total amount of injuries. We will have 3 groups, 0-9 MPG, 10-19 MPG, and 20+ MPG"),
    p("As you can see by the graph, there is a clear upward trend of the amount of injuries increasing as MPG increases. This can be explained by many reasons, most self-explanatory. The longer a player plays, the more opportunities they have to get injuried. On top of that the longer a player plays, the more physical toll their body goes through."),
    plotlyOutput("MPGPlot"),
    
    
    h2("Points Per Game"),
    p("We will correlate the points made per game with the total amount of injuries. We will have 4 groups, 0-4 PPG, 5-9 PPG, 10-14 PPG, and 15+ PPG"),
    p("By the results of this graph, there is a significant correlation between PPG and injuries. The graph shows that there are more injuries as PPG increases. Likewise to MPG, this can be explained by with similar reasons. First, a higher PPG suggests a higher MPG too. A high PPG player also suggests that the player has the ball in his hands a lot, that he takes more shots and has more reps. This also means that this player draws attention to himself for defenders to guard him more intensely. In short, high PPG players are under a lot more physical pressures and demands because of their volume and their defenders."),
    plotlyOutput("PPGPlot"),
  ),
  
  tabPanel(
    title = "Conclusion",
    h2("Conclusion"),
    p("Basketball injuries are a critical aspect of the sport. By exploring injury patterns and understanding the types of injuries players experience, we can enhance player safety and develop preventive measures. This application provides a starting point to delvelop into the world of basketball injuries."),
    p("The Change Over Time section proved the need and significance of this study. And through the Contrast section we were able to find multiple factors of NBA players that correlates to injury in the NBA. The first one was age. We came to the conclusion that older players and rookies are the most vunerable to injuries. The second was position. Since there was no correlation in the total amount of all injuries, we had to get more specific and target certain types of injuries. So we came to the conclusion that certain positions are more vunurable to certain types of injuries. The next one was Minutes Per Game. We came to the conclusion that players that have a higher MPG are more vunerabe to injuries. In short, the more you play the more you get injuried. The last factor was Points Per Game. We came to the conclusion that players who have a higher PPG are more vunerable to injuries. In short, the more you score the more you get injuried."),
    p("We hope that this study can be used to help the demanding need of injury prevention in the NBA."),
    tags$div(
      class = "col-md-4",
      tags$img(
        src = "https://thumbs.dreamstime.com/b/basketball-players-wheelchair-vector-silhouette-illustration-isolated-basketball-players-wheelchair-vector-silhouette-190643832.jpg",  # Replace with your image URL
        alt = "Basketball Image",
        style = "max-width: 100%; height: auto;"
      )
    )
  )
)

