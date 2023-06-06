library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library(shiny)

df_1 <- read.csv("injuries_2010-2020.csv") 
df_2 <- read.csv("Seasons_Stats.csv")
df_3 <- read.csv("data.csv")
readme_content <- readLines("README.md")

inj_df <- df_1
sts_df <- df_2

# Filter dataframes
sts_df <- filter(sts_df, sts_df$Year >= 2010)
inj_df <- filter(inj_df, inj_df$Date <= 2018)

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .navbar-default {
        background-color: navy !important;
      }
    "))
  ),
  navbarPage(
    title = "NBA Data Story App",
    tabPanel("Introduction", 
             fluidRow(
               column(12, align = "center",
                      h2("Introduction to the NBA Data Story App"),
                      p("This app provides insights and visualizations based on NBA data. Explore player statistics and injury trends to uncover interesting patterns."),
               ),
               column(12,
                      p("In today's NBA, we see a lot more players getting injured. Unfortunately, that's just the nature of the sport. The sport has evolved to such a high pace, quick movement, high flying, and physical game. Because of the nature of the game, it demands so much on the bodies of NBA players. And ultimately because of that, NBA players are prone and vulnerable to injury each and every time they step on the court. But the thing is NBA players come in all shapes and sizes. What I mean by that is that all NBA players have many different roles and characteristics. For example, some players play more minutes than others. Or some players are older than others. Or some players are shorter than others. Basically, there are endless categories we can place/separate NBA players in. So for our study, we want to see if we can identify or find any factors within the performance and demographic of NBA players that correlate to the amount and type of injuries in the NBA."),
                      p("The strong, central, unifying theme is trying to have a better understanding of the factors that can correlate to injury proneness in the NBA. By having a better understanding of these factors, we can have a better idea on what makes NBA players vulnerable to certain injuries. Thus, NBA players and sports trainers can take the necessary steps to develop important, effective, and beneficial treatment strategies to ultimately help prevent injuries. In short, by gathering data about the factors that correlate to injury proneness in the NBA, it can ultimately help prevent injuries in the NBA."),
                      p("There are many different angles and ways by going about finding factors that can correlate to injury proneness in the NBA. Since there are endless types of stats and information about NBA players, we can categorize NBA players through either their physical attributes or performance stats. Then after having these different categories, we can correlate each category to the amount of injuries or type of injury within NBA players in the category. Then we can ultimately compare each category to each other to identify which factors or categories of players are most vulnerable/prone to injury. Not only that, but we can find factors or categories of players that are most vulnerable to specific types of injuries(such as ACL injuries, hand injuries, knee injuries, ect). An element of drama in this research is the possibility of finding a category or player demographic that is significantly more prone and vulnerable to injury. If this research project did find a significant correlation, that category or demographic of players could be at risk of being cut or not drafted."),
              )
             )
    ),
    tabPanel("Player Stats",
             fluidRow(
               column(12, align = "center",
                      h2("Player Statistics"),
                      p(paste("There are", nrow(df_3), "players in this dataset."))
               )
             )
    ),
    tabPanel("Injury Stats",
             fluidRow(
               column(12, align = "center",
                      h2("Injury Statistics"),
                      p("According to a recent study that tackled injury trends 
                      in the modern NBA, there has been an increase in missed 
                      games due to unique injuries that weren't so common back 
                      in the day. In the early years of the NBA, players thought
                      of injuries as minor setbacks or transient difficulties 
                      that they could easily get beyond. These little inconveniences, 
                      such as a sprained ankle here or a pulled muscle there, were 
                      thought to test a player's fortitude. However, as the 
                      years went on, both the severity of the injuries and the 
                      game's intensity increased"),
                      plotOutput("injuryPlot")
               )
             )
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Render injury plot
  output$injuryPlot <- renderPlot({
    ggplot(df_1, aes(x = Year, fill = Team)) +
      geom_bar() +
      labs(x = "Year", y = "Number of Injuries", title = "Injury Statistics")
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
