#in progress working in r studio, will update here
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
  navbarPage(
    title = "NBA Data Story App",
    tabPanel("Introduction", 
             fluidRow(
               column(12, align = "center",
                      h2("Introduction to the NBA Data Story App"),
                      p("This app provides insights and visualizations based on NBA data."),
                      p("Explore player statistics and injury trends to uncover interesting patterns.")
               ),
               column(12,
                      verbatimTextOutput("readmeContent")
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
  
  # Render readme content
  output$readmeContent <- renderPrint({
    readme_content
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
