library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)
library(plotly)

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
          p("NBA players are prone to injury due to the sport's high pace, quick movement, and physical nature. This study aims to identify factors that correlate to injury proneness in the NBA, so that NBA players and sports trainers can develop effective treatment strategies to prevent injuries."),
          p("By gathering data about the factors that correlate to injury proneness, it can help prevent injuries in the NBA."),
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
      p("According to a recent study on injury patterns in the modern NBA, there has been a rise in lost games due to unusual ailments that were not as frequent in the past. In the early days of the NBA, players saw injuries as minor setbacks or temporary challenges that they could readily overcome. However, as time passed, both the severity of the injuries and the intensity of the game rose. As you can see here, minus the unsual spike of injuries in 2012, there is a clear pattern of the amount of injuries increasing overtime"),
      
      plotlyOutput("injuryOverTimePlot"),
      plotlyOutput("injuryChangeOverTimePlot"),
      plotlyOutput("restPlot")
    )
  ),
  
  tabPanel(
    title = "Conclusion",
    h2("Conclusion"),
    p("Basketball injuries are a critical aspect of the sport. By exploring injury patterns and understanding the types of injuries players experience, we can enhance player safety and develop preventive measures. This application provides a starting point to delve into the world of basketball injuries.")
  )
)


#Define server    
server <- function(input, output){
  output$injuryPlot <-  renderPlotly({
    plot_data <- df
    if (input$positionInput != "All") {
      plot_data <- filter(plot_data, plot_data$Pos == input$positionInput)
    }
    if (input$injuryInput != "All") {
      plot_data <- filter(plot_data, str_detect(plot_data$Notes, input$injuryInput))
    }
    totals <- nrow(plot_data)
    p <- ggplot(data=plot_data, aes(x = MPG, y = Age))+
      geom_point(aes(col=Pos))+
      ggtitle(paste("Total Injuries: ", totals))+
      labs(y = "Age of Player", x = "Minutes Per Game", text = "injuries")
    ggplotly(p)
  })
  
  output$injuryOverTimePlot <- renderPlotly({
    p <- ggplot(df_year, aes(x = Year, y = total)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(x = "Year", y = "Injury Count") +
      geom_hline(yintercept=mean(df_year$total), color = "blue")+
      scale_x_continuous(labels=as.character(df_year$Year),breaks=df_year$Year)
      
    # Convert the ggplot to a plotly object for interactivity
    ggplotly(p)
  })
  output$injuryChangeOverTimePlot <- renderPlotly({
    p <- ggplot(df_year, aes(x = Year, y = total)) +
      geom_line(color = "steelblue") +
      geom_point(color = "steelblue", size = 3) +
      labs(x = "Season", y = "Injury Count") +
      theme_minimal()
    
    # Convert the ggplot to a plotly object for interactivity
    ggplotly(p)
  })
  
}


#Run the app
shinyApp(ui, server)