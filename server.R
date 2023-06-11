library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)
library(plotly)
library(rsconnect)

source("final.R") #loads in your analysis file
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
      labs(x = "Year", y = "Injury Count") +
      theme_minimal()
    
    # Convert the ggplot to a plotly object for interactivity
    ggplotly(p)
  })
  output$AgePlot <- renderPlotly({
    p <- ggplot(df, aes(x = Age, y = TOT_INJ)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(x = "Age", y = "Injury Count") +
      scale_x_continuous(labels=as.character(df$Age),breaks=df$Age)
    
    ggplotly(p)
  })
  
  output$AgePlot2 <- renderPlotly({
    p <- ggplot(df_age, aes(x = Age, y = mean_inj)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs( y = "Mean Injury Count", x = "Age") +
      geom_hline(yintercept=mean(df_age$mean_inj), color = "blue")+
      scale_x_continuous(labels=as.character(df_age$Age),breaks=df_age$Age)
    
    ggplotly(p)
  })
  output$PositionPlot <- renderPlotly({
    p <- ggplot(df_position, aes(x = Pos, y = mean_inj)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs( y = "Mean Injury Count", x = "Position")+
      geom_hline(yintercept=mean(df_position$mean_inj), color = "blue")
    
    ggplotly(p)
  })
  output$PositionsPlot <-  renderPlotly({
    df_plot <- df
    if (input$InjuriesInput != "All") {
      df_plot <- filter(df_plot, str_detect(df_plot$Notes, input$InjuriesInput))
    }
    
    df_plot <- group_by(df_plot, Pos)
    df_plot <- summarize(df_plot, mean_inj = mean(INJ_IN_YR))
    
    p <- ggplot(df_plot, aes(x = Pos, y = mean_inj)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs( y = "Mean Injury Count", x = "Position")+
      geom_hline(yintercept=mean(df_plot$mean_inj), color = "blue")+
      theme_minimal()
    
    ggplotly(p)
    
  })
  
  output$MPGPlot <- renderPlotly({
    p <- ggplot(df_mpg, aes(x = GROUP, y = tot_inj, group = 3)) +
      geom_line(color = "steelblue") +
      geom_point(color = "steelblue", size = 3) +
      labs(x = "MPG", y = "Injury Count") +
      theme_minimal()
    
    # Convert the ggplot to a plotly object for interactivity
    ggplotly(p)
  })
  
  output$PPGPlot <- renderPlotly({
    p <- ggplot(df_pts, aes(x=factor(GROUP, level=c("0-4 PPG", "5-9 PPG", "10-14 PPG", "15+ PPG")), y=tot_inj, group = 3)) +
      geom_line(color = "steelblue") +
      geom_point(color = "steelblue", size = 3) +
      labs(x = "PPG", y = "Injury Count") +
      theme_minimal()
    
    # Convert the ggplot to a plotly object for interactivity
    ggplotly(p)
  })
  
}

