
rm(list = ls())
graphics.off()
#setwd("~/Uni/4. Semester/Data Visualisation/PORTFOLIO/Scripts/Tennis")


# Packages ----------------------------------------------------------------

library(showtext)
library(tidyverse)
library(ggplot2)
library(plotly)
library(data.table)
library(shiny)
library(rsconnect)
library(RColorBrewer)


# Data --------------------------------------------------------------------

load("ATP.RData")


# UI ----------------------------------------------------------------------

UI <- fluidPage(
  
  #theme = "stylesheet.css",
  tags$head(includeCSS("stylesheet.css")),
  
  titlePanel(h1("ATP Tennis Ranking", align = "center")),
  
  titlePanel(h2(HTML("Every <span style=\"color:#EC2049;\">10 years</span> Newcomers appear in the <span style=\"color:#EC2049;\">Top 20</span>"), align = "center")),
  
  fluidRow(
    column(8,
           div(plotlyOutput(outputId = "myPlot", height = "100%"), style="height: 35em;")
    ),
    column(4,
           p("This application illustrates various characteristics of the players featured in the ATP Tennis Ranking from 1983 to 2017. Due to a lack of data the ranking points and tournaments can only be examined in the period after 1995.", style="margin-top:2.5em;"),
           selectInput("x", "x-Axis:",
                       c("Ranking Points" = "Ranking Points",
                         "Tourneys Played" = "Tourneys Played",
                         "Player Age" = "Player Age"),
                       "ranking_points",
                       width = "12em"),
           checkboxInput("display_name","Display Player Names",
                         TRUE),
           sliderInput("rank_number", "Range of Rankings",
                       min = 1, max = 150, 
                       value = 20,
                       step = 1,
                       width = "80%",
                       sep = ""),
           sliderInput("week_year", "Year",
                       min = 1983, max = 2017, 
                       value = 1996,
                       step = 1,
                       animate = animationOptions(loop=FALSE, interval = 3000),
                       width = "80%",
                       sep = ""),
           align="center")
  ),
  fluidRow(
    column(12,
           HTML("<p>Source: https://datahub.io/sports-data/atp-world-tour-tennis-data"),
           align = "right")
  )
)


# Server ------------------------------------------------------------------

Server <- function(input,output){
  
  output$myPlot <- renderPlotly({if(input$display_name){plot_ly(data = filter(myData,
                                                        week_year == input$week_year,
                                                        week_month == 10,
                                                        week_day >=25,
                                                        rank_number <= input$rank_number),
                                          y = ~rank_number,
                                          x = if(input$x == "Ranking Points") {
                                            ~ranking_points} else if(input$x == "Tourneys Played") {
                                              ~tourneys_played} else {
                                                ~player_age
                                              },
                                          text = ~paste('Name: ', player_name,
                                                        '</br>Age: ', player_age,
                                                        '</br>Rank: ', rank_number,
                                                        '</br>Ranking Points: ', ranking_points,
                                                        '</br>Toruney Played: ', tourneys_played),
                                          type = "bar",
                                          mode = "markers",
                                          marker=list(
                                            color=~player_age,
                                            colorbar=list(title='Player Age'),
                                            colorscale=list(c(0, "#ccff00"), list(1, "#FF4500")),
                                            cauto = FALSE, # hier wird die colorscale gefixed
                                            cmax = 35,
                                            cmin = 16
                                          ),
                                          orientation = 'h'
                                          ) %>%
      add_annotations(text = ~player_name,
                      xref = "x",
                      yref = "y",
                      xanchor = 'left',
                      showarrow = F,
                      font = list(color = 'black',
                                  family = 'Garabd',
                                  size = 12)) %>%
      layout(paper_bgcolor=NA,plot_bgcolor=NA,
           yaxis = list(
             color = 'black',
             title = "Ranking",
             range = c(input$rank_number+0.5, 0.5),
             tick0 = 1,
             dtick = 1),
           xaxis = list(
             color = 'black',
             title = input$x),
           font = list(
             family = "Garabd",
             size = 15),
           autosize = TRUE,
           legend = list(x = 10, y = 10),
           showlegend = FALSE
      )
  } else {
    plot_ly(data = filter(myData,
                          week_year == input$week_year,
                          week_month == 10,
                          week_day >=25,
                          rank_number <= input$rank_number),
            y = ~rank_number,
            x = if(input$x == "Ranking Points") {
              ~ranking_points} else if(input$x == "Tourneys Played") {
                ~tourneys_played} else {
                  ~player_age
                },
            text = ~paste('Name: ', player_name,
                          '</br>Age: ', player_age,
                          '</br>Rank: ', rank_number,
                          '</br>Ranking Points: ', ranking_points,
                          '</br>Tourneys Played: ', tourneys_played),
            type = "bar",
            mode = "markers",
            marker=list(
              color=~player_age,
              colorbar=list(title='Player Age'),
              colorscale=list(c(0, "#ccff00"), list(1, "#FF4500")),
              cauto = FALSE,
              cmax = 35,
              cmin = 16
            ),
            orientation = 'h'
    ) %>%
      layout(paper_bgcolor=NA,plot_bgcolor=NA,
             yaxis = list(
               color = 'black',
               title = "Ranking",
               range = c(input$rank_number+0.5, 0.5),
               tick0 = 1,
               dtick = 1),
             xaxis = list(
               color = 'black',
               title = input$x),
             font = list(
               family = "Garabd",
               size = 15),
             autosize = TRUE,
             showlegend = FALSE
      )
    }
  })
}


# RunApp ------------------------------------------------------------------

shinyApp(ui = UI, server = Server)