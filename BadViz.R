# Plotting Economic Developent
# FIRST: set the working directory!
# Type ctrl-L to clear the console


# ATP Tennis Ranking

rm(list = ls())
graphics.off()

library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(plotly)
library(data.table)


load("Twitter.RData")
myData <- TwitterActive

# Creating animation with shiny
library(shiny)

# UI ----------------------------------------------------------------------

UI <- fluidPage(
  
  #theme = "stylesheet.css",
  tags$head(includeCSS("stylesheet.css")),
  
  titlePanel(h1("Twitter Statistics", align = "center")),
  
  titlePanel(h2(HTML("<span style=\"color:#EC2049;\">Top 20</span> Most Active Twitter Users <span style=\"color:#EC2049;\">in 2019</span>"), align = "center")),
  
  fluidRow(
    column(8,
           div(plotlyOutput(outputId = "myPlot", height = "100%"), style="height: 35em;")
    ),
    column(4,
           p("This application illustrates various characteristics (tweets per day, followers and total tweets) of the currently most active Twitter users.", style="margin-top:2.5em;"),
           selectInput(inputId = "x", 
                       label = "X-axis:",
                       choices = c("Tweets per Day" = "Tweets per Day",
                                   "Followers" = "Followers",
                                   "Total Tweets" = "Total Tweets"),
                       selected = "ranking_points",
                       width = "12em"),
           sliderInput(inputId = "Rank", 
                       label = "Ranking (range):", 
                       min = 1, max = 25, 
                       value = 20,
                       step=1),
           align="center")
  ),
  fluidRow(
    column(12,
           HTML("<p>Source: https://sysomos.com/inside-twitter/most-active-twitter-user-data/"),
           align = "right")
  )
)


# Server ------------------------------------------------------------------

Server <- function(input,output){
  
  output$myPlot <- renderPlotly({{plot_ly(data = filter(myData, Rank <= input$Rank),
                                                                y = ~Rank,
                                                                x = if(input$x == "Tweets per Day") {
                                                                  ~Tweets_Day} else if(input$x == "Followers") {
                                                                    ~Followers} else {
                                                                      ~Tweets_Total
                                                                    },
                                                                text = ~paste('User: ', User,
                                                                              '</br>Tweets per Day: ', Tweets_Day,
                                                                              '</br>Rank: ', Rank,
                                                                              '</br>Followers: ', Followers,
                                                                              '</br>Total Tweets: ', Tweets_Total),
                                                                type = "bar",
                                                                mode = "markers",
                                                                marker=list(
                                                                  color=~Tweets_Day,
                                                                  colorbar=list(title='Tweets per Day'),
                                                                  colorscale=list(c(0, "white"), list(1, "blue")),
                                                                  cauto = FALSE, # hier wird die colorscale gefixed
                                                                  cmax = 140,
                                                                  cmin = 20
                                                                ),
                                                                orientation = 'h'
  ) %>%
      add_annotations(text = ~User,
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
               range = c(input$Rank+0.5, 0.5),
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
  } 
  })
}


# RunApp ------------------------------------------------------------------

shinyApp(ui = UI, server = Server)