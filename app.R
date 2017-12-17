#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(wordcloud)
library(plotly)
library(dplyr)
library(wordcloud)
library(tidytext)
library(tidyr)
library(reshape2)
library(RColorBrewer)
league_text<-readRDS("league_text.rds")
heroes_text<-readRDS("heroes_text.rds")

bing_word_counts_league<-readRDS("bing_word_counts_league.rds")
bing_word_counts_heroes<-readRDS("bing_word_counts_heroes.rds")
league_map<-readRDS("league_map.rds")
heroes_map<-readRDS("heroes_map.rds")
map.data <- map_data("state") 

# Define UI for application that draws a histogram
ui<-fluidPage(titlePanel(fluidRow(
                    column(8, 
                           h3("MA615 Twitter Data Mining Project"),
                           h6("by Yukun He. 
                               All the codes from", a(href="https://github.com/YukunHe0321/615twitter", "YukunHe0321 Github"))
                    ))),
                    navbarPage(title= "",
                    tabPanel("Introduction",
                           hr(),
                           p("I was a player of both online games, League of Legends and Heroes of the Storm.
                           I want to explore and compare online critics for these two games, since they are major competitors in the MOBA game category.
                           Also I want to start my future career in the gaming industry, so that this project can prepare me for the future."),
                           p("Links of the games:"),
                           hr(),
                           a(href="https://na.leagueoflegends.com/en/","League of Legends"),
                           hr(),
                           a(href="http://us.battle.net/heroes/en/","Heroes of the Storm")
                    ),
                    tabPanel("Maps",
                           h4("Locations where people send Tweets about League of Legends and Heroes of the Storm"),
                           p("Google allows only 2500 requests per day for geocode, these two maps are not comprehensive.
                           These maps provide a general overview of the locations", span("Leagues of Legends & Heroes of the Storm", style="color:blue"),
                           "From the map, we observe that Tweets are mainly located at West Coast, Great Boston and New York Area, Chicago, and Texas.
                           These regions have a lot of colleges, which represents the fact that the players are mainly college students.
                           Other than these regions, Tweets are observed all over the USA, which means that the two games are both popular over the country."),
                           hr(),
                           sidebarLayout(
                           sidebarPanel(
                           selectInput(inputId = "mapinput",
                                      label="Select game",
                                      choices = c("League of Legends","Heroes of the Storm"))
                           ),
                           mainPanel(plotOutput("maps"))
                           )
                    ),
             
                    tabPanel("Sentiment",
                           h4("Sentiment Analysis"),
                           p("From the sentiment analysis we can directly observe the most frequent words used in the Tweets we choose, 
                           and the positive and negative sentiments of those words."),
                           hr(),
                           sidebarLayout(
                           sidebarPanel(selectInput(inputId = "sentiinput",
                                                 label="Select game",
                                                 choices = c("League of Legends","Heroes of the Storm"))
                           ),
                           mainPanel(plotOutput("sentiment")))
                    ),
             
             tabPanel("Word Cloud",
                      h4("Word Cloud"),
                      p("From the word cloud, we can divide words into positive (Blue) and negative (Red) categories
                        And the larger the size, the bolder the font, the more frequently that word is observed."),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "wordinput",
                                      label="Select game",
                                      choices = c("League of Legends","Heroes of the Storm"))
                        ),
                        mainPanel(
                          plotOutput("wordcloud")
                        )
                      )
             )
             )
  
  )

#server
server<-function(input, output){
  
  ### maps
  output$maps<-renderPlot({
    if(input$mapinput=="League of Legends"){
      ggplot(map.data) + 
        geom_map(aes(map_id = region),  
                 map = map.data,  
                 fill = "white",             
                 color = "grey20", size = 0.25) + 
        expand_limits(x = map.data$long, y = map.data$lat) +            
        theme(axis.line = element_blank(),  
              axis.text = element_blank(),  
              axis.ticks = element_blank(),                     
              axis.title = element_blank(),  
              panel.background = element_blank(),  
              panel.border = element_blank(),                     
              panel.grid.major = element_blank(), 
              plot.background = element_blank(),                     
              plot.margin = unit(0 * c( -1.5, -1.5, -1.5, -1.5), "lines")) +  
        geom_point(data = league_map,             
                   aes(x = x, y = y), size = 1,  
                   alpha = 1/5, color = "blue")+labs(title="Map for League of Legends")
    }
    else{
      ggplot(map.data) + 
        geom_map(aes(map_id = region),  
                 map = map.data,  
                 fill = "white",             
                 color = "grey20", size = 0.25) + 
        expand_limits(x = map.data$long, y = map.data$lat) +            
        theme(axis.line = element_blank(),  
              axis.text = element_blank(),  
              axis.ticks = element_blank(),                     
              axis.title = element_blank(),  
              panel.background = element_blank(),  
              panel.border = element_blank(),                     
              panel.grid.major = element_blank(), 
              plot.background = element_blank(),                     
              plot.margin = unit(0 * c( -1.5, -1.5, -1.5, -1.5), "lines")) +  
        geom_point(data = heroes_map,             
                   aes(x = x, y = y), size = 1,  
                   alpha = 1/5, color = "red")+labs(title="Map for Heroes of the Storm")
    }
  }
  )
  
  
  ##sentiment
  output$sentiment<-renderPlot({
    if(input$sentiinput=="League of Legends"){
      bing_word_counts_league%>%
        group_by(sentiment) %>%
        top_n(10) %>%
        ungroup() %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(word, n, fill = sentiment)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~sentiment, scales = "free_y") +
        labs(y = "Contribution to sentiment",
             x = NULL) +
        coord_flip()+labs(title="Sentiment Analysis for League of Legends")
    }
    else{
      bing_word_counts_heroes%>%
        group_by(sentiment) %>%
        top_n(10) %>%
        ungroup() %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(word, n, fill = sentiment)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~sentiment, scales = "free_y") +
        labs(y = "Contribution to sentiment",
             x = NULL) +
        coord_flip()+labs(title="Sentiment Analysis for Heroes of the Storm")
    }
  }
  )
  
  ##word cloud
  output$wordcloud<-renderPlot({
    if(input$wordinput=="League of Legends"){
      league_text %>%
        inner_join(get_sentiments("bing")) %>%
        count(word, sentiment, sort = TRUE) %>%
        acast(word ~ sentiment, value.var = "n", fill = 0) %>%
        comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                         max.words = 60,title.size=3)
    }
    else{
      heroes_text %>%
        inner_join(get_sentiments("bing")) %>%
        count(word, sentiment, sort = TRUE) %>%
        acast(word ~ sentiment, value.var = "n", fill = 0) %>%
        comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                         max.words = 60,title.size=3)
    }
  }
  )
}
# Run the application 
shinyApp(ui = ui, server = server)

