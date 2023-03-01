library(gapminder)
library(tidyverse)
library(shiny)
library(shinythemes)
library(DT)
library(dplyr)

#load data
movies <- read_csv("movies.csv")
movies_codebook <- read_csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_4850/datasets/movies_codebook.csv")
# head(movies)
# str(movies)

#define UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel(div("Movie browser, 1970 - 2014", style = "color: #0492C2")),
  sidebarLayout(
    sidebarPanel(
      titlePanel(div("Plotting", style = "color: #0492C2")),
      
      selectInput("y", tags$b("Y-axis: "), 
                  choices = c(
                    "IMDB rating" = "imdb_rating",
                    "IMDB number of votes" = "imdb_num_votes",
                    "Critics Score" = "critics_score" ,
                    "Audience Score" = "audience_score",
                    "Runtime" = "runtime"
                  ), selected = "audience_score"),
      
      selectInput("x", tags$b("X-axis: "), 
                  choices = c(
                    "IMDB rating" = "imdb_rating",
                    "IMDB number of votes" = "imdb_num_votes",
                    "Critics Score" = "critics_score" ,
                    "Audience Score" = "audience_score",
                    "Runtime" = "runtime"
                  ), selected = "critics_score"),
      
      selectInput("color", tags$b("Color by: "),
                  choices = c(
                    "Title Type" = "title_type", 
                    "Genre" = "genre",
                    "MPAA Rating" = "mpaa_rating",
                    "Critics Rating" = "critics_rating",
                    "Audience Rating" = "audience_rating"
                  ),selected = "mpaa_rating"),
      
      hr(),
      
      sliderInput("alpha", tags$b("Alpha: "), min = 0, max = 1, value = 1),
      
      sliderInput("size", tags$b("Size : "), min = 0, max = 5, value = 1),
      
      textInput("title", tags$b("Plot title: "), value = "",placeholder = "Enter text to be used as plot title"),
      
      titlePanel(div("Subsetting and sampling", style = "color: #0492C2")),
      
      radioButtons("movie_types", tags$b("Slelect movie type(s): "), 
                   # choices = c("Documentary" = "Documentary","Feature Film" = "Feature Film"),  #my way
                   choices = levels(factor(movies$title_type)),  #better way
                   selected = "Feature Film" ),
        
      numericInput("sample_size", tags$b("Sample size: "), min = 1, max = nrow(movies),value = 50),
      
      hr(),
      
      checkboxInput("show_data", "Show data table", value = TRUE)
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", 
                 plotOutput("plot"),span(textOutput("text"), style = "color: #0492C2")),
        
        tabPanel("Data", 
                 dataTableOutput("table")),
        
        tabPanel("Codebook", 
                 dataTableOutput("codebook"))
      )
    )
  )
)


#define server funciton
server <- function(input, output){
  
  filtered_data <- reactive({
    movies %>%
      
      filter(
        title_type == input$movie_types
      )%>% sample_n(input$sample_size)
  })
  
  output$plot <- renderPlot({
    
    p <- ggplot(filtered_data(), aes_string(x = input$x, y = input$y,color = input$color)) +
      geom_point(size = input$size,  alpha = input$alpha) + 
      labs(
        x = input$x,
        y = input$y,
        color = input$color,
        title = input$title
      ) 
    p
    
  })
  
  output$text <- renderText({
  
    paste("There are ", input$sample_size, input$movie_types, " movies in this dataset. ")
  })
  
  
  output$table <- DT::renderDataTable({
    
    if(input$show_data){
      data <- filtered_data()
      data
    }

  })
  
  output$codebook <- DT::renderDataTable({
    movies_codebook
  })
  
}

#create shiny object
shinyApp(ui = ui, server = server)
