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
library(dplyr)


bikedata <- read.csv("bike_buyers.csv", stringsAsFactors = FALSE)


ui <- fluidPage(
  titlePanel("Exploratory Data Analysis on the Bike Buyers Dataset to understand whether a buyer should buy a bike or not based on various factors"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("incomeInput", "Income", 10000, 200000, c(20000, 25000), pre = "USD"),
      uiOutput("maritalOutput"),
      uiOutput("genderOutput"),
      uiOutput("occupationOutput"),
      uiOutput("commOutput"),
      uiOutput("educationOutput")
    ),
    mainPanel(
      plotOutput("plot"),
      br(), br(),
      tableOutput("results")
    )
  )
)

server <- function(input, output) {
  output$maritalOutput <- renderUI({
    selectInput("maritalInput", "Marital Status",
                sort(unique(bikedata$Marital.Status)),
                selected = "Single")
  }) 
  
  output$occupationOutput <- renderUI({
    selectInput("occupationInput", "Occupation",
                sort(unique(bikedata$Occupation)),
                selected = "Clerical")
  }) 
  
  
  output$educationOutput <- renderUI({
    selectInput("educationInput", "Education",
                sort(unique(bikedata$Education)),
                selected = "Bachelors")
  }) 
  
  output$commOutput <- renderUI({
    selectInput("commInput", "Region",
                sort(unique(bikedata$Region)),
                selected = "Europe")
  }) 
  
  output$genderOutput <- renderUI({
    selectInput("genderInput", "Gender",
                sort(unique(bikedata$Gender)),
                selected = "Male")
  }) 
  
  filtered <- reactive({
    if (is.null(input$incomeInput)) {
      return(NULL)
    }    
    
    bikedata %>%
      filter(Income >= input$incomeInput[1],
             Income <= input$incomeInput[2],
             Education == input$educationInput,
             Gender == input$genderInput,
             Occupation == input$occupationInput,
             Region == input$commInput,
             Marital.Status == input$maritalInput
      )
  })
  
  output$plot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    ggplot(filtered(), aes(Cars)) +
      geom_histogram()
  })
  
  
  output$results <- renderTable({
    filtered()
  })
}

shinyApp(ui = ui, server = server)

