library(shiny)

shinyUI(fluidPage(
  titlePanel("Power Law Demo"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Exploratory analysis of the frequency of words found in 
               Moby Dick. The distribution of words follows a power law.
               Select a function type to view empirical distribution of 
               the data."),
    
      br(),
      selectInput("func", 
                  label = "Choose a Function Type",
                  choices = list("Probability Density Function",
                                 "Inverse Cummulative Density Function"),
                  selected = "Inverse Cummulative Density Function"),
      
      br(),
      checkboxGroupInput("logscale", 
                    label = "Log Scale", 
                    choice = list("X Axis" = 1,
                                  "Y Axis" = 2),
                    select = c(1,2)),
      
      numericInput("xmin", 
                   label = "Starting Value of Model", 
                   value = 0, min=0),
      textOutput("cmin"),
      actionButton("goButton", "Go!")
    ),
    
    mainPanel(plotOutput("plot"))
  )
))