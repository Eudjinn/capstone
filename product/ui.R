library(shiny)

shinyUI(fluidPage(
    
    titlePanel("Word prediction"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("pmethod", 
                        label = h5("Select prediction method:"), 
                        choices = list("Stupid Backoff" = "SBO", 
                                       "Interpolation" = "I"), 
                        selected = 1),
            sliderInput("numwords",
                        "Number of words:",
                        min = 1,
                        max = 10,
                        value = 3),
            sliderInput("wcscale",
                        "Word cloud scale:",
                        min = 0.2,
                        max = 0.5,
                        value = 1)),
    mainPanel(
                h5("Text input."),
                textOutput("numwords"),
                textOutput("pmethod"),
                textInput("phrase", label = h3("Enter part of a sentense:"), 
                          value = "i don't understand"),
                actionButton("predict", label = "Predict"),
                textOutput("phrase"),
                verbatimTextOutput("words"),
                plotOutput("wc")
            ))
    )
)