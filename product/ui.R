library(shiny)

shinyUI(fluidPage(theme = "bootstrap.css",
    
    fluidRow(column(12,
        h2("Word prediction application", align = "center"),
        fluidRow(
            column(4,
                   h3("Settings:", align = "center"),
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
            column(8,
                h3("Prediction", align = "center"),
                fluidRow(
                    column(6,
                        textInput("phrase", label = h5("Type sentense to predict next word:"), 
                                  value = "I hope you"),
#                        actionButton("predict", label = "Predict"),
                        h4("Completed sentense:"),
                        h4(textOutput("sentense"), style = "color:red")),
                    column(6,
                           h5("Predicted words:"),
                           verbatimTextOutput("words"),
                           plotOutput("wc")
                           )
                    )
                )
             )
            ))
    ))