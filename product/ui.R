library(shiny)

shinyUI(fluidPage(theme = "bootstrap.css",
    
    fluidRow(column(12,
        h2("Word prediction application", align = "center"),
        fluidRow(
            column(4,
                   h3("Settings:", align = "center"),
                   wellPanel(
                       radioButtons("pmethod", 
                                    label = h5("Select prediction method:"), 
                                    choices = list("Stupid Backoff" = "SBO", 
                                                   "Interpolation" = "I"),
                                    selected = "SBO"),
                       sliderInput("numwords",
                                   "Number of words:",
                                   min = 1,
                                   max = 10,
                                   value = 3)),
                   # This outputs the dynamic UI component
                   uiOutput("ui")
                   ),
#                radioButtons("pmethod", 
#                             label = h5("Select prediction method:"), 
#                             choices = list("Stupid Backoff" = "SBO", 
#                                            "Interpolation" = "I"),
#                             selected = "SBO"), 
            column(8,
                h3("Prediction", align = "center"),
                fluidRow(
                    column(6,
                        textInput("phrase", label = h5("Type sentence to predict next word:"), 
                                  value = "I hope you"),
#                        actionButton("predict", label = "Predict"),
                        h4("Completed sentence:"),
                        h4(textOutput("sentence"), style = "color:red")),
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