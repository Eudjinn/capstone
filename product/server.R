library(shiny)
library(wordcloud)
source(file.path("model", "utils.R"))
source(file.path("model", "predict.R"))

# read prediction model upon startup
fit <- readRDS("data/model.rds")

shinyServer(function(input, output, session) {
    output$ui <- renderUI({
        # Depending on prediction algorithm chosen, we'll generate a different
        # UI component and send it to the client.
        switch(input$pmethod,
               "SBO" = wellPanel(h4("Backoff coefficient:"),
                                 sliderInput("alpha", "Alpha",
                                      min = 0.05, max = 1, value = 0.4),
                                 h5("Changing alpha coefficient does not guarantee any changes in prediction output for particular sentence."),
                                 h5(textOutput("alpha"), style = "color:black")),
               "I" = wellPanel(h4("Predefined interpolation coefficients:"), 
                               h5(textOutput("lambda"), style = "color:black"))
        )
    })
    
#    observe({
#        if(!is.null(input$pmethod)){
#            if(input$pmethod == "I") {
#                if(!is.null(input$lambda4) & !is.null(input$lambda3) & !is.null(input$lambda2) & !is.null(input$lambda1)) {
#                    lambda <- c(input$lambda1, input$lambda2, input$lambda3, input$lambda4)
#                    lambda <- round(lambda/sum(lambda), 2)
#                    output$lambda <- renderText({
#                        paste0("Lambda = (", lambda[1], ", ", lambda[2], ", ", lambda[3], ", ", lambda[4], ")")
#                    })
#                }
#            }
#        }
#    })
    
    getPrediction <- reactive({
#        if (input$predict == 0)
#            return()
            predictTM(model = fit, 
                      phrase = input$phrase, 
                      n = input$numwords, 
                      method = input$pmethod, 
                      alpha = input$alpha,
                      l = c(0.1, 0.15, 0.3, 0.45))
        
    })

    output$lambda <- renderText({
        lambda <- c(0.1, 0.15, 0.3, 0.45)
        paste0("Lambda = (", lambda[1], ", ", lambda[2], ", ", lambda[3], ", ", lambda[4], ")")
    })
        
    output$alpha <- renderText({
        paste("Alpha =", input$alpha)
    })
    
    output$words <- renderText({
        prediction <- getPrediction()
        words <- prediction$Word
        paste(paste0(seq(1:length(words)), ":"), words, collapse = "\n")
    })
    
#    wordcloud_rep <- repeatable(wordcloud)
#    output$wc <- renderPlot({
#        prediction <- getPrediction()
#        words <- prediction$Word
#        probs <- prediction$Prob
#        probs.adjusted <- probs + input$wcscale

        
#        wordcloud_rep(words, probs.adjusted, scale=c(max(probs.adjusted) * 5, min(probs.adjusted)),
#                      min.freq=0, colors=brewer.pal(8, "Dark2"))
#    }, width = 200, height = 200)
    
    output$numwords <- renderText({
        paste("Selected number of words:", input$numwords)
    })
    
    output$pmethod <- renderText({
        paste("Selected method:", input$pmethod)
    })

    output$sentence <- renderText({
        prediction <- getPrediction()
        paste(input$phrase, prediction$Word[1])
    })
    
    output$firstword <- renderText({
        prediction <- getPrediction()
        prediction$Word[1]
    })
})