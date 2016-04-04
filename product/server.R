library(shiny)
library(wordcloud)
source("../model/predict.R")
shinyServer(function(input, output) {
    # read prediction model upon startup
    fit <- readRDS("model.rds")
    # reread oil prices only when related parameters change, 
    # not when any parameter changes
    getPrediction <- reactive({
#        if (input$predict == 0)
#            return()
        cat("getPrediction\n", file = "shiny.log", append = TRUE)
            predictTM(model = fit, 
                      phrase = input$phrase, 
                      n = input$numwords, 
                      method = input$pmethod, 
                      l = c(0.1, 0.15, 0.3, 0.45))
    })
            
    output$words <- renderText({
        prediction <- getPrediction()
        words <- prediction$Word
        probs <- prediction$Prob
        paste(words, probs, collapse = "\n")
    })
    
    wordcloud_rep <- repeatable(wordcloud)
    output$wc <- renderPlot({
        prediction <- getPrediction()
        words <- prediction$Word
        probs <- prediction$Prob
        probs.adjusted <- probs + input$wcscale

        
        wordcloud_rep(words, probs.adjusted, scale=c(max(probs.adjusted) * 5, min(probs.adjusted)),
                      min.freq=0, colors=brewer.pal(8, "Dark2"))
    }, width = 200, height = 200)
    
    output$numwords <- renderText({
        paste("Selected number of words:", input$numwords)
    })
    
    output$pmethod <- renderText({
        paste("Selected method:", input$pmethod)
    })

    output$phrase <- renderText({
        paste("Phrase:", input$phrase)
    })
})