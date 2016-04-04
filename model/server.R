library(shiny)
library(wordcloud)
source("predict.R")
shinyServer(function(input, output) {
    # read prediction model upon startup
    fit <- readRDS("model.rds")
    # reread oil prices only when related parameters change, 
    # not when any parameter changes
    getPrediction <- reactive({
#        if (input$predict == 0)
#            return()
            predictTM(model = fit, 
                      phrase = input$phrase, 
                      n = input$numwords, 
                      method = input$pmethod, 
                      l = c(0.1, 0.15, 0.3, 0.45))
    })
            
    output$words <- renderText({
        prediction <- getPrediction()
        words <- prediction$Word
        paste(paste0(seq(1:length(words)), ":"), words, collapse = "\n")
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

    output$sentense <- renderText({
        prediction <- getPrediction()
        paste(input$phrase, prediction$Word[1])
    })
    
    output$firstword <- renderText({
        prediction <- getPrediction()
        prediction$Word[1]
    })
})