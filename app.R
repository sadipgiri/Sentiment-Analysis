# R project - Sentiment Analysis Project Using Libraries
# Author: Sadip Giri (sadipgiri@bennington.edu)
# Created: May 18th, 2016

library(shiny)     # Hosting using Shiny Package
library(ggplot2)   # visualization
library(tm)        # text mining
library(wordcloud) # generating word cloud
library(syuzhet)   # sentiment analysis

ui <- fluidPage(
        
        titlePanel("Sentiment Analysis"),
        
        sidebarPanel(
                fileInput(inputId = "fileName", label = "Choose Your File"),
                hr(),
                sliderInput("freq",
                                  "Minimum Frequency:",
                                  min = 1,  max = 50, value = 15),
                sliderInput("max",
                                  "Maximum Number of Words:",
                                  min = 1,  max = 300,  value = 100)
                # hr(),
                # textInput(inputId = "miningWords", label = "Filter Words")
        ),
        
        mainPanel(
                plotOutput(outputId = "wordcloudPlotting"),
                plotOutput(outputId = "ggPlotting"),
                h4("by Sadip Giri")
        )
)



server <- function(input, output){
        output$wordcloudPlotting <- renderPlot({
             
                file1 <- input$fileName$datapath
                text <- readLines(file1)
                
                if(is.null(input$miningWords) != TRUE){
                        filteringTheseWords <- input$miningWords
                }
                
                d <- returnDataFrame(text)
                
                set.seed(1056)
                wordcloud(words = d$word, freq = d$freq, min.freq = input$freq,
                          max.words=input$max, random.order=FALSE, rot.per=0.35,
                          colors=brewer.pal(8, "Dark2"))
        })
        
        output$ggPlotting <- renderPlot({
                file1 <- input$fileName$datapath
                texts <- readLines(file1)
                returnGGPlot(texts)
        })
        
}


returnDataFrame <- function(text){
        #creating corpus
        docs <- Corpus(VectorSource(text))
        
        trans <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
        
        docs <- tm_map(docs, trans, "/")
        docs <- tm_map(docs, trans, "@")
        #docs <- tm_map(docs, trans, "//|")
        docs <- tm_map(docs, content_transformer(tolower))
        docs <- tm_map(docs, removeNumbers)
        docs <- tm_map(docs, removeWords, stopwords("english"))
        docs <- tm_map(docs, removePunctuation)
        docs <- tm_map(docs, stripWhitespace)
        #docs <- tm_map(docs, stemDocument)
        docs <- tm_map(docs, removeWords, c("applause", "trump"))
        
        dtm <- TermDocumentMatrix(docs)
        mat <- as.matrix(dtm)
        v <- sort(rowSums(mat), decreasing=TRUE)
        
        data.frame(word = names(v),freq=v)
}

returnGGPlot <- function(texts){
        Sentiment <- get_nrc_sentiment(texts)
        text <- cbind(texts, Sentiment)
        
        TotalSentiment <- data.frame(colSums(text[, c(2:11)]))
        names(TotalSentiment) <- "count"
        TotalSentiment <- cbind("sentiment" = rownames(TotalSentiment), TotalSentiment)
        rownames(TotalSentiment) <- NULL
        
        
        ggplot(data = TotalSentiment, aes(x = sentiment, y = count)) +
                geom_bar(aes(fill = sentiment), stat = "identity") +
                theme(legend.position = "none") +
                xlab("Sentiment") +
                ylab("Total Count") +
                ggtitle("Sentiment Score")
}

shinyApp(ui = ui, server = server)
