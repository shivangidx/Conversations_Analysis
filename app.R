#Load libraries
library(shiny)
library(dplyr)
library(tm)
library(wordcloud2)
library(wordcloud)
library(readxl)
library(rsconnect)
library(shinycssloaders)
library(tidytext)
library(stringr)
library(DT)
library(syuzhet)
library(tibble)
library(tidyr)
library(text)
library(ggplot2)
library(shinyWidgets)
library(textstem)

# Define UI
ui <- fluidPage(
  #set background color
  setBackgroundColor(
    color = c("#DCECE3","#A0B0CD"),
    gradient = "linear",
    direction = "bottom"
  ),
  
  # App title
  titlePanel(      tags$div(
    style = "padding: 0px; margin-bottom: 12px;",
    tags$h2("Conversations Analysis", style = "text-align: left; font-weight: bold;font-color:blue;")
  )),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    # Sidebar panel for inputs
    sidebarPanel(
      fileInput("dataFile", "Upload Conversations File",
                accept = c(".csv", ".xlsx", ".xls", ".rds", ".sas7bdat", ".xpt")),
      
      selectInput("column", "Select Column", choices = NULL),
      conditionalPanel(
        condition = "input.tabSelected === 'Wordcloud'",
        sliderInput("maxwords","Number of words to show:",
                    min = 1,  max = 500,  value = 100),
        sliderInput("textsize","Choose size of the text:",
                    min = 0,  max = 1,  value = 0.7, step=0.05),
        textAreaInput("omit", 
                      "Words to remove (separate with commas)", 
                      height = "50px", width="500px"),
        actionButton("show", "Show wordcloud")
      )
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      tabsetPanel(id = "tabSelected",
                  tabPanel("Wordcloud", br(), br(), br(), 
                           div(style = "display: flex; justify-content: center; align-items: center; height: 100%;",wordcloud2Output('wordcloud', width = "60%", height = "300px"))),
                  tabPanel("N-grams",
                           br(),
                           fluidRow(
                             column(5,radioButtons("ngram_type", "Choose N-gram:",
                                                   choices = c("Unigram" = "1", "Bigram" = "2", "Trigram" = "3","Fourgram" = "4"),
                                                   inline = TRUE)),
                             column(2,style="margin-top: 10px",
                                    actionButton('show_ngrams',"Show"))
                           ),
                           br(),dataTableOutput("ngram_table"),
                           br(),plotOutput("ngram_chart"),br(),br()
                  ),
                  tabPanel("Sentiment", 
                           br(), actionButton('show_sentiment',"Display graph"),
                           br(),br(),plotOutput('SentimentChart')),
                  tabPanel("Filter Conversations",
                           br(),actionButton('show_btn',"Show conversations"),
                           br(),br(),DTOutput('filtered_conv', height="300px", width = "90%")
                  )
      )
    )
  )
)

#Define server
server <- function(input, output, session) {
  
  # Reactive expression to read the uploaded file based on its extension
  uploadedData <- reactive({
    req(input$dataFile)
    data<- switch(tools::file_ext(input$dataFile$name),
                  csv = read.csv(input$dataFile$datapath),
                  xls = read_excel(input$dataFile$datapath),
                  xlsx = read_excel(input$dataFile$datapath),
                  rds = readRDS(input$dataFile$datapath),
                  sas7bdat = haven::read_sas(input$dataFile$datapath),
                  xpt = haven::read_xpt(input$dataFile$datapath),
                  stop("Unsupported file format"))
    data %>% select_if(is.character)
  })
  
  # Update the columns based on the uploaded data
  observeEvent(uploadedData(),{
    df <- uploadedData()
    updateSelectInput(session, "column", choices = colnames(df))
  })
  
  # Clean data for analysis
  preprocessText <- function(data, column) {
    texts <- tolower(data[[column]])
    texts <- removeWords(texts, stopwords("english"))
    texts <- removePunctuation(texts)
    texts <- removeNumbers(texts)
    texts <- stripWhitespace(texts)
    texts <- lemmatize_strings(texts)
    return(texts)
  }
  
  # Generate data for wordcloud
  myData <- eventReactive(input$show, {
    data <- uploadedData() %>% select(input$column)
    texts <- preprocessText(data, input$column)
    # Generate table for frequency counts
    word_freqs <- sort(table(unlist(strsplit(texts, " "))), decreasing = TRUE)
    # Convert to data frame
    whole <- data.frame(word = names(word_freqs), freq = as.integer(word_freqs))
    # Omit specified words
    omitted <- strsplit(tolower(input$omit), "\\s+|,+")[[1]]
    whole <- whole[!whole$word %in% omitted, ]
    # Limit to maxwords
    whole <- head(whole, n = input$maxwords)
    return(whole)
  }, ignoreNULL = FALSE)
  
  # Generate wordcloud on 'Show' button click
  observeEvent(input$show, {
    output$wordcloud <- renderWordcloud2({
      req(myData())
      wordcloud2(data = myData(), color = "random-dark", backgroundColor = "white", 
                 size = input$textsize, fontWeight = "normal", fontFamily = 'cambria')
    })
  })
  
  # Reactive expression for generating n-grams
  ngramsData <- eventReactive(input$show_ngrams, {
    req(input$dataFile) 
    req(input$column) 
    data <- uploadedData() %>% select(input$column)
    
    # Apply the data cleaning function
    texts <- preprocessText(data, input$column)
    # Convert the cleaned text back to a data frame
    data_preprocessed <- tibble(text = texts)
    # Tokenizing text into n-grams
    n <- as.numeric(input$ngram_type)
    ngrams_df <- data_preprocessed %>%
      unnest_tokens(output = "ngram", input = text, token = "ngrams", n = n) %>%
      count(ngram, sort = TRUE) %>%
      filter(ngram != "") %>% 
      top_n(200, wt = n)
    
    return(ngrams_df)
  })
  
  # Show N-grams on button click
  observeEvent(input$show_ngrams, {
    
    top_ngrams <- ngramsData() %>%
      arrange(desc(n)) %>%
      top_n(5) %>%
      mutate(ngram = factor(ngram, levels = ngram))
    
    # Render the n-gram table
    output$ngram_table <- renderDataTable({
      ngramsData()
    }, options = list(pageLength = 10)) 
    
    # Render the n-gram plot
    output$ngram_chart <- renderPlot({
      ggplot(top_ngrams, aes(x = ngram, y = n, fill = ngram)) +
        geom_col(show.legend = FALSE, color = "black", fill = "lightgreen") + 
        geom_text(aes(label = n), vjust = -0.3, size = 4) +  
        labs(x = NULL, y = "Frequency", title = "Top 5 N-Grams Frequency") + 
        theme_minimal() + 
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  
          axis.text.y = element_text(size = 12),  
          axis.title.y = element_text(size = 12),  
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5)  
        )
    }) 
  })
  
  # Calculate sentiment scores
  sentimentScores <- eventReactive(input$show_sentiment, {
    req(input$dataFile) 
    req(input$column)
    data <- uploadedData() %>% select(input$column)
    texts <- preprocessText(data, input$column)
    data_preprocessed <- tibble(text = texts)
    tokens <- data_preprocessed %>% unnest_tokens(word, text)
    
    sentiment<-tokens %>%
      inner_join(get_sentiments("bing")) %>% 
      count(sentiment) %>% 
      spread(sentiment, n, fill = 0)
    
    # Calculate percentages
    total <- sentiment$positive + sentiment$negative
    sentiment$positive <- (sentiment$positive / total) * 100
    sentiment$negative <- (sentiment$negative / total) * 100
    sentiment <- as.data.frame(sentiment)
    
    return(sentiment)
  })
  
  # Show sentiment plot on button click
  observeEvent(input$show_sentiment, {
    output$SentimentChart <- renderPlot({
      sentiment <- sentimentScores()
      
      # Convert to long format, ensure order is positive then negative
      sentiment_long <- pivot_longer(sentiment, cols = everything(), names_to = "sentiment", values_to = "percentage") %>%
        mutate(sentiment = factor(sentiment, levels = c("positive", "negative")))
      
      ggplot(sentiment_long, aes(x = sentiment, y = percentage, fill = sentiment)) +
        geom_col(show.legend = FALSE) +
        geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.3, size = 5) +
        scale_fill_manual(values = c("positive" = "green", "negative" = "red")) +
        labs(x = "Sentiment", y = "Percentage", title = "Sentiment Analysis as Percentage") +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
          axis.title.x = element_text(face = "bold", size = 14),
          axis.title.y = element_text(face = "bold", size = 14),
          axis.text.x = element_text(size = 12, hjust = 1),
          axis.text.y = element_text(size = 12)
        )
    })
  })
  
  #Show filtered conversations on button click
  output$filtered_conv <- DT::renderDT({
    req(input$show_btn)
    data <- uploadedData() %>% select(input$column)
    DT::datatable(data, 
                  options = list(
                    paging = TRUE, 
                    searching = TRUE,  
                    ordering = TRUE, 
                    autoWidth = TRUE,  
                    info = TRUE, 
                    pageLength = 5,  
                    dom = 'Bfrtip',  
                    scrollX = TRUE  
                  ))
  })
  
} 

shinyApp(ui, server)

rsconnect::setAccountInfo(name='shivangiprojects', token='DD53A681E8808E282FD293F221C7DF6D', secret='whJ6AYlYZ7w5IxfjVBNaN8IZZKQ14Kqgra+3RYZM')

deployApp()




