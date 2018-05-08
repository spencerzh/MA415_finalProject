# library

options(warn = -1)
library(shiny)
library(shinydashboard)
library(twitteR)
library(tidyverse)
library(ggplot2)
library(tidytext)
library(stringr)
library(scales)
library(wordcloud2)
library(htmlwidgets)
library(tidyr)
library(dplyr)
library(wordcloud)
data(stop_words)

#Add stop words to filter out
stop_words <- add_row(stop_words, word = "t.co", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "https", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "http", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "rt", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "1", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "2", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "3", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "9", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "rt", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "https", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "http", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "t.co", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "1", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "2", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "3", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "de", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "amp", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "na", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "false", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "iphone", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "android", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "0", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "07", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "05", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "true", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "twitter", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "twitter.com", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "rel", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "nofollow", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "2018", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "ed", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "href", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "download", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "19", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "00a0", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "00bd", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "0627", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "18", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "0644", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "20", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "15846407", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "0646", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "998", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "ueextuv8no", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "22", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "21", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "0648", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "17", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "23", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "737", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "bu5isy87gr", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "0082", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "00b8", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "0643", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "0631", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "00b1", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "00bc", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "008f", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "2661", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "00b6", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "00be", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "tpiwmxe4kr", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "0642", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "0645", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "0623", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "0629", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "0628", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "064a", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "nu5isy87gr", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "kyderby", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "py5k1jryr3", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "icymi", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "6b", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "0633", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "0647", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "0641", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "12", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "39", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "0651", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "57", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "15", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "59", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "54", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "16", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "49", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "0639", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "008d", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "0649", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "062b", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "01", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "10", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "55", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "mobile.twitter.com", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "ipad", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "08", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "06", lexicon = "SMART")

getFreq <- function(file, name, num) {
  text <- readLines(file)
  text_df <- data_frame(line = 1:length(text), text = text )
  text_df <- text_df %>%
    unnest_tokens(word, text) %>% 
    count(word, sort = TRUE) %>% 
    filter(n > num) %>% 
    anti_join(stop_words, by = "word") %>% 
    mutate(word = reorder(word, n)) %>% 
    ggplot(aes(word, n), col = "blue") +
    geom_col() +
    xlab(NULL) +
    ggtitle(name) +
    coord_flip()
}

getWordCloud <- function(file, min_freq, max_word) {
  df <- getTidyTibble(file)
  cloud <- df %>% anti_join(stop_words, by = "word") %>%
    count(word, sort = TRUE)
  temp <- wordcloud(words = cloud$word, freq = cloud$n, min.freq = min_freq,
                    max.words=max_word, random.order=FALSE, rot.per=0.35, 
                    colors=brewer.pal(8, "Dark2"))
  return(temp)
}

getTidyTibble <- function(file) {
  text <- readLines(file)
  df <- data_frame(line = 1:length(text), text = text)
  df <- df %>% 
    unnest_tokens(word, text) %>% 
    anti_join(stop_words)
  return(df)
}



ui <- dashboardPage(
  dashboardHeader(title = "The Ellen Show vs. FallonTonight Show"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Most used words", tabName = "used_word"),
      menuItem("Sentimental Analysis", tabName = "sentiment"),
      menuItem("Similarity Analysis", tabName = "similarity_analysis"),
      menuItem("World Cloud", tabName = "world_cloud")
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "used_word",
              fluidRow(
                box(selectInput("mode1",
                                "Most frequent word in tweets mentioning The Ellon Show and FallonTonight show: ",
                                choices = list("tweets mentioning TheEllonShow", "tweets mentioning FallonTonight")),    
                    plotOutput("plot1"), width = 12)
              )
      ),
      
      tabItem(tabName = "sentiment",
              fluidRow(
                box(selectInput("mode2",
                                "Sentimental analysis for The Ellen show and FallonTonight show:",
                                choices = list("Sentimental analysis for The Ellen show", "Sentimental analysis for FallonTonight show")), 
                    plotOutput("plot2"), width = 12)
              )
      ),
      tabItem(tabName = "similarity_analysis",
              fluidRow(
                box(selectInput("mode3",
                                "Similarity analysis for The Ellen show and FallonTonight show: ",
                                choices = list("Similarity analysis with Martin Luther King", "Similarity analysis with Shakespeare")),    
                    plotOutput("plot3"), width = 12)
              )
      ),
      tabItem(tabName = "world_cloud",
              fluidRow(
                box(selectInput("mode4",
                                "World cloud for The Ellon Show and FallonTonight show: ",
                                choices = list("World cloud for The Ellon Show", "World cloud for The Ellon Show FallonTonight show")),    
                    plotOutput("plot4"), width = 12)
              )
      )
    )
  ))


server <- function(input, output) {
  
  
  output$plot1 <- renderPlot({
    
    input1 <- input$mode1
    
    if (input1 =="tweets mentioning TheEllonShow"){
      graph <- getFreq('TheEllenShow.txt', 'Most used words in tweets mentioing TheEllenShow', 200)
      print(graph)
    }
    
    if (input1 =="tweets mentioning FallonTonight"){
      graph <- getFreq('FallonTonight.txt', 'Most used words in tweet mentioing FallonTonight', 200)
      print(graph)
    }
    
    
    
  })
  
  output$plot2 <- renderPlot({
    
    input2 <- input$mode2
    
    if (input2 =="Sentimental analysis for The Ellen show"){
      nrc_joy <- get_sentiments("nrc") %>% 
        filter(sentiment == "joy")
      nrc_anger <- get_sentiments("nrc") %>% 
        filter(sentiment == "anger")
      nrc_anticipation <- get_sentiments("nrc") %>% 
        filter(sentiment == "anticipation")
      nrc_sadness <- get_sentiments("nrc") %>% 
        filter(sentiment == "sadness")
      ellen_joy <- tidy_ellen %>%
        inner_join(nrc_joy) %>%
        count(word, sort = TRUE) %>%
        filter(n > 10) %>%
        mutate(word = reorder(word, n))
      
      ellen_anger <- tidy_ellen %>%
        inner_join(nrc_anger) %>%
        count(word, sort = TRUE) %>%
        filter(n > 5) %>%
        mutate(word = reorder(word, n))
      
      ellen_anticipation <- tidy_ellen %>%
        inner_join(nrc_anticipation) %>%
        count(word, sort = TRUE) %>%
        filter(n > 10) %>%
        mutate(word = reorder(word, n))
      
      ellen_sadness <- tidy_ellen %>%
        inner_join(nrc_sadness) %>%
        count(word, sort = TRUE) %>%
        filter(n > 5) %>%
        mutate(word = reorder(word, n))
      
      graph <- gridExtra::grid.arrange(
        ggplot(ellen_joy, aes(word, n)) +
          geom_col() +
          xlab(NULL) +
          coord_flip() +
          ggtitle("Ellen Joy"),
        ggplot(ellen_anger, aes(word, n)) +
          geom_col() +
          xlab(NULL) +
          coord_flip() +
          ggtitle("Ellen Anger"),
        ggplot(ellen_anticipation, aes(word, n)) +
          geom_col() +
          xlab(NULL) +
          coord_flip() +
          ggtitle("Ellen Anticipation"),
        ggplot(ellen_sadness, aes(word, n)) +
          geom_col() +
          xlab(NULL) +
          coord_flip() +
          ggtitle("Ellen Sadness"),
        nrow=1
      )
      print(graph)
    }
    
    if (input2 =="Sentimental analysis for FallonTonight show"){
      # Sentiment analysis with nrc
      nrc_joy <- get_sentiments("nrc") %>% 
        filter(sentiment == "joy")
      nrc_anger <- get_sentiments("nrc") %>% 
        filter(sentiment == "anger")
      nrc_anticipation <- get_sentiments("nrc") %>% 
        filter(sentiment == "anticipation")
      nrc_sadness <- get_sentiments("nrc") %>% 
        filter(sentiment == "sadness")
      
      # Fallon show sentiment analysis with nrc
      fallon_joy <- tidy_fallon %>%
        inner_join(nrc_joy) %>%
        count(word, sort = TRUE) %>%
        filter(n > 10) %>%
        mutate(word = reorder(word, n))
      
      fallon_anger <- tidy_fallon %>%
        inner_join(nrc_anger) %>%
        count(word, sort = TRUE) %>%
        filter(n > 5) %>%
        mutate(word = reorder(word, n))
      
      fallon_anticipation <- tidy_fallon %>%
        inner_join(nrc_anticipation) %>%
        count(word, sort = TRUE) %>%
        filter(n > 10) %>%
        mutate(word = reorder(word, n))
      
      fallon_sadness <- tidy_fallon %>%
        inner_join(nrc_sadness) %>%
        count(word, sort = TRUE) %>%
        filter(n > 5) %>%
        mutate(word = reorder(word, n))
      
      graph <- gridExtra::grid.arrange(
        ggplot(fallon_joy, aes(word, n)) +
          geom_col() +
          xlab(NULL) +
          coord_flip() +
          ggtitle("Fallon Joy"),
        ggplot(fallon_anger, aes(word, n)) +
          geom_col() +
          xlab(NULL) +
          coord_flip() +
          ggtitle("Fallon Anger"),
        ggplot(fallon_anticipation, aes(word, n)) +
          geom_col() +
          xlab(NULL) +
          coord_flip() +
          ggtitle("Fallon Anticipation"),
        ggplot(fallon_sadness, aes(word, n)) +
          geom_col() +
          xlab(NULL) +
          coord_flip() +
          ggtitle("Fallon Sadness"),
        nrow=1
      )
      print(graph)
    }
  })
  
  
  
  
  
  output$plot3 <- renderPlot({
    
    input3 <- input$mode3
    
    if (input3 =="Similarity analysis with Martin Luther King"){
      ellen_df <- getTidyTibble("TheEllenShow.txt")
      fallon_df <- getTidyTibble("FallonTonight.txt")
      mlk_df <- getTidyTibble('mlk.txt')
      
      frequency <- bind_rows(mutate(ellen_df, author = "Ellen"),
                             mutate(fallon_df, author = "Fallon"),
                             mutate(mlk_df, author = "Martin Luther King")) %>% 
        mutate(word = str_extract(word, "[a-z']+")) %>%
        count(author, word) %>%
        group_by(author) %>%
        mutate(proportion = n / sum(n)) %>% 
        select(-n) %>% 
        spread(author, proportion) %>% 
        gather(author, proportion, `Ellen`:`Fallon`)
      
      graph <- ggplot(frequency, aes(x = proportion, y = `Martin Luther King`, color = abs(`Martin Luther King` - proportion))) +
        geom_abline(color = "gray40", lty = 2) +
        geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
        geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
        scale_x_log10(labels = percent_format()) +
        scale_y_log10(labels = percent_format()) +
        ggtitle('Similarity analysis between Ellen and Fallon tweets with MLK I Have a Dream speech') +
        scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
        facet_wrap(~author, ncol = 2) +
        theme(legend.position="none") +
        labs(y = "Martin Luther King", x = NULL)
      print(graph)
    }
    
    if (input3 =="Similarity analysis with Shakespeare"){
      ellen_df <- getTidyTibble("TheEllenShow.txt")
      fallon_df <- getTidyTibble("FallonTonight.txt")
      hamlet_df <- getTidyTibble('hamlet.txt')
      
      frequency <- bind_rows(mutate(ellen_df, author = "Ellen"),
                             mutate(fallon_df, author = "Fallon"),
                             mutate(mlk_df, author = "Shakespeare")) %>% 
        mutate(word = str_extract(word, "[a-z']+")) %>%
        count(author, word) %>%
        group_by(author) %>%
        mutate(proportion = n / sum(n)) %>% 
        select(-n) %>% 
        spread(author, proportion) %>% 
        gather(author, proportion, `Ellen`:`Fallon`)
      
      graph <- ggplot(frequency, aes(x = proportion, y = `Shakespeare`, color = abs(`Shakespeare` - proportion))) +
        geom_abline(color = "gray40", lty = 2) +
        geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
        geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
        scale_x_log10(labels = percent_format()) +
        scale_y_log10(labels = percent_format()) +
        ggtitle('Similarity analysis between Ellen and Fallon tweets with Shakespeare Hamlet') +
        scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
        facet_wrap(~author, ncol = 2) +
        theme(legend.position="none") +
        labs(y = "Shakespeare", x = NULL)
      print(graph)
    }
    
    
    
  })
  
  
  output$plot4 <- renderPlot({
    
    input4 <- input$mode4
    
    if (input4 =="World cloud for The Ellon Show"){
      print(getWordCloud('TheEllenShow.txt', 200, 600))
    }
    
    if (input4 =="World cloud for The Ellon Show FallonTonight show"){
      print(getWordCloud('FallonTonight.txt', 200, 600))
    }
    
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
