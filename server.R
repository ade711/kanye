
library(shiny)
library("tidyverse")
library("dplyr")
library(ggplot2)
library(wordcloud)
library(wordcloud2)
library(tidytext)
library(plotly)
library(ggraph)
library(igraph)
library(ggwordcloud)
source("dropdownModule.R")

# Read pre-processed data in
kw_albums <- readRDS("kw_albums.Rds")
tidy_lyrics_tok <- readRDS("tidy_lyrics_token.Rds")
tf_idf <- readRDS("tf_idf.Rds")

# Convert lyrics data to bigrams
lyrics_bigram <- kw_albums %>%
  unnest_tokens(bigram, lyric, token = "ngrams", n=2)
bigrams_separated <- lyrics_bigram %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# Function to create df of word and its use per album
user_wordcount <- function(user_word, lyrics_data){
  df <- lyrics_data %>% 
    count(album, word) %>% 
    complete(album, word, fill =list(n=0)) %>% 
    filter(word == user_word)
  return(df)
}

rm_words <- c("cuz","la","lah","ba","c'mon","ayy","i.d","da","ooohh",
              "sof","wah","bam","ey","na","duh","mhm","woo")

server <- function(input, output) {
  
  # Using modules to access input dropdown
  wordlist <- callModule(dropdownserver,"words")
  album_data <- callModule(dropdownserver, "albums")
  album_data2 <- callModule(dropdownserver, "albums2")
  
  # Tidy lyrics data subset for plotting
  tidy_lyrics_plot <- reactive({
    tidy_lyrics_tok %>%
    filter(album == album_data()$albumname) %>%
    count(word, sort = TRUE) %>%
    head(100)
    })
  
################ Dashboard tab 1 
  
  output$text2 <- renderUI({
    HTML('The Ye Cloud <br><br><p>')}
  )
  
  # flipped column chart
  output$chart <- renderPlot({
    #req(fill..ko)
    if(input$rb == "Freq"){
      data <- tidy_lyrics_plot()
      ylabel <- "Frequency"
    }
    else{
      data <- tf_idf%>%
        filter(!(word %in% rm_words))%>%
        filter(album == album_data()$albumname)%>%
        select(-n)%>%
        rename(n = tf_idf)
      ylabel <- "TF-IDF"
    }
    
      data%>%    
      head(10) %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n)) +
      geom_col(fill= album_data()$album_fill, 
               color =album_data()$album_border) +
      xlab(NULL) +
      coord_flip()+
      geom_text(aes(label = reorder(word, n)), 
                hjust = "inward",
                color = "black", 
                size = 3.0
                )+
      labs(y = ylabel, 
           x = NULL
           #title = "Kanye's most frequent words in the album"
           )+
      theme_minimal()+
      theme(axis.text.y = element_blank(),
            panel.grid = element_line(color = album_data()$album_border),
            panel.grid.minor.y = element_blank(),
            panel.grid.major = element_blank() ,
            panel.spacing = unit(1, "cm"),
            panel.border = element_rect(fill = NA, colour = album_data()$album_border, size = 1),
            axis.ticks.x = element_line()
            )
  })
  
  
  output$wordcloud <- renderPlot({
    set.seed(42)
    tidy_lyrics_plot()%>%
      ggplot(aes(label = word, size = n)) +
      geom_text_wordcloud_area(
        color=album_data()$album_border
        ) +
      scale_radius(range = c(0, 20), limits = c(0, NA)) +
      theme_minimal()+
      theme(
        panel.border = element_rect(fill = NA, colour = album_data()$album_border, size = 1)
      )
  })
  
  
  output$myImage <- renderUI({
    tags$img(
      src= album_data()$sv_path,
      width = 150,
      height = 150
    )
  })
  
#####Dashboard Tab 2
  
  ### Column chart
  output$charttab2 <- renderPlot({
    user_wordcount(wordlist(), tidy_lyrics_tok)%>%
      mutate(album = fct_relevel(album, unique(kw_albums$album)))%>%
      ggplot(aes(album, n, fill = album)) +
      geom_col()+
      scale_fill_manual("legend",values=chart_config$album_fill)+
      labs(y = "Number of Uses", 
           x = "Albums",
           title = paste("How many times did Kanye use the word '",
                         wordlist(),
                         "' in each album"))+
      theme_minimal()+
      theme(axis.text.x = element_blank(),
            axis.ticks = element_blank())
  })
  
  # network viz
  output$network <- renderPlot({
    bigram_count <- bigrams_separated %>%
      filter(album == album_data2()$albumname)%>%
      filter(word1 == wordlist() | word2 == wordlist()) %>%
      count(word1, word2, sort = TRUE) 
    if (dim(bigram_count)[1] == 0){
      # what to do here
      print('')
    }
    else
      {
        bigram_graph <- bigram_count%>%
          graph_from_data_frame()
        
        set.seed(2016)
        
        a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
        
        ggraph(bigram_graph, layout = "fr") +
          geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                         arrow = a, end_cap = circle(.07, 'inches')) +
          geom_node_point(color = album_data2()$album_fill, size = 5) +
          geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
          theme_void()
      }
    
  })
  
  #####Dashboard Tab 3
  ye_sentiment$album <- factor(ye_sentiment$album, levels= unique(kw_albums$album))
  ### facet chart
  output$charttab3 <- renderPlot({
    ye_sentiment%>%
      ggplot(aes(reorder(track_title, sentiment), sentiment, fill = album)) +
      geom_col(show.legend = FALSE, width = 0.7) +
      facet_wrap(~album, ncol = 3, scales = "free")+
      labs(x = NULL,
           y = "Sentiment",
           title = "Kanye West's songs ranked by sentiment",
           caption = "")+
      #scale_y_continuous(name = "", limits = c(-1,1))+
      theme_minimal()+
      theme(axis.text.y = element_text(size = 4.5),
            axis.text.x = element_text(size = 4.5),
            strip.text = element_text(size = 6)
            #axis.
      )+
      coord_flip()
  })
  
   # output$var <- renderText({paste("You have selected",
   #                                 album_data2()$albumname,
   #                                 "as the Kanye West album you
   #                                   would like to analyse")})
  
}

#mask <-png::readPNG("www/dropoutbear.png")