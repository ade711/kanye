library(shinydashboard)
library(shinyWidgets)
source("dropdownModule.R")

# Raise thing mentioned in mastering shiny book about session$userdata
# add commentary or look into it - change title in if statement?


# move buttons to box? - done
# add space in wordcloud box -done
# change colour of whole board - done
# unit testing experiment - done


ui <- 
  # dashboard page
  dashboardPage(
    skin = "black",
    dashboardHeader(title = "The Kanyeboard"),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Intro", tabName = "Intro", 
                 icon = icon("dashboard")),
        menuItem("Top Words", tabName = "TopWords", 
                 icon = icon("dashboard")),
        menuItem("Words over Time", tabName = "WordsOverTime",
                 icon = icon("dashboard")),
        menuItem("Sentiment Analysis", tabName = "SentimentAnalysis",
                 icon = icon("dashboard"))
      )
    ),
    
    dashboardBody(
      tags$head(tags$style(HTML('
      .content-wrapper {
        background-color: #fff;
      }
    '
      ))),
      tabItems(
        tabItem(
          tabName = "Intro",
          fluidRow(
            
            h1("Welcome To The Ultimate Kanye West Analysis"),
             p("Kanye West is one of the most influential and critically lauded artists
               of the early 21st century. He went from hip-hop
               beatmaker to worldwide hitmaker as his production work 
               for artists such as Jay-Z led to a major-label recording 
               contract and, ultimately, a wildly successful solo career 
               that counted an unbroken string of chart-topping, 
               multi-platinum albums and nearly two dozen Grammy Awards"),
             p("--Artist Biography by Neil Z. Yeung"),
            p("This Shiny App is an attempt to take a look at the
              music behind the man"),
            img(src = "mascot_albums.png", 
                height = 200, width = 450),
        )
        ),
        
######## The second tab
        tabItem(
          tabName = "TopWords",
                 p("First we take a look at his use of words in 
                   each individual album. You can use the dropdown
                   to filter on which album you are interested in.
                   On the left are the Top 10 words and on the right
                   is a wordcloud of prominent words"),
          fluidRow(
          column(6,dropdownUI("albums", "Albums",
                              unique(kw_albums$album))
          ),
          
          column(6, uiOutput("myImage"))#,
          #column(1, offset =1,box("Info",collapsible = TRUE)),
          ),
          fluidRow(),
          fluidRow(
            box(
                radioButtons("rb","", c("Freq", "TFIDF"),
                             selected = "Freq",
                             inline = TRUE,
                             width = "50%"),
                plotOutput("chart"),
                width = 6,
                #background = "white"
                ),
            box(htmlOutput("text2"), 
                plotOutput("wordcloud"),
                status = "primary",
                #background = "white",
                  )
          )    
          #textOutput("var"),
              ),

######## The third tab
        tabItem(
          tabName = "WordsOverTime",
          p("Here you can use the words in the drop down list to
            see how Kanye has used it across his albums"),
  
          dropdownUI("words", "Kanye's Words", 
                     unique(tidy_lyrics_tok$word)),
          
            plotOutput("charttab2"),
          p("But the frequency of words is only so useful. Below 
            you can filter by album to see how the word is used
             in context. What words come before or after the fisrt
            word. The arrows indicate how the words are used."),

          dropdownUI("albums2", "Kanye's Words in Context", 
                     unique(kw_albums$album)),
          plotOutput("network"),
          #textOutput("var"),
          
            
              ),
### The fourth Tab
      tabItem(
        tabName = 'SentimentAnalysis',
        p("Sentiment analysis page"),
        plotOutput("charttab3")
        
      )
    
      )
    )
  )

