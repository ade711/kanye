## Initial shiny app
library(shiny)
library("tidyverse")
library("dplyr")
library(ggplot2)
library(wordcloud)
library(tidytext)


kw_albums <- readRDS("kw_albums.Rds")
tidy_lyrics_tok <- readRDS("tidy_lyrics_token.Rds")


# Define UI ----
ui <- 
    #navbarPage("My Application",
    #           tabPanel("Component 1"),
    #           tabPanel("Component 2"))
    
    fluidPage(
    
        column(3,offset = 4, titlePanel("The Kanye Analysis")),
        #column(3,offset = 4,img(src = "KanyeMadness.jpg", 
        #height = 100, width = 150)),
        
        column(3,offset = 4,selectInput(
            "albums",
            label = "Albums",
            choices = unique(kw_albums$album),
                )),
        uiOutput("myImage"),
        
    fluidRow(
       column(6, plotOutput("chart")),
       column(5, offset = 1, plotOutput("wordcloud"))
        #fluidRow(
        #    splitLayout(cellWidths = c("50%", "50%"), plotOutput("chart"),
        #                plotOutput("wordcloud") )
        #    )
    )    
    #textOutput("var"),
    
    
    
    
)

# Define server logic ----
server <- function(input, output) {
    
    albumname <- unique(kw_albums$album)
    sv_path <- c("the_college_dropout.jpg",
                 "Late_registration.jpg",
                 "Graduation.jpg",
                 "808s_Heartbreak.png",
                 "MBDTF.jpg",
                 "Yeezus.png",
                 "The_life_of_pablo.jpg",
                 "Ye.jpg",
                 "Jesus_Is_King.png")
    
    album_fill <- c("#e8982e", "#633119","#b25186",
                    "#b72124", "#ad2110","#fe0000",
                    "#f58b57", "#00FF00", "#0000fe")
    album_border <- c("#411218", "#7398aa", "#853082",
                      "#c2cece", "#ffd700", "#C0C0C0",
                      "#412517","#87ceeb","#ffd700")
    chart_config <- data.frame(albumname,sv_path, album_fill,
                               album_border)
    
    output$var <- renderText({paste("You have selected",
                                    input$albums,
                                    "as the Kanye West album you
                                    would like to analyse")})

    #fill <- reactiveVal({})
    
    output$chart <- renderPlot({
        
        #req(fill..ko)
        
        fil <- chart_config %>% filter(albumname== input$albums)
        tidy_lyrics_tok %>%
        filter(album == fil$albumname) %>%
        count(word, sort = TRUE) %>%
        head(10) %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(word, n)) +
        geom_col(fill= fil$album_fill, color =fil$album_border) +
        xlab(NULL) +
        coord_flip()+
        geom_text(aes(label = reorder(word, n)), 
                      hjust = 1.2,vjust = 0.3, color = "white", 
                      size = 5,  family="Harlow Solid Italic")+
        labs(y = "Number  of times mentioned", 
                 x = NULL,
                 title = "Kanye's most frequent words in the album")+
        theme_minimal()+
        theme(axis.text.y = element_blank())  
            })
    
    # Word cloud
   output$wordcloud <- renderPlot({
       fil <- chart_config %>% filter(albumname== input$albums)
       tidy_lyrics_tok %>%
        filter(album == fil$albumname) %>%
        count(word) %>%
        with(wordcloud(word, n, max.words = 100, min.freq = 5, fixed.asp = TRUE))
   })
   
    output$myImage <- renderUI({
        # this is repeated, can it be global?
        fil2 <- chart_config %>% filter(albumname== input$albums)
        tags$img(
            src= fil2$sv_path,
            width = 200,
            height = 200
                 )
    })
    
}
# Run the app ----
shinyApp(ui = ui, server = server)

