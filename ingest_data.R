# Kanye lyric and album analysis

# Ingest data from genius.com

library("genius")
library("tidyverse")
library("dplyr")
library("tidygraph")
library(igraph)
library(ggraph)
library(tidytext)
library(spotifyr)

#Sys.setenv(SPOTIFY_CLIENT_ID = '')
#Sys.setenv(SPOTIFY_CLIENT_SECRET = '')

access_token <- get_spotify_access_token()


# List of relevant albums
#album_lyrics = list()
album_lyrics <- vector(mode = "list", length = 9)

artist_name <- "Kanye West"

names = c('The College Dropout', 'Late Registration', 
         'Graduation','808s & Heartbreak', 
         'My Beautiful Dark Twisted Fantasy',
         'Yeezus', 'The Life of Pablo', 'Ye', 
         'Jesus Is King')

tracklist <- genius_tracklist("Kanye West", "The College Dropout")
faillist <- kw1%>% count(track_n, track_title)%>% filter(n<5)

# get rid of some tracks to add manually or leave out
faillist <- faillist %>% filter(track_n != 17)

kw1_fails <- map2_dfr(.f = genius_lyrics, 
                     .x = "Kanye West", 
                     .y = gsub("\\s*\\([^\\)]+\\)","",
                               as.character(faillist$track_title))
                     )

kw1_fails <- merge(kw1_fails, tracklist)

kw_fails <- kw1_fails[,-c(5,7)]
rbind(kw1, kw1_fails)


kw1 <- genius_album(artist = artist_name, 
                    album = "The College Dropout") %>% mutate(album = "The College Dropout")
kw2 <- genius_album(artist = artist_name, 
                    album = "Late Registration") %>% mutate(album = "Late Registration")
kw3 <- genius_album(artist = artist_name, 
                    album = "Graduation") %>% mutate(album = "Graduation")
kw4 <- genius_album(artist = artist_name, 
                    album = "808s - Heartbreak") %>% mutate(album = "808s & Heartbreak")
kw5 <- genius_album(artist = artist_name, 
                    album = "My Beautiful Dark Twisted Fantasy") %>% mutate(album = "My Beautiful Dark Twisted Fantasy")
kw6 <- genius_album(artist = artist_name, 
                    album = "Yeezus") %>% mutate(album = "Yeezus")
kw7 <- genius_album(artist = artist_name, 
                    album = "The Life of Pablo") %>% mutate(album = "The Life of Pablo")
kw8 <- genius_album(artist = artist_name, 
                    album = "Ye") %>% mutate(album = "Ye")
kw9 <- genius_album(artist = artist_name, 
                    album = "Jesus Is King") %>% mutate(album = "Jesus Is King")

#Putting all together in the same df
kw_albums <- rbind(kw1, kw2, kw3, kw4, kw5, kw6, kw7 ,kw8, kw9)

#save
#saveRDS(kw_albums, file = "kw_albums2.Rds")


# Clean the data

# drop na
kw_albums <- na.omit(kw_albums)

# unnest lyrics into individual word tokens

lyrics_tok <- kw_albums %>% unnest_tokens(word, lyric)

# Remove generic stopwords - Might need to revsit this
data("stop_words")
kanye_words <- data.frame(
  word = c("yeah", "mhm", "hey", "la", "ey", "
           dem", "yo", "ya", "gon", "gonna"),
                lexicon= c("Kanye_words"))

stop_words <- rbind(stop_words, kanye_words)

tidy_lyrics_tok <- lyrics_tok %>%
  anti_join(stop_words)

less_5 <- tidy_lyrics_tok %>% 
  count(word) %>% 
  filter(n < 5)

tidy_lyrics_tok <- tidy_lyrics_tok %>%
  anti_join(less_5)

saveRDS(tidy_lyrics_tok, file = "tidy_lyrics_token.Rds")

######################### Get bigrams
# Use lyrics before removing stop words

lyrics_bigram <- kw_albums %>%
  unnest_tokens(bigram, lyric, token = "ngrams", n=2)

# Looking at count of bigrams we see the top is dominated with 
# uninteresting stopwords
lyrics_bigram %>% 
  count(bigram, sort = TRUE)

# It's worth removing these like we did in the indiviudal tokens 
# But first its worth separating the words in the column into 2

bigrams_separated <- lyrics_bigram %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# filter stopwords

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)


bigram_graph <- bigrams_separated %>%
  filter(album == "Yeezus")%>%
  filter(word1 == "love" | word2 == "love") %>%
  count(word1, word2, sort = TRUE) %>%
  graph_from_data_frame()

### Plot the bigrams in a graph
set.seed(2016)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


##### TF IDF 
lyrics <- lyrics_tok%>% 
  count(album, word, sort = TRUE)

total_lyrics<- lyrics%>%
  group_by(album)%>%
  summarize(total = sum(n))

lyrics <- left_join(lyrics, total_lyrics)

ggplot(lyrics, aes(n/total, fill = album)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~album, ncol = 2, scales = "free_y")

tf_idf<-lyrics %>%
  bind_tf_idf(word, album, n)%>%
  arrange(desc(tf_idf))

# save tf_idf data
saveRDS(tf_idf, file = "tf_idf.Rds")

######## Other descriptive stats

# Lexical diversity - Number of unique words

tidy_lyrics_tok%>%
  group_by(album)%>%
  summarise(lex_div = n_distinct(word))%>%
  arrange(desc(lex_div))%>%

# Lexical density - unique words by total number of words
## As lexical density increases, repetition decreases
## hypothesis is the 'poppier' the album - the higher the lex density
## worth comparing with spotify data

lyrics_tok%>%
  group_by(album)%>%
  summarise(lex_dense = n_distinct(word)/n())%>%
  arrange(desc(lex_dense))%>%
  ggplot(aes(album, lex_dense), fill=album)+
  geom_col()

###### Sentiment Analysis

nrc <- nrc%>%
  filter(!sentiment %in% c('anticipation', 'surprise'))%>%
  mutate(sentiment_ = ifelse(sentiment %in% c("trust","positive","joy"),
                             "positive", "negative"))%>%
  select(-sentiment)%>%
  rename(sentiment_ , sentiment)

names(nrc)[names(nrc)== 'sentiment_']<- 'sentiment'
combo <- rbind (nrc, bing)
combo <- combo%>%distinct()
conflict_dupes <- combo$word[combo$word%>%duplicated() == TRUE]
combo <- combo%>%subset(!word %in% conflict_dupes)
saveRDS(combo, file = "nrc_bing_vocab.Rds")


                      
test_sentiment <- tidy_lyrics_tok%>%
  filter(album == "808s & Heartbreak", track_n == 11)%>%
  left_join(combo)%>% 
  count(album, track_title, sentiment)%>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment_per_word = (positive - negative)/(positive+negative))

ye_sentiment <- tidy_lyrics_tok%>%
  left_join(combo)%>% 
  count(album, track_title, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)%>%
  mutate(sentiment_per_word = (positive - negative)/(positive+negative))

ye_sentiment$album <- factor(ye_sentiment$album, levels= unique(kw_albums$album))

ye_sentiment%>%
  ggplot(aes(reorder(track_title, sentiment_per_word), sentiment_per_word, fill = album)) +
  geom_col(show.legend = FALSE, width = 0.2) +
  facet_wrap(~album, ncol = 3, scales = "free")+
  labs(x = NULL,
       y = "Sentiment",
       title = "Kanye West's songs ranked by sentiment",
       caption = "")+
  theme_minimal()+
  theme(axis.text.y = element_text(size = 4.5),
        axis.text.x = element_text(size = 4.5),
        axis.title  = element_text(size = 1)
        )+
  coord_flip()


##### Spotify Data

spotify_ye <- get_artist_audio_features('kanye west')%>%
  filter(tolower(album_name) %in% tolower(kw_albums$album))%>%
  select(6,7,9,10,11,12,13,14,15,16,17,18,19,22,23,26,27,30,32,36,37,38,39)%>%
  filter(!(track_name %in% 
                      spotify_ye$track_name[duplicated(spotify_ye$track_name)] &
                      explicit == 'FALSE'))


###########################################################################
# Visualise top words of all albums
library(ggplot2)
library(wordcloud)

#plot <- 
#  ggplot(subset(tidy_lyrics_tok, tidy_lyrics_tok$album==album[i]),

tidy_lyrics_tok %>%
  filter(album == "The College Dropout") %>%
  count(word, sort = TRUE) %>%
  head(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill= "#e8982e", color ="#411218") +
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



tidy_lyrics_tok %>%
  filter(album == "Late Registration") %>%
  count(word, sort = TRUE) %>%
  head(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill= "#e8982e", color ="#411218") +
  xlab(NULL) +
  coord_flip()

# Word cloud
tidy_lyrics_tok %>%
  filter(album == "Late Registration") %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

# Function that takes user inputted word 
#then returns df with album and wordcount

user_wordcount <- function(user_word, lyrics_data){
  df <- lyrics_data %>% 
    count(album, word) %>% 
    complete(album, word, fill =list(n=0)) %>% 
    filter(word == user_word)
  return(df)
}

user_wordcount('kanye', tidy_lyrics_tok)%>%
  ggplot(aes(album, n)) +
  geom_col()+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))

# make bars interactive? put into new shiny tab
# unit testing the function?
#