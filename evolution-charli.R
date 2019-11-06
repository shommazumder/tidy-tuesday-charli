####

####

rm(list = ls())

#### PACKAGES ####
library(spotifyr) #talk to spotify api
library(tidyverse) #tidy functions
library(stringr) #work with strings
library(ggridges) #ridge plots
library(hrbrthemes) #better gg themes
library(purrr) #functional programming
library(genius) #album lyrics
library(tidytext) #tidy text
library(quanteda) #functions to work with text
library(stm) #structural topic model (LDA)


#### SETUP ####

# set up authentication
source("spotify-credentials.R")

access_token <- get_spotify_access_token()


#### COLLECT DATA #####

charli_audio_features <- get_artist_audio_features(artist = "Charli XCX") %>%
  as_tibble()

#### CHECK SOME PLOTS ####
ggplot(charli_audio_features, aes(x = danceability, y = album_release_year)) + 
  geom_density_ridges(aes(y = as.factor(album_release_year))) + 
  theme_ipsum_rc(axis_title_size = 20,
                 plot_title_size = 20) +
  labs(title = "Charli XCX's Tracks Have Gotten More Hype Over Time...", 
       subtitle = "Distribution of song energy over time across album release years.",
       caption = "Based on energy variable pulled from Spotify's Web API with spotifyr",
       x = "Danceability",
       y = "Album Release Year")

ggplot(charli_audio_features, aes(x = danceability, y = valence)) + 
  geom_point(size = 5,shape = 21,fill = "dodgerblue") + 
  geom_smooth(method = "lm",color = "indianred") + 
  theme_ipsum_rc(axis_title_size = 20,
                 plot_title_size = 20) +
  labs(title = "Charli's Most Danceable Songs Are Also Her Most Positive Songs...", 
       caption = "Source: Spotify's Web API with spotifyr",
       x = "Danceability",
       y = "Valence")

# frequency of keys
ggplot(charli_audio_features, aes(x = key_name)) + 
  geom_bar() + 
  theme_ipsum_rc(axis_title_size = 20,
                 plot_title_size = 20) +
  labs(title = "Charli Tends to Perform Songs in C...", 
       subtitle = "Frequency of song keys across all albums.",
       caption = "Source: Spotify's Web API with spotifyr",
       x = "Song Key",
       y = "Count")

#### NOW GET LYRICS ####

# function to get lyrics (modified to allow for errors)
get_lyrics <- function(artist,song){

  # get lyrics
  out <- tryCatch(genius_lyrics(artist = artist, song = song),
                  error = function(e) NA)

  #collapse lines into one string
  #out_collapsed <- NA
  out_collapsed <- tryCatch(paste(out$lyric, sep="", collapse=" "),error = function(e) NA)
  return(out_collapsed)

  #return(out_collapsed)
}

# get lyrics
charli_audio_features <- charli_audio_features %>%
  mutate(lyrics = map2_chr(artist_name,track_name,get_lyrics))

#### PREPROCESS TEXT DATA ####

#convert to corpus object
charli_corpus <- corpus(charli_audio_features, 
                          docid_field = "track_name",
                          text_field = "lyrics")

#tokenize into unigrams
charli_corpus_dfm_unigram <- tokens(charli_corpus,
                                 remove_punct = TRUE,
                                 remove_numbers = TRUE) %>% 
  tokens_select(stopwords('en'), selection = 'remove') %>%
  dfm()

#### APPLY LDA ####

#estimate topic model
charli_topic_model <- stm(charli_corpus_dfm_unigram, 
                          verbose = TRUE, 
                          K = 4,
                          init.type = "Spectral",
                          prevalence = ~energy + danceability + valence + album_release_year)

td_beta_stm <- tidy(charli_topic_model)

# reordering function (from drlib)
reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}

# plot (code from Julia Silge)
td_beta_stm %>%
  mutate(topic_interp = case_when(
    topic == 1 ~ "Topic 1: Good Vibes",
    topic == 2 ~ "Topic 2: Na, Na, Na",
    topic == 3 ~ "Topic 3: I'm Alone",
    topic == 4 ~ "Topic 4: Love"
  )) %>%
  group_by(topic_interp) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic_interp))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic_interp, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  theme_ipsum_rc(axis_title_size = 20,
                 plot_title_size = 20) + 
  labs(x = NULL, y = expression(beta),
       title = "Charli Sings About Good Vibes, Bad Vibes, and Being in Love...",
       subtitle = "Different words are associated with different topics",
       caption = "Models estimated using structural topic modeling (variant on LDA).")
