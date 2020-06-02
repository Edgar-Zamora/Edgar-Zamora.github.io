library(tidyverse)
library(spotifyr)
library(knitr)
library(ggtext)
library(DT)


Sys.setenv(SPOTIFY_CLIENT_ID = 'your_client_id')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'your_client_secret')

access_token <- get_spotify_access_token()

fifa_playlist <- read_csv("fifa_music.csv") %>% 
  mutate(title = as.character(title),
         year = as.character(year)) %>% 
  rename(track_name = title,
         artist_name = artist) %>% 
  filter(track_name != "Kids",
         year >= "2002") %>% 
  group_by_all() %>% 
  summarise() %>% 
  ungroup()


track_info <- function(artist, title) {
  
  get_artist_audio_features({{artist}}) %>% 
    select(artist_name, track_name, album_release_year, danceability:tempo, duration_ms, key_mode) %>% 
    filter(track_name == {{title}})
}

#Prepaing artist and tracks as to be able to iterate over them
artists <- fifa_playlist %>% 
  slice(1:100) %>% 
  pull(artist_name)

tracks <- fifa_playlist %>% 
  slice(1:100) %>% 
  pull(track_name)


combining_data_set <- function(min_track, max_track){
  
  #Prepaing artist and tracks as to be able to iterate over them
  artists <- fifa_playlist %>% 
    slice({{min_track}}:{{max_track}}) %>% 
    pull(artist_name)
  
  tracks <- fifa_playlist %>% 
    slice({{min_track}}:{{max_track}}) %>% 
    pull(track_name)
  
  fifa_music_attributes <- fifa_playlist %>% 
    slice({{min_track}}:{{max_track}}) %>% 
    mutate(output = map2(artists, tracks, safely(track_info, 
                                                 otherwise = NA)))
  
  return(fifa_music_attributes)
  
}


fifa_music_attributes1 <- combining_data_set(1, 100)
fifa_music_attributes2 <- combining_data_set(101, 200)
fifa_music_attributes3 <- combining_data_set(201, 300)
fifa_music_attributes4 <- combining_data_set(301, 400)
fifa_music_attributes5 <- combining_data_set(401, 500)
fifa_music_attributes6 <- combining_data_set(501, 600)
fifa_music_attributes7 <- combining_data_set(601, 664)



complete_fifa_music <- rbind(fifa_music_attributes1, fifa_music_attributes2, fifa_music_attributes3, fifa_music_attributes4, fifa_music_attributes5, fifa_music_attributes6,
             fifa_music_attributes7)

#There might be more observations than the total becuase some tracks have two 
#observations which will be rememided through averaging both tracks

fifa_music_unnested <- complete_fifa_music %>% 
  unnest(output) %>% 
  mutate(error = if_else(is.na(output), "error", "none")) %>% 
  slice(seq(1, n(), by = 2)) %>% 
  rename(artist_name.x = artist_name,
         track_name.y = track_name) %>% 
  filter(error != "error") %>% #removing those tracks not found in the Spotify API (90)
  mutate(row_nums = map_dbl(output, nrow)) %>% 
  filter(row_nums > 0 ) %>% #removing those tracks that did not return attributes when though there was an error (253)
  unnest(cols = c(output))


#Averaging those tracks that have differing attributes.
fifa_music_averaged <- fifa_music_unnested %>% 
  group_by(artist_name.x, track_name.y, year) %>% 
  #print(n = Inf)
  summarise(
    danceability = mean(danceability),
    energy = mean(energy),
    loudness = mean(loudness),
    speechiness = mean(speechiness),
    acousticness = mean(acousticness),
    instrumentalness  = mean(instrumentalness),
    liveness = mean(liveness),
    tempo = mean(tempo),
    valence = mean(valence),
    duration_ms = mean(duration_ms)) %>%
  ungroup()

#Writing file
write_csv(fifa_music_averaged, "fifa_music_cleaned.csv")




