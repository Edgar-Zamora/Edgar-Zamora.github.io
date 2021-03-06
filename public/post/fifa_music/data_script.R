library(tidyverse)
library(spotifyr)
library(knitr)
library(ggtext)
library(DT)


Sys.setenv(SPOTIFY_CLIENT_ID = '9d31a6fe89fc4bbda898cd726a12d593')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '79c0d581fa314b999d6f747e380dd412')


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

fifa_playlist %>% 
  count(year)


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


font_import('EA Font v1.5 by Ghettoshark')



fifa_music_attributes1 <- combining_data_set(1, 100)
fifa_music_attributes2 <- combining_data_set(101, 200)
fifa_music_attributes3 <- combining_data_set(201, 300)
fifa_music_attributes4 <- combining_data_set(301, 400)
fifa_music_attributes5 <- combining_data_set(401, 500)
fifa_music_attributes6 <- combining_data_set(501, 600)
fifa_music_attributes7 <- combining_data_set(601, 664)



zzz <- rbind(fifa_music_attributes1, fifa_music_attributes2, fifa_music_attributes3, fifa_music_attributes4, fifa_music_attributes5, fifa_music_attributes6,
             fifa_music_attributes7)

#There might be more observations than the total becuase some tracks have two observations which will be rememided through averaging both tracks
www <- zzz %>% 
  unnest(output) %>% 
  mutate(error = if_else(is.na(output), "error", "none")) %>% 
  slice(seq(1, n(), by = 2)) %>% 
  rename(artist_name.x = artist_name,
         track_name.y = track_name) %>% 
  unnest(output)


www


vvv <- www %>% 
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


vvv %>% 
  filter(!is.na(artist_name)) %>% 
  ungroup() %>% 
  ggplot(aes(year, danceability)) + 
  geom_boxplot() +
  geom_point(alpha = .2) +
  #scale_y_continuous(limits = c(0, 1)) +
  theme(
    panel.grid.major = element_blank(),
    panel.background = element_blank())


options(max.print=1000000)
font_import()
fonttable()


