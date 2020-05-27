library(spotifyr)
library(tidyverse)
source("funs.R")

# created_playlist_info <- create_playlist(user_id, name = "test_truncate", authorization = access_token)
# created_playlist_info$uri %>% dput()

scopes_needed <- c(
  "playlist-read-private",
  "playlist-modify-public",
  "playlist-modify-private",
  "playlist-read-collaborative")

# pass this to every function for authorization
access_token <- get_spotify_authorization_code(scope = scopes_needed)


# Get user profile
me <- get_my_profile(access_token)

observed_playlists <- get_my_playlists( authorization = access_token) %>%
  select(name, owner.display_name, id)

observed_playlists

# one has to pass username and playlist id 
playlist_username <- 'spotify'
# playlist_username <- 'Elo' - name of test user
playlist_uris <- c('37i9dQZF1DX3rxVfibe1L0', '37i9dQZF1DXcF6B6QPhFDv')


playlist_audio_features <- get_playlist_audio_features(playlist_username, playlist_uris, authorization = access_token$credentials$access_token)

playlist_audio_features 

playlist_audio_features_sliced <- playlist_audio_features[, 6:16]
playlist_audio_features_sliced <- playlist_audio_features_sliced[,-2]
playlist_audio_features_sliced 

# details of how to get artists names from nested format are in smell_tests.R file


