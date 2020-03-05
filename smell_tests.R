# install.packages("spotifyr")

# Installing from github (not from CRAN) to enable removing playlist function
# (From PR 105)
# devtools::install_github("https://github.com/charlie86/spotifyr")
library(spotifyr)
library(tidyverse)

# Authorisation ----
# Look up environment variables
# Sys.getenv("SPOTIFY_CLIENT_ID")
# Sys.getenv("SPOTIFY_CLIENT_SECRET")

readRenviron("../spotify_playlist/.Renviron")

# Get access token
access_token <- get_spotify_access_token()

scopes_needed <- c(
  # "user-library-read",
  # "user-library-modify",
  "playlist-read-private",
  "playlist-modify-public",
  "playlist-modify-private",
  "playlist-read-collaborative",
  # "user-read-recently-played",
  # # "user-top-read",
  # "user-read-private",
  # # "user-read-email",
  # "user-read-birthdate",
  "streaming",
  # "user-modify-playback-state",
  "user-read-currently-playing",
  # "user-read-playback-state",
  # "user-follow-modify",
  "user-follow-read")

# pass this to every function for authorization
access_token <- get_spotify_authorization_code(scope = scopes_needed)


# Smell tests ----

# Get user profile
me <- get_my_profile(access_token)
user_id <- me$id # this id is present in playlists list

# Has limit and offset set (default 20)
get_my_playlists(authorization = access_token, include_meta_info = F) -> my_playlists

my_playlists$name # playlist names
my_playlists$tracks.total # no. of tracks - can be useful for offsetting

my_playlists %>%
  filter(owner.id == user_id) -> playlists_created_by_me

# 37i9dQZF1DXcBWIGoYBM5M - today's top hits
# 2NWKDNyECPgF1lcQ3lozkA - test1 (created)
follow_playlist(playlist_id = "37i9dQZF1DXcBWIGoYBM5M", authorization = access_token)
unfollow_playlist(playlist_id = "37i9dQZF1DXcBWIGoYBM5M", authorization = access_token)

# Different auth than in other functions
tracks_from_playlist <- get_playlist_tracks(playlist_id = "37i9dQZF1DXcBWIGoYBM5M", authorization = access_token$credentials$access_token)

tracks_from_playlist <- as_tibble(tracks_from_playlist)
# Lots of variables (40)
names(tracks_from_playlist)

interesting_cols <-
  c("added_at",
    "added_by.id",
    "track.artists",
    "track.duration_ms",
    "track.id",
    "track.uri",
    "track.name",
    "track.popularity"
  )

tracks_from_playlist_stripped <- tracks_from_playlist[interesting_cols]
tracks_from_playlist_stripped
# artists info is nested as df


# Artist extraction as other column
collapse_sign <- ", "

tracks_from_playlist_stripped$track.artists %>%
  map_chr(function(x) paste0(x$name, collapse = collapse_sign)) -> coerced_artists

tracks_from_playlist_stripped$artists_simple <- coerced_artists

tracks_from_playlist_stripped$track.artists <- NULL
tracks_from_playlist_stripped

# add songs to playlist
songs_to_add <- tracks_from_playlist_stripped$track.uri[1:5]
add_tracks_to_playlist("2NWKDNyECPgF1lcQ3lozkA", songs_to_add, authorization = access_token)
# Max 100 songs, can pass positions into which insert to (!!!)


# Remove songs from playlist (my function, created by PR 105, not on CRAN yet)
remove_tracks_from_playlist("2NWKDNyECPgF1lcQ3lozkA", songs_to_add, authorization = access_token)


# Create playlist
created_playlist_info <- create_playlist(user_id, name = "test from API", authorization = access_token)

created_playlist_info # To get URI of new playlist etc.

# Watch out - it's possible to create two playlists with the same name

# Method for removing playlist not avaliable - use unfollow feature

# Get audio features for a playlist
playlist_audio_features <- get_playlist_audio_features(user_id, "2NWKDNyECPgF1lcQ3lozkA", authorization = access_token$credentials$access_token)



# Not run - access forbidden
# get_my_top_artists_or_tracks(type = 'tracks',
#                              time_range = 'short_term',
#                              limit = 5,
#                              authorization = access_token)



