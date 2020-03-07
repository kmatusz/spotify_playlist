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
user_id <- me$id

tracks_uris <- c(
  "spotify:track:15jdGVFP05yosJLbvUl3rn", 
  "spotify:track:66K8qooiMtrqctlqMQbyGG", 
  "spotify:track:4jCBTqp4F1KVjSo8rHC6pp",
  "spotify:track:1VKcBxn9HKaSiZVKiiCvAd", 
  "spotify:track:3NKBHNnJaiSVMxsQBV5j6Z")

playlist_uri <- "4D0mJHOTSpzrHIV39VoTb3"

add_tracks_to_playlist(playlist_uri, 
                       tracks_uris, 
                       authorization = access_token)


# Test truncating playlist ----
# Error
truncate(1, access_token)
truncate(c("a", "b"), access_token)

truncate(playlist_uri, "a")

# warnings

# setup - add songs and remove twice. Second time should give a warning
add_tracks_to_playlist(playlist_uri, tracks_uris, authorization = access_token)
truncate(playlist_uri, access_token)
truncate(playlist_uri, access_token)


# Test forking playlist from other user ----

my_playlist <- "5qTv1VVxfSMz3lsLZRzWAX"
foreign_playlist <- "37i9dQZF1DX5T2mzbF9W6j"


# ok
check_args("aaa", "ddd", T, "user")

check_args(foreign_playlist_uri = "a",
           my_playlist_uri = "b",
           append = T,
           user_id = NULL)

check_args(foreign_playlist_uri = "a",
           my_playlist_uri = "b",
           append = F,
           user_id = NULL)


check_args(foreign_playlist_uri = "a",
           my_playlist_uri = NULL,
           append = F,
           user_id = "a")


# stop
check_args("aaa", 3, T, "user")
check_args("aaa", NULL, T, "user")
check_args("aaa", NULL, F, NULL)

# Test appending existing playlist
fork_foreign_playlist(foreign_playlist,
                      my_playlist,
                      append = TRUE,
                      authorization = access_token)

# clean-up
truncate(my_playlist, access_token)


# Test copying playlist to existing (truncate, replace name, add new songs)
fork_foreign_playlist(foreign_playlist,
                      my_playlist,
                      append = FALSE,
                      authorization = access_token,
                      user_id = user_id)

# clean-up
truncate(my_playlist, access_token)
change_playlist_details(
  my_playlist,
  name = "test_copy_foreign",
  description = "a",
  authorization = access_token
)

# Test new playlist creation - don't run too often !
# fork_foreign_playlist(foreign_playlist,
#                       # my_playlist,
#                       # append = FALSE,
#                       authorization = access_token,
#                       user_id = user_id)


# Shuffle playlist ----

playlist_uri <- "3z04SRqhVPAcU97TEHSioz"

shuffle_pernamently(playlist_uri, access_token)


