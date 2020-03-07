library(spotifyr)
library(tidyverse)
source("funs.R")

created_playlist_info <-
  create_playlist(user_id, name = "test_copy_foreign", authorization = access_token)
created_playlist_info$uri %>% dput()

scopes_needed <- c(
  "playlist-read-private",
  "playlist-modify-public",
  "playlist-modify-private",
  "playlist-read-collaborative"
)

# pass this to every function for authorization
access_token <-
  get_spotify_authorization_code(scope = scopes_needed)


# Get user profile
me <- get_my_profile(access_token)
user_id <- me$id

my_playlist <- "5qTv1VVxfSMz3lsLZRzWAX"
foreign_playlist <- "37i9dQZF1DX5T2mzbF9W6j"


foreign_playlist_data <- get_playlist(
  foreign_playlist,
  authorization = access_token$credentials$access_token ,
  fields = c("description", "name", "tracks.items.track.uri")
)
foreign_tracks_uris <- foreign_playlist_data$tracks$items$track.uri

add_tracks_to_playlist(my_playlist,
                       foreign_tracks_uris,
                       authorization = access_token)



check_args("aaa", "ddd", T, "user")


# stop
check_args("aaa", 3, T, "user")
check_args("aaa", NULL, T, "user")
check_args("aaa", NULL, F, NULL)

# Test appending
fork_foreign_playlist(foreign_playlist,
                      my_playlist,
                      append = TRUE,
                      authorization = access_token)

truncate(my_playlist, access_token)


# Test copying
fork_foreign_playlist(foreign_playlist,
                      my_playlist,
                      append = FALSE,
                      authorization = access_token,
                      user_id = user_id)

truncate(my_playlist, access_token)
change_playlist_details(
  my_playlist,
  name = "test_copy_foreign",
  description = "a",
  authorization = access_token
)

# Test new playlist creation - not run
fork_foreign_playlist(foreign_playlist,
                      # my_playlist,
                      # append = FALSE,
                      authorization = access_token,
                      user_id = user_id)



previous_tracks <- truncate(playlist_uri, access_token)

add_tracks_to_playlist(playlist_uri, previous_tracks, authorization = access_token)
