scopes_needed <- c(
  "playlist-read-private",
  "playlist-modify-public",
  "playlist-modify-private",
  "playlist-read-collaborative"
)
r <- new.env()

# pass this to every function for authorization
r$access_token <-
  get_spotify_authorization_code(scope = scopes_needed)


# Get user profile
me <- get_my_profile(r$access_token)
r$user_id <- me$id

r$AUTHORIZED <- TRUE


if (r$AUTHORIZED) {
  get_my_playlists(authorization = r$access_token,
                   include_meta_info = F)
} else {
  NULL
}



get_my_playlists(authorization = r$access_token,
                 include_meta_info = F) %>% as_tibble() -> my_playlists


my_playlists %>%
  filter(owner.id == r$user_id) -> playlists_created_by_me


playlists_created_by_me %>%
  select(name)


display_my_playlists <- function()



