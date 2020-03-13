library(R6)

# Test R6 class creation - an object for storing user data
UserProfile <- R6Class("UserProfile", list(
  authorized = FALSE,
  access_token = NULL,
  user_id = NULL,
  scopes_needed = c(
    "playlist-read-private",
    "playlist-modify-public",
    "playlist-modify-private",
    "playlist-read-collaborative"
  ),
  authorize = function(scopes_needed = NULL){
    
    # pass this to every function for authorization
    self$access_token <-
      get_spotify_authorization_code(scope = scopes_needed)
    
    
    # Get user profile
    me <- get_my_profile(self$access_token)
    self$user_id <- me$id
    
    self$authorized <- TRUE
  }
  
  )
)

a <- UserProfile$new()
a$authorized
a$access_token

a$authorize()
a$authorized
a$access_token




