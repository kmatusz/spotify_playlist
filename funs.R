truncate <- function(playlist_id, authorization){
  # Remove all songs from playlist but keep the playlist.
  # Returns invisibly list of previous track uri's
  
  # Check if playlist id is char with length 1 
  stopifnot(is.character(playlist_id))
  stopifnot(length(playlist_id) == 1)
  
  # Check if token correct
  if (!inherits(authorization, "R6")) stop("authorization method is incorrect (not Token)")
  
  # 
  previous_tracks <- get_all_playlist_tracks(playlist_id = playlist_id, 
                                         authorization = authorization$credentials$access_token)
  
  if (length(previous_tracks) == 0){
    warning("There were no tracks on the playlist")
    return(invisible(NULL))
  }
  
  previous_tracks_uris <- previous_tracks$track.uri
  
  if (is.null(previous_tracks_uris)){
    warning("There were no tracks on the playlist")
    return(invisible(NULL))
  }
  
  
  remove_tracks_from_playlist(playlist_id, 
                              previous_tracks_uris, 
                              authorization = authorization)
  
  invisible(previous_tracks_uris)
}

union_playlists <- function(foreign_playlist_uris,
                            my_playlist_uri,
                            authorization =
                              spotifyr::get_spotify_authorization_code()) {
  # Function copies speicified multiple playlists to one playlist created by the user 
  # foreign_playlist_uris - from which to copy
  # my_playlist_uri - use existing user playlist
  
  if (length(foreign_playlist_uris) == 0){
    stop("No playlists to copy from")
  }
  
  for (i in 1:length(foreign_playlist_uris)){
    fork_foreign_playlist(foreign_playlist_uris[i],
                          my_playlist_uri,
                          append = TRUE,
                          authorization = authorization)
  }
  
}


fork_foreign_playlist <- function(foreign_playlist_uri,
                                  my_playlist_uri = NULL,
                                  append = FALSE,
                                  user_id = NULL,
                                  authorization =
                                    spotifyr::get_spotify_authorization_code()) {
  # Foreign playlist - from which to copy
  # my_playlist_uri - if not null, use existing user playlist
  # append - if true, append tracks from other playlist to existing playlist
  # if false, truncate all songs and replace playlist details (description, name)
  check_args(foreign_playlist_uri,
             my_playlist_uri,
             append,
             user_id)
  
  # Get details of foreign playlist - name, desc, tracks
  foreign_playlist_data <- get_all_playlist_tracks(
    foreign_playlist_uri,
    authorization = authorization$credentials$access_token)
  
  foreign_playlist_metadata <- get_playlist(
    foreign_playlist_uri,
    authorization = authorization$credentials$access_token ,
    fields = c("description", "name")
  )
  
  foreign_tracks_uris <-
    foreign_playlist_data$track.uri
  
  if (length(foreign_tracks_uris) == 0) {
    warning("No tracks on foreign playlist. Aborting")
    return(NULL)
  }
  
  # if new playlist,
  # populate with songs
  # return new_playlist ur
  # if old playlist and append
  # add tracks

  if (is.null(my_playlist_uri)){
    created_playlist_info <- create_playlist(user_id = user_id, 
                                             name = foreign_playlist_metadata$name,
                                             description = foreign_playlist_metadata$description,
                                             authorization = authorization
    )
    
    uris_chunked <- split(foreign_tracks_uris, ceiling(seq_along(foreign_tracks_uris)/50))
    for (chunk_of_uris in uris_chunked) {
      # browser()
      add_tracks_to_playlist(created_playlist_info$id, chunk_of_uris, authorization = authorization)
      Sys.sleep(1)
    }
    # add_tracks_to_playlist(created_playlist_info$id, foreign_tracks_uris, authorization = authorization)
    
    return(invisible(foreign_tracks_uris))
  }
  
  
  # if old and append
  if (append) {
      uris_chunked <- split(foreign_tracks_uris, ceiling(seq_along(foreign_tracks_uris)/50))
      for (chunk_of_uris in uris_chunked) {
        add_tracks_to_playlist(my_playlist_uri, chunk_of_uris, authorization = authorization)
        Sys.sleep(1)
      }
    return(invisible(foreign_tracks_uris))
  }
  
  
  # if old and not append
  # truncate playlist
  # add tracks
  # replace details
  if (!is.null(my_playlist_uri) && !append) {
    suppressWarnings(truncate(my_playlist, authorization))
    
    # use pagination
    uris_chunked <- split(foreign_tracks_uris, ceiling(seq_along(foreign_tracks_uris)/50))
    for (chunk_of_uris in uris_chunked) {
      add_tracks_to_playlist(my_playlist_uri, chunk_of_uris, authorization = authorization)
      Sys.sleep(1)
    }
    
    change_playlist_details(
      my_playlist_uri,
      name = foreign_playlist_data$name,
      description = foreign_playlist_data$description,
      authorization = authorization
    )
    return(invisible(foreign_tracks_uris))
    
    
  }
  
  
}

check_args <- function(foreign_playlist_uri,
                       my_playlist_uri,
                       append,
                       user_id) {
  # Defensive checks for forking playlist function
  # check if both are character with length 1
  stopifnot(is.character(foreign_playlist_uri))
  stopifnot(is.character(my_playlist_uri) ||
              is.null(my_playlist_uri))
  
  stopifnot(length(foreign_playlist_uri) == 1)
  stopifnot((length(my_playlist_uri) == 1) ||
              is.null(my_playlist_uri))
  
  if (is.null(my_playlist_uri) && append) {
    stop("To append, my_playlist_uri must be specified")
  }
  
  if (!append && is.null(user_id) && is.null(my_playlist_uri)) {
    stop("user_id needed when creating new playlist (append = FALSE)")
  }
  
  
  
  
  # if my_playlist_uri null and append = TRUE, warning
  if (is.null(my_playlist_uri) && append) {
    warning("Append option selected but no playlist uri specified.
            Append option will be ignored")
  }
  
  
}



shuffle_pernamently <- function(playlist_id, authorization){
  # Shuffle the order of the songs on a playlist
  previous_tracks <- get_all_playlist_tracks(playlist_id = playlist_uri, 
                                         authorization = access_token$credentials$access_token)
  if (!inherits(previous_tracks, "data.frame")){
    stop("not a data.frame")
  }
  
  if(nrow(previous_tracks) ==0){
    warning("No songs in playlist")
    return(NULL)
  }
  
  previous_tracks <- previous_tracks$track.uri
  new_order <- sample(previous_tracks)
  
  truncate(playlist_uri, access_token)
  
  
  # add_tracks_to_playlist(playlist_uri, new_order, authorization = access_token)
  
  uris_chunked <- split(new_order, ceiling(seq_along(new_order)/50))
  for (chunk_of_uris in uris_chunked) {
    add_tracks_to_playlist(my_playlist_uri, chunk_of_uris, authorization = authorization)
    Sys.sleep(1)
  }
  
  invisible(previous_tracks)
}

get_playlists_names_uri <- function(authorization, user_id, return_only_owned = TRUE){
  # Returns a vector of playlists names. 
  get_my_playlists(authorization = authorization,
                   include_meta_info = F) %>% 
    as_tibble() -> my_playlists
  
  if(nrow(my_playlists) == 0){
    warning("No playlists avaliable")
    return(c())
  }
  
  if (return_only_owned){
    my_playlists %>%
      filter(owner.id == user_id) -> playlists_created_by_me
    setNames(playlists_created_by_me$id, playlists_created_by_me$name)
    
  } else {
    setNames(my_playlists$id, my_playlists$name)
  }
  
}

get_songs_from_playlist_to_display <- function(authorization, playlist_id){
  
  tracks_from_playlist <- get_all_playlist_tracks(playlist_id = playlist_id, 
                                              authorization = authorization$
                                                credentials$access_token)
  tracks_from_playlist <- as_tibble(tracks_from_playlist)
  
  if(nrow(tracks_from_playlist) == 0){
    warning("No songs avaliable")
    return(tibble(Name = character(), Artists = character()))
  }
  
  interesting_cols <-
    c(
      "track.name",
      "track.artists",
      "track.uri"
    )
  
  tracks_from_playlist_stripped <- tracks_from_playlist[interesting_cols]
  
  # Artist extraction as other column
  collapse_sign <- ", "
  
  tracks_from_playlist_stripped$track.artists %>%
    map_chr(function(x) paste0(x$name, collapse = collapse_sign)) -> coerced_artists
  
  tracks_from_playlist_stripped$artists_simple <- coerced_artists
  
  tracks_from_playlist_stripped$track.artists <- NULL
  names(tracks_from_playlist_stripped) <- c("Name","track_uri","Artists")
  
  tracks_from_playlist_stripped
}



get_audio_features_for_playlists <- function(authorization, playlist_id = NULL){
  # obtain audio features for all playlists that logged user is observing/has created. 
  # if playlist_id is passed, features are obtained for this playlist only.
  
  
  observed_playlists <- get_my_playlists( authorization = authorization) %>%
    select(name, owner.display_name, id)
  
  if(!is.null(playlist_id)){
    observed_playlists <- observed_playlists %>% 
      filter(id == playlist_id)
  }
  
  
  all_playlists_features <- vector('list', nrow(observed_playlists))
  for (i in 1:nrow(observed_playlists)){
    tryCatch({
      all_playlists_features[[i]] <- get_playlist_audio_features(observed_playlists[i,2], 
                                                                 observed_playlists[i,3], 
                                                                 authorization = authorization$credentials$access_token)
    }, error = function(e) {
      message(paste0('Error during getting audio features for playlist with id: ', 
                     observed_playlists[i,3]))
    }
    )
  }
  
  all_playlists_features %>% 
    bind_rows() 
}

get_all_playlist_tracks <- function(playlist_id, authorization) {
  # Wrapper for function get_playlist_tracks, to account for pagination of results 
  # done by API
  res <- get_playlist_tracks(playlist_id, 
                             authorization = authorization, 
                             offset = 0)
  
  for (i in 1:100){
    tryCatch({
      temp = get_playlist_tracks(playlist_id, 
                                 authorization = authorization, 
                                 offset = i*100)
    })
    
    if (is.null(nrow(temp)) || nrow(temp)==0) break()
    res = rbind(res, temp)
  }
  
  res
}


