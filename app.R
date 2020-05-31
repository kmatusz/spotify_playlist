library(spotifyr)
library(tidyverse)
library(shiny)
library(DT)
library(corrplot)
library(sortable)
library(shinythemes)
library(shinyjs)

source("funs.R", encoding = "UTF-8")
# source("playlist_features_poc.R")



ui <- navbarPage(
  
  theme = shinytheme("cerulean"),
  "Hello Spotify!",
  
  tabPanel("One playlist",
           sidebarLayout(
             sidebarPanel(
               uiOutput("dynamic_playlist_selector")
             ),
             mainPanel(
               tags$div(
                 "Manage songs from one playlist"
               ),
               DT::dataTableOutput("songs_from_selected_playlist"),
               DT::dataTableOutput("my_playlists"),
               actionButton("copy_songs", "Copy", class = "btn-primary"),
               actionButton("delete_songs", "Delete", class = "btn-danger"),
               actionButton("save_changes", "Save changes", class = "btn-success")
             )
           )),
  tabPanel("All playlists",
           shinyjs::useShinyjs(),
           fluidPage(
             fluidRow(
               h4('My playlists')
             ),
             fluidRow(
               column(4,
                      fluidRow("Click on some of the playlist on the right to highlight options"),
                      fluidRow(actionButton("ap_copy_all_songs", "Copy all songs from selected playlists", class = "btn-primary"),style='padding:5px;'),
                      fluidRow(actionButton("ap_delete_playlists", "Delete selected playlists", class = "btn-primary"),style='padding:5px;'),
                      fluidRow(actionButton("ap_new_playlist", "Create new playlist", class = "btn-primary"),style='padding:5px;')
               ),
               column(6,
                      DT::dataTableOutput("ap_all_playlists_my")
               )
             ),
             fluidRow(
               h4('Observed playlists')
             ),
             fluidRow(
               column(4,
                      fluidRow(actionButton("ap_fork_playlist", "Fork", class = "btn-primary")),
               ),
               column(6,
                      DT::dataTableOutput("ap_all_playlists_followed")
               )
             ),
           )
  ),
  
  tabPanel(
    "Visualization",
    fluidPage(
      titlePanel("Visualization of song atributes from our playlist"),
      fluidRow(
        column(4, uiOutput("report_dynamic_playlist_selector")),
        column(4, uiOutput('dynamic_report_download'))
      ),
      fluidRow(
        column(6,plotOutput(outputId="myPlot1",height="500px")),
        column(6,
               fluidRow(plotOutput(outputId="myPlot2",height="250px")),
               fluidRow(plotOutput(outputId="myPlot3",height="250px"))
        ),
        column(6,plotOutput(outputId="myPlot6",height="1000px")),
        column(6,
               fluidRow(plotOutput(outputId="myPlot4",height="500px")),
               fluidRow(plotOutput(outputId="myPlot5",height="500px"))
        ),
        
      )
    )
  )
  
  
)



server <- function(input, output, session) {
  
  # Authorization ----
  r <- reactiveValues(
    AUTHORIZED = FALSE,
    access_token = NULL,
    user_id = NULL,
    my_playlists_audio_features = NULL
  )
  
  showModal(
    modalDialog(
      title = "Authorization",
      "Welcome to advanced Spotify playlist manager.
    Click the button below to log in on your Spotify account.
    Don't worry, we won't gather your password or any personal data.
    ",
      
      easyClose = FALSE,
      footer = actionButton("authorize", "Log in")
    )
  )
  
  observeEvent(input$authorize, {
    message("AUTHORIZATION STARTED")
    scopes_needed <- c(
      "playlist-read-private",
      "playlist-modify-public",
      "playlist-modify-private",
      "playlist-read-collaborative"
    )
    
    # pass this to every function for authorization
    r$access_token <-
      get_spotify_authorization_code(scope = scopes_needed)
    
    
    # Get user profile
    me <- get_my_profile(r$access_token)
    r$user_id <- me$id
    
    r$AUTHORIZED <- TRUE
    
    removeModal()
    message("AUTHORIZATION ENDED")
  })
  
  # Visualisation tab -----
  
  # select for currently shown playlist
  output$report_dynamic_playlist_selector <- renderUI({
    if (r$AUTHORIZED) {
      selectizeInput(
        "report_playlist_selector",
        label = "Choose playlist for which you want to see the visualisation",
        
        choices = c(
          # I couldn't set it as NA or NULL, as javascript is converting these to character "NA"
          c('Select playlist' = 'NOT_RUN'),
          get_playlists_names_uri(r$access_token,
                                  r$user_id,
                                  return_only_owned = F)
        )
      )
    }
    
  })
  
  report_playlist_selector <- reactive({
    # encapsulate input, when equals to NOT_RUN the reactive becomes NULL
    if (is.null(input$report_playlist_selector) || input$report_playlist_selector == 'NOT_RUN') {
      return(NULL)
    }
    input$report_playlist_selector
  })
  
  # my_playlists_audio_features - dynamic data frame
  my_playlists_audio_features <- reactive({
    if (is.null(report_playlist_selector())) {
      return(NULL)
    }
    withProgress(message = 'Obtaining songs features', value = 0, {
      get_audio_features_for_playlists(r$access_token, playlist_id = report_playlist_selector())
    })
  })
  
  playlist_audio_features_sliced <- reactive({
    if (is.null(my_playlists_audio_features())) {
      return(NULL)
    }
    playlist_audio_features_sliced <-
      my_playlists_audio_features()[, 6:16]
    playlist_audio_features_sliced <-
      playlist_audio_features_sliced[,-2]
    
    playlist_audio_features_sliced
    
  })
  
  playlist_audio_features <- reactive({
    if (is.null(my_playlists_audio_features())) {
      return(NULL)
    }
    playlist_audio_features<-
      my_playlists_audio_features()
    
    playlist_audio_features
    
  })
  
  
  # Visualize atributes
  
  output$myPlot1 <- renderPlot({
    
    if (is.null(playlist_audio_features_sliced())) {
      return(NULL)
    }
    
    playlist_audio_features_sliced() %>%
      gather(Features, value, 1:10) %>%
      ggplot(aes(x = value, fill = Features)) +
      geom_histogram(colour = "black", show.legend = FALSE) +
      facet_wrap(~ Features, scales = "free_x") +
      labs(x = "Values", y = "Frequency",
           title = "Song Features - Histograms") +
      theme_bw()
    
  })
  
  
  
  
  output$myPlot2 <- renderPlot({
    
    if (is.null(playlist_audio_features())) {
      return(NULL)
    }
    
    playlist_audio_features() %>%
      mutate(track.popularity = cut(playlist_audio_features()$track.popularity, breaks = 5)) %>%
      ggplot( aes(x=track.popularity ))+
      geom_bar(width=0.2) +
      coord_flip() +
      labs(x = "Values", y = "Popularity",
           title = "Song Features - Popularity") + 
      theme_bw()
    
  })
  
  output$myPlot3 <- renderPlot({
    
    if (is.null(playlist_audio_features())) {
      return(NULL)
    }
    
    playlist_audio_features() %>%
      mutate(energy = cut(playlist_audio_features()$energy, breaks = 10)) %>%
      ggplot( aes(x=energy ))+
      geom_bar(width=0.2) +
      coord_flip() +
      labs(x = "Values", y = "Energy",
           title = "Song Features - Energy") + 
      theme_bw() 
    
  })
  
  output$myPlot4 <- renderPlot({
    
    if (is.null(playlist_audio_features())) {
      return(NULL)
    }
    
    playlist_audio_features() %>%
      mutate(track.duration_ms = cut(playlist_audio_features()$track.duration_ms, breaks = 10)) %>%
      ggplot( aes(x=track.duration_ms ))+
      geom_bar(width=0.2) +
      coord_flip() +
      labs(x = "Values", y = "Duration",
           title = "Song Features - Duration") + 
      theme_bw()  
    
  })
  
  output$myPlot5 <- renderPlot({
    
    if (is.null(playlist_audio_features_sliced())) {
      return(NULL)
    }
    
    options(repr.plot.width = 20, repr.plot.height = 15)
    
    corr <- cor(playlist_audio_features_sliced())
    
    num <- corrplot(corr, method = "number")
    
  })
  
  output$myPlot6 <- renderPlot({
    
    if (is.null(playlist_audio_features())) {
      return(NULL)
    }
    
    playlist_audio_features() %>%
      mutate(track.popularity = cut(playlist_audio_features()$track.popularity, breaks = 5)) %>%
      ggplot( aes(x = track.name, y = track.popularity)) +
      geom_boxplot(aes(x = track.name, y = track.popularity)) +
      labs(x = "Track Name",
           y = "Popularity",
           title = "Distribution plot of Popularity") +
      coord_flip()+
      theme_bw()
    
  })
  
  # download report
  
  # dynamic download button - 
  # loads only if first information abut the playlists were loaded for viz in the app
  output$dynamic_report_download <- renderUI({
    if (is.null(my_playlists_audio_features())) {
      return(NULL)
    }
    downloadButton("report", "Generate report")
  })
  
  # actual content and knitting the report
  output$report <- downloadHandler(
    filename = "report.html",
    content = function(file) {
      use_temp_folder <- FALSE
      
      if (use_temp_folder) {
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        path_to_file <- tempReport
      } else {
        path_to_file <- "report.Rmd"
      }
      
      env_to_rmd <- new.env()
      env_to_rmd$playlist_audio_features <-
        my_playlists_audio_features()
      
      rmarkdown::render(
        path_to_file,
        output_file = file,
        params = params,
        envir = env_to_rmd
      )
    }
  )
  
  
  
  # One playlist view ----
  
  output$dynamic_playlist_selector <- renderUI({
    if (r$AUTHORIZED) {
      selectizeInput(
        "playlist_selector",
        label = "Choose playlist",
        choices = get_playlists_names_uri(r$access_token,
                                          r$user_id,
                                          return_only_owned = F)
      )
    }
  })
  
  sp_all_tracks <- reactive({
    if (!r$AUTHORIZED) {
      return(NULL)
    }
    # take dependency on global refresh variable - this is incremented every time some api
    # modification call is run
    refresh()
    
    a <- get_songs_from_playlist_to_display(authorization = r$access_token,
                                            playlist_id = input$playlist_selector)
    
  })
  
  sp_all_tracks_df <- reactive({
    if (!r$AUTHORIZED) {
      return(NULL)
    }
    sp_all_tracks()
  })
  
  output$songs_from_selected_playlist <-
    DT::renderDataTable(server = FALSE, {
      if (!is.null(input$playlist_selector) && r$AUTHORIZED) {
        message(paste0(
          "Run rendering of songs for playlist: ",
          input$playlist_selector
        ))
        
        DT::datatable(sp_all_tracks_df()%>%
                        select(!Track_uri),
          colnames = c(ID = 1),
          # add the name
          extensions = c('RowReorder', 'Buttons', 'Select'),
          selection = 'none',
          options = list(
            order = list(list(0, 'asc')),
            rowReorder = TRUE,
            dom = 'Bfrtip',
            buttons = c('colvis', 'selectAll', 'selectNone', 'selectRows'),
            select = list(style = 'os', items = 'row'),
            rowId = 0
          )
        )
        
        
      } else {
        NULL
      }
      
    })
  
  #Rows selector
  sp_selected_my <- reactive({
    # id's of clicked (selected) tracks belonging to logged user
    sp_all_tracks_df()[input$songs_from_selected_playlist_rows_selected,]$Track_uri
  })
  
  observeEvent(input$songs_from_selected_playlist_rows_selected, {
    message(input$songs_from_selected_playlist_rows_selected)
  })
  
  # Deleting tracks
  observeEvent(input$delete_songs, {
    message('SP: delete selected tracks clicked')
    showModal(modal_sp_delete_tracks())
  })
  
  modal_sp_delete_tracks <- reactive({
    modalDialog(
      title = "Delete tracks",
      HTML(
        "Are you sure you want to delete selected playlists? This operation cannot be undone. <br/>",
        paste0(sp_selected_my(), collapse = '<br/>')
      ),
      
      easyClose = TRUE,
      footer = tags$div(
        actionButton("sp_delete_abort", "No, abort", class = "btn-success"),
        actionButton("sp_delete_confirm", "Yes, delete", class = "btn-danger")
      )
    )
  })
  
  
  observeEvent(input$sp_delete_confirm, {
    message('Deleting tracks:', paste0(sp_selected_my(), collapse = " "))
    removeModal()
    if (connect_to_api){
      for (uri in sp_selected_my()) {
        remove_tracks_from_playlist(playlist_id = input$playlist_selector, 
                                    uris=uri,
                          authorization = r$access_token)
      }
    }
    refresh(refresh() + 1)
  })
  
  observeEvent(input$sp_delete_abort, {
    message('Aborting')
    removeModal()
  })
  
  
  
  
  
  
  
  
  
  
  ### Buttons action
  refresh <- reactiveVal(0)
  
  observeEvent(refresh(), {
    message('Refreshing tables')
  })
  
  
  

######################################################################
  # All playlist view ----
  connect_to_api <- TRUE
  
  ap_all_playlists <- reactive({
    if (!r$AUTHORIZED) {
      return(NULL)
    }
    # take dependency on global refresh variable - this is incremented every time some api
    # modification call is run
    refresh()
    
    a <- get_my_playlists( authorization = r$access_token)
    
    a %>%
      select(name, owner.display_name, tracks.total, description, id, owner.id)
  })
  
  ap_all_playlists_my_df <- reactive({
    if (!r$AUTHORIZED) {
      return(NULL)
    }
    
    ap_all_playlists() %>%
      filter(owner.id == r$user_id) %>%
      select(-owner.display_name, -owner.id)
  })
  
  output$ap_all_playlists_my <-
    DT::renderDataTable(server = TRUE, {
      df <- ap_all_playlists_my_df() %>%
        select(-id)
      if (is.null(df)) {
        return(NULL)
      }
      create_datatable(df)
    })
  
  create_datatable <- function(df){
    DT::datatable(
      df,
      # colnames = c(ID = 1),
      # extensions = c('Buttons', 'Select'),
      # selection = 'none',
      options = list(
        order = list(list(0, 'asc')),
        rowReorder = TRUE,
        dom = 'rti',
        # buttons = c('colvis', 'selectAll', 'selectNone', 'selectRows'),
        # select = list(style = 'os', items = 'row'),
        # rowId = 0,
        pageLength= 5000,
        ordering= FALSE
      )
    )
  }
  
  ap_selected_my <- reactive({
    # id's of clicked (selected) playlists belonging to logged user
    ap_all_playlists_my_df()[input$ap_all_playlists_my_rows_selected,]$id
  })
  
  observeEvent(input$ap_all_playlists_my_rows_selected, {
    message(input$ap_all_playlists_my_rows_selected)
  })
  
  # Deleting playlists
  observeEvent(input$ap_delete_playlists, {
    message('AP: delete playlists clicked')
    showModal(modal_ap_delete_playlists())
  })
  
  modal_ap_delete_playlists <- reactive({
    modalDialog(
      title = "Delete playlists",
      HTML(
        "Are you sure you want to delete selected playlists? This operation cannot be undone. <br/>",
        paste0(ap_selected_my(), collapse = '<br/>')
      ),
      
      easyClose = TRUE,
      footer = tags$div(
        actionButton("ap_delete_abort", "No, abort", class = "btn-success"),
        actionButton("ap_delete_confirm", "Yes, delete", class = "btn-danger")
      )
    )
  })
  
  observeEvent(input$ap_delete_confirm, {
    message('Deleting playlists:', paste0(ap_selected_my(), collapse = " "))
    removeModal()
    if (connect_to_api){
      for (id in ap_selected_my()) {
        unfollow_playlist(playlist_id = id, 
                          authorization = r$access_token)
      }
    }
    refresh(refresh() + 1)
  })
  
  observeEvent(input$ap_delete_abort, {
    message('Aborting')
    removeModal()
  })
  
  # New playlist
  observeEvent(input$ap_new_playlist, {
    message('AP: create playlist clicked')
    showModal(modal_ap_new_playlist())
  })
  
  modal_ap_new_playlist <- reactive({
    modalDialog(
      title = "New playlist playlists",
      HTML(
        "Are you sure you want to delete selected playlists? This operation cannot be undone. <br/>",
        paste0(ap_selected_my(), collapse = '<br/>')
      ),
      textInput("ap_new_playlist_name",label = 'Insert playlist name:'),
      
      easyClose = TRUE,
      footer = tags$div(
        actionButton("ap_create_new_confirm", "Create playlist", class = "btn-success")
      )
    )
  })
  
  observeEvent(input$ap_create_new_confirm, {
    message('AP: Creating playlist:', input$ap_new_playlist_name)
    removeModal()
    if (connect_to_api){
      create_playlist(r$user_id, name = input$ap_new_playlist_name, authorization = r$access_token)
    }
    refresh(refresh() + 1)
  })
  
  
  # Copying songs from selected playlists
  observeEvent(input$ap_copy_all_songs, {
    message('AP: copy all songs from playlists clicked')
    showModal(modal_ap_copy_all_songs())
  })
  
  modal_ap_copy_all_songs <- reactive({
    modalDialog(
      title = "Copying song",
      HTML(
        "Select to which playlist you would like to copy following playlists songs? <br/>",
        paste0(ap_selected_my(), collapse = '<br/>'),
        "<br/>"
      ),
      selectizeInput(
        "ap_add_playlist_selector",
        label = "Choose playlist",
        choices = get_playlists_names_uri(r$access_token,
                                          r$user_id,
                                          return_only_owned = T)
      ),
      
      easyClose = TRUE,
      footer = tags$div(
        actionButton("ap_add_confirn", "Copy selected playlists contents", class = "btn-success")
      )
    )
  })
  
  
  observeEvent(input$ap_add_confirn, {
    message('Copying playlists contents from:', paste0(ap_selected_my(), collapse = " "), " to: ", input$ap_add_playlist_selector)
    removeModal()
    if (connect_to_api){
      for (playlist_uri in ap_selected_my()){
        union_playlists(playlist_uri, 
                        input$ap_add_playlist_selector, 
                        r$access_token)
      }
    }
    refresh(refresh() + 1)
  })
  
  # disable buttons
  
  observeEvent(ap_selected_my(), {
    if (length(ap_selected_my()) == 0) {
      shinyjs::disable("ap_delete_playlists")
      shinyjs::disable("ap_copy_all_songs")
      
    } else {
      shinyjs::enable("ap_delete_playlists")
      shinyjs::enable("ap_copy_all_songs")
      
    }
  })
  
  observeEvent(ap_selected_followed(), {
    if (length(ap_selected_followed()) == 0) {
      shinyjs::disable("ap_fork_playlist")
      
    } else {
      shinyjs::enable("ap_fork_playlist")
      
    }
  })
  
  
  # Foreign playlists
  
  ap_all_playlists_followed_df <- reactive({
    if (!r$AUTHORIZED) {
      return(NULL)
    }
    
    ap_all_playlists() %>%
      filter(owner.id != r$user_id) %>%
      select(-owner.id)
  })
  
  
  output$ap_all_playlists_followed <-
    DT::renderDataTable(server = TRUE, {
      create_datatable(ap_all_playlists_followed_df() %>%
                         select(-id))
    })
  
  ap_selected_followed <- reactive({
    # id's of clicked (selected) playlists belonging to logged user
    ap_all_playlists_followed_df()[input$ap_all_playlists_followed_rows_selected,]$id
  })
  
  observeEvent(input$ap_fork_playlist, {
    message('AP: fork playlists clicked: ', paste0(ap_selected_followed(), collapse = " "))
    # browser()
    if (connect_to_api){
      for (playlist_uri in ap_selected_followed()) {
        fork_foreign_playlist(
          playlist_uri,
          authorization = r$access_token,
          user_id = r$user_id)
        refresh(refresh() + 1)
        
      }
    }
    
    
  })
  
  
}

shiny::shinyApp(ui, server, options = list("port" = 1410))
