library(spotifyr)
library(tidyverse)
library(shiny)
library(DT)
library(corrplot)
library(sortable)
library(shinythemes)
source("funs.R", encoding = "UTF-8")
# source("playlist_features_poc.R")


ui <- navbarPage(
  theme = shinytheme("cerulean"),
  "Hello Spotify!",
  
  
  tabPanel("One playlist",
           sidebarLayout(
             sidebarPanel(
               "W tym widoku robimy akcje na poziomie
                                         konkretnych utworów (przesuwanie w playliście, usuwanie, kopiowanie itd.)",
               # actionButton("authorize", "Authorize"),
               uiOutput("dynamic_playlist_selector")
               # selectInput("playlist", "Która playlista?", c("A", "B"))
             ),
             mainPanel(
               tags$div(
                 "Poniżej będzie widok tabelki z utworami.\n
                                       Będzie się dało zaznaczyć kilka utworów.\n
                                       Po zaznaczeniu na dole pojawią się przyciski:\n
                                       Kopiuj do...(po kliknięciu pojawia się lista playlist),\n
                                       Usuń.\n
                                       Jeżeli będzie zaznaczony tylko 1 utwór, \n
                                       pojawią się przyciski kopiuj, usuń, ale też przesuń w górę/w dół.\n
                                       Z perspektywy komunikacji z backendem powinien być przycisk w stylu 'commit',\n
                                       i dopiero po naciśnięciu wysyła się zmiany przez API.\n
                                       Taki feature nice to have to coś w stylu rollback - cofnij ostatnio wysłane zmiany\n
                                       "
               ),
               DT::dataTableOutput("songs_from_selected_playlist"),
               DT::dataTableOutput("my_playlists"),
               actionButton("copy_songs", "Copy", class = "btn-primary"),
               actionButton("delete_songs", "Delete", class = "btn-danger"),
               actionButton("save_changes", "Save changes", class = "btn-success")
             )
           )),
  
  tabPanel(
    "Visualization",
    fluidPage(
      titlePanel("Visualization of song atributes from our playlist"),
      uiOutput("report_dynamic_playlist_selector"),
      plotOutput("myPlot1"),
      plotOutput("myPlot2"),
      plotOutput("myPlot3"),
      plotOutput("myPlot4"),
      plotOutput("myPlot5"),
      uiOutput('dynamic_report_download')
    )
  ),
  
  tabPanel("All playlists",
           sidebarLayout(
             sidebarPanel(
               "W tej zakładce można wykonywać akcje na playlistach.
                                         Po lewej stronie mogą być przyciski z akcjami,
                                         powinny być: scal playlisty, usuń kilka playlist naraz,
                                         kopiuj playlistę od kogoś innego."
             ),
             mainPanel(
               "Tutaj będzie tabelka z playlistami.
                                      Playlisty nie stworzone przez użytkownika powinny być jakoś oddzielone -
                                      - nie wiem czy lepiej w oddzielnej tabeli czy jakoś zaznaczyć.",
               DT::dataTableOutput("all_playlist")
             )
           ))
)



server <- function(input, output) {
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
        # I couldn't set it as NA or NULL, as javascript is converting these to character "NA"
        choices = c(
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
    get_audio_features_for_playlists(r$access_token, playlist_id = report_playlist_selector())
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
  
  output$songs_from_selected_playlist <-
    DT::renderDataTable(server = FALSE, {
      if (!is.null(input$playlist_selector) && r$AUTHORIZED) {
        message(paste0(
          "Run rendering of songs for playlist: ",
          input$playlist_selector
        ))
        
        DT::datatable(
          get_songs_from_playlist_to_display(
            authorization = r$access_token,
            playlist_id = input$playlist_selector
          ),
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
  
  ### Buttons action
  
  # All playlist view ----
  output$all_playlist <- DT::renderDataTable({
    if (!is.null(input$playlist_selector) && r$AUTHORIZED) {
      message(paste0(
        "Run rendering of songs for playlist: ",
        input$playlist_selector
      ))
      
      DT::datatable(get_playlists_names_uri(r$access_token,
                                            r$user_id,
                                            return_only_owned = F))
      
      
    } else {
      NULL
    }
    
  })
}

shiny::shinyApp(ui, server, options = list("port" = 1410))
