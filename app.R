library(spotifyr)
library(tidyverse)
library(shiny)
library(DT)

ui <- navbarPage("Hello Spotify!",
                 tabPanel("Authorize",
                          "Nie wiem do końca jak rozwiązać autoryzację. Pewnie 
                          można zrobić oddzielną zakładkę na której będzie wprowadzenie i guzik 'autoryzuj' 
                          (jak w zakłądce TEST). Można też zrobić routing (jak w pythonie): https://github.com/Appsilon/shiny.router  ."
                          ),
                 tabPanel("All playlists",
                          sidebarLayout(
                            sidebarPanel("W tej zakładce można wykonywać akcje na playlistach.
                                         Po lewej stronie mogą być przyciski z akcjami,
                                         powinny być: scal playlisty, usuń kilka playlist naraz, 
                                         kopiuj playlistę od kogoś innego.
                                         "),
                            mainPanel("Tutaj będzie tabelka z playlistami. 
                                      Playlisty nie stworzone przez użytkownika powinny być jakoś oddzielone - 
                                      - nie wiem czy lepiej w oddzielnej tabeli czy jakoś zaznaczyć.")
                          )
                          ),
                 
                 tabPanel("One playlist",
                          sidebarLayout(
                            sidebarPanel("W tym widoku robimy akcje na poziomie
                                         konkretnych utworów (przesuwanie w playliście, usuwanie, kopiowanie itd.)",
                                         selectInput("playlist", "Która playlista?", c("A", "B"))
                            ),
                            mainPanel(
                              tags$div("Poniżej będzie widok tabelki z utworami.\n
                              
                                       Będzie się dało zaznaczyć kilka utworów.\n
                                       Po zaznaczeniu na dole pojawią się przyciski:\n
                                       Kopiuj do...(po kliknięciu pojawia się lista playlist),\n
                                       Usuń.\n
                                       Jeżeli będzie zaznaczony tylko 1 utwór, \n
                                       pojawią się przyciski kopiuj, usuń, ale też przesuń w górę/w dół.\n
                                       
                                       Z perspektywy komunikacji z backendem powinien być przycisk w stylu 'commit',\n
                                       i dopiero po naciśnięciu wysyła się zmiany przez API.\n
                                       Taki feature nice to have to coś w stylu rollback - cofnij ostatnio wysłane zmiany\n
                                       
                                       "),
                              tags$div("Można wykorzystać DataTables: https://datatables.net/extensions/select/examples/initialisation/checkbox.html")
                            )
                          )
                 ),
                 tabPanel("TEST",
                          sidebarLayout(
                            # Sidebar panel for inputs ----
                            sidebarPanel(actionButton("authorize", "Authorize")),
                            
                            # Main panel for displaying outputs ----
                            mainPanel(DT::dataTableOutput("playlists"))
                          )))


server <- function(input, output) {
  r <- reactiveValues(AUTHORIZED = FALSE,
                      access_token = NULL)
  
  output$playlists <- DT::renderDataTable({
    if (r$AUTHORIZED) {
      get_my_playlists(authorization = r$access_token,
                       include_meta_info = F)
    } else {
      NULL
    }
    
  })
  
  
  observeEvent(input$authorize, {
    message("aaa")
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
    user_id <- me$id
    
    r$AUTHORIZED <- TRUE
  })
  
}

shiny::shinyApp(ui, server, options = list("port" = 1410))
