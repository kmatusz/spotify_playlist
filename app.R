library(shiny)
library(DT)

ui <- fluidPage(
  
  # App title ----
  titlePanel("Hello Spotify!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      actionButton("authorize", "Authorize")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      DT::dataTableOutput("playlists")
      
    )
  )
)


server <- function(input, output) {
  r <- reactiveValues(
    AUTHORIZED = FALSE,
    access_token = NULL
  )
  
  output$playlists <- DT::renderDataTable({
    if(r$AUTHORIZED){
      get_my_playlists(authorization = r$access_token, include_meta_info = F)
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
      "playlist-read-collaborative")
    
    # pass this to every function for authorization
    r$access_token <- get_spotify_authorization_code(scope = scopes_needed)
    
    
    # Get user profile
    me <- get_my_profile(r$access_token)
    user_id <- me$id
    
    r$AUTHORIZED <- TRUE
  })
  
}

shiny::shinyApp(ui, server, options = list("port" = 1410))

