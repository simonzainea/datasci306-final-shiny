library(shiny)
library(dplyr)
library(ggplot2)

# ---- Load data once when the app starts ----
load("data/spotify_mpd_1perc.RData")  # gives attrs_down, tracks_down, etc.

# We'll mainly use attrs_down
spotify <- attrs_down

# ---- Keep only genres with at least min_n tracks ----
genre_counts <- spotify |>
  count(track_genre, name = "n_tracks")

min_n <- 50  # <<< you can change this to 100, 300, etc.

valid_genres <- genre_counts |>
  filter(n_tracks >= min_n) |>
  arrange(track_genre) |>
  pull(track_genre)

# ---- UI ----
ui <- fluidPage(
  titlePanel("Spotify – Attribute Data Explorer"),
  
  tabsetPanel(
    
    # Tab 1: raw data preview
    tabPanel(
      "Data preview",
      tableOutput("preview")
    ),
    
    # Tab 2: popularity distribution by genre
    tabPanel(
      "Popularity by genre",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "genre",
            "Choose a genre:",
            choices = c("All genres", valid_genres),
            selected = "All genres"
          )
        ),
        mainPanel(
          h4("Popularity distribution"),
          plotOutput("pop_hist")
        )
      )
    ),
    
    # Tab 3: feature vs popularity explorer
    tabPanel(
      "Feature explorer",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "genre_feat",
            "Choose a genre:",
            choices = c("All genres", valid_genres),
            selected = "All genres"
          ),
          selectInput(
            "feature",
            "Choose a feature:",
            choices = c(
              "danceability", "energy", "key", "loudness", "speechiness",
              "acousticness", "instrumentalness", "liveness",
              "valence", "tempo"
            ),
            selected = "danceability"
          )
        ),
        mainPanel(
          h4("Popularity vs selected feature"),
          plotOutput("feature_plot")
        )
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  # Tab 1: preview a few rows of the data
  output$preview <- renderTable({
    head(spotify)
  })
  
  # ---- Helper: filtered data for the popularity tab ----
  filtered_spotify <- reactive({
    if (input$genre == "All genres") {
      spotify |>
        filter(track_genre %in% valid_genres)
    } else {
      spotify |>
        filter(track_genre == input$genre)
    }
  })
  
  # Tab 2: popularity histogram
  output$pop_hist <- renderPlot({
    df <- filtered_spotify()
    
    validate(
      need(nrow(df) > 0, "No songs in this genre.")
    )
    
    ggplot(df, aes(x = popularity)) +
      geom_histogram(bins = 30) +
      labs(
        x = "Popularity",
        y = "Number of tracks",
        title = if (input$genre == "All genres") {
          "Popularity distribution (filtered to genres with enough tracks)"
        } else {
          paste("Popularity distribution –", input$genre)
        }
      )
  })
  
  # ---- Helper: filtered data for the feature explorer tab ----
  filtered_feat <- reactive({
    if (input$genre_feat == "All genres") {
      spotify |>
        filter(track_genre %in% valid_genres)
    } else {
      spotify |>
        filter(track_genre == input$genre_feat)
    }
  })
  
  # Tab 3: feature vs popularity plot
  output$feature_plot <- renderPlot({
    df <- filtered_feat()
    
    validate(
      need(nrow(df) > 0, "No songs in this genre.")
    )
    
    ggplot(df, aes_string(x = input$feature, y = "popularity")) +
      geom_point(alpha = 0.3) +
      geom_smooth(method = "loess", se = FALSE) +
      labs(
        x = input$feature,
        y = "Popularity",
        title = if (input$genre_feat == "All genres") {
          paste("Popularity vs", input$feature, "(genres with enough tracks)")
        } else {
          paste("Popularity vs", input$feature, "in", input$genre_feat)
        }
      )
  })
}

shinyApp(ui = ui, server = server)





