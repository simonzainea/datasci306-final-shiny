library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)   # <<< NEW: for pivot_longer

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

### NEW: numeric features we'll use for "strongest features"
feature_vars <- c(
  "danceability", "energy", "key", "loudness", "speechiness",
  "acousticness", "instrumentalness", "liveness",
  "valence", "tempo"
)

### NEW: restrict to genres we actually keep
spotify_use <- spotify |>
  filter(track_genre %in% valid_genres)

### NEW: overall mean / sd for each feature (across all kept genres)
overall_stats <- spotify_use |>
  summarize(
    across(
      all_of(feature_vars),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd   = ~ sd(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = c("feature", ".value"),
    names_sep = "_"
  )

### NEW: genre-specific means and z-scores (how “distinctive” a feature is)
genre_feature_strength <- spotify_use |>
  group_by(track_genre) |>
  summarize(
    across(
      all_of(feature_vars),
      ~ mean(.x, na.rm = TRUE),
      .names = "{.col}_mean_genre"
    ),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = ends_with("_mean_genre"),
    names_to = "feature",
    values_to = "genre_mean"
  ) |>
  mutate(feature = sub("_mean_genre$", "", feature)) |>
  left_join(overall_stats, by = "feature") |>
  mutate(
    z_score = (genre_mean - mean) / sd,
    z_score = ifelse(is.na(z_score), 0, z_score)
  ) |>
  rename(genre = track_genre)

### NEW: correlations with popularity, by genre
genre_pop_cor <- spotify_use |>
  group_by(track_genre) |>
  summarize(
    across(
      all_of(feature_vars),
      ~ cor(.x, popularity, use = "complete.obs"),
      .names = "{.col}_cor"
    ),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = ends_with("_cor"),
    names_to = "feature",
    values_to = "correlation"
  ) |>
  mutate(feature = sub("_cor$", "", feature)) |>
  rename(genre = track_genre)

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
            choices = feature_vars,
            selected = "danceability"
          )
        ),
        mainPanel(
          h4("Popularity vs selected feature"),
          plotOutput("feature_plot")
        )
      )
    ),
    
    ### NEW TAB: strongest features by genre
    tabPanel(
      "Strongest features by genre",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "genre_strength",
            "Choose a genre:",
            choices = valid_genres,
            selected = valid_genres[1]
          ),
          sliderInput(
            "top_k",
            "Number of top features to show:",
            min = 3, max = length(feature_vars),
            value = 5, step = 1
          )
        ),
        mainPanel(
          h4("Features that define this genre vs all genres"),
          plotOutput("genre_signature_plot"),
          hr(),
          h4("Features most related to popularity within this genre"),
          plotOutput("genre_pop_plot")
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
  
  ### NEW: strongest features tab
  
  # Distinctive features for chosen genre
  genre_strength_data <- reactive({
    genre_feature_strength |>
      filter(genre == input$genre_strength) |>
      arrange(desc(abs(z_score))) |>
      slice_head(n = input$top_k)
  })
  
  output$genre_signature_plot <- renderPlot({
    df <- genre_strength_data()
    
    ggplot(df, aes(x = reorder(feature, z_score), y = z_score)) +
      geom_col() +
      coord_flip() +
      labs(
        x = "Feature",
        y = "Standardized difference (z-score)",
        title = paste("Most distinctive features for", input$genre_strength)
      )
  })
  
  # Popularity-related features for chosen genre
  genre_pop_data <- reactive({
    genre_pop_cor |>
      filter(genre == input$genre_strength) |>
      arrange(desc(abs(correlation))) |>
      slice_head(n = input$top_k)
  })
  
  output$genre_pop_plot <- renderPlot({
    df <- genre_pop_data()
    
    ggplot(df, aes(x = reorder(feature, correlation), y = correlation)) +
      geom_col() +
      coord_flip() +
      labs(
        x = "Feature",
        y = "Correlation with popularity",
        title = paste("Features most related to popularity in", input$genre_strength)
      )
  })
}

shinyApp(ui = ui, server = server)






