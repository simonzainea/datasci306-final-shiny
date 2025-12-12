library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr) # <<< NEW: for pivot_longer
library(tidyverse)

# ---- Load data once when the app starts ----
load("data/spotify_mpd_1perc.RData")  # gives attrs_down, tracks_down, etc.

# We'll mainly use attrs_down
spotify <- attrs_down

# ---- Keep only genres with at least min_n tracks ----
genre_counts <- spotify |>
  count(track_genre, name = "n_tracks")

min_n <- 50

valid_genres <- genre_counts |>
  filter(n_tracks >= min_n) |>
  arrange(track_genre) |>
  pull(track_genre)

# Restrict to genres we actually keep
spotify_use <- spotify |>
  filter(track_genre %in% valid_genres)

# --- Key labels ---
key_lookup <- tibble(
  key = 0:11,
  key_name = c("C", "C♯/D♭", "D", "D♯/E♭", "E", "F",
               "F♯/G♭", "G", "G♯/A♭", "A", "A♯/B♭", "B")
)

spotify_keys <- spotify_use |>
  filter(key %in% 0:11) |>   # drops key = -1
  left_join(key_lookup, by = "key")


# --- Most Distinctive Keys ---
overall_key_dist <- spotify_keys |>
  count(key_name) |>
  mutate(overall_prop = n / sum(n)) |>
  select(key_name, overall_prop)

genre_key_distinctive <- spotify_keys |>
  count(track_genre, key_name) |>
  group_by(track_genre) |>
  mutate(genre_prop = n / sum(n)) |>
  ungroup() |>
  rename(genre = track_genre) |>
  left_join(overall_key_dist, by = "key_name") |>
  mutate(diff_prop = genre_prop - overall_prop)

# --- Keys Most Related to Popularity ---
genre_key_popularity <- spotify_keys |>
  group_by(track_genre, key_name) |>
  summarize(
    n = n(),
    key_mean_pop = mean(popularity, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(track_genre) |>
  mutate(
    genre_mean_pop = weighted.mean(key_mean_pop, w = n, na.rm = TRUE),
    diff_pop = key_mean_pop - genre_mean_pop
  ) |>
  ungroup() |>
  rename(genre = track_genre)




# numeric features for "strongest features"
feature_vars <- c(
  "danceability", "energy", "loudness", "speechiness", #### TOOK OUT KEY
  "acousticness", "instrumentalness", "liveness",
  "valence", "tempo"
)


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
      .names = "{.col}_mean_genre"),
    .groups = "drop") |>
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

### Popularity and Genre correlations
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
  tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap",
      rel = "stylesheet"
    ),
    tags$style(HTML("
    body {
      font-family: 'Inter', sans-serif;
    }

    h1, h2, h3 {
      font-weight: 600;
    }

    .sidebar {
      font-weight: 500;
    }

    .shiny-input-container label {
      font-size: 0.85rem;
      letter-spacing: 0.02em;
    }
  "))
  ),
  
  
  
  
  titlePanel("Spotify – Attribute Data Explorer"),
  
  tabsetPanel(
    
    # Tab 1: Raw Data Preview
    tabPanel(
      "Data preview",
      tableOutput("preview")
    ),
    
    # Tab 2: Popularity Distribution by Genre
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
    
    # Tab 3: Feature vs Popularity Explorer
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
    
    # Tab 4: Strongest Features By Genre
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
          plotOutput("genre_pop_plot"),
          hr(),
          h4("Most distinctive keys for this genre (vs overall)"),
          plotOutput("key_distinctive_plot"),
          hr(),
          h4("Keys most related to popularity within this genre"),
          plotOutput("key_popularity_plot"),
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
  
  # Tab 2: Popularity Histogram
  output$pop_hist <- renderPlot({
    df <- filtered_spotify()
    
    validate(
      need(nrow(df) > 0, "No songs in this genre.")
    )
    
    ggplot(df, aes(x = popularity)) +
      geom_histogram(
        aes(fill = after_stat(count)),
        bins = 30,
        color = NA
      ) +
      scale_fill_gradient(
        low = "#1ED760",
        high = "#0B5D2A"
      ) +
      labs(
        x = "Popularity",
        y = "Number of tracks",
        fill = "Track count",
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
  
  # Tab 3: Feature vs Popularity Plot
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
  
  # Tab 4: Strongest Features Tab
  
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
      geom_col(aes(fill = abs(z_score)), color = NA) +
      scale_fill_gradient(low = "#1ED760", high = "#0B5D2A") +
      coord_flip() +
      labs(
        x = "Feature",
        y = "Standardized difference (z-score)",
        fill = "|z|",
        title = paste("Most distinctive features for", input$genre_strength)
      ) +
      theme(legend.position = "none")
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
      geom_col(aes(fill = abs(correlation)), color = NA) +
      scale_fill_gradient(low = "#1ED760", high = "#0B5D2A") +
      coord_flip() +
      labs(
        x = "Feature",
        y = "Correlation with popularity",
        fill = "|r|",
        title = paste("Features most related to popularity in", input$genre_strength)
      ) +
      theme(legend.position = "none")
  })
  
  # --- Key: distinctive within genre vs overall ---
  key_distinctive_data <- reactive({
    genre_key_distinctive |>
      filter(genre == input$genre_strength) |>
      arrange(desc(abs(diff_prop))) |>
      slice_head(n = min(input$top_k, 12))
  })
  
  output$key_distinctive_plot <- renderPlot({
    df <- key_distinctive_data()
    
    ggplot(df, aes(x = reorder(key_name, diff_prop), y = diff_prop)) +
      geom_col(aes(fill = abs(diff_prop)), color = NA) +
      scale_fill_gradient(low = "#1ED760", high = "#0B5D2A") +
      coord_flip() +
      labs(
        x = "Key",
        y = "Difference in proportion vs overall",
        fill = "|Δ|",
        title = paste("Most over/under-represented keys in", input$genre_strength)
      ) +
      theme(legend.position = "none")
  })
  
  # --- Key: popularity association within genre ---
  key_popularity_data <- reactive({
    genre_key_popularity |>
      filter(genre == input$genre_strength) |>
      arrange(desc(abs(diff_pop))) |>
      slice_head(n = min(input$top_k, 12))
  })
  
  output$key_popularity_plot <- renderPlot({
    df <- key_popularity_data()
    
    ggplot(df, aes(x = reorder(key_name, diff_pop), y = diff_pop)) +
      geom_col(aes(fill = abs(diff_pop)), color = NA) +
      scale_fill_gradient(low = "#1ED760", high = "#0B5D2A") +
      coord_flip() +
      labs(
        x = "Key",
        y = "Avg popularity minus genre avg",
        fill = "|Δ|",
        title = paste("Keys most associated with popularity in", input$genre_strength),
        subtitle = "Positive = higher avg popularity than the genre baseline"
      ) +
      theme(legend.position = "none")
  })
  
  
}

shinyApp(ui = ui, server = server)