Spotify – Attribute Data Explorer

This repository contains a Shiny application developed for the DATASCI 306 final project.
The application allows users to explore Spotify track audio attributes and popularity across genres using interactive visualizations.

How to run locally

  1. Download or clone this repository so that app.R and the data folder are in the same directory.

  2. Open RStudio and set the working directory to the project folder.

  3. Install required packages by running:

    install.packages(c("shiny", "dplyr", "ggplot2", "tidyr"))

  4. Run the application by opening app.R and clicking Run App, or by running:

    shiny::runApp()

Data

The application uses a subset of the Spotify Million Playlist Dataset stored in:
data/spotify_mpd_1perc.RData

Online version

The app is deployed on ShinyApps.io and can be accessed at:
https://9aaua9-simon-zainea.shinyapps.io/datasci306-final-shiny/

Notes

All file paths in the application are relative to the project directory.
