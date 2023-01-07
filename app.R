# STAT 302 - Final Project Code
# hamburglar



# DATA CLEANING  -----------------------------------------------------------------------------------------------------------------

# load libraries
library(tidyverse)
library(dplyr)
library(janitor)
library(readr)
library(shiny)

# load data
top_2016 <- readRDS("data/top_songs_2016.rds")
top_2017 <- readRDS("data/top_songs_2017.rds")
top_2018 <- readRDS("data/top_songs_2018.rds")
top_2019 <- readRDS("data/top_songs_2019.rds")
top_2020 <- readRDS("data/top_songs_2020.rds")
top_2021 <- readRDS("data/top_songs_2021.rds")


# adding year data to each dataset
top_2016['playlist_year'] = 2016
top_2017['playlist_year'] = 2017
top_2018['playlist_year'] = 2018
top_2019['playlist_year'] = 2019
top_2020['playlist_year'] = 2020
top_2021['playlist_year'] = 2021

top <- bind_rows(top_2016, top_2017, top_2018, top_2019, top_2020, top_2021) # combining annual data into one dataframe

# identifying the columns to drop
to_drop <- c("artists_name_feature_1", # feature artist names - vast majority of songs don't have feature artists
             "artists_name_feature_2",
             "artists_name_feature_3",
             "artists_name_feature_4",
             "artists_name_feature_5",
             "artists_name_feature_6",
             "artists_name_feature_7",
             "artists_name_feature_8",
             "artists_id_main", # artist ids - not interpretable for front-end applications
             "artists_id_feature_1",
             "artists_id_feature_2",
             "artists_id_feature_3",
             "artists_id_feature_4",
             "artists_id_feature_5",
             "artists_id_feature_6",
             "artists_id_feature_7",
             "artists_id_feature_8",
             "genres" # contains multiple genres but we only care about the broad genre
)

top <- top[ , !(names(top) %in% to_drop)] # cleaned df (600 observations, 31 columns)

quartile_attr <- function(x) {quart_breaks <- c(-Inf, # function to categorize songs by quartile
                                                quantile(x, probs = c(.25, .5, .75), na.rm = TRUE), 
                                                Inf) 
cut(x = x, breaks = quart_breaks, labels = FALSE)}


# PRIMARY DATAFRAME
top <- top %>% # adding values for quartile
  mutate(
    danceability_quant = quartile_attr(top$danceability),
    energy_quant = quartile_attr(top$energy),
    speechiness_quant = quartile_attr(top$speechiness),
    liveness_quant = quartile_attr(top$liveness),
    loudness_quant = quartile_attr(top$loudness)
  )


# DATAFRAME 1: getting count of distinct artists, albums, and genres by year 

counts_all <- top %>% 
  group_by(playlist_year) %>% 
  summarize(num_artists = n_distinct(artists_name_main),
            num_albums= n_distinct(album_name),
            num_genres = n_distinct(broad_genre)
  ) %>% 
  select(playlist_year, contains("num_")) %>% 
  pivot_longer(cols = -playlist_year, # all columns you want to combine into the new "category" column
               names_to = "variable",
               values_to = "value")

# DATAFRAME 2: song attribute data

top$loudness = abs(top$loudness) # standardize loudness
top$loudness = top$loudness / (max(top$loudness) - min(top$loudness))

attributes_all <- top %>%  # dataframe
  group_by(playlist_year) %>% 
  summarize(avg_danceability = mean(danceability),
            avg_energy = mean(energy),
            avg_speechiness = mean(speechiness),
            avg_liveness = mean(liveness),
            avg_loudness = mean(loudness)) %>% 
  select(playlist_year, contains("avg_")) %>% 
  pivot_longer(cols = -playlist_year, # get data into long format
               names_to = "attribute",
               values_to = "value")

# DATAFRAME 3: polar plot data

Mode <- function(x) { # function to compute the most common value in groupby aggergation
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]}

quartiles_annual <- top %>% # dataframe
  group_by(playlist_year) %>%
  summarize(danceability_quant_m = Mode(danceability_quant), # mode
            energy_quant_m = Mode(energy_quant),
            speechiness_quant_m = Mode(speechiness_quant),
            liveness_quant_m = Mode(liveness_quant),
            loudness_quant_m = Mode(loudness_quant)) %>%
  select(playlist_year, contains("_m")) %>%
  pivot_longer(cols = -playlist_year, # get data into long format
               names_to = "attribute",
               values_to = "mode_quant")


# SHINY WEBPAGE -----------------------------------------------------------------------------------------------------------------

# Define UI 
ui <- fluidPage(
  
  # creating navigation bar
  navbarPage("Julia's Spotify Library",
             
             
             # TAB 1 - Dashboard
             tabPanel("Dashboard",
                      
                      # text to provide context
                      titlePanel("Analysis of my Top Spotify Songs (2016-2021)"),
                      helpText("As a lifelong listener and performer of music, I’ve always been fascinated by listening patterns and preferences. Music preferences are not developed deliberately, yet we consistently find ourselves drawn towards specific genres and artists.",strong(" This dashboard aims to dissect my personal listening habits by examining 6 years’ worth of listening data provided by Spotify."), "Each year’s data considers my top 100 most played songs from that year, as determined by the “Your Top Songs” playlists that Spotify customizes for users at the end of each calendar year. "),
                      br(),
                      br(),
                      
                      # creating dashboard content
                      sidebarLayout(
                        
                        # sidebar with interactive widgets
                        sidebarPanel(
                          # drop-down - choosing a year
                          selectInput("year",
                                      label = "Choose year(s):",
                                      choices = c(2016, 2017, 2018, 2019, 2020, 2021, "All"),
                                      selected = "All"
                                      ),
                          # drop-down - choosing a variable 
                          selectInput("stat", 
                                      label = "Variable:",
                                      choices = c("Variety", "Song Attributes"),
                                      selected = "Variety"
                                      )
                          ),
                        
                        # main panel to display visualization
                        mainPanel(plotOutput("graph", width = 1000, height = 600))
                        
                        ) # end to sidebarLayout
                      ), # end tab 1
             
             
             # TAB 2 - Additional Information
             tabPanel("Additional Information",
                      titlePanel("Additional Information"),
                      br(),
                      h4("Background and Widget Information"),
                      p("This Shiny dashboard provides insight into my Spotify listening habits, as represented by my top 100 most-played songs from each year across 2016 to 2021. Through interactive visualizations, it highlights the attributes of the songs I tend to gravitate towards and furthermore shows how my tastes have changed across time. The dashboard consists of two interactive widgets: "),
                      br(),
                      strong("(1)	selectInput (Year)"),
                      br(),
                      p("The year selectInput gives users the option to visualize data for any individual year from 2016-2021 or to view visualizations that make comparisons across all years. This widget is crucial in helping demonstrate change across time. By interacting with the widget, users can juxtapose data from different years side-by-side to help make direct comparisons, or they can select a particular year’s listening history to take a deeper look at what characterized my music taste at the time."),
                      strong("(2)	selectInput (Variable)"),
                      br(),
                      p("This widget offers two options to users. The first option, “Variety”, provides information regarding the prevalence of certain genres, albums, and artists among my most played songs. The second option, “Song Attributes”, provides a more detailed look into the aural qualities of these songs across several different dimensions."),
                      br(),
                      br(),
                      h4("Insights and Conclusions"),
                      p("By interacting with these widgets, users are able to see how drastically my music tastes have shifted across time. Some key insights I realized were as follows:"),
                      p(strong("•	My music tastes have become increasingly homogenized across time"), ". This is most apparent when examining the number of distinct albums and artists represented by my 2016 Top Songs Playlist vs my 2021 Top Songs playlist. In 2016, 80 different albums and 64 different artists were represented in my top 100 most-played songs, whereas only 68 different albums and 39 different artists were represented in 2021."),
                      p("•	While interacting with the song attribute polar plot, users can understand the common attributes of songs that I began to gravitate towards. Most notably, this included songs that were characterized by Spotify as having greater levels of “speechiness” and “danceability”.", strong("This indicates a growing preference towards upbeat vocal music."), "Cross referencing this with my genre preferences confirms that I began to shift away from classical music and towards traditionally speech-dominated genres like pop and modern rock."),
                      br()
                      ), # end tab 2
             
             
             # TAB 3 - References
             tabPanel("References",
                      h2("References + Acknowledgements"),
                      br(),
                      p("I referenced the following resources when creating this project:"),
                      br(),
                      p(strong ("•	My personal Spotify library and Spotify Web API"), a("     (Link)", href= "https://developer.spotify.com/dashboard/applications")),
                      p("The Spotify Web Developers' dashboard provided the Client ID and Client Secret required to access Spotify data. All data that I accessed came from my personal library across 2016-2021."),
                      br(),
                      p(strong ("•	spotify_get_playlists.R"), "(Created by Leila Darwiche - NU stats + journalism student)"),
                      p("Leila provided me with the R script (included in project folder) to pull Spotify data via API, which she had previously created for a personal project. Although I still had to perform my own data cleaning (primarily in combining dataframes, performing groupby aggregations, and removing unnecessary columns), she greatly simplified the process of getting Spotify data into a legible format."),
                      br(),
                      p(strong ("•	StackOverflow - Most frequent value (mode) by group"), a("     (Link)",href = "https://stackoverflow.com/questions/29255473/most-frequent-value-mode-by-group")),
                      p("To build the polar plots, I used the most common song dimension quartile value for each year. I chose to use the mode instead of the mean to guarantee that I had whole number values. I pulled the 'Mode' function from this link, since I couldn't find one built into R.")
                      ) # end tab 3
             
             ) # end Navbarpage
  ) # end fluidpage


# Define server logic 
server <- function(input, output) {
  
  output$graph <- renderPlot({
    
    # I. Individual year graphs -------------------------

    # a) barplot - counts of artist, genre, albums 
    
    # create title
    year_counts_title <- switch(input$year,
                                "2016" = "Genre Breakdown of Most-Played Songs (2016)",
                                "2017" = "Genre Breakdown of Most-Played Songs (2017)",
                                "2018" = "Genre Breakdown of Most-Played Songs (2018)",
                                "2019" = "Genre Breakdown of Most-Played Songs (2019)",
                                "2020" = "Genre Breakdown of Most-Played Songs (2020)",
                                "2021" = "Genre Breakdown of Most-Played Songs (2021)")
    
    # filter data by year + create plot
    year_counts <- top %>% 
      filter(playlist_year == input$year) %>% 
      ggplot(aes(x = broad_genre)) +
      geom_bar(fill = "#1DB954") +
      geom_text(aes(label = ..count..), 
                stat = "count", 
                vjust = -0.5, colour = "black") +
      labs(title = year_counts_title,
           subtitle = "Count of songs belonging to my most-listened to genres",
           y = "Count",
           x = "") +
      scale_y_continuous(limits = c(0,45),
                         expand = c(.01,0)) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 20),
            plot.subtitle = element_text(size = 12, color = "#727574"),
            legend.position = c(1,1),
            legend.justification = c(1,1))
   
    
    
    # b) polar plot - visualizing songs across 5 dimensions
    
    # create title
    year_attributes_title <- switch(input$year,
                                    "2016" = "Attributes of Most-Played Songs (2016)",
                                    "2017" = "Attributes of Most-Played Songs (2017)",
                                    "2018" = "Attributes of Most-Played Songs (2018)",
                                    "2019" = "Attributes of Most-Played Songs (2019)",
                                    "2020" = "Attributes of Most-Played Songs (2020)",
                                    "2021" = "Attributes of Most-Played Songs (2021)")
    
    # filter data by year + create plot
    year_attributes <- quartiles_annual %>% 
      filter(playlist_year == input$year) %>% 
      ggplot(mapping = aes(x = attribute, y = mode_quant)) +
      geom_col(width = 1, fill = "#1DB954") + 
      scale_x_discrete(name = NULL,
                       limits = c("danceability_quant_m", 
                                  "energy_quant_m",
                                  "speechiness_quant_m",
                                  "liveness_quant_m",
                                  "loudness_quant_m"),
                       expand = c(0,0)) + 
      geom_segment(x = 0.5:4.5, y = 0, xend = 0.5:4.5, yend = 4) +  
      scale_y_continuous(name = NULL, expand = c(0,0)) +
      geom_hline(yintercept = 1:4) +
      labs(title = year_attributes_title) +
      annotate(geom = "text", 
               label = c("Danceability", 
                         "Energy",
                         "Speechiness",
                         "Liveness",
                         "Loudness"),
               fontface = "bold",
               x = 1:5, 
               y = rep(5,5),
               size = 5) +
      annotate(geom = "text", # quartiles represented by percentiles
               label = c("1st-25th", 
                         "25th-50th",
                         "50th-75th",
                         "75th-99th"),
               x = 3,
               y = .75:3.75,
               size = 4) + 
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            plot.subtitle = element_text(size = 12), 
            plot.margin = unit(c(0,0,0,0), "cm"),
            legend.position ="none") + 
      coord_polar() # convert to polar coordinates
        
        
    # II. All years graphs -------------------------
    
    # a) side-by-side barplot - counts of artist, genre, albums 
    all_bar <- ggplot(data = counts_all, 
                      aes(x = playlist_year,
                          y = value,
                          fill = variable)) +
      geom_bar(stat = "identity",
               position = "dodge") +
      scale_fill_manual(name = "", 
                        labels = c("Albums", "Artists", "Genres"),
                        values = c("#1DB954", "#F9D03B", "#F37778")) + # "#1DB954", "#1d75db", "#2b7262"
      scale_y_continuous(limits = c(0, 85),
                         expand = c(.01,0)) +
      scale_x_discrete(limits = c(2016, 2017, 2018, 2019, 2020, 2021), 
                       labels = c(2016, 2017, 2018, 2019, 2020, 2021),
                       expand = c(.02,0)) +
      geom_text(aes(label = value, y = value+ 3), 
                position = position_dodge(0.9),
                size = 4) + 
      labs(title = "Composition of Distinct Album, Artist, and Genres (2016-2021)",
           subtitle = "Number of unique albums, artists, and genres represented across each year's top played songs",
           y = "Count",
           x = "") +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 20),
            plot.subtitle = element_text(size = 12, color = "#727574"),
            legend.position = "bottom")
    
    
    # b) lineplot - visualizing change in mean song attribute values across time   
    all_line <- ggplot(data = attributes_all, 
                       aes(x = playlist_year, y = value, color = attribute)) +
      geom_line(size = .8) +
      geom_point(aes(shape = attribute), size = 3) + 
      scale_color_manual(name = "", 
                         labels = c("Danceability", 
                                    "Energy", 
                                    "Liveness",
                                    "Loudness",
                                    "Speechiness"),
                         values = c("#1DB954", 
                                    "#F9D03B", 
                                    "#F37778",
                                    "#1d75db", 
                                    "#2b7262")) +
      scale_shape_discrete(name = "", 
                           labels = c("Danceability", 
                                      "Energy", 
                                      "Liveness",
                                      "Loudness",
                                      "Speechiness")) +
      labs(title = "Characteristics of My Most-Played Tracks on Spotify (2016-2021)",
           subtitle = "Breaking down the attributes of my most played songs to understand how my tastes have changed across time",
           x = "",
           y = "Score") +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 20),
            plot.subtitle = element_text(size = 12, color = "#727574"),
            legend.position = "bottom") 
    
    
    
    # assuming user selects all years, this chooses which variable to plot
    all_graphs <- switch(input$stat,
                         "Variety" = all_bar,
                         "Song Attributes" = all_line)
    
    # assuming user selects an individual year, this chooses which variable to plot
    year_graphs <- switch(input$stat,
                          "Variety" = year_counts,
                          "Song Attributes" = year_attributes)
    
    # final graph to plot, dependent upon both user inputs
    final <- switch(input$year,
                    "All" = all_graphs,
                    year_graphs)
    
    final
    
  })
  
}



# Run the application 
shinyApp(ui = ui, server = server)




