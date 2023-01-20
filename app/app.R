library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(forcats)
library(tidyr)
library(stringi)

data <- read.csv("dane_maciej.csv", sep = ";")

ui <- fluidPage(
  
  
  titlePanel("Analiza najczęściej wybieranych artystów i utworów"),
  
  fluidRow(
    column(6, textOutput("opis")),
    column(6, selectInput("wykonawcy", "Wybierz wykonawców",
                          unique(data %>% 
                                   mutate(Wykonawca = master_metadata_album_artist_name) %>% 
                                   group_by(Wykonawca) %>% 
                                   summarise(n = n()) %>% 
                                   arrange(desc(n)) %>% 
                                   top_n(20) %>% 
                                   select(Wykonawca)), multiple = TRUE))
  ),
  
  fluidRow(
    column(6, plotlyOutput("wykres1")),
    column(6, plotlyOutput("wykres3"))
  ),
  
  fluidRow(
    column(6, textOutput("opis3")),
    column(6)
  ),
  
  fluidRow(
    column(6, textOutput("opis2")),
    column(6)
  ),
  
  fluidRow(
    column(6, 
           selectInput("wykonawca", "Wybierz wykonawcę", 
                          unique(data %>% 
                                   mutate(Wykonawca = master_metadata_album_artist_name) %>% 
                                   group_by(Wykonawca) %>% 
                                   summarise(n = n()) %>% 
                                   arrange(desc(n)) %>% 
                                   top_n(20) %>% 
                                   select(Wykonawca))),
           plotlyOutput("wykres2")),
    column(6, uiOutput("utwory"),
           plotlyOutput("wykres4"))
  )
  
  
)

server <- function(input, output) {
  
  output$opis <- renderText({
    
    "Wykres po lewej stronie przedstawia 20 najczęściej słuchanych wykonawców 
    wraz z liczbą odsłuchań. Po wyborze interesujących nas artystów po prawej stronie 
    wyświetli się mapa ciepła prezentująca liczbę odsłuchań w zależności od godziny.
    Dodając kolejnych artystów dostajemy możliwość porównania pory dnia, w której 
    najczęściej słuchaliśmy wybranych wykonawców."
    
  })
  
  
  output$opis2 <- renderText({
    
    "Po wybraniu wykonawcy, na wykresie po lewej stronie zobaczymy 20 najczęściej słuchanych utworów,
    zaś po wyborze interesujących nas utworów ujrzymy mapę ciepła prezentującą liczbę odsłuchań w zależności
    od godziny."
    
  })
  
  output$utwory <- renderUI({
    
    utwory <- unique(data %>% 
                       filter(master_metadata_album_artist_name == input$wykonawca) %>% 
                       group_by(master_metadata_track_name) %>% 
                       summarise(n = n()) %>% 
                       arrange(desc(n)) %>% 
                       top_n(20) %>% 
                       mutate(Utwór = master_metadata_track_name) %>% 
                       select(Utwór)) 
    selectInput("utwory", "Wybierz utwory", utwory, multiple = TRUE)
    
  })
  

  output$wykres1 <- renderPlotly({

      data %>% 
        group_by(master_metadata_album_artist_name) %>% 
        summarise(Odsluchania = n()) %>% 
        arrange(desc(Odsluchania)) %>% 
        top_n(20) %>% 
        mutate(Artysta = fct_rev(factor(master_metadata_album_artist_name, levels = master_metadata_album_artist_name))) %>% 
        ggplot(aes(x = Artysta, y = Odsluchania)) +
        geom_col(fill = "#1ED760") +
        coord_flip() +
        theme_minimal() +
        labs(
          x = "Wykonawca",
          y = "Liczba odsłuchań"
        ) +
        scale_y_continuous(expand = c(0,0))

  
  })
  
  output$wykres2 <- renderPlotly({

    data %>% 
      filter(master_metadata_album_artist_name %in% input$wykonawca) %>% 
      group_by(master_metadata_track_name) %>% 
      summarise(Odsluchania = n()) %>% 
      arrange(desc(Odsluchania)) %>%
      top_n(20) %>% 
      mutate(Utwor = fct_rev(factor(master_metadata_track_name, levels = master_metadata_track_name))) %>% 
      ggplot(aes(x = Utwor, y = Odsluchania)) +
      geom_col(fill = "#1ED760") +
      coord_flip() +
      theme_minimal() +
      labs(
        x = "Utwór",
        y = "Liczba odsłuchań"
      ) +
      scale_y_continuous(expand = c(0,0))
    
  })
  
  output$wykres3 <- renderPlotly({
    

    validate(
      need(input$wykonawcy, "Proszę wybrać wykonawców"),
    )
    
    godziny <- data.frame(master_metadata_album_artist_name = rep(unique(data$master_metadata_album_artist_name), each  = 24),
                          hour = c("00", "01", "02", "03", "04", "05", "06", "07", "08",
                                   "09", "10", "11", "12", "13", "14", "15", "16", "17",
                                   "18", "19", "20", "21", "22", "23"))
    x <- data %>% 
      mutate(hour = stri_sub(ts, 12,13)) %>% 
      group_by(master_metadata_album_artist_name, hour) %>% 
      summarise(count = n()) %>% 
      right_join(godziny, by = c("master_metadata_album_artist_name", "hour"))
    x[is.na(x$count), "count"] <- 0

    x %>% 
      filter(master_metadata_album_artist_name %in% input$wykonawcy) %>%
      mutate(Odsłuchania = count, Godzina = hour, Artysta = master_metadata_album_artist_name) %>% 
      ggplot(aes(x = Godzina, y = Artysta, fill = Odsłuchania)) +
      geom_tile() +
      scale_fill_continuous(low="#000000", high="#1ED760") +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_y_discrete(expand = c(0,0)) +
      labs(
        x = "Godzina",
        y = "Wykonawca"
      )
    

  })
  
  output$wykres4 <- renderPlotly({
    
    validate(
      need(input$utwory, "Proszę wybrać utwory"),
    )
    
    kawalki <- data %>% filter(master_metadata_album_artist_name %in% input$wykonawca)
    godziny <- data.frame(master_metadata_track_name = rep(unique(kawalki$master_metadata_track_name), each  = 24),
                          hour = c("00", "01", "02", "03", "04", "05", "06", "07", "08",
                                   "09", "10", "11", "12", "13", "14", "15", "16", "17",
                                   "18", "19", "20", "21", "22", "23"))
    x <- data %>% 
      mutate(hour = stri_sub(ts, 12,13)) %>% 
      filter(master_metadata_album_artist_name %in% input$wykonawca) %>% 
      group_by(master_metadata_track_name, hour) %>% 
      summarise(count = n()) %>% 
      right_join(godziny, by = c("master_metadata_track_name", "hour"))
    x[is.na(x$count), "count"] <- 0
    
    x %>% 
      filter(master_metadata_track_name %in% input$utwory) %>%
      mutate(Odsłuchania = count, Godzina = hour, Utwór = master_metadata_track_name) %>% 
      ggplot(aes(x = Godzina, y = Utwór, fill = Odsłuchania)) +
      geom_tile() +
      scale_fill_continuous(low="#000000", high="#1ED760") +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_y_discrete(expand = c(0,0)) +
      labs(
        x = "Godzina",
        y = "Utwór"
      )
    
  })

}

shinyApp(ui = ui, server = server)
