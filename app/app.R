library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(forcats)
library(tidyr)
library(stringi)
library(lubridate)
library(scales)

data <- read.csv("dane_maciej.csv", sep = ";")

ui <- fluidPage(
  
  titlePanel("Analiza najczęściej wybieranych artystów i utworów"),
  
  fluidRow(
    column(6),
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
    column(6, 
           selectInput("wykonawca", "Wybierz wykonawcę", 
                          unique(data %>% 
                                   mutate(Wykonawca = master_metadata_album_artist_name) %>% 
                                   group_by(Wykonawca) %>% 
                                   summarise(n = n()) %>% 
                                   arrange(desc(n)) %>% 
                                   top_n(20) %>% 
                                   select(Wykonawca)), selected = "Quebonafide"),
           plotlyOutput("wykres2")),
    column(6, uiOutput("utwory"),
           plotlyOutput("wykres4"))
  )
  

)

server <- function(input, output) {
  
  output$utwory <- renderUI({
    
    utwory <- unique(data %>% 
                       filter(master_metadata_album_artist_name == input$wykonawca) %>% 
                       group_by(master_metadata_track_name) %>% 
                       summarise(n = n()) %>% 
                       arrange(desc(n)) %>% 
                       top_n(20) %>% 
                       select(master_metadata_track_name))
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

    data %>% 
      mutate(time = as.POSIXct(strptime(stri_sub(ts, 12, 19), "%H:%M:%S"))) %>% 
      mutate(wykonawca = master_metadata_album_artist_name) %>% 
      filter(master_metadata_album_artist_name %in% input$wykonawcy) %>% 
      ggplot(aes(time, fill = wykonawca)) + 
      geom_density(alpha = 0.5) + 
      scale_x_datetime(breaks = date_breaks("2 hours"), labels=date_format("%H:%M")) +
      theme_minimal()

  })
  
  output$wykres4 <- renderPlotly({
    
    data %>% 
      mutate(time = paste(stri_sub(ts, 12, 14), "00", sep="")) %>% 
      mutate(utwor = master_metadata_track_name) %>% 
      filter(master_metadata_album_artist_name == input$wykonawca, utwor %in% input$utwory) %>% 
      ggplot(aes(time, fill = utwor)) + 
      geom_bar(position = "dodge") + 
      theme_minimal()
    
  })

}

shinyApp(ui = ui, server = server)
