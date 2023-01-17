library(dplyr)
library(shiny)
library(ggplot2)
library(tsibble)
library(forcats)


df <- rbind(read.csv("kisiel_0.csv"), read.csv("kisiel_1.csv"), read.csv("dane_michal_1.csv"), read.csv("dane_michal_2.csv"), read.csv("dane_maciej.csv", sep = ";")) %>% 
  arrange(ts)
df$ts <- as.Date(substr(df$ts,1,10))

ui <- shinyUI(fluidPage(
  titlePanel(HTML("Najczesciej sluchane utwory na spotify kazdego z nas")),
  fluidRow(
    column(width = 6,
      sidebarPanel(
                   sliderInput("ts1","Okres:",
                               min = min(df$ts), 
                               max = max(df$ts),
                               value = c(min(df$ts),max(df$ts)),
                               step = 1),
                   selectInput("user1", "Osoba: ",
                               choices = c(
                                 "Maciej" = "kkefrqwsrsyg2z394vrq46v5b",
                                 "Michal" = "21argkw6dz4lqxvriyyehsu6y",
                                 "Kamil" = "kisielek03"
                               ))
      )
    ),
    column(width = 6,
      plotOutput("barPlot1")
    )
  ),
  fluidRow(
    column(width = 6,
      sidebarPanel(
                   sliderInput("ts2","Dzien:",
                               min = min(df$ts), 
                               max = max(df$ts),
                               value = min(df$ts),
                               step = 30,
                               animate = TRUE),
                   selectInput("user2", "Osoba: ",
                               choices = c(
                                 "Maciej" = "kkefrqwsrsyg2z394vrq46v5b",
                                 "Michal" = "21argkw6dz4lqxvriyyehsu6y",
                                 "Kamil" = "kisielek03"
                               ))
      )
    ),
    column(width = 6,
      plotOutput("barPlot2")
    )
  )
))

shinyServer(
  server <- function(input, output){
    output$barPlot1 <- renderPlot({
      df <- df %>% 
        filter(ts >= input$ts1[1], ts <= input$ts1[2], username == input$user1[1]) %>% 
        count(master_metadata_track_name, sort = TRUE) %>% 
        head(10)
      colnames(df)[2] <- "number_of_plays"
      ggplot(df, aes(x = reorder(master_metadata_track_name, number_of_plays), y = number_of_plays, label = number_of_plays)) +
        geom_bar(stat = "identity") +
        geom_text(size = 10, position = position_stack(vjust = 0.5)) +
        labs(title = "Najczesciej sluchane utwory w wybranym okresie", x = "Tytul utworu", y = "Liczba odtworzen") +
        coord_flip()
    })
    output$barPlot2 <- renderPlot({
      df <- df %>% filter(ts <= input$ts2[1], username == input$user2[1])
      df <- df %>%
        count(master_metadata_track_name, sort = TRUE) %>% 
        head(10)
      colnames(df)[2] <- "number_of_plays"
      ggplot(df, aes(x = reorder(master_metadata_track_name, number_of_plays), y = number_of_plays, label = number_of_plays)) +
        geom_bar(stat = "identity") +
        geom_text(size = 10, position = position_stack(vjust = 0.5)) +
        labs(title = "Najpopularniejsze utwory do danego dnia", x = "Tytul utworu", y = "Liczba odtworzen") +
        coord_flip()
    })
  }
)

shinyApp(ui, server)