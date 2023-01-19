library(dplyr)
library(ggplot2)
library(shiny)
#library(tidyverse)
library(eurostat)
library(leaflet)
library(sf)
library(cowplot)
library(plotly)
library(scales)
library(cowplot)
library(shinyWidgets)
remotes::install_github("RinteRface/fullPage")
library(fullPage)
library(shinydashboard)

# library(sysfonts)
# font_add_google("Open Sans")
# Zabawa z czcionkami - chuj 

df1 <- read.csv("dane_michal_1.csv")
df2 <- read.csv("dane_michal_2.csv")
df3 <- read.csv("dane_maciej.csv", sep = ";")
df <- rbind(df1, df2, df3)

df$reason_end[df$reason_end == "fwdbtn"] <- "Przycisk dalej"
df$reason_end[df$reason_end == "backbtn"] <- "Przycisk wstecz"
df$reason_end[df$reason_end == "logout"] <- "Wylogowano"
df$reason_end[df$reason_end == "trackdone"] <- "Koniec piosenki"
df$reason_end[df$reason_end == "unexpected-exit"] <- "Wyjście z aplikacji"
df$reason_end[df$reason_end == "unexpected-exit-while-paused"] <- "Niespodziewane zakończenie\ndziałania aplikacji"
df$reason_end[df$reason_end == "unknown"] <- "Nieznane"
df$reason_end[df$reason_end == "endplay"] <- "Endplay???"

paleta_kolor_1 <- c("#EB1D36", "#CFD2CF","#FA9494", "#E38B29","#F5EDDC","#F7A76C")

df$ts <- gsub("T", " ", df$ts)
df$ts <- gsub("Z", "", df$ts)
df$ts <- as.POSIXct(df$ts, format("%Y-%m-%d %H:%M:%OS"), tz = "UTC")
kraje <- df %>% 
  mutate(year = format(df$ts, format = "%Y", tz = "UTC")) %>% 
  filter(year == 2022) %>% 
  group_by(conn_country) %>% 
  summarise(kraj = n()) %>% 
  select(conn_country)
SHP_0 <- get_eurostat_geospatial(resolution = 10, 
                                 nuts_level = 0, 
                                 year = 2016)
EU28 <- eu_countries %>% 
  select(geo = code, name)
EU27 <- eu_countries %>% 
  filter(code != 'UK') %>% 
  select(geo = code, name)  
SHP_28 <- SHP_0 %>% 
  select(geo = NUTS_ID, geometry) %>% 
  inner_join(EU28, by = "geo") %>% 
  arrange(geo) %>% 
  st_as_sf()
SHP_27 <- SHP_0 %>% 
  select(geo = NUTS_ID, geometry) %>% 
  inner_join(EU27, by = "geo") %>% 
  arrange(geo) 




SHP_27 %>% 
  mutate(czy_tak = ifelse(geo %in% kraje$conn_country, 1, 0)) %>%
  ggplot(aes(fill = czy_tak)) +
  geom_sf() +
  scale_x_continuous(limits = c(-10, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  theme_void()


ui_resend <- shinyUI(fullPage(
              fluidRow(
                column(width = 12,
                       element_rect(colour = "green")
                       )
              ),
              fluidRow(
                column(width = 3,
                      selectInput(
                        inputId = "user",
                        label = "Osoba:",
                        choices = c(
                          "Maciej" = "kkefrqwsrsyg2z394vrq46v5b",
                          "Michal" = "21argkw6dz4lqxvriyyehsu6y"
                          #"Kamil" = ""
                        ))),
                column(width = 6, 
                  titlePanel("S S S S"),
                  plotlyOutput("Plot1",  width = "150%"),
                  setBackgroundColor("#2f2f2f")),
                column(width = 3,
                       element_text("tekst1"))
  )
        )
)

ui_resend2 <- shinyUI(fluidPage(
  
  titlePanel("Mapa"),
  
  
  mainPanel(
    sliderInput("rokSuwak", "Wybierz lata", min = 2016, max = 2022, value = c(2016,2022)),
    plotOutput("nwmWsm", width = "79%")
  ),
  setBackgroundColor("#2f2f2f")
  
  
  
))

server <- function(input, output, session) {

  output$Plot1 <- renderPlotly({
    
    p1 <- df %>% 
      mutate(year = as.integer(format(df$ts, format = "%Y", tz = "UTC"))) %>% 
      filter(username == input$user) %>% 
      group_by(reason_end, year) %>% 
      summarise(ilosc = n()) %>% 
      filter(ilosc > 1000) %>% 
      ggplot(aes(x = year, y = ilosc, colour = reason_end)) +
      geom_path() 
    
    p2 <- p1 + 
      labs(title = "Jak kończą się piosenki?") + 
      ylab("Ilość zakończeń") +
      xlab("Rok") +
      scale_color_manual(name="Powód końca", values= paleta_kolor_1)+
      theme(
        panel.background = element_rect(fill = "#151515", colour = "#000000",
                                        size = 2, linetype = "solid"),
        plot.background = element_rect(fill = "#2f2f2f"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "#302f2f"), 
        panel.grid.minor = element_line(size = 0.5, linetype = 'solid',
                                        colour = "#302f2f"),
        title = element_text(face = "bold", color = "#1DB954",
                             size = 18),
        axis.text.x = element_text(face = "bold", color = "#1DB954",
                                   size = 9),
        axis.text.y = element_text(face = "bold", color = "#1DB954", 
                                   size = 9),
        legend.background = element_rect(fill = "#2f2f2f"),
        legend.text = element_text(face = "bold", color = "#1DB954", 
                                   size = 10)
      )
    
    ggplotly(p2)
  })
  
  output$nwmWsm <- renderPlot({
    kraje <- df %>% 
      mutate(year = format(df$ts, format = "%Y", tz = "UTC")) %>% 
      filter(year >= input$rokSuwak[1], year <= input$rokSuwak[2]) %>% 
      group_by(conn_country) %>% 
      summarise(kraj = n()) %>% 
      select(conn_country)
    
    SHP_0 <- get_eurostat_geospatial(resolution = 10, 
                                     nuts_level = 0, 
                                     year = 2016)
    EU28 <- eu_countries %>% 
      select(geo = code, name)
    EU27 <- eu_countries %>% 
      filter(code != 'UK') %>% 
      select(geo = code, name)  
    SHP_28 <- SHP_0 %>% 
      select(geo = NUTS_ID, geometry) %>% 
      inner_join(EU28, by = "geo") %>% 
      arrange(geo) 
    
    SHP_27 <- SHP_0 %>% 
      select(geo = NUTS_ID, geometry) %>% 
      inner_join(EU27, by = "geo") %>% 
      arrange(geo)
    
    f1 <- SHP_27 %>% 
      mutate(czy_tak = ifelse(geo %in% kraje$conn_country, 1, 0)) %>%
      ggplot(aes(fill = czy_tak)) +
      geom_sf() +
      scale_x_continuous(limits = c(-10, 35)) +
      scale_y_continuous(limits = c(35, 65)) +
      theme_void()+
      scale_fill_gradient(low = "#bdbbbb", high = "#1DB954") + 
      theme(
        legend.position = "none",
        plot.background = element_rect(fill = "#2f2f2f"),
        plot.margin = margin(0,0,0,0,"cm")
        )
      
    f1  
    
    
    
    
  })
  output$tekst1 <- renderText({
    
    # ile <- df %>% 
    #   mutate(year = format(df$ts, format = "%Y", tz = "UTC")) %>% 
    #   filter(year >= input$rokSuwak[1], year <= input$rokSuwak[2]) %>% 
    #   #filter(year == 2017) %>% 
    #   group_by(reason_end) %>%
    #   summarise(ilosc = n()) %>% 
    #   filter(reason_end == "trackdone") %>% 
    #   select(ilosc)
    # 
    # ile_lacznie <- df %>% 
    #   mutate(year = format(df$ts, format = "%Y", tz = "UTC")) %>% 
    #   filter(year >= input$rokSuwak[1], year <= input$rokSuwak[2]) %>% 
    #   #filter(year == 2021) %>% 
    #   summarise(ilosc = n()) %>% 
    #   select(ilosc)
    # paste("W latach wybranych latach zostało pominiętych ", ile ,
    #       "piosenek, co daje ", 100*ile/ile_lacznie, "procent piosenek w pełni przesłuchanych.")
    "ODDDKK"
    
    
    
    
  })
}

app_ui <- navbarPage(
  title = "Spoti",
  tabPanel("uno", ui_resend),
  tabPanel("dos", ui_resend2)
)



shinyApp(app_ui, server)


