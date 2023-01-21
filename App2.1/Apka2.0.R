library(dplyr)
library(ggplot2)
library(shiny)
library(eurostat)
library(leaflet)
library(sf)
library(cowplot)
library(plotly)
library(scales)
library(cowplot)
library(shinyWidgets)
library(fullPage)
library(shinydashboard)
library(shinydashboardPlus)
library(shinycssloaders)
library(dashboardthemes)
library(forcats)
library(tidyr)
library(stringi)
# remotes::install_github("RinteRface/fullPage")

theme_spoti <- shinyDashboardThemeDIY(
  appFontFamily = "Futura" # ta czcionka przypomina trochę tą ze spotify i ma polskie znaki
  ,appFontColor = "#BAA7C6" # zmiana
  ,primaryFontColor = "#434C5E"
  ,infoFontColor = "#434C5E"
  ,successFontColor = "#434C5E"
  ,warningFontColor = "#434C5E"
  ,dangerFontColor = "#434C5E"

  ,bodyBackColor = "#232323" # zmiana
  ,logoBackColor = "#151515" # zmiana
  
  ,headerButtonBackColor = "#151515" # kolor  na ikonce chowania
  ,headerButtonIconColor = "#D8DEE9" # kolor pasków na ikonce chowania sidebaru
  ,headerButtonBackColorHover = "#1DB954" # hover ikonki chowania
  ,headerButtonIconColorHover = "#151515" # kolor paskow podczas hovera
  ,headerBackColor = "#151515" # header
  ,headerBoxShadowColor = "" #cien pod headerem - nie zmieniac
  ,headerBoxShadowSize = "0px 0px 0px"  # nie zmieniac
  
  
  ,sidebarBackColor = "#151515" # kolor sidebaru
  ,sidebarPadding = 0 # ma byc 0
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 5 # odleglosc wybranego od bokow sidebaru
  ,sidebarMenuBorderRadius = 5
  
  ,sidebarShadowRadius = "" 
  ,sidebarShadowColor = "0px 0px 0px"
  
  ,sidebarUserTextColor = "#D8DEE9"
    
  ,sidebarSearchBackColor = "#4C566A"
  ,sidebarSearchIconColor = "#151515"
  ,sidebarSearchBorderColor = "#4C566A"
    
  ,sidebarTabTextColor = "#ECEFF4"
  ,sidebarTabTextSize = 14
  ,sidebarTabBorderStyle = "none"
  ,sidebarTabBorderColor = "#000000"
  ,sidebarTabBorderWidth = 0
  
  ,sidebarTabBackColorSelected = "#1DB954"# kolor wybrany sidebar
  ,sidebarTabTextColorSelected = "#000000" # kolor tekst wybrany sidebar
  ,sidebarTabRadiusSelected = "20px" # zaokraglenie wybranego
  
  ,sidebarTabBackColorHover = "#1DB954"
  ,sidebarTabTextColorHover = "#000000"
  ,sidebarTabBorderStyleHover = "none"
  ,sidebarTabBorderColorHover = "none"
  ,sidebarTabBorderWidthHover = 0
  ,sidebarTabRadiusHover = "20px" # zaokroglenie hoverowanego
  
  ,boxBackColor = "#232323" # tlo wykresu
  ,boxBorderRadius = 5
  ,boxShadowSize = "0px 0px 0px"
  ,boxShadowColor = ""
  ,boxTitleSize = 18
  ,boxDefaultColor = "#232323"
  ,boxPrimaryColor = "#232323"
  ,boxInfoColor = "#232323"
  ,boxSuccessColor = "#232323"
  ,boxWarningColor = "#232323"
  ,boxDangerColor = "#232323"
    
  ,tabBoxTabColor = "#151515"
  ,tabBoxTabTextSize = 16
  ,tabBoxTabTextColor = "#151515"
  ,tabBoxTabTextColorSelected = "#151515"
  ,tabBoxBackColor = "#BF616A"
  ,tabBoxHighlightColor = "#4C566A"
  ,tabBoxBorderRadius = 5 
  
  ,buttonBackColor = "#151515"
  ,buttonTextColor = "#2E3440"
  ,buttonBorderColor = "#2E3440"
  ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "#151515"
  ,buttonTextColorHover = "#232323"
  ,buttonBorderColorHover = "#2E3440"
    
  ,textboxBackColor = "#151515"   ####
  ,textboxBorderColor = "#1ED760"  # ramka
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "#151515"
  ,textboxBorderColorSelect = "#1ED760"
    
  ### tables
  ,tableBackColor = "#151515"
  ,tableBorderColor = "#2E3440"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
)

# przygotowanie danych
df1 <- read.csv("dane_michal_1.csv")
df2 <- read.csv("dane_michal_2.csv")
df3 <- read.csv("dane_maciej.csv", sep = ";")
df4 <- read.csv("kisiel_0.csv")
df5 <- read.csv("kisiel_1.csv")
colnames(df4)[1] = "ts"
colnames(df5)[1] = "ts"
df <- rbind(df1, df2, df3)
colnames(df)[1] = "ts"
df <- rbind(df1, df2, df3, df4, df5)
data <- df


header <- dashboardHeader(
  title = tags$img(src = "https://storage.googleapis.com/pr-newsroom-wp/1/2018/11/Spotify_Logo_CMYK_Green.png",
                                             height = 40, width = 130),
  titleWidth = 250,
  leftUi = tagList(
    dropdownBlock(
      id = "sidebarmenu",
      title = "Osoba",
      badgeStatus = NULL,
      sidebarMenu(id = "sidebarmenu2",
        selectInput(
        inputId = "user",
        label = "Wybierz osobę:",
        choices = c(
          "Maciej" = "kkefrqwsrsyg2z394vrq46v5b",
          "Michał" = "21argkw6dz4lqxvriyyehsu6y",
          "Kamil" = "kisielek03"
        ))
      
      )
    )
  )
)


sidebar <- dashboardSidebar(

  sidebarMenu(
    menuItem("Analiza odsłuchań", tabName = "odsłuchania"),
    menuItem("Analiza zakończeń", tabName = "końce")
  ),
  width = 250
  
)

body <- dashboardBody(
  
  tabItems(
    
  ### --------- sekcja Michała ---------
  
    tabItem(tabName = "końce",
          fluidRow(
            column(width = 8, 
                   box(title = tags$p("Jak kończą się piosenki?", style = "font-size: 250%; text-align: center; color: #1DB954;"), 
                       solidHeader = TRUE, width = NULL, status = "warning",
                       plotlyOutput("plotKoniec", height = 450, width = 750)
                   ),
                   box(
                     title = "Mapa", status = "primary", solidHeader = FALSE, width = NULL,
                     collapsible = TRUE, collapsed = TRUE,
                     plotOutput("plotMapa", height =750)
                   )
            ),
            column(width = 4,
                   valueBoxOutput(width = NULL,"skippedBox"),
                   valueBoxOutput(width = NULL,"skipped2Box"),
                   valueBoxOutput(width = NULL,"skipped3Box")
            )
            
          )),
    
  ### --------- sekcja Maćka ---------
    
    tabItem(tabName = "odsłuchania",
            fluidRow(
              box(title = tags$p("Analiza najczęściej wybieranych artystów i ich utworów", 
                                 style = "font-size: 250%; text-align: center; color: #1DB954;"), 
                  solidHeader = TRUE, width = NULL, status = "warning")
            ),
            fluidRow(
              column(6, textOutput("opis")),
              column(6, uiOutput("listaWykonawcow2"))
            ),

            fluidRow(
              column(6, 
                     shinycssloaders::withSpinner(
                       plotlyOutput("wykres1"),
                       type = getOption("spinner.type", default = 6),
                       color = getOption("spinner.color", default = "#1DB954")
                     )),
              column(6, 
                     shinycssloaders::withSpinner(
                       plotlyOutput("wykres3"),
                       type = getOption("spinner.type", default = 6),
                       color = getOption("spinner.color", default = "#1DB954")
                    ))
            ),

            br(),
            br(),
            fluidRow(
              column(6, textOutput("opis2")),
              column(6)
            ),
            br(),
            fluidRow(
              column(6, uiOutput("listaWykonawcow")),
              column(6, uiOutput("utwory"))
            ),
            
            fluidRow(
              column(6, shinycssloaders::withSpinner(
                plotlyOutput("wykres2"),
                type = getOption("spinner.type", default = 6),
                color = getOption("spinner.color", default = "#1DB954")
              )),
              column(6, shinycssloaders::withSpinner(
                plotlyOutput("wykres4"),
                type = getOption("spinner.type", default = 6),
                color = getOption("spinner.color", default = "#1DB954")
              ))
            )
            )
    
  ### --------- sekcja Kamila ---------
  
  
        ### tu miejsce dla ciebie Kamil
  
  
  ),
    
  ### --------- sekcja technczna ---------
  
  theme_spoti,
  tags$head(
    tags$style("#opis{
                  color: #BAA7C6;
                  font-size: 15px;
                  }
                #opis2{
                  color: #BAA7C6;
                  font-size: 15px;
                  }
                #sidebarmenu:hover{
                  background-color: #1DB954;
                  color: #151515;
                  }"
               )
    ),
  tags$head(
    tags$style(HTML("
                  .selectize-input {
                  font-size: 12pt;
                  padding-top: 10px;
                  title: white !important;
                  }
                  .selectize-dropdown-content .active {
                  background: #1ED760 !important;
                  color: white !important;
                  }
                  .item {
                  background: #1ED760 !important;
                  color: black !important;
                  border-radius: 3px;
                  padding: 2px;
                  }
                  .multi {
                  width: 450px;
                  }
                  .selectize-control .option {
                  background-color: #1ED760;
                  text-decoration: none
                  }
                  .menu {
                    background-color: black !important;
                  }
                  .dropdown-menu {
                    background-color: black !important;
                    border-color: black !important;
                  }
                  .full {
                    background-color: black !important;
                  }
                "
                    )
               )
    )
  )



### ---------

ui <- dashboardPage(
  
  header,
  sidebar,
  body
  
)

server <- function(input, output) {
  
  ### --------------- poniżej się bawi Michał ----------------
  
  df$ts <- gsub("T", " ", df$ts)
  df$ts <- gsub("Z", "", df$ts)
  df$ts <- as.POSIXct(df$ts, format("%Y-%m-%d %H:%M:%OS"), tz = "UTC")
  
  df$reason_end[df$reason_end == "fwdbtn"] <- "Przycisk dalej"
  df$reason_end[df$reason_end == "backbtn"] <- "Przycisk wstecz"
  df$reason_end[df$reason_end == "logout"] <- "Wylogowano"
  df$reason_end[df$reason_end == "trackdone"] <- "Koniec piosenki"
  df$reason_end[df$reason_end == "unexpected-exit"] <- "Wyjście z aplikacji"
  df$reason_end[df$reason_end == "unexpected-exit-while-paused"] <- "Niespodziewane zakończenie\ndziałania aplikacji"
  df$reason_end[df$reason_end == "unknown"] <- "Nieznane"
  df$reason_end[df$reason_end == "endplay"] <- "Endplay???"
  df$reason_end[df$reason_end == "remote"] <- "Zdalne wyłączenie"
  
  paleta_kolor_1 <- c("#EB1D36", "#CFD2CF","#FA9494", "#E38B29","#F5EDDC","#F7A76C", "#8EA7E9", 
                               "#AACB73", "#FFEA20", "#03C988")
  
  
  
  output$plotKoniec <- renderPlotly({
    
    if(input$user != "21argkw6dz4lqxvriyyehsu6y"){
      temp_df <- df %>% 
        mutate(year = as.integer(format(df$ts, format = "%Y", tz = "UTC"))) %>% 
        filter(username == input$user) %>% 
        group_by(reason_end, year) %>% 
        summarise(ilosc = n()) %>% 
        filter(ilosc > 50)
      p1 <- temp_df %>% 
        ggplot(aes(x = year, y = ilosc, colour = reason_end)) +
        geom_path() }
    else {
      temp_df <- df %>% 
        mutate(year = as.integer(format(df$ts, format = "%Y", tz = "UTC"))) %>% 
        filter(username == "21argkw6dz4lqxvriyyehsu6y") %>% 
        group_by(reason_end, year) %>% 
        summarise(ilosc = n()) %>% 
        filter(ilosc > 1000)
      p1 <- temp_df %>% 
        ggplot(aes(x = year, y = ilosc, colour = reason_end)) +
        geom_path()
    }
    
    
    p2 <- p1 + 
      ylab("Ilość zakończeń") +
      xlab("Rok") +
      scale_color_manual(name="Powód końca", values= paleta_kolor_1) +
      scale_x_discrete(name = "Rok" ,breaks = unique(temp_df$year),labels = waiver(), limits = unique(temp_df$year)) + 
      theme(
        panel.background = element_rect(fill = "#151515", colour = "#000000",
                                        size = 2, linetype = "solid"),
        plot.background = element_rect(fill = "#232323"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "#302f2f"), 
        panel.grid.minor = element_line(size = 0.5, linetype = 'solid',
                                        colour = "#302f2f"),
        title = element_text( color = "#1DB954",
                             size = 18),
        axis.text.x = element_text( color = "#1DB954",
                                   size = 9),
        axis.text.y = element_text( color = "#1DB954", 
                                   size = 9),
        legend.background = element_rect(fill = "#232323"),
        legend.text = element_text(face = "bold", color = "#1DB954", 
                                   size = 10)
        
      ) 
    ggplotly(p2)
  })
  
  output$plotMapa <- renderPlot({
    
    kraje <- df %>% 
      mutate(year = format(df$ts, format = "%Y", tz = "UTC")) %>%
      filter(username == input$user) %>% 
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
    
    
    SHP_27$geo %in% kraje$conn_country
    
    SHP_27 %>% 
      mutate(czy_tak = ifelse(geo %in% kraje$conn_country, 1, 0)) %>%
      ggplot(aes(fill = czy_tak)) +
      geom_sf() +
      scale_x_continuous(limits = c(-10, 35)) +
      scale_y_continuous(limits = c(35, 65)) +
      theme_void() +
      scale_fill_gradient(low = "#bdbbbb", high = "#1DB954") + 
      theme(
        legend.position = "none",
        plot.background = element_rect(fill = "#232323"),
        plot.margin = margin(0,0,0,0,"cm")
      )
    
    
    
  })
  
  
  output$skippedBox <- renderValueBox({
  ile_piosenek <- df %>% 
    mutate(year = as.integer(format(df$ts, format = "%Y", tz = "UTC"))) %>% 
    filter(username == input$user) %>% 
    summarise(ilosc = n()) 
  
  ile_skip <- df %>% 
    mutate(year = as.integer(format(df$ts, format = "%Y", tz = "UTC"))) %>% 
    filter(username == input$user) %>% 
    group_by(reason_end) %>% 
    summarise(ilosc = n()) %>% 
    arrange(-ilosc) %>% 
    head(1)
  
  rok_najwiecej <- df %>% 
    mutate(year = as.integer(format(df$ts, format = "%Y", tz = "UTC"))) %>% 
    filter(username == input$user) %>% 
    group_by(reason_end,year) %>% 
    summarise(ilosc = n()) %>% 
    filter(reason_end == ile_skip[1]) %>% 
    arrange(-ilosc) %>% 
    head(1)
  
  ile_piosenek_w_rok_najwiecej <- df %>% 
    mutate(year = as.integer(format(df$ts, format = "%Y", tz = "UTC"))) %>% 
    filter(username == input$user) %>% 
    filter(year == as.integer(rok_najwiecej[2])) %>% 
    summarise(ilosc = n())  
  valueBox(tags$p(ile_piosenek, style = "font-size: 175%; text-align: center;color: #FFFFFF;"),
           tags$p("piosenek przesluchanych na spotify", style = "font-size: 125%; text-align: center;color: #FFFFFF;"), 
           #icon = icon("music", lib = "font-awesome", style = "font-size: 125%; margin-left: auto; margin-right: auto;"),
           color = "green")    
  })
  output$skipped2Box <- renderValueBox({
    ile_piosenek <- df %>% 
      mutate(year = as.integer(format(df$ts, format = "%Y", tz = "UTC"))) %>% 
      filter(username == input$user) %>% 
      summarise(ilosc = n()) 
    
    ile_skip <- df %>% 
      mutate(year = as.integer(format(df$ts, format = "%Y", tz = "UTC"))) %>% 
      filter(username == input$user) %>% 
      group_by(reason_end) %>% 
      summarise(ilosc = n()) %>% 
      arrange(-ilosc) %>% 
      head(1)
    
    rok_najwiecej <- df %>% 
      mutate(year = as.integer(format(df$ts, format = "%Y", tz = "UTC"))) %>% 
      filter(username == input$user) %>% 
      group_by(reason_end,year) %>% 
      summarise(ilosc = n()) %>% 
      filter(reason_end == ile_skip[1]) %>% 
      arrange(-ilosc) %>% 
      head(1)
    
    ile_piosenek_w_rok_najwiecej <- df %>% 
      mutate(year = as.integer(format(df$ts, format = "%Y", tz = "UTC"))) %>% 
      filter(username == input$user) %>% 
      filter(year == as.integer(rok_najwiecej[2])) %>% 
      summarise(ilosc = n())
    
    procent = as.integer((ile_skip[2]/ile_piosenek[1])*100)
    
    
    valueBox(tags$p(paste0(procent, "%"), style = "font-size: 175%; text-align: center; color: #FFFFFF;"),
             tags$p(paste0("piosenek skończyło się metodą '", ile_skip[1],"'"), style = "font-size: 125%; text-align: center;color: #FFFFFF;"), 
             #icon = icon("thumbsas-up", lib = "glyphicon"),
             color = "green")    
  })
  
  output$skipped3Box <- renderValueBox({
    ile_piosenek <- df %>% 
      mutate(year = as.integer(format(df$ts, format = "%Y", tz = "UTC"))) %>% 
      filter(username == input$user) %>% 
      summarise(ilosc = n()) 
    
    ile_skip <- df %>% 
      mutate(year = as.integer(format(df$ts, format = "%Y", tz = "UTC"))) %>% 
      filter(username == input$user) %>% 
      group_by(reason_end) %>% 
      summarise(ilosc = n()) %>% 
      arrange(-ilosc) %>% 
      head(1)
    
    rok_najwiecej <- df %>% 
      mutate(year = as.integer(format(df$ts, format = "%Y", tz = "UTC"))) %>% 
      filter(username == input$user) %>% 
      group_by(reason_end,year) %>% 
      summarise(ilosc = n()) %>% 
      filter(reason_end == ile_skip[1]) %>% 
      arrange(-ilosc) %>% 
      head(1)
    
    ile_piosenek_w_rok_najwiecej <- df %>% 
      mutate(year = as.integer(format(df$ts, format = "%Y", tz = "UTC"))) %>% 
      filter(username == input$user) %>% 
      filter(year == as.integer(rok_najwiecej[2])) %>% 
      summarise(ilosc = n())
    procent2 = as.integer((rok_najwiecej[3]/ile_piosenek_w_rok_najwiecej[1])*100)
    valueBox(tags$p(paste0(procent2, "%"), style = "font-size: 175%; text-align: center;color: #FFFFFF;"),
             tags$p(paste("zostało tak zakończonych w rekordowym ", rok_najwiecej[2], "roku"), style = "font-size: 125%; text-align: center;center;color: #FFFFFF;"), 
             #icon = icon("thumbs-up", lib = "glyphicon"),
             color = "green")    
  })
  
  
  
  ### --------------- poniżej się bawi Maciek ----------------
  
  
  output$opis <- renderText({
    
    "Wykres po lewej stronie przedstawia 20 najczęściej słuchanych wykonawców 
    wraz z liczbą odsłuchań. Po wyborze interesujących nas artystów po prawej stronie 
    wyświetli się mapa ciepła prezentująca liczbę odsłuchań w zależności od godziny.
    Dodając kolejnych artystów dostajemy możliwość porównania pory dnia, w której 
    najczęściej ich słuchaliśmy."
    

  })
  
  
  output$opis2 <- renderText({
    
    "Po wybraniu wykonawcy, na wykresie po lewej stronie zobaczymy 20 najczęściej słuchanych utworów,
    zaś po wyborze interesujących nas utworów ujrzymy mapę ciepła prezentującą liczbę odsłuchań w zależności
    od godziny."
    
  })
  
  output$utwory <- renderUI({
    
    utwory <- unique(data %>% 
                       filter(username == input$user) %>% 
                       filter(master_metadata_album_artist_name == input$wykonawca) %>% 
                       group_by(master_metadata_track_name) %>% 
                       summarise(n = n()) %>% 
                       arrange(desc(n)) %>% 
                       top_n(20) %>% 
                       mutate(Utwór = master_metadata_track_name) %>% 
                       select(Utwór)) 
    
    selectInput("utwory", "Wybierz utwory", utwory$Utwór, multiple = TRUE)
    
    
  })
  
  output$listaWykonawcow <- renderUI({
    
    dane <- unique(data %>%
                     filter(username == input$user) %>% 
                     mutate(Wykonawca = master_metadata_album_artist_name) %>%
                     group_by(Wykonawca) %>%
                     summarise(n = n()) %>%
                     arrange(desc(n)) %>%
                     top_n(20) %>%
                     select(Wykonawca))
    selectInput("wykonawca", "Wybierz wykonawcę",
                dane$Wykonawca)
    
    
    
  })
  
  output$listaWykonawcow2 <- renderUI({
    
    dane <- unique(data %>%
                     filter(username == input$user) %>%
                     mutate(Wykonawca = as.character(master_metadata_album_artist_name)) %>%
                     group_by(Wykonawca) %>%
                     summarise(n = n()) %>%
                     arrange(desc(n)) %>%
                     top_n(20) %>%
                     select(Wykonawca))
    selectInput("wykonawcy", "Wybierz wykonawców",
                dane$Wykonawca, multiple = TRUE)
    
    
  })
  
  temacik <- theme(
    
    panel.background = element_rect(fill = "#151515", colour = "#000000",
                                    size = 2, linetype = "solid"),
    plot.background = element_rect(fill = "#232323"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "#302f2f"), 
    panel.grid.minor = element_line(size = 0.5, linetype = 'solid',
                                    colour = "#302f2f"),
    title = element_text( color = "#95FFB7",
                          size = 10),
    axis.text.x = element_text( color = "#95FFB7",
                                size = 8),
    axis.text.y = element_text( color = "#95FFB7", 
                                size = 8),
    legend.background = element_rect(fill = "#232323"),
    legend.text = element_text(face = "bold", color = "#1DB954", 
                               size = 10)
    
  )
  
  
  output$wykres1 <- renderPlotly({
    
    data %>% 
      filter(username == input$user) %>% 
      group_by(master_metadata_album_artist_name) %>% 
      summarise(Odsluchania = n()) %>% 
      arrange(desc(Odsluchania)) %>% 
      top_n(20) %>% 
      mutate(Artysta = fct_rev(factor(master_metadata_album_artist_name, levels = master_metadata_album_artist_name))) %>% 
      ggplot(aes(x = Artysta, y = Odsluchania)) +
      geom_col(fill = "#1ED760") +
      labs(
        x = "WYKONAWCA",
        y = "LICZBA ODSŁUCHAŃ"
      ) +
      scale_y_continuous(expand = c(0,0)) +
      temacik +
      coord_flip()


    
  })
  
  output$wykres2 <- renderPlotly({
    
    data %>% 
      filter(username == input$user) %>% 
      filter(master_metadata_album_artist_name %in% input$wykonawca) %>% 
      group_by(master_metadata_track_name) %>% 
      summarise(Odsluchania = n()) %>% 
      arrange(desc(Odsluchania)) %>%
      top_n(20) %>% 
      mutate(Utwor = fct_rev(factor(master_metadata_track_name, levels = master_metadata_track_name))) %>% 
      ggplot(aes(x = Utwor, y = Odsluchania)) +
      geom_col(fill = "#1ED760") +
      coord_flip() +
      labs(
        x = "UTWÓR",
        y = "LICZBA ODSŁUCHAŃ"
      ) +
      scale_y_continuous(expand = c(0,0)) +
      temacik
    
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
      filter(username == input$user) %>% 
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
      theme(legend.position = "none") +
      scale_y_discrete(expand = c(0,0)) +
      labs(
        x = "GODZINA",
        y = ""
      ) +
      temacik
    
    
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
      filter(username == input$user) %>% 
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
      theme(legend.position = "none") +
      scale_y_discrete(expand = c(0,0)) +
      labs(
        x = "GODZINA",
        y = ""
      ) +
      temacik
    
  })
  
  
  
  ### --------------- poniżej się bawi Kamil  ----------------
  
  
  
  ### ---------------
  
  
}

shinyApp(ui, server)
