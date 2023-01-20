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
library(fullPage)
library(shinydashboard)
library(shinydashboardPlus)
library(dashboardthemes)

# hex spotify zielony #1DB954
# hex czarny #000000
# hex niby-czarny #151515
# szarawy #2f2f2f
# paleta 6 kolorów - c("#EB1D36", "#CFD2CF","#FA9494", "#E38B29","#F5EDDC","#F7A76C")
remotes::install_github("RinteRface/fullPage")
theme_spoti <- shinyDashboardThemeDIY(
  appFontFamily = "Arial" # zmiana
  ,appFontColor = "#151515" # zmiana
  ,primaryFontColor = "#434C5E"
  ,infoFontColor = "#434C5E"
  ,successFontColor = "#434C5E"
  ,warningFontColor = "#434C5E"
  ,dangerFontColor = "#434C5E"

  ,bodyBackColor = "#2f2f2f" # zmiana
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
  ,sidebarMenuBorderRadius = 10
  
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
  
  
  ,boxBackColor = "#2f2f2f" # tlo wykresu
  ,boxBorderRadius = 5
  ,boxShadowSize = "0px 0px 0px"
  ,boxShadowColor = ""
  ,boxTitleSize = 18
  #idk co to
  ,boxDefaultColor = "#2f2f2f"
  ,boxPrimaryColor = "#2f2f2f"
  ,boxInfoColor = "#2f2f2f"
  ,boxSuccessColor = "#2f2f2f"
  ,boxWarningColor = "#2f2f2f"
  ,boxDangerColor = "#2f2f2f"
    
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
  ,buttonTextColorHover = "#2f2f2f"
  ,buttonBorderColorHover = "#2E3440"
    
  ,textboxBackColor = "#4C566A"
  ,textboxBorderColor = "#2f2f2f"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "#3B4252"
  ,textboxBorderColorSelect = "#2E3440"
    
  ### tables
  ,tableBackColor = "#151515"
  ,tableBorderColor = "#2E3440"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
)

  header <- dashboardHeader(title = tags$img(src = "https://storage.googleapis.com/pr-newsroom-wp/1/2018/11/Spotify_Logo_CMYK_Green.png",
                                             height = 40, width = 130),
                            
                          leftUi = tagList(
                            dropdownBlock(
                              id = "uzytkownik",
                              title = "Osoba",
                              #icon = icon("sliders"),
                              selectInput(
                                inputId = "user",
                                label = "Osoba:",
                                choices = c(
                                  "Maciej" = "kkefrqwsrsyg2z394vrq46v5b",
                                  "Michal" = "21argkw6dz4lqxvriyyehsu6y",
                                  "Kamil" = "kisielek03"
                                ))
                            )
                          )
                          
                      )



sidebar <- dashboardSidebar(
  
          
  sidebarMenu(
    menuItem("Końce", tabName = "koniec")
  )
)

body <- dashboardBody(
  # tabItems(
  #   # First tab content
  #   tabItem(tabName = "mapka",
  #           fluidRow(
  #           box( title = "Mapka", width = 8, solidHeader = TRUE, status = "primary", 
  #                plotOutput("plotMapa", height = 550)),
  #           box(title = "Title 2", width = 4, solidHeader = FALSE,
  #               "duże D")
  #           )),
    
    
    # Second tab content
    tabItem(tabName = "koniec",
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
            
    ))
  ,
  theme_spoti
  # ,
  # tags$head(tags$style(HTML("
  #     /* pasek na lewo */
  #     .skin-blue .main-sidebar {
  #                             background-color: #302f2f;
  #     }
  #     /*  pasek na górze */
  #     .skin-blue .main-header .navbar {
  #                             background-color: #151515;
  #                             }
  #     /* logo w głownym pasku */
  #       .skin-blue .main-header .logo {
  #                             background-color: #151515;
  #       }
  #     /* logo najechane */
  #       .skin-blue .main-header .logo:hover {
  #                             background-color: #302f2f;
  #                             }
  #     /* wybrane na lewo */
  #       .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
  #                             background-color: #1DB954;
  #                             }
  #    
  # 
  # ")))
)



ui <- dashboardPage(
  header,
  sidebar,
  body
)

server <- function(input, output) {
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
        plot.background = element_rect(fill = "#2f2f2f"),
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
        legend.background = element_rect(fill = "#2f2f2f"),
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
        plot.background = element_rect(fill = "#2f2f2f"),
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
    
    
    valueBox(tags$p(paste(procent, "%"), style = "font-size: 175%; text-align: center; color: #FFFFFF;"),
             tags$p(paste("piosenek skończyło się metodą ", ile_skip[1]), style = "font-size: 125%; text-align: center;color: #FFFFFF;"), 
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
    valueBox(tags$p(paste(procent2, "%"), style = "font-size: 175%; text-align: center;color: #FFFFFF;"),
             tags$p(paste("zostało tak zakończonych w rekordowym ", rok_najwiecej[2], "roku"), style = "font-size: 125%; text-align: center;center;color: #FFFFFF;"), 
             #icon = icon("thumbs-up", lib = "glyphicon"),
             color = "green")    
  })
}
shinyApp(ui, server)
