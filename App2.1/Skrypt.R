library(dplyr)
library(ggplot2)
library(shiny)
library(tidyverse)
library(eurostat)
library(leaflet)
library(sf)
library(scales)
library(cowplot)
library(ggthemes)
library(plotly)

# hex spotify zielony #1DB954
# hex czarny #000000
# hex niby-czarny #151515
# paleta 6 kolorów - c("#EB1D36", "#CFD2CF","#FA9494", "#E38B29","#F5EDDC","#F7A76C")


df1 <- read.csv("dane_michal_1.csv")
df2 <- read.csv("dane_michal_2.csv")
df3 <- read.csv("dane_maciej.csv", sep = ";")
df <- rbind(df1, df2, df3)

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

paleta_kolor_1 <- c("#EB1D36", "#CFD2CF","#FA9494", "#E38B29","#F5EDDC","#F7A76C")


p1 <- df %>% 
  mutate(year = as.integer(format(df$ts, format = "%Y", tz = "UTC"))) %>% 
  filter(username == "21argkw6dz4lqxvriyyehsu6y") %>% 
  group_by(reason_end, year) %>% 
  summarise(ilosc = n()) %>% 
  filter(ilosc > 1000) %>% 
  ggplot(aes(x = year, y = ilosc, colour = reason_end)) +
  geom_path() 
  
p2 <- p1 + 
  labs(title = "Jak kończą się piosenki?") + 
  ylab("Ilość zakończeń") +
  xlab("Rok") +
  scale_color_discrete(name="Powód końca") +
  scale_color_manual(name="Powód końca",values= paleta_kolor_1)+
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
  



#class(jd$)


# df %>% 
#   mutate(year = format(df$ts, format = "%Y", tz = "UTC")) %>% 
#   filter(year == 2022) %>% 
#   group_by(conn_country) %>% 
#   summarise(kraj = n()) %>% 
#   filter(conn_country != "PL") %>% 
#   ggplot(aes(x = conn_country, y = kraj)) +
#   geom_col()
  
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


SHP_27$geo %in% kraje$conn_country

SHP_27 %>% 
  mutate(czy_tak = ifelse(geo %in% kraje$conn_country, 1, 0)) %>%
  ggplot(aes(fill = czy_tak)) +
  geom_sf() +
  scale_x_continuous(limits = c(-10, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  theme_void()





