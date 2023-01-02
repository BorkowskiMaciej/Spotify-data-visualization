library(dplyr)

data <- read.csv("data.csv", sep = ";")

x <- data %>%
  group_by(artistName) %>% 
  summarise(total = sum(msPlayed)/(3600*1000)) %>% 
  arrange(desc(total))

