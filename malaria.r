library(tidyverse)
library(tmap)

deaths <- read_csv("malaria_deaths.csv")
colnames(deaths)[4] <- "deathsper100k"


deaths <- deaths %>% 
  spread(Year, deathsper100k)
colnames(deaths)[3] <- "deaths1990"
colnames(deaths)[29] <- "deaths2016"

deaths$change <- deaths$deaths2016 - deaths$deaths1990

data("World")
World@data$name <- as.character(World@data$name)


World@data <- World@data %>% 
  filter(name != "NA")

World@data <- left_join(World@data, deaths,  by=c("name" =  "Entity"))

tm_shape(World) +
  tm_polygons("1990", n=6, palette = "Reds", style="fixed", breaks = c(0,25,50,100,150,200,Inf), 
              title = "") +
  tm_layout(main.title= "Malaria deaths per 100,000 (1990)",
            main.title.size=1)

tm_shape(World) +
  tm_polygons("change", n=6, palette = "-RdBu", 
              title = "") +
  tm_layout(main.title= "Change in Malaria deaths per 100,000 from 1990 to 2016",
            main.title.size=1)
