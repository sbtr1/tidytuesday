library(tidyverse)
library(ggthemes)
library(ggridges)
library(ggmap)
library(leaflet)
library(caret)
library(randomForest)

addresses <- read_csv("ascendas_geocode.csv")
addresses$X1 <- NULL


addresses %>% 
  ggplot(aes(x=Ascendas_Building, y=psf, color=`Property Type`)) +
  geom_jitter() +
  theme(axis.title.x = element_blank(), 
        legend.position = "bottom") +
  labs(title = "Exploring Ascendas and non-Ascendas properties") +
  NULL

addresses %>% 
  mutate(District = fct_reorder(District, psf, fun=median)) %>% 
  ggplot(aes(x=psf, y=District, fill=Region)) +
  geom_density_ridges(scale = 1.8, rel_min_height = 0.01) +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        legend.position = "bottom") +
  xlim(0,15) +
  labs(title = "Distribution of psf by District") +
  NULL

addresses %>% 
  mutate(District = fct_reorder(District, psf, fun=median)) %>% 
  ggplot(aes(x=psf, y=District, fill=Region)) +
  geom_density_ridges(scale = 1.8, rel_min_height = 0.01) +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        legend.position = "bottom") +
  xlim(0,15) +
  labs(title = "Distribution of psf by District - Ascendas v. non-Ascendas") +
  facet_wrap(~Ascendas_Building) + 
  NULL

addresses %>% 
  mutate(`Property Type` = fct_reorder(`Property Type`, psf, fun=median)) %>% 
  ggplot(aes(x=psf, y=`Property Type`, fill=`Property Type`)) +
  geom_density_ridges(scale = 1.2, rel_min_height = 0.01) +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        legend.position = "bottom") +
  xlim(0,15) +
  labs(title = "Distribution of psf by Property Type") +
  NULL

addresses %>% 
  mutate(`Property Type` = fct_reorder(`Property Type`, psf, fun=median)) %>% 
  ggplot(aes(x=psf, y=`Property Type`, fill=`Property Type`)) +
  geom_density_ridges(scale = 1.2, rel_min_height = 0.01) +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        legend.position = "bottom") +
  xlim(0,15) +
  labs(title = "Distribution of psf by Property Type - Ascendas v. non-Ascendas") +
  facet_wrap(~Ascendas_Building) + 
  NULL

addresses %>% 
  mutate(lease = fct_reorder(lease, psf, fun=median)) %>% 
  ggplot(aes(x=psf, y=lease, fill=lease)) +
  geom_density_ridges(scale = 1.2, rel_min_height = 0.01) +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        legend.position = "bottom") +
  xlim(0,15) +
  labs(title = "Distribution of psf by Lease") +
  facet_wrap(~`Property Type`) +
  NULL

addresses %>% 
  filter(lease != "NA") %>% 
  filter(lease != "Unknown Tenure") %>% 
  mutate(lease = fct_reorder(lease, psf, fun=median)) %>% 
  ggplot(aes(x=psf, y=lease, fill=lease)) +
  geom_density_ridges(scale = 1.2, rel_min_height = 0.01) +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        legend.position = "bottom") +
  xlim(0,15) +
  labs(title = "Distribution of psf by Lease - Ascendas v. non-Ascendas") +
  facet_wrap(~Ascendas_Building) +
  NULL

pal <- colorFactor(c("navy", "red", "green", "blue", "grey"), domain = c("Office", "Factory / Workshop", "Light Industrial", "Warehouse", "Business / Science Park"))

m <- leaflet() %>% 
  addTiles() %>% 
  setView(lng = 103.8, lat = 1.35, zoom = 11) %>% 
  addCircleMarkers(lng = addresses$lon, lat = addresses$lat,
                   radius=addresses$psf**0.6,
                   color = ~pal(`Property Type`),
                   fillOpacity = 0.3)
m

addresses %>% 
  ggplot(aes(x=psf,y=log(sqft), color=`Property Type`)) +
  geom_point(alpha=0.2) +
  geom_smooth(method='lm') +
  #facet_wrap(~`Property Type`) +
  ylim(0, 15) + 
  labs(title = "Do larger properties have lower psf?", subtitle = "Log(sqft) vs psf($)") +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        legend.position = "bottom") +
  NULL


myvars <- c("District", "Property Type", "Ascendas_Building", "lease", "sqft", "psf")

address_modelling <- addresses[myvars]

dmy <- dummyVars(" ~ .", data=address_modelling, fullRank=T)
address_transformed <- data.frame(predict(dmy, newdata=address_modelling))

trainIndex <- createDataPartition(address_transformed$psf, p=0.8, list=FALSE, times=1)


address_transformed[is.na(address_transformed)] <- 0
addressTrain <- address_transformed[trainIndex,]
addressTest <- address_transformed[-trainIndex,]

Xtrain <- addressTrain[,1:38]
yTrain <- addressTrain[,39]
Xtest <- addressTest[,1:38]
yTest <- addressTest[,39]

model_lm <- train(psf ~ ., 
                  data = addressTrain, 
                  method = "lm")
  
  
summary(model_lm)

