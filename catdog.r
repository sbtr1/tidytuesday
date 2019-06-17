library(rgdal)
library(spdep)
library(rgeos)
library(tmap)
library(cartogram)
library(tidyverse)
library(gridExtra)
library(GGally)
library(GWmodel)
library(spdep)
library(cluster)
library(reshape2)
library(stringr)
library(raster)
library(lattice)
library(cartogram)

#Read the shape files and change the FIPS Code:
boundaries <- readOGR(dsn = "States3", layer = "cb_2016_us_state_500k")
boundaries <- boundaries[!boundaries@data$STATEFP %in% c("02", "15", "72", "78", "69", "66", "60"),]




#Read the csv file and join:
catdog_data<- read_csv("catdog.csv")
catdog_data$NAME <- catdog_data$state
boundaries@data <- merge(boundaries@data, catdog_data,  by="NAME")



#Add some new variables:
boundaries@data$petpopulation <-(boundaries@data$dog_population + boundaries@data$cat_population) # catDogRatio: dog percentage from a total of dogs + cats
boundaries_carto_cat <- cartogram(boundaries, "n_households", itermax=5)

#Some plots:
tm_shape(boundaries_carto_cat) +  
  tm_fill(col="percent_cat_owners",palette="YlOrRd",style="cont", size=0.2, id="geo_label", title="") + 
  tm_layout(
    main.title="% of population that owns cats (area sized to no. of households)",
    main.title.size=1,
    legend.text.size=0.6,
    main.title.position = c("center", "top"),
    legend.position = c("right","bottom"),
    frame=FALSE,
    legend.outside=FALSE)


tm_shape(boundaries_carto_cat) +  
  tm_fill(col="percent_dog_owners",palette="YlGn",style="cont", size=0.2, id="geo_label", title="") + 
  tm_layout(
    main.title="% of population that owns dogs (area sized to no. of households)",
    main.title.size=1,
    legend.text.size=0.6,
    main.title.position = c("center", "top"),
    legend.position = c("right","bottom"),
    frame=FALSE,
    legend.outside=FALSE)


tm_shape(boundaries_carto_cat) +  
  tm_fill(col="percent_pet_households",palette="Greens",style="cont", size=0.2, id="geo_label", title="") + 
  tm_layout(
    main.title="% of population that owns pets (area sized to no. of households)",
    main.title.size=1,
    legend.text.size=0.6,
    title.position = c("center", "top"),
    legend.position = c("center","bottom"),
    frame=FALSE,
    legend.outside=TRUE)






tm_shape(boundaries) +  
  tm_fill(col="cat_population",palette="YlOrRd",style="cont", size=0.2, id="geo_label", title="") + 
  tm_layout(
    title="Cat population",
    title.snap.to.legend=TRUE,
    title.size=0.8,
    legend.text.size=0.6,
    title.position = c("right", "center"),
    legend.position = c("right","center"),
    frame=FALSE,
    legend.outside=TRUE)



#Some plots:
tm_shape(wisconsin) +  
  tm_fill(col="Trump",palette="YlOrRd",style="cont", size=0.2, id="geo_label", title="") + 
  tm_layout(
    title="% of Trump vote",
    title.snap.to.legend=TRUE,
    title.size=0.8,
    legend.text.size=0.6,
    title.position = c("right", "center"),
    legend.position = c("right","center"),
    frame=FALSE,
    legend.outside=TRUE)

#Correlation plots:
michigan@data %>%
  gather(c(median_income,unemployment_rate,median_age,veteran_rate,pct_white, pct_bluecollar, pct_degree, pop_density), 
key = "vars", value = "value") %>%
  ggplot(aes(x=value, y=Trump))+ 
  geom_point(aes(fill=Trump, size=Population),pch=21) +
  scale_fill_gradient(low = "#F08080", high = "#8C1717") +
  stat_smooth(method=lm, se=FALSE, size=1, colour="#525252")+
  facet_wrap(~vars, scales="free")+
  theme_bw() +
  theme(legend.position="none")

wisconsin@data %>%
  gather(c(median_income,unemployment,median_age,veteran_rate,pct_white, pct_bluecollar, pct_degree, pop_density), 
         key = "vars", value = "value") %>%
  ggplot(aes(x=value, y=Trump))+ 
  geom_point(aes(fill=Trump, size=Population),pch=21) +
  scale_fill_gradient(low = "#F08080", high = "#8C1717") +
  stat_smooth(method=lm, se=FALSE, size=1, colour="#525252")+
  facet_wrap(~vars, scales="free")+
  theme_bw() +
  theme(legend.position="none")

#Check for multicollinearity, eigenvalues of the correlation matrix
regressors <- subset(michigan@data, select=c("median_income", "median_age", "pct_white", "pct_bluecollar", "pct_degree", "pop_density", "veteran_rate"))
cormatrix <- round(cor(regressors),2)
eigen(cormatrix)$values 

#Drop veteran_rate
regressors <- subset(wisconsin@data, select=c("pop_density", "median_age", "pct_white", "pct_degree"))
cormatrix <- round(cor(regressors),2)
eigen(cormatrix)$values 
rm(regressors)
rm(cormatrix)

#Linear model:
mich_model <- lm(Trump ~ median_income + median_age + pct_white + pct_degree + pop_density, data=michigan@data)
summary(mich_model)
wisc_model <- lm(Trump ~  median_age + pct_white + pct_degree  , data=wisconsin@data)
summary(wisc_model)



#Plot the residuals
michigan@data$resids <- resid(mich_model)
tm_shape(michigan) +  
  tm_fill(col="resids",palette="RdBu",style="cont", size=0.2, id="geo_label", title="") + 
  tm_layout(
    title="Residuals",
    title.snap.to.legend=TRUE,
    title.size=0.8,
    legend.text.size=0.6,
    title.position = c("right", "center"),
    legend.position = c("right","center"),
    frame=FALSE,
    legend.outside=TRUE)

#Plot the residuals
wisconsin@data$resids <- resid(wisc_model)
tm_shape(wisconsin) +  
  tm_fill(col="resids",palette="RdBu",style="cont", size=0.2, id="geo_label", title="") + 
  tm_layout(
    title="Residuals",
    title.snap.to.legend=TRUE,
    title.size=0.8,
    legend.text.size=0.6,
    title.position = c("right", "center"),
    legend.position = c("right","center"),
    frame=FALSE,
    legend.outside=TRUE)



#Normalize with Z-score:
michigan@data <- michigan@data %>%
  mutate(
    Trump = (Trump-mean(Trump))/sd(Trump),
    mean_income = (median_income-mean(median_income))/sd(median_income),
    median_age = (median_age-mean(median_age))/sd(median_age),
    pct_white = (pct_white-mean(pct_white))/sd(pct_white),
    pct_degree = (pct_degree-mean(pct_degree))/sd(pct_degree),
    pop_density = (pop_density-mean(pop_density))/sd(pop_density)
  ) 

#Find optimal bandwidth (71):
bandwidth <- bw.gwr(Trump ~ median_income + median_age + pct_white + pct_degree + pop_density, data=michigan, approach = "AICc", kernel = "bisquare", adaptive = TRUE)

#Geographically-weighted model:
gw_model <- gwr.basic(Trump ~ median_income + median_age + pct_white + pct_degree + pop_density, data=michigan, bw = bandwidth,  kernel = "bisquare", adaptive = TRUE, F123.test = TRUE)


#Geographically-weighted spatial statistics:
gw_ss <- gwss(michigan, vars  =  c("Trump", "median_income", "median_age" , "pct_white", "pct_degree", "pop_density"),
              kernel = "bisquare", adaptive = TRUE, bw = bandwidth, quantile = TRUE)

#Plot GWSS
tm_shape(gw_ss$SDF) +
  tm_fill(col=colnames(gw_ss$SDF@data[46:50]), title="correlation coefficients", style="cont",palette="RdBu", size=0.2) + 
  tm_facets(free.scales = FALSE) +
  tm_layout(
    frame=FALSE,
    panel.show=TRUE,
    panel.labels=c("median income", "median age", "% white", "% degree educated", "population density"),
    panel.label.bg.color="white",
    title.snap.to.legend=FALSE,
    title.size=1,
    title.position = c("left", "top"),
    inner.margins = c(0,0,0.15,0),
    legend.title.size=1,
    legend.text.size=0.6,
    legend.outside=TRUE)

#Normalize with Z-score:
wisconsin@data <- wisconsin@data %>%
  mutate(
    Trump = (Trump-mean(Trump))/sd(Trump),
    median_age = (median_age-mean(median_age))/sd(median_age),
    pct_white = (pct_white-mean(pct_white))/sd(pct_white),
    pct_degree = (pct_degree-mean(pct_degree))/sd(pct_degree)
  ) 

#Find optimal bandwidth (71):
wisc_bandwidth <- bw.gwr(Trump ~ median_age + pct_white + pct_degree, data=wisconsin, approach = "AICc", kernel = "bisquare", adaptive = TRUE)

#Geographically-weighted model:
wisc_gw_model <- gwr.basic(Trump ~  median_age + pct_white + pct_degree , data=wisconsin, bw = wisc_bandwidth,  kernel = "bisquare", adaptive = TRUE, F123.test = TRUE)


#Geographically-weighted spatial statistics:
wisc_gw_ss <- gwss(wisconsin, vars  =  c("Trump", "median_age" , "pct_white", "pct_degree"),
              kernel = "bisquare", adaptive = TRUE, bw = wisc_bandwidth, quantile = TRUE)

#Plot GWSS
tm_shape(wisc_gw_ss$SDF) +
  tm_fill(col=colnames(wisc_gw_ss$SDF@data[27:29]), title="correlation coefficients", style="cont",palette="RdBu", size=0.2) + 
  tm_facets(free.scales = FALSE) +
  tm_layout(
    frame=FALSE,
    panel.show=TRUE,
    panel.labels=c("median age", "% white", "% degree educated"),
    panel.label.bg.color="white",
    title.snap.to.legend=FALSE,
    title.size=1,
    title.position = c("left", "top"),
    inner.margins = c(0,0,0.15,0),
    legend.title.size=1,
    legend.text.size=0.6,
    legend.outside=TRUE)




#Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
data <- michigan@data[,c(20,24:25,27:28)]
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     main="Elbow method for K means clustering, Michigan")

#K-means clustering:
michigan@data$cluster <- NULL
michCluster <- kmeans(michigan@data[,c(20,24:25,27:28)], 3, nstart = 20)
michigan@data <- cbind(michigan@data, michCluster$cluster)
names(michigan@data)[names(michigan@data) == 'michCluster$cluster'] <- 'cluster'

#Plot the clusters:
colors <- c('#1b9e77','#d95f02','#7570b3')
colors2 <- c('#8dd3c7','#ffffb3','#bebada','#fb8072')
tm_shape(michigan) +  
  tm_fill(col="cluster",style="cont", palette=colors, size=0.2, id="geo_label", title.show=FALSE, legend.show=FALSE) + 
  tm_layout(frame=FALSE)

#Same for Wisconin:
#Elbow Method for finding the optimal number of clusters
# Compute and plot wss for k = 2 to k = 15.
data <- wisconsin@data[,c(16:17,19)]
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     main="Elbow method for K means clustering, Wisconsin")

#K-means clustering:
wisconsin@data$cluster <- NULL
wiscCluster <- kmeans(wisconsin@data[,c(16:17,19)], 4, nstart = 20)
wisconsin@data <- cbind(wisconsin@data, wiscCluster$cluster)
names(wisconsin@data)[names(wisconsin@data) == 'wiscCluster$cluster'] <- 'cluster'

#Plot the clusters:
tm_shape(wisconsin) +  
  tm_fill(col="cluster",style="cont", palette=colors2, size=0.2, id="geo_label", title.show=FALSE, legend.show=FALSE) + 
  tm_layout(frame=FALSE)