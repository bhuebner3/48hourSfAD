library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
library(forcats)
library(sp)
library(rworldmap)


fao <- read.csv("FAO.csv")

str(fao)
# cleaning the data, by replacing NA values with 0:
fao[is.na(fao)] <- 0

# to figure out the continent from lat,long:

points = data.frame(lon=fao$longitude, lat=fao$latitude)

# The single argument to this function, points, is a data.frame in which:
#   - column 1 contains the longitude in degrees
#   - column 2 contains the latitude in degrees
coords2continent = function(points)
{  
    countriesSP <- getMap(resolution='low')
    #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
    
    # converting points to a SpatialPoints object
    # setting CRS directly to that from rworldmap
    pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
    
    
    # use 'over' to get indices of the Polygons object containing each point 
    indices = over(pointsSP, countriesSP)
    
    #indices$continent   # returns the continent (6 continent model)
    indices$REGION   # returns the continent (7 continent model)
    #indices$ADMIN  #returns country name
    #indices$ISO3 # returns the ISO3 code 
}
cont_fao <- coords2continent(points)
str(cont_fao)

# adding the continent column:
fao <- fao %>% add_column(continent = cont_fao) 

# to visualize the change between years for the elements: feed and food
fao %>% select(Element, Y1961:Y2013) %>%
    pivot_longer(., cols = c(Y1961:Y2013), names_to = "Var", values_to = "Val") %>%
    ggplot(aes(fill=Element, y=Val, x=Var)) + 
    geom_bar(position="stack", stat="identity")

# counting the items, to know the importance of each one
itm_count <- fct_count(fao$Item, sort = TRUE)

