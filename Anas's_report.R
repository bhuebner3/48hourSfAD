library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
library(forcats)
library(sp)
library(rworldmap)
library(reshape2)
​
​
fao <- read.csv("FAO.csv")
​
str(fao)
# cleaning the data, by replacing NA values with 0:
fao[is.na(fao)] <- 0
​
# to figure out the continent from lat,long:
​
points = data.frame(lon=fao$longitude, lat=fao$latitude)
​
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
​
# adding the continent column:
fao <- fao %>% add_column(continent = cont_fao) 
​
# to visualize the change between years for the elements: feed and food
fao %>% select(Element, Y1961:Y2013) %>%
    pivot_longer(., cols = c(Y1961:Y2013), names_to = "Var", values_to = "Val") %>%
    ggplot(aes(fill=Element, y=Val, x=Var)) + 
    geom_bar(position="stack", stat="identity")
​
fao %>% select(continent, Y1961:Y2013) %>%
    pivot_longer(., cols = c(Y1961:Y2013), names_to = "Var", values_to = "Val") %>%
    ggplot(aes(fill=continent, y=Val, x=Var)) + 
    geom_bar(position="stack", stat="identity")
​
# counting the items, to know the importance of each one
itm_count <- fct_count(fao$Item, sort = TRUE)
​
​
​
​
### MORITZ
### Categorizing Vegan / Vegetarian / Carnivorous
​
# distinct list of item code and item:
item_codes <- fao %>% 
    group_by(Item, Item.Code) %>% 
    tally()
#write.csv(diet_group,"\\diet_groups.csv", row.names = TRUE)
​
# group food into vegetarian/meat by item code
code_list_vegan         <-  c(2511:2680, 2775, 2805, 2905:2924)
code_list_vegetarian    <-  c(2740:2745, 2848, 2949)
code_list_carnivorous   <-  c(2731:2737, 2761:2769, 2781, 2782, 2928:2948, 2960, 2961)
​
# I think we should use Animal/Non-Animal classification to gein the best fit with the Feed/Food division of the data but this could be up for discussion.
grouped_fao <- fao %>% 
    mutate(diet        = case_when( Item.Code %in% code_list_vegan        ~ "Vegan",
                                    Item.Code %in% code_list_vegetarian   ~ "Vegetarian",
                                    Item.Code %in% code_list_carnivorous  ~ "Carnivourous"))  %>%
    mutate(diet_meat   = case_when( Item.Code %in% code_list_vegan        ~ "Non-Meat",
                                    Item.Code %in% code_list_vegetarian   ~ "Non-Meat",
                                    Item.Code %in% code_list_carnivorous  ~ "Meat"))          %>%
    mutate(diet_animal = case_when( Item.Code %in% code_list_vegan        ~ "Non-Animal",
                                    Item.Code %in% code_list_vegetarian   ~ "Animal",
                                    Item.Code %in% code_list_carnivorous  ~ "Animal"))        %>%
    mutate(flag_meat   = case_when( Item.Code %in% code_list_vegan        ~ FALSE,
                                    Item.Code %in% code_list_vegetarian   ~ FALSE,
                                    Item.Code %in% code_list_carnivorous  ~ TRUE))            %>%
    mutate(flag_animal = case_when( Item.Code %in% code_list_vegan        ~ FALSE,
                                    Item.Code %in% code_list_vegetarian   ~ TRUE,
                                    Item.Code %in% code_list_carnivorous  ~ TRUE))           
​
​
diet_group <- grouped_fao %>% 
    group_by(diet) %>% 
    tally()
​
​
​
### ANAS
### converting the wide data set into a tall one:
melted_fao <- grouped_fao %>% select(Area, Element, Item, diet, flag_animal, Y1961 : Y2013) %>%
    melt(id = c("Area","Element","Item", "diet", "flag_animal"))
                   
