library(ggplot2)
library(dplyr)
library(tidyr)
fao <- read.csv("FAO.csv")

str(fao)
# cleaning the data, by removing the NA values:
cl_fao <- fao[complete.cases(fao),]

# to visualize the change between years for the elements: feed and food
cl_fao %>% select(Element, Y1961:Y2013) %>%
    pivot_longer(., cols = c(Y1961:Y2013), names_to = "Var", values_to = "Val") %>%
    ggplot(aes(fill=Element, y=Val, x=Var)) + 
    geom_bar(position="stack", stat="identity")
