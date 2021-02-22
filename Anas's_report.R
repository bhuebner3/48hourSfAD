library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
fao <- read.csv("FAO.csv")

str(fao)
# cleaning the data, by replacing NA values with 0:
fao[is.na(fao)] <- 0

# to visualize the change between years for the elements: feed and food
fao %>% select(Element, Y1961:Y2013) %>%
    pivot_longer(., cols = c(Y1961:Y2013), names_to = "Var", values_to = "Val") %>%
    ggplot(aes(fill=Element, y=Val, x=Var)) + 
    geom_bar(position="stack", stat="identity")

# counting the items, to know the importance of each one
itm_count <- fct_count(fao$Item, sort = TRUE)

