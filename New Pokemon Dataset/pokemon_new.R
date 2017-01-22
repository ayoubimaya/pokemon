library(data.table)
library(ggplot2)

pokemon <- read.csv("C://Users//lenovo//Desktop//pokemon_new.csv")

pokemanz <- subset(pokemon, Index>721 & Index<803)

stats <- pokemanz[ ,c(7,8,9,10,11,12)]
summary(stats)
