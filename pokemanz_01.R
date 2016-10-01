library(ggplot2)
library(repr)
library(dplyr)
library(tidyr)
library(Hmisc)
library(corrplot)


pokemanz <- read.csv("C://Users//lenovo//Desktop//pokemon.csv", header = T, stringsAsFactors = F)
head(pokemanz)
str(pokemanz)
describe(pokemanz)

##Basic plots

qplot(pokemanz$Total, geom ="histogram", binwidth = 6, main = "Histogram for Pokemon Total",
      xlab = "Total", fill = I("blue"), col = I("red"), alpha = I(.2),
      xlim = c(200, 800))

qplot(pokemanz$Speed, geom ="histogram", binwidth = 3, main = "Histogram for Pokemon Speed",
      xlab = "Speed", fill = I("blue"), col = I("red"), alpha = I(.2),
      xlim = c(1, 200))

qplot(pokemanz$Attack, geom ="histogram", binwidth = 3, main = "Histogram for Pokemon Attack",
      xlab = "Attack", fill = I("blue"), col = I("red"), alpha = I(.2),
      xlim = c(1, 200))

qplot(pokemanz$SpAtk, geom ="histogram", binwidth = 3, main = "Histogram for Pokemon Special Attack",
      xlab = "Special Attack", fill = I("blue"), col = I("red"), alpha = I(.2),
      xlim = c(1, 200))

qplot(pokemanz$Defense, geom ="histogram", binwidth = 3, main = "Histogram for Pokemon Defense",
      xlab = "Defense", fill = I("blue"), col = I("red"), alpha = I(.2),
      xlim = c(1, 200))

qplot(pokemanz$SpDef, geom ="histogram", binwidth = 3, main = "Histogram for Pokemon Special Defense",
      xlab = "Special Defense", fill = I("blue"), col = I("red"), alpha = I(.2),
      xlim = c(15, 200))

qplot(pokemanz$HP, geom ="histogram", binwidth = 3, main = "Histogram for Pokemon HP",
      xlab = "HP", fill = I("blue"), col = I("red"), alpha = I(.2),
      xlim = c(15, 200))

ggplot(data=pokemanz, aes(pokemanz$Total)) +
  geom_histogram(aes(y=..density..),
                 breaks = seq(200, 800, by = 15),
                 col = "red",
                 fill = "green",
                 alpha = .5) +
  geom_density(col = 2) +
  labs(title = "Histogram of Pokemon Total") +
  labs(x = "Total", y = "Count")

ggplot(data=pokemanz, aes(pokemanz$Speed)) +
  geom_histogram(aes(y=..density..),
                 breaks = seq(0, 200, by = 5),
                 col = "red",
                 fill = "green",
                 alpha = .5) +
  geom_density(col = 2) +
  labs(title = "Histogram of Pokemon Speed") +
  labs(x = "Speed", y = "Count")

ggplot(data=pokemanz, aes(pokemanz$Attack)) +
  geom_histogram(aes(y=..density..),
                 breaks = seq(1, 200, by = 5),
                 col = "red",
                 fill = "green",
                 alpha = .5) +
  geom_density(col = 2) +
  labs(title = "Histogram of Pokemon Attack") +
  labs(x = "Attack", y = "Count")

ggplot(data=pokemanz, aes(pokemanz$SpAtk)) +
  geom_histogram(aes(y=..density..),
                 breaks = seq(1, 200, by = 5),
                 col = "red",
                 fill = "green",
                 alpha = .5) +
  geom_density(col = 2) +
  labs(title = "Histogram of Pokemon Special Attack") +
  labs(x = "Special Attack", y = "Count")

ggplot(data=pokemanz, aes(pokemanz$Defense)) +
  geom_histogram(aes(y=..density..),
                 breaks = seq(1, 200, by = 5),
                 col = "red",
                 fill = "green",
                 alpha = .5) +
  geom_density(col = 2) +
  labs(title = "Histogram of Pokemon Defense") +
  labs(x = "Defense", y = "Count")

ggplot(data=pokemanz, aes(pokemanz$SpDef)) +
  geom_histogram(aes(y=..density..),
                 breaks = seq(10, 200, by = 5),
                 col = "red",
                 fill = "green",
                 alpha = .5) +
  geom_density(col = 2) +
  labs(title = "Histogram of Pokemon Special Defense") +
  labs(x = "Special Defense", y = "Count")

ggplot(data=pokemanz, aes(pokemanz$HP)) +
  geom_histogram(aes(y=..density..),
                 breaks = seq(10, 200, by = 5),
                 col = "red",
                 fill = "green",
                 alpha = .5) +
  geom_density(col = 2) +
  labs(title = "Histogram of Pokemon Special Defense") +
  labs(x = "HP", y = "Count")

##Type 1 Pokemon
pokemanz_plot01 <- ggplot(pokemanz, aes(x=Total, fill=Type1)) + geom_density(alpha = 1)
pokemanz_plot01 <- pokemanz_plot01 + 
  facet_wrap(~Type1) + 
  labs(x = "Total", y = "Density", title = "Pokemon Total Score (Type 1)") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

pokemanz_plot01


pokemanz_plot02 <- ggplot(pokemanz, aes(x=Total, fill=Type1)) + geom_density(alpha = 1)
pokemanz_plot02<- pokemanz_plot02 + 
  facet_wrap(~Type1) + 
  labs(x = "Speed", y = "Density", title ="Pokemon Speed Score") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

pokemanz_plot02


pokemanz_plot03 <- ggplot(pokemanz, aes(x=Attack, fill=Type1)) + geom_density(alpha = 1)
pokemanz_plot03 <- pokemanz_plot03 + 
  facet_wrap(~Type1) + 
  labs(x = "Attack", y = "Density", title ="Pokemon Attack Score") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

pokemanz_plot03

pokemanz_plot04 <- ggplot(pokemanz, aes(x=SpAtk, fill=Type1)) + geom_density(alpha = 1)
pokemanz_plot04 <- pokemanz_plot04 + 
  facet_wrap(~Type1) + 
  labs(x = "Special Attack", y = "Density", title ="Pokemon Special Attack Score") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

pokemanz_plot04

pokemanz_plot05 <- ggplot(pokemanz, aes(x=Defense, fill=Type1)) + geom_density(alpha = 1)
pokemanz_plot05<- pokemanz_plot05 + 
  facet_wrap(~Type1) + 
  labs(x = "Defense", y = "Density", title ="Pokemon Defense Score") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

pokemanz_plot05

pokemanz_plot06 <- ggplot(pokemanz, aes(x=SpDef, fill=Type1)) + geom_density(alpha = 1)
pokemanz_plot06<- pokemanz_plot06 + 
  facet_wrap(~Type1) + 
  labs(x = "Special Defense", y = "Density", title ="Pokemon Special Defense Score") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

pokemanz_plot06

pokemanz_plot07 <- ggplot(pokemanz, aes(x=HP, fill=Type1)) + geom_density(alpha = 1)
pokemanz_plot07<- pokemanz_plot07 + 
  facet_wrap(~Type1) + 
  labs(x = "HP", y = "Density", title ="Pokemon HP Score") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

pokemanz_plot07


