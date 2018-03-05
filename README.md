---
title: "Boston"
author: "Jackie Chan"
date: "February 27, 2018"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = F, message = F, warning = F)

library(MASS)
library(ggplot2)
library(plyr)
library(dplyr)
data(Boston)

# Create a categorical variable, quarters, from medv.
quarter <- rep(NA, length(Boston$medv))
count <- 0
for (i in 1:length(quarter)){
        if (Boston$medv[i] >= 5 & Boston$medv[i] < 17.02){
                count <- count + 1
                quarter[i] <- "Q1"
        } else if (Boston$medv[i] >= 17.02 & Boston$medv[i] < 21.2){
                count <- count + 1
                quarter[i] <- "Q2"
        } else if (Boston$medv[i] >= 21.2 & Boston$medv[i] < 25){
                count <- count + 1
                quarter[i] <- "Q3"
        } else {
                count <- count + 1
                quarter[i] <- "Q4"
        }
}
Boston$quarters <- factor(quarter)
rm(count, quarter, i)
```

Background:
The dataset is in the library MASS, named Boston
Housing values in suburbs of Boston
Relation between housing values and different characteristics

Hypothesis:

house size: bigger house is more expensive than smaller one
house location: houses has proximity to facilities are more expensive
house year: older houses are cheaper
house amenities: furnished houses are more expensive
community facilities: houses in more established community are more expensive
stability: houses in lower crime rate are more expensive
famous: houses in famous community are more expensive
transport: houses close to public transport are more expensive
house environment: houses have garden are more expensive
interest rate: the higher interest rate, the lower the demand of buying houses. When demand < supply, price drops
economic growth: bad economic environment lower the housing price, vice versa
room number: the more the rooms a house has, the higher the housing value
bathroom number: same as room number

Variables in the dataset:

crim: per capita crime rate by town.
zn: proportion of residential land zoned for lots over 25,000 sq.ft.
indus: proportion of non-retail business acres per town.
chas: Charles River dummy variable (= 1 if tract bounds river; 0 otherwise).
nox: nitrogen oxides concentration (parts per 10 million).
rm: average number of rooms per dwelling.
age: proportion of owner-occupied units built prior to 1940.
dis: weighted mean of distances to five Boston employment centres.
rad: index of accessibility to radial highways.
tax: full-value property-tax rate per \$10,000.
ptratio: pupil-teacher ratio by town.
black: 1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town.
lstat: lower status of the population (percent).
medv: median value of owner-occupied homes in \$1000s.

```{r Single variable plot}
# crime rate
ggplot(Boston, aes(x = crim)) +
        geom_histogram(aes(y = ..density..),
                       color = "black",
                       fill = "white", 
                       binwidth = 5) +
        geom_density(alpha = .2, fill = "#FF6666") +
        geom_vline(xintercept = mean(Boston$crim, na.rm = T),
                   linetype = "dashed",
                   color = "red", 
                   size = 1) +
        ggtitle("Distribution of per Capita Crime Rate by Town") +
        xlab("Per Capita Crime Rate by Town")

qboston <- ddply(Boston, "quarters", summarise, crime_mean = mean(crim, na.rm = T))

ggplot(Boston, aes(x = crim, fill = quarters)) +
        geom_histogram(binwidth = 5, alpha = .5, position = "identity") +
        geom_vline(data = qboston, aes(xintercept = crime_mean, color = quarters), 
                   linetype = "dashed", size = 1)

ggplot(Boston, aes(x = crim, color = quarters)) +
        geom_density() +
        geom_vline(data = qboston, aes(xintercept = crime_mean, color = quarters), 
                   linetype = "dashed", size = 1)

# non-retail business arces
ggplot(Boston, aes(x = indus)) +
        geom_histogram(aes(y = ..density..),
                       color = "black",
                       fill = "white",
                       binwidth = 1) +
        geom_density(fill = "#FF6666", alpha = .2) +
        geom_vline(xintercept = mean(Boston$indus, na.rm = T),
                   linetype = "dashed",
                   size = 1,
                   color = "red") +
        ggtitle("Distribution of proportion of non-retail business acres per town") +
        xlab("Proportion of non-retial business acres per town")

qboston <- ddply(Boston, "quarters", summarise, indus_mean = mean(indus, na.rm = T))

ggplot(Boston, aes(x = indus, fill = quarters)) +
        geom_histogram(binwidth = 1,
                       position = "identity",
                       alpha = .5) +
        geom_vline(data = qboston, aes(xintercept = indus_mean, color = quarters),
                   linetype = "dashed",
                   size = 1)

ggplot(Boston, aes(x = indus, fill = quarters, color = quarters)) +
        geom_density(alpha = 0.2,
                     size = 1) +
        geom_vline(data = qboston, aes(xintercept = indus_mean, color = quarters),
                   size = 1,
                   linetype = "dashed")

# nitrogen oxide concentration
ggplot(Boston, aes(x = nox)) +
        geom_histogram(aes(y = ..density..),
                       color = "black", 
                       fill = "white",
                       binwidth = 0.02) +
        geom_density(alpha = 0.2,
                     fill = "#FF6666") +
        geom_vline(xintercept = mean(Boston$nox, na.rm = T),
                   linetype = "dashed",
                   color = "red",
                   size = 1) +
        ggtitle("Distribution of nitrogen oxide concentration") +
        xlab("Nitrogen oxide concentration (parts per 10 million")

qboston <- ddply(Boston, "quarters", summarise, nox_mean = mean(nox, na.rm = T))

ggplot(Boston, aes(x = nox)) +
        geom_density(alpha = 0.3,
                     aes(fill = quarters,
                         color = quarters),
                     size = 1) +
        geom_vline(data = qboston, aes(xintercept = nox_mean, 
                                       color = quarters),
                   linetype = "dashed",
                   size = 1)
```
