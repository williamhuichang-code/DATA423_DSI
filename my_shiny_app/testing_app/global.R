library(shiny)
library(shinyjs)
library(vcd)
library(MASS)
library(RColorBrewer)
library(datasets)
library(corrgram)
library(visdat)
library(forecast)
library(tidytext)
library(tidyverse)
library(janeaustenr)
library(stringr)
library(wordcloud2)
library(reshape2)
library(pls)
library(ggplot2)
library(devtools)
library(car)
# library(tabplot)
library(textdata)
library(seriation)
library(visNetwork)
library(leaflet)
library(shinycssloaders)

data(Titanic)
data(airquality)
data(EuStockMarkets)
data(gasoline)
data(iris)

lexicon <- textdata::lexicon_afinn(dir = "www")

nodes <- c("Linear Models" = 1, "Danger Zone" = 2, "Regularisation" = 3, "Errors in Data" = 4, 
           "Data Cleaning" = 5, "Distance Metrics" = 6, "Dimensionality" = 7, "Dimensional Reduction" = 8, 
           "Kernel Methods" = 9, "Outliers" = 10, "Life Cycle" = 11, "Ethics" = 12, "Paradigms" = 13, 
           "Visualisation" = 14, "Robustness" = 15)
nodes <- data.frame(id = nodes, label = names(nodes), title = names(nodes))
from <- c(1, 1, 2, 7, 2, 4, 4, 5, 5,10, 6, 8, 9, 15, 6, 1,10, 5)
to   <- c(8, 3, 8, 2, 3, 2, 5,10, 6, 6, 7, 7, 7, 10,15,4, 14,11)
label <- c("PCR", "elasticNet", "eased by", "a cause of", "mitigated by", "are fitted in", "mitigated by", 
           "of", "exploits", "measured by", "cursed by","due to","blessed by", "mitigate", "exploit",
           "suffer from","demands","part of")
edges <- data.frame(from, to, label, title = label)


choicesA <- colnames(as.data.frame(Titanic))
choicesA <- choicesA[-length(choicesA)]
choicesB <- colnames(airquality)
choicesB <- choicesB[-1]
