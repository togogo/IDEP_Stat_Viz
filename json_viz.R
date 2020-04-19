#libraries
library(jsonlite)
library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)

#data reading
x = fromJSON("./data/result_try2.json") #first read it as a list, then work on it individually.

#data reorganizing
textAmountCount <- data.frame(x['text amount count'])
endText <- x['final text']
genInfo <- data.frame(x['genaration info'])
genText <- data.frame(x['generated text'])

#plotting
textAmountCountPlot <- ggplot(textAmountCount, aes(x = text.amount.count.elapsedSec, y = text.amount.count.textNum)) + geom_area()
