#libraries
library(jsonlite)
library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)

#data reading
#x = fromJSON("./data/result_try2.json") #first read it as a list, then work on it individually.
#x = fromJSON("./data/result_try3.json") #first read it as a list, then work on it individually.
#x = fromJSON("./data/result_try4.json") #first read it as a list, then work on it individually.
#x = fromJSON("./data/result_try5.json") #first read it as a list, then work on it individually.
#x = fromJSON("./data/result_try6.json") #first read it as a list, then work on it individually.
#x = fromJSON("./data/result_try7.json") #first read it as a list, then work on it individually.
#x = fromJSON("./data/result_try8.json") #first read it as a list, then work on it individually.
#x = fromJSON("./data/result_try9.json") #first read it as a list, then work on it individually.
#x = fromJSON("./data/result_try10.json") #first read it as a list, then work on it individually.
#x = fromJSON("./data/result_try11.json") #first read it as a list, then work on it individually.
#x = fromJSON("./data/result_try12.json") #first read it as a list, then work on it individually.
x = fromJSON("./data/result_try13.json") #first read it as a list, then work on it individually.


#data reorganizing
textAmountCount <- data.frame(x['text amount count'])
endText <- x['final text']
genInfo <- data.frame(x['generation info'])
genText <- data.frame(x['generated text'])

#plot making
textAmountCountPlot <- ggplot(textAmountCount, aes(x = text.amount.count.elapsedSec, y = text.amount.count.textNum)) + geom_area()

#plotting
textAmountCountPlot

