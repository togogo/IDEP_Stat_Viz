#ok, let's write something...


library(jsonlite)
library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)

x = fromJSON("./data/test_result.json") %>% as.data.frame

testplot <- ggplot(x, aes(x = text.amount.count.elapsedSec, y = text.amount.count.textNum)) +geom_line()
  
