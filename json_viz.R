#libraries
library(jsonlite)
library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)

#data reading

#TESTING
#x = fromJSON("./data/preliminary_test/result_try15.json")#With AI, Subject 1

#WITH AI
#x = fromJSON("./data/group1_with_ai/with_ai_subject1_2020_04_25.json")#With AI, Subject 1 (Togo Kida)
sub1 = fromJSON("./data/group1_with_ai/sakamoto/2020_4_27_18_3_45_result.json")#With AI, Subject 2-1 (Ai Sakamoto)
sub2 = fromJSON("./data/group1_with_ai/sakamoto/2020_4_27_18_9_10_result.json")#With AI, Subject 2-2 (Ai Sakamoto)
sub2AdjustedSec <- sub2$`text amount count`['elapsedSec'] + 551#adjust the elapsed sec count since it's coming after sub1
sub2Adjusted <- data.frame(textNum = sub2$`text amount count`['textNum'], elapsedSec = sub2AdjustedSec)

#NO AI
x = fromJSON("./data/group3_no_help/no_help_subject1_2020_4_27_0_8_7_result.json")#No HelpI, Subject 1 (Yuka Kida)

#databinding test
#testbind <- rbind(sub1$`text amount count`, sub2$`text amount count`)
testbind <- rbind(sub1$`text amount count`, sub2Adjusted)

#data reorganizing
textAmountCount <- testbind
#textAmountCount <- data.frame(sub1['text amount count'])
#textAmountCount <- data.frame(sub2['text amount count'])
endText <- x['final text']
genInfo <- data.frame(x['generation info'])
genText <- data.frame(x['generated text'])

#plot making
#textAmountCountPlot <- ggplot(textAmountCount, aes(x = text.amount.count.elapsedSec, y = text.amount.count.textNum)) + geom_line()
textAmountCountPlot <- ggplot(textAmountCount, aes(x = elapsedSec, y = textNum)) + geom_line()

#plotting
textAmountCountPlot

