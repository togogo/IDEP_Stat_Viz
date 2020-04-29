#libraries
library(jsonlite)
library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(gdata)#for binding columns
library(cowplot)
library(gridExtra)


#############################
#                           #
# D A T A   L O A D I N G   #
#                           #
#############################

#TESTING
test1 = fromJSON("./data/preliminary_test/result_try15.json")#With AI, Subject 1
test2 = fromJSON("./data/preliminary_test/group3_test.json")#testing group 3 prototype
test3 = fromJSON("./data/preliminary_test/2020_4_30_0_12_3_result.json")#just double checking group 1 stuff

#WITH AI(Group 1)
withAI1 = fromJSON("./data/group1_with_ai/with_ai_subject1_2020_04_25.json")#With AI, Subject 1 (Togo Kida)
sub1 = fromJSON("./data/group1_with_ai/sakamoto/2020_4_27_18_3_45_result.json")#With AI, Subject 2-1 (Ai Sakamoto)
sub2 = fromJSON("./data/group1_with_ai/sakamoto/2020_4_27_18_9_10_result.json")#With AI, Subject 2-2 (Ai Sakamoto)
sub2AdjustedSec <- sub2$`text amount count`['elapsedSec'] + 551#adjust the elapsed sec count since it's coming after sub1
sub2Adjusted <- data.frame(textNum = sub2$`text amount count`['textNum'], elapsedSec = sub2AdjustedSec)
withAI2_textNum <- rbind(sub1$`text amount count`, sub2Adjusted)
withAI3 = fromJSON("./data/group1_with_ai/with_ai_subject3_2020_4_28_23_59_37_result.json")#With AI, Subject 3 (Nadya Kirillova)
withAI_error = fromJSON("./data/group1_with_ai/with_ai_subject4_2020_4_29_6_52_5_result.json")#With AI, Subject 4 (Takuma Kudo)....data insufficient
withAI4 = fromJSON("./data/group1_with_ai/with_ai_subject5_2020_4_30_0_50_11_result.json")#With AI, Subject 4 (Ryusuke Nanki)

#NO AI(Group 2)
noAI1 = fromJSON("./data/group3_no_help/no_help_subject1_2020_4_27_0_8_7_result.json")#No Help, Subject 1 (Yuka Kida)
noAI2 = fromJSON("./data/group3_no_help/no_help_subject2_2020_4_27_22_23_12_result.json")#No Help, Subject 2 (Sungwon Kim)
noAI3 = fromJSON("./data/group3_no_help/no_help_subject3_2020_4_28_6_12_35_result.json")#No Help, Subject 3 (Ryoya Sugano)

#WITH SYSTEM(Group 3)
#(more to come...)

###############################
#                             #
# T E S T   P L O T T I N G   #
#                             #
###############################

# x <- withAI4
# 
# #data reorganizing
# textAmountCount <- data.frame(x['text amount count'])
# endText <- x['final text']
# genInfo <- data.frame(x['generation info'])
# genText <- data.frame(x['generated text'])
# 
# #plot making
# textAmountCountPlot <- ggplot(textAmountCount, aes(x = text.amount.count.elapsedSec, y = text.amount.count.textNum)) + geom_line()
# #textAmountCountPlot <- ggplot(textAmountCount, aes(x = elapsedSec, y = textNum)) + geom_line()
# 
# #plotting
# textAmountCountPlot


###############################
#                             #
# P L O T   B U I L D I N G   #
#                             #
###############################

#WITH AI GROUP (Group 1)
group1_x <- data.frame(x = seq(0, 900, by = 1))
group1_sub1 <- data.frame(sub1 = withAI1['text amount count']$`text amount count`$textNum)
group1_sub2 <- data.frame(sub2 = withAI2_textNum$textNum)
group1_sub3 <- data.frame(sub3 = withAI3['text amount count']$`text amount count`$textNum)
group1_sub4 <- data.frame(sub4 = withAI4['text amount count']$`text amount count`$textNum)

group1_textNum_Combined_Data <- cbindX(group1_x, group1_sub1, group1_sub2, group1_sub3, group1_sub4)

group1_textNumPlot <- ggplot(group1_textNum_Combined_Data, aes(x = x)) + 
  labs(title = "Group 1(with the assistance of AI models)", subtitle = "Elapsed Seconds vs Total Amount of Text Entered", caption = "n = 4") +
  xlab("Elapsed Seconds") +
  ylab("Total Amount of Text") +
  ylim(0, 600) +
  geom_line(aes(y = sub1, color = "sub1")) + 
  geom_line(aes(y = sub2, color = "sub2")) + 
  geom_line(aes(y = sub3, color = "sub3")) +
  geom_line(aes(y = sub4, color = "sub4"))


#NO AI GROUP (Group 2)
group2_x <- data.frame(x = seq(0, 900, by = 1))
group2_sub1 <- data.frame(sub1 = noAI1['text amount count']$`text amount count`$textNum)
group2_sub2 <- data.frame(sub2 = noAI2['text amount count']$`text amount count`$textNum)
group2_sub3 <- data.frame(sub3 = noAI3['text amount count']$`text amount count`$textNum)

group2_textNum_Combined_Data <- cbindX(group2_x, group2_sub1, group2_sub2, group2_sub3)

group2_textNumPlot <- ggplot(group2_textNum_Combined_Data, aes(x = x)) + 
  labs(title = "Group 2(without the assistance of AI models)", subtitle = "Elapsed Seconds vs Total Amount of Text Entered", caption = "n = 3") +
  xlab("Elapsed Seconds") +
  ylab("Total Amount of Text") +
  ylim(0, 600) +
  geom_line(aes(y = sub1, color = "sub1")) + 
  geom_line(aes(y = sub2, color = "sub2")) + 
  geom_line(aes(y = sub3, color = "sub3"))

###############################
#                             #
#      P L O T T I N G        #
#                             #
###############################

#group1_textNumPlot
#group2_textNumPlot
grid.arrange(group1_textNumPlot, group2_textNumPlot, nrow = 2)

###############################
#                             #
#  P L O T   S A V I N G      #
#                             #
###############################
