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
# G L O B A L  V A R S      #
#                           #
#############################

textCountYMin <- 0
textCountYMax <- 750


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
withAI_error = fromJSON("./data/group1_with_ai/with_ai_subject_error_2020_4_29_6_52_5_result.json")#With AI, Subject 4 (Takuma Kudo)....data insufficient
withAI4 = fromJSON("./data/group1_with_ai/with_ai_subject4_2020_4_30_0_50_11_result.json")#With AI, Subject 4 (Ryusuke Nanki)
withAI5 = fromJSON("./data/group1_with_ai/with_ai_subject5_2020_4_30_15_39_33_result.json")#With AI, Subject 5 (Yuki Wada)
withAI6 = fromJSON("./data/group1_with_ai/with_ai_subject6_2020_4_30_14_36_44_result.json")#With AI, Subject 6 (Kanako Wada)
withAI7 = fromJSON("./data/group1_with_ai/with_ai_subject7_2020_4_30_17_17_2_result.json")#With AI, Subject 7 (Ryoko Matsuoka Baba)

#NO AI(Group 2)
noAI1 = fromJSON("./data/group2_no_help/no_help_subject1_2020_4_27_0_8_7_result.json")#No Help, Subject 1 (Yuka Kida)
noAI2 = fromJSON("./data/group2_no_help/no_help_subject2_2020_4_27_22_23_12_result.json")#No Help, Subject 2 (Sungwon Kim)
noAI3 = fromJSON("./data/group2_no_help/no_help_subject3_2020_4_28_6_12_35_result.json")#No Help, Subject 3 (Ryoya Sugano)

#WITH SYSTEM(Group 3)
withSystem1 = fromJSON("./data/group3_with_random/with_random_subject1_2020_4_30_20_39_35_result.json")#with system, Subject 1 (Kana Yamaguchi)
withSystem_error = fromJSON("./data/group3_with_random/with_random_subject2_2020_4_30_14_42_28_result.json")#with system, Subject(Chihiro Kato) .... data insufficient



###############################
#                             #
# P L O T   B U I L D I N G   #
#                             #
###############################


#-----------------------------------------------------------------------------------------
#Time vs Text Amount Plot Building

#
#WITH AI GROUP (Group 1)
#
group1_x <- data.frame(x = seq(0, 900, by = 1))
group1_sub1 <- data.frame(sub1 = withAI1['text amount count']$`text amount count`$textNum)
group1_sub2 <- data.frame(sub2 = withAI2_textNum$textNum)
group1_sub3 <- data.frame(sub3 = withAI3['text amount count']$`text amount count`$textNum)
group1_sub4 <- data.frame(sub4 = withAI4['text amount count']$`text amount count`$textNum)
group1_sub5 <- data.frame(sub5 = withAI5['text amount count']$`text amount count`$textNum)
group1_sub6 <- data.frame(sub6 = withAI6['text amount count']$`text amount count`$textNum)
group1_sub7 <- data.frame(sub7 = withAI7['text amount count']$`text amount count`$textNum)

group1_textNum_Combined_Data <- cbindX(group1_x, group1_sub1, group1_sub2, group1_sub3, group1_sub4, group1_sub5, group1_sub6, group1_sub7)

group1_textNumPlot <- ggplot(group1_textNum_Combined_Data, aes(x = x), color = "subjects") + 
  labs(title = "Group 1(with the assistance of AI models)", subtitle = "Elapsed Seconds vs Total Amount of Text Entered", caption = "n = 6") +
  xlab("Elapsed Seconds") +
  ylab("Total Amount of Text") +
  ylim(textCountYMin, textCountYMax) +
  geom_line(aes(y = sub1, color = "sub1")) + 
  geom_line(aes(y = sub2, color = "sub2")) + 
  geom_line(aes(y = sub3, color = "sub3")) +
  geom_line(aes(y = sub4, color = "sub4")) +
  geom_line(aes(y = sub5, color = "sub5")) + 
  geom_line(aes(y = sub6, color = "sub6")) +
  geom_line(aes(y = sub7, color = "sub7"))

  
#
#NO AI GROUP (Group 2)
#
group2_x <- data.frame(x = seq(0, 900, by = 1))
group2_sub1 <- data.frame(sub1 = noAI1['text amount count']$`text amount count`$textNum)
group2_sub2 <- data.frame(sub2 = noAI2['text amount count']$`text amount count`$textNum)
group2_sub3 <- data.frame(sub3 = noAI3['text amount count']$`text amount count`$textNum)

group2_textNum_Combined_Data <- cbindX(group2_x, group2_sub1, group2_sub2, group2_sub3)

group2_textNumPlot <- ggplot(group2_textNum_Combined_Data, aes(x = x)) + 
  labs(title = "Group 2(without any form of assistance)", subtitle = "Elapsed Seconds vs Total Amount of Text Entered", caption = "n = 3") +
  xlab("Elapsed Seconds") +
  ylab("Total Amount of Text") +
  ylim(textCountYMin, textCountYMax) +
  geom_line(aes(y = sub1, color = "sub1")) + 
  geom_line(aes(y = sub2, color = "sub2")) + 
  geom_line(aes(y = sub3, color = "sub3"))


#
#NO RANDOM (Group 3)
#
group3_x <- data.frame(x = seq(0, 900, by = 1))
group3_sub1 <- data.frame(sub1 = withSystem1['text amount count']$`text amount count`$textNum)
#group3_sub2 <- data.frame(sub2 = withSystem2['text amount count']$`text amount count`$textNum)

group3_textNum_Combined_Data <- cbindX(group3_x, group3_sub1)

group3_textNumPlot <- ggplot(group3_textNum_Combined_Data, aes(x = x)) + 
  labs(title = "Group 3(with the assistance of randomly selected phrases)", subtitle = "Elapsed Seconds vs Total Amount of Text Entered", caption = "n = 1") +
  xlab("Elapsed Seconds") +
  ylab("Total Amount of Text") +
  ylim(textCountYMin, textCountYMax) +
  geom_line(aes(y = sub1, color = "sub1")) 
  #geom_line(aes(y = sub2, color = "sub2"))



#-----------------------------------------------------------------------------------------
#
#Number of Times Generated vs Text Amount...Group 1
#
withAI1TextLength <- withAI1['text amount count']$`text amount count`$textNum[length(withAI1['text amount count']$`text amount count`$textNum)]
withAI2TextLength <- nrow(group1_sub2)
withAI3TextLength <- withAI3['text amount count']$`text amount count`$textNum[length(withAI3['text amount count']$`text amount count`$textNum)]
withAI4TextLength <- nrow(group1_sub4)
withAI5TextLength <- nrow(group1_sub5)
withAI6TextLength <- nrow(group1_sub6)
withAI7TextLength <- nrow(group1_sub7)

withAI1GenNum <- length(withAI1['generation info']$`generation info`$elapsedSec)
withAI2GenNum <- 12 #you know what, just hard code it...get stuff done matters more now!!!
withAI3GenNum <- length(withAI3['generation info']$`generation info`$elapsedSec)
withAI4GenNum <- length(withAI4['generation info']$`generation info`$elapsedSec)
withAI5GenNum <- length(withAI5['generation info']$`generation info`$elapsedSec)
withAI6GenNum <- length(withAI6['generation info']$`generation info`$elapsedSec)
withAI7GenNum <- length(withAI7['generation info']$`generation info`$elapsedSec)

group1TextLength <- c(withAI1TextLength, withAI2TextLength, withAI3TextLength, withAI4TextLength, withAI5TextLength, withAI6TextLength, withAI7TextLength)
group1GenNum <- c(withAI1GenNum, withAI2GenNum, withAI3GenNum, withAI4GenNum, withAI5GenNum, withAI6GenNum, withAI7GenNum)

group1GenNumTextAmount <- data.frame(genNum = group1GenNum, totalText = group1TextLength)

group1GenNumTextAmountPlot <- ggplot(group1GenNumTextAmount, aes(x = genNum, y = totalText)) +
  labs(title = "Group 1(with the assistance of AI models)", subtitle = "Number of Times Suggestion Generated vs Total Amount of Text Entered", caption = "n = 7") +
  xlab("Number of Times Suggestion Generated") +
  ylab("Total Amount of Text") +
  geom_point() +
  geom_smooth(method = lm)

#
#Number of Times Generated vs Text Amount...Group 1
#

###############################
#                             #
#      P L O T T I N G        #
#                             #
###############################

group1_textNumPlot
group2_textNumPlot
group3_textNumPlot
grid.arrange(group1_textNumPlot, group2_textNumPlot, group3_textNumPlot, nrow = 3)


group1GenNumTextAmountPlot


###############################
#                             #
#  P L O T   S A V I N G      #
#                             #
###############################



# will use it later...


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
