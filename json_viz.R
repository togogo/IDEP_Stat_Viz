#libraries
library(jsonlite)
library(readr)
library(tidyverse)
library(readxl)
library(writexl)
library(dplyr)
library(ggplot2)
library(gdata)#for binding columns
library(cowplot)
library(gridExtra)
library(scales)
library(ggthemes)
#library(ggthemer)

#############################
#                           #
# G L O B A L  V A R S      #
#                           #
#############################

textCountYMin <- 0
textCountYMax <- 750

genNumMin <- 0
genNumMax <- 25

group1Num <- 10
group2Num <- 10
group3Num <- 10

histScoreMin <- 0
histScoreMax <- 11

textLabelSize <- 8


#############################
#                           #
# D A T A   L O A D I N G   #
#                           #
#############################

#TESTING
test1 = fromJSON("./data/preliminary_test/result_try15.json")#With AI, Subject 1
test2 = fromJSON("./data/preliminary_test/group3_test.json")#testing group 3 prototype
test3 = fromJSON("./data/preliminary_test/2020_4_30_0_12_3_result.json")#just double checking group 1 stuff
test4 = fromJSON("./data/preliminary_test/2020_5_3_1_36_53_result.json")#just double checking group 3 stuff

#WITH AI(Group 1)
withAI1 = fromJSON("./data/group1_with_ai/with_ai_subject1_2020_04_25.json")#With AI, Subject 1 (Togo Kida)
sub1 = fromJSON("./data/group1_with_ai/sakamoto/2020_4_27_18_3_45_result.json")#With AI, Subject 2-1 (Ai Sakamoto)
sub2 = fromJSON("./data/group1_with_ai/sakamoto/2020_4_27_18_9_10_result.json")#With AI, Subject 2-2 (Ai Sakamoto)
sub2AdjustedSec <- sub2$`text amount count`['elapsedSec'] + 551#adjust the elapsed sec count since it's coming after sub1
sub2Adjusted <- data.frame(textNum = sub2$`text amount count`['textNum'], elapsedSec = sub2AdjustedSec)
withAI2_textNum <- rbind(sub1$`text amount count`, sub2Adjusted)
withAI3 = fromJSON("./data/group1_with_ai/with_ai_subject3_2020_4_28_23_59_37_result.json")#With AI, Subject 3 (Nadya Kirillova)
withAI_error = fromJSON("./data/group1_with_ai/with_ai_subject_error_2020_4_29_6_52_5_result.json")#With AI, Subject 4 (Takuma Kudo)....data insufficient
#withAI4 = fromJSON("./data/group1_with_ai/with_ai_subject4_2020_4_30_0_50_11_result.json")#With AI, Subject 4 (Ryusuke Nanki)
withAI4 = fromJSON("./data/group1_with_ai/with_ai_subject7_2020_4_30_17_17_2_result.json")#With AI, Subject 4 (Ryoko Matsuoka Baba)
withAI5 = fromJSON("./data/group1_with_ai/with_ai_subject5_2020_4_30_15_39_33_result.json")#With AI, Subject 5 (Yuki Wada)
withAI6 = fromJSON("./data/group1_with_ai/with_ai_subject6_2020_4_30_14_36_44_result.json")#With AI, Subject 6 (Kanako Wada)
withAI7 = fromJSON("./data/group1_with_ai/with_ai_subject1_2020_5_6_13_33_25_result.json")#With AI, Subject 7 (Hiroki Baba)
withAI8 = fromJSON("./data/group1_with_ai/with_ai_subject8_2020_5_2_19_18_18_result.json")#With AI, Subject 8 (Kyoko Yonezawa)
withAI9 = fromJSON("./data/group1_with_ai/with_ai_subject9_2020_5_5_19_12_40_result.json")#With AI, Subject 9 (Toru Urakawa)
withAI10 = fromJSON("./data/group1_with_ai/with_ai_subject10_2020_5_6_10_41_56_result.json")#With AI, Subject 10 (Natsumi Wada)

#NO AI(Group 2)
noAI1 = fromJSON("./data/group2_no_help/no_help_subject1_2020_4_27_0_8_7_result.json")#No Help, Subject 1 (Yuka Kida)
noAI2 = fromJSON("./data/group2_no_help/no_help_subject2_2020_4_27_22_23_12_result.json")#No Help, Subject 2 (Sungwon Kim)
noAI3 = fromJSON("./data/group2_no_help/no_help_subject3_2020_4_28_6_12_35_result.json")#No Help, Subject 3 (Ryoya Sugano)
noAI4 = fromJSON("./data/group2_no_help/no_help_subject4_2020_5_1_13_31_0_result.json")#No Help, Subject 4 (Shota Ekuni)
noAI5 = fromJSON("./data/group2_no_help/no_help_subject5_2020_5_3_7_27_25_result.json")#No Help, Subject 5 (Masahiro Iwata)
noAI6 = fromJSON("./data/group2_no_help/no_help_subject6_2020_5_4_11_14_19_result.json")#No Help, Subject 6 (Rie Furuta)
#noAI7 = fromJSON("./data/group2_no_help/no_help_subject7_2020_5_5_3_43_44_result.json")#No Help, Subject 7 (Naomi Okamura)
noAI7 = fromJSON("./data/group2_no_help/no_help_subject7_2020_5_8_4_47_37_result.json")#No Help, Subject 7 (Naomi Okamura)
#noAI8 = fromJSON("./data/group2_no_help/no_help_subject8_2020_5_5_5_49_26_result.json")#No Help, Subject 8 (Takaaki Koshiba)
noAI8 = fromJSON("./data/group2_no_help/no_help_subject8_2020_5_6_20_33_3_result.json")#No Help, Subject 8 (Ryuto Satoshi)
noAI9 = fromJSON("./data/group2_no_help/no_help_subject9_2020_5_5_12_30_8_result.json")#No Help, Subject 9 (Chiharu Kai)
noAI10 = fromJSON("./data/group2_no_help/no_help_subject10_2020_5_5_18_43_10_result.json")#No Help, Subject 10 (Rai Yoshitoki)

#WITH SYSTEM(Group 3)
withSystem1 = fromJSON("./data/group3_with_random/with_random_subject1_2020_4_30_20_39_35_result.json")#with system, Subject 1 (Kana Yamaguchi)
withSystem_error = fromJSON("./data/group3_with_random/with_random_subject2_error_2020_4_30_14_42_28_result.json")#with system, Subject(Chihiro Kato) .... data insufficient
withSystem2 = fromJSON("./data/group3_with_random/with_random_subject2_2020_5_1_18_37_19_result.json")#with system, Subject 2 (Takayoshi Murakami)...something wrong...?
withSystem3 = fromJSON("./data/group3_with_random/with_random_subject3_2020_5_1_18_43_6_result.json")#with system, Subject 3 (Fumito Nitto)
withSystem4 = fromJSON("./data/group3_with_random/with_random_subject4_2020_5_1_17_47_22_result.json")#with system, Subject 4 (Fumiko Horikoshi)...something wrong...?
withSystem5 = fromJSON("./data/group3_with_random/with_random_subject5_2020_5_3_23_25_55_result.json")#with system, Subject 5 (Takashi Koyama)
withSystem6 = fromJSON("./data/group3_with_random/with_random_subject6_2020_5_4_21_50_19_result.json")#with system, Subject 6 (Rei Hanada)
withSystem7 = fromJSON("./data/group3_with_random/with_random_subject7_2020_5_4_15_8_34_result.json")#with system, Subject 7 (Aya Matsubara)
withSystem8 = fromJSON("./data/group3_with_random/with_random_subject8_2020_5_5_16_4_0_result.json")#with system, Subject 8 (Maiya Kinoshita)
withSystem9 = fromJSON("./data/group3_with_random/with_random_subject11_2020_5_7_10_11_52_result.json")#with system, Subject 9 (Takuya Kodama)
#withSystem9 = fromJSON("./data/group3_with_random/with_random_subject9_2020_5_6_12_26_32_result.json")#with system, Subject 9 (Shuji Shibata)
withSystem10 = fromJSON("./data/group3_with_random/with_random_subject10_2020_5_7_11_20_3_result.json")#with system, Subject 10 (Akira Suzuki)

##additional collection
#withSystem2 = fromJSON("./data/group3_with_random/with_random_extra1_2020_5_11_17_58_42_result.json")#with system, Subject 2 (Sayaka Arimoto) as dummy
withSystem4 = fromJSON("./data/group3_with_random/with_random_extra2_2020_5_9_17_17_14_result.json")#with system, Subject 4 (Takeshi Oishi) as dummy

#post-questionnaire
test_post_gr1_gr3 <- read_excel("./data/post/test_post_gr1_gr3.xlsx",sheet=1)#for testing purposes...
test_post_gr2 <- read_excel("./data/post/test_post_gr2.xlsx", sheet = 1)#for testing purposes...

post_gr2 <- read_excel("./data/post/post_gr2.xlsx", sheet = 1)
post_gr1_gr3 <- read_excel("./data/post/post_gr1_gr3.xlsx", sheet = 1)

#score results
score <- read_excel("./data/score/scoring_after.xlsx")


###################################
#                                 #
# O U T P U T   B U I L D I N G   #
#                                 #
###################################

final_text_comp <- c(withAI1$`final text`, 
                     sub2$`final text`, 
                     withAI3$`final text`, 
                     withAI4$`final text`, 
                     withAI5$`final text`, 
                     withAI6$`final text`, 
                     withAI7$`final text`, 
                     withAI8$`final text`, 
                     withAI9$`final text`,
                     withAI10$`final text`,
                     noAI1$`final text`,
                     noAI2$`final text`,
                     noAI3$`final text`,
                     noAI4$`final text`,
                     noAI5$`final text`,
                     noAI6$`final text`,
                     noAI7$`final text`,
                     noAI8$`final text`,
                     noAI9$`final text`,
                     noAI10$`final text`,
                     withSystem1$`final text`,
                     withSystem2$`final text`,
                     withSystem3$`final text`,
                     withSystem4$`final text`,
                     withSystem5$`final text`,
                     withSystem6$`final text`,
                     withSystem7$`final text`,
                     withSystem8$`final text`,
                     withSystem9$`final text`,
                     withSystem10$`final text`
                     )

group_cat <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3)

final_text_df <- data.frame(text = final_text_comp, group = group_cat)

write_xlsx(final_text_df, "scoreing_material.xlsx")

###############################
#                             #
# P L O T   B U I L D I N G   #
#                             #
###############################


#-----------------------------------------------------------------------------------------
#P L O T 1
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
group1_sub8 <- data.frame(sub8 = withAI8['text amount count']$`text amount count`$textNum)
group1_sub9 <- data.frame(sub9 = withAI9['text amount count']$`text amount count`$textNum)
group1_sub10 <- data.frame(sub10 = withAI10['text amount count']$`text amount count`$textNum)

group1_textNum_Combined_Data <- cbindX(group1_x, group1_sub1, group1_sub2, group1_sub3, group1_sub4, group1_sub5, group1_sub6, group1_sub7, group1_sub8, group1_sub9, group1_sub10)

group1_textNumPlot <- ggplot(group1_textNum_Combined_Data, aes(x = x), color = "subjects") + 
  labs(title = "Elapsed Seconds vs Total Amount of Text Entered", subtitle = "With the assistance of AI models(group 1)", caption = "n = 10") +
  xlab("Elapsed Seconds") +
  ylab("Total Amount of Text") +
  ylim(textCountYMin, textCountYMax) +
  geom_line(aes(y = sub1, color = "01")) + 
  geom_line(aes(y = sub2, color = "02")) + 
  geom_line(aes(y = sub3, color = "03")) +
  geom_line(aes(y = sub4, color = "04")) +
  geom_line(aes(y = sub5, color = "05")) + 
  geom_line(aes(y = sub6, color = "06")) +
  geom_line(aes(y = sub7, color = "07")) +
  geom_line(aes(y = sub8, color = "08")) +
  geom_line(aes(y = sub9, color = "09")) +
  geom_line(aes(y = sub10, color = "10"))+
  theme_economist() + scale_fill_economist()

  
#
#NO AI GROUP (Group 2)
#
group2_x <- data.frame(x = seq(0, 900, by = 1))
group2_sub1 <- data.frame(sub1 = noAI1['text amount count']$`text amount count`$textNum)
group2_sub2 <- data.frame(sub2 = noAI2['text amount count']$`text amount count`$textNum)
group2_sub3 <- data.frame(sub3 = noAI3['text amount count']$`text amount count`$textNum)
group2_sub4 <- data.frame(sub4 = noAI4['text amount count']$`text amount count`$textNum)
group2_sub5 <- data.frame(sub5 = noAI5['text amount count']$`text amount count`$textNum)
group2_sub6 <- data.frame(sub6 = noAI6['text amount count']$`text amount count`$textNum)
group2_sub7 <- data.frame(sub7 = noAI7['text amount count']$`text amount count`$textNum)
group2_sub8 <- data.frame(sub8 = noAI8['text amount count']$`text amount count`$textNum)
group2_sub9 <- data.frame(sub9 = noAI9['text amount count']$`text amount count`$textNum)
group2_sub10 <- data.frame(sub10 = noAI10['text amount count']$`text amount count`$textNum)

group2_textNum_Combined_Data <- cbindX(group2_x, group2_sub1, group2_sub2, group2_sub3, group2_sub4, group2_sub5, group2_sub6, group2_sub7, group2_sub8, group2_sub9, group2_sub10)

group2_textNumPlot <- ggplot(group2_textNum_Combined_Data, aes(x = x)) + 
  labs(title = "Elapsed Seconds vs Total Amount of Text Entered", subtitle = "Without any form of assistance(group 2)", caption = "n = 10") +
  xlab("Elapsed Seconds") +
  ylab("Total Amount of Text") +
  ylim(textCountYMin, textCountYMax) +
  geom_line(aes(y = sub1, color = "01")) + 
  geom_line(aes(y = sub2, color = "02")) + 
  geom_line(aes(y = sub3, color = "03")) +
  geom_line(aes(y = sub4, color = "04")) +
  geom_line(aes(y = sub5, color = "05")) + 
  geom_line(aes(y = sub6, color = "06")) +
  geom_line(aes(y = sub7, color = "07")) +
  geom_line(aes(y = sub8, color = "08")) +
  geom_line(aes(y = sub9, color = "09")) +
  geom_line(aes(y = sub10, color = "10"))+
  theme_economist() + scale_fill_economist()

#
#NO RANDOM (Group 3)
#
group3_x <- data.frame(x = seq(0, 900, by = 1))
group3_sub1 <- data.frame(sub1 = withSystem1['text amount count']$`text amount count`$textNum)
group3_sub2 <- data.frame(sub2 = withSystem2['text amount count']$`text amount count`$textNum)
group3_sub3 <- data.frame(sub3 = withSystem3['text amount count']$`text amount count`$textNum)
group3_sub4 <- data.frame(sub4 = withSystem4['text amount count']$`text amount count`$textNum)
group3_sub5 <- data.frame(sub5 = withSystem5['text amount count']$`text amount count`$textNum)
group3_sub6 <- data.frame(sub6 = withSystem6['text amount count']$`text amount count`$textNum)
group3_sub7 <- data.frame(sub7 = withSystem7['text amount count']$`text amount count`$textNum)
group3_sub8 <- data.frame(sub8 = withSystem8['text amount count']$`text amount count`$textNum)
group3_sub9 <- data.frame(sub9 = withSystem9['text amount count']$`text amount count`$textNum)
group3_sub10 <- data.frame(sub10 = withSystem10['text amount count']$`text amount count`$textNum)

group3_textNum_Combined_Data <- cbindX(group3_x, group3_sub1, group3_sub2, group3_sub3, group3_sub4, group3_sub5, group3_sub6, group3_sub7, group3_sub8, group3_sub9, group3_sub10)

group3_textNumPlot <- ggplot(group3_textNum_Combined_Data, aes(x = x)) + 
  labs(title = "Elapsed Seconds vs Total Amount of Text Entered", subtitle = "With the assistance of randomly selected phrases(group 3)", caption = "n = 10") +
  xlab("Elapsed Seconds") +
  ylab("Total Amount of Text") +
  ylim(textCountYMin, textCountYMax) +
  geom_line(aes(y = sub1, color = "01")) +
  geom_line(aes(y = sub2, color = "02")) +
  geom_line(aes(y = sub3, color = "03")) +
  geom_line(aes(y = sub4, color = "04")) +
  geom_line(aes(y = sub5, color = "05")) +
  geom_line(aes(y = sub6, color = "06")) +
  geom_line(aes(y = sub7, color = "07")) +
  geom_line(aes(y = sub8, color = "08")) +
  geom_line(aes(y = sub9, color = "09")) +
  geom_line(aes(y = sub10, color = "10"))+
  theme_economist() + scale_fill_economist()



#-----------------------------------------------------------------------------------------
#P L O T 2
#Number of Times Generated vs Text Amount...Group 1
#
withAI1TextLength <- withAI1['text amount count']$`text amount count`$textNum[length(withAI1['text amount count']$`text amount count`$textNum)]
withAI2TextLength <- withAI2_textNum['textNum']$textNum[length(withAI2_textNum['textNum']$textNum)]
withAI3TextLength <- withAI3['text amount count']$`text amount count`$textNum[length(withAI3['text amount count']$`text amount count`$textNum)]
withAI4TextLength <- withAI4['text amount count']$`text amount count`$textNum[length(withAI4['text amount count']$`text amount count`$textNum)]
withAI5TextLength <- withAI5['text amount count']$`text amount count`$textNum[length(withAI5['text amount count']$`text amount count`$textNum)]
withAI6TextLength <- withAI6['text amount count']$`text amount count`$textNum[length(withAI6['text amount count']$`text amount count`$textNum)]
withAI7TextLength <- withAI7['text amount count']$`text amount count`$textNum[length(withAI7['text amount count']$`text amount count`$textNum)]
withAI8TextLength <- withAI8['text amount count']$`text amount count`$textNum[length(withAI8['text amount count']$`text amount count`$textNum)]
withAI9TextLength <- withAI9['text amount count']$`text amount count`$textNum[length(withAI9['text amount count']$`text amount count`$textNum)]
withAI10TextLength <- withAI10['text amount count']$`text amount count`$textNum[length(withAI10['text amount count']$`text amount count`$textNum)]

withAI1GenNum <- length(withAI1['generation info']$`generation info`$elapsedSec)
withAI2GenNum <- 12 #you know what, just hard code it...get stuff done matters more now!!!
withAI3GenNum <- length(withAI3['generation info']$`generation info`$elapsedSec)
withAI4GenNum <- length(withAI4['generation info']$`generation info`$elapsedSec)
withAI5GenNum <- length(withAI5['generation info']$`generation info`$elapsedSec)
withAI6GenNum <- length(withAI6['generation info']$`generation info`$elapsedSec)
withAI7GenNum <- length(withAI7['generation info']$`generation info`$elapsedSec)
withAI8GenNum <- length(withAI8['generation info']$`generation info`$elapsedSec)
withAI9GenNum <- length(withAI9['generation info']$`generation info`$elapsedSec)
withAI10GenNum <- length(withAI10['generation info']$`generation info`$elapsedSec)

group1TextLength <- c(withAI1TextLength, withAI2TextLength, withAI3TextLength, withAI4TextLength, withAI5TextLength, withAI6TextLength, withAI7TextLength, withAI8TextLength, withAI9TextLength, withAI10TextLength)
group1GenNum <- c(withAI1GenNum, withAI2GenNum, withAI3GenNum, withAI4GenNum, withAI5GenNum, withAI6GenNum, withAI7GenNum, withAI8GenNum, withAI9GenNum, withAI10GenNum)

group1GenNumTextAmount <- data.frame(genNum = group1GenNum, totalText = group1TextLength)

group1GenNumTextAmountPlot <- ggplot(group1GenNumTextAmount, aes(x = genNum, y = totalText)) +
  labs(title = "Number of Times Suggestion Generated vs Total Amount of Text Entered", subtitle = "With the assistance of AI models(group1)", caption = "n = 10") +
  xlab("Number of Times Suggestion Generated") +
  ylab("Total Amount of Text") +
  ylim(textCountYMin, textCountYMax) +
  xlim(genNumMin, genNumMax) +
  geom_point() +
  geom_smooth(method = lm) +
  theme_economist() + scale_fill_economist()

#
#Number of Times Generated vs Text Amount...Group 3
#

withSystem1TextLength <- withSystem1['text amount count']$`text amount count`$textNum[length(withSystem1['text amount count']$`text amount count`$textNum)]
withSystem2TextLength <- withSystem2['text amount count']$`text amount count`$textNum[length(withSystem2['text amount count']$`text amount count`$textNum)]
withSystem3TextLength <- withSystem3['text amount count']$`text amount count`$textNum[length(withSystem3['text amount count']$`text amount count`$textNum)]
withSystem4TextLength <- withSystem4['text amount count']$`text amount count`$textNum[length(withSystem4['text amount count']$`text amount count`$textNum)]
withSystem5TextLength <- withSystem5['text amount count']$`text amount count`$textNum[length(withSystem5['text amount count']$`text amount count`$textNum)]
withSystem6TextLength <- withSystem6['text amount count']$`text amount count`$textNum[length(withSystem6['text amount count']$`text amount count`$textNum)]
withSystem7TextLength <- withSystem7['text amount count']$`text amount count`$textNum[length(withSystem7['text amount count']$`text amount count`$textNum)]
withSystem8TextLength <- withSystem8['text amount count']$`text amount count`$textNum[length(withSystem8['text amount count']$`text amount count`$textNum)]
withSystem9TextLength <- withSystem9['text amount count']$`text amount count`$textNum[length(withSystem9['text amount count']$`text amount count`$textNum)]
withSystem10TextLength <- withSystem10['text amount count']$`text amount count`$textNum[length(withSystem10['text amount count']$`text amount count`$textNum)]

withSystem1GenNum <- length(withSystem1['generation info']$`generation info`$elapsedSec)
withSystem2GenNum <- length(withSystem2['generation info']$`generation info`$elapsedSec)
withSystem3GenNum <- length(withSystem3['generation info']$`generation info`$elapsedSec)
withSystem4GenNum <- length(withSystem4['generation info']$`generation info`$elapsedSec)
withSystem5GenNum <- length(withSystem5['generation info']$`generation info`$elapsedSec)
withSystem6GenNum <- length(withSystem6['generation info']$`generation info`$elapsedSec)
withSystem7GenNum <- length(withSystem7['generation info']$`generation info`$elapsedSec)
withSystem8GenNum <- length(withSystem8['generation info']$`generation info`$elapsedSec)
withSystem9GenNum <- length(withSystem9['generation info']$`generation info`$elapsedSec)
withSystem10GenNum <- length(withSystem10['generation info']$`generation info`$elapsedSec)

group3TextLength <- c(withSystem1TextLength, withSystem2TextLength, withSystem3TextLength, withSystem4TextLength, withSystem5TextLength, withSystem6TextLength, withSystem7TextLength, withSystem8TextLength, withSystem9TextLength, withSystem10TextLength)
group3GenNum <- c(withSystem1GenNum, withSystem2GenNum, withSystem3GenNum, withSystem4GenNum, withSystem5GenNum, withSystem6GenNum, withSystem7GenNum, withSystem8GenNum, withSystem9GenNum, withSystem10GenNum)

#alternative
#group3TextLength <- c(withSystem1TextLength, withSystem3TextLength,  withSystem5TextLength, withSystem6TextLength, withSystem7TextLength)
#group3GenNum <- c(withSystem1GenNum, withSystem3GenNum, withSystem5GenNum, withSystem6GenNum, withSystem7GenNum)

group3GenNumTextAmount <- data.frame(genNum = group3GenNum, totalText = group3TextLength)

group3GenNumTextAmountPlot <- ggplot(group3GenNumTextAmount, aes(x = genNum, y = totalText)) +
  labs(title = "Number of Times Suggestion Generated vs Total Amount of Text Entered", subtitle = "With the assistance of randomly selected phrases(group 3)", caption = "n = 10") +
  xlab("Number of Times Suggestion Generated") +
  ylab("Total Amount of Text") +
  ylim(textCountYMin, textCountYMax) +
  xlim(genNumMin, genNumMax) +
  geom_point() +
  geom_smooth(method = lm) + 
  theme_economist() + scale_fill_economist()


#-----------------------------------------------------------------------------------------
#P L O T 3
#Average Seconds Spent among Groups

#group1
withAI1TotalTime <- length(withAI1['text amount count']$`text amount count`$elapsedSec)
withAI2TotalTime <- length(withAI2_textNum['elapsedSec'])
withAI3TotalTime <- length(withAI3['text amount count']$`text amount count`$elapsedSec)
withAI4TotalTime <- length(withAI4['text amount count']$`text amount count`$elapsedSec)
withAI5TotalTime <- length(withAI5['text amount count']$`text amount count`$elapsedSec)
withAI6TotalTime <- length(withAI6['text amount count']$`text amount count`$elapsedSec)
withAI7TotalTime <- length(withAI7['text amount count']$`text amount count`$elapsedSec)
withAI8TotalTime <- length(withAI8['text amount count']$`text amount count`$elapsedSec)
withAI9TotalTime <- length(withAI9['text amount count']$`text amount count`$elapsedSec)
withAI10TotalTime <- length(withAI10['text amount count']$`text amount count`$elapsedSec)


withAIAvgTime <- (withAI1TotalTime + withAI2TotalTime + withAI3TotalTime + withAI4TotalTime + withAI5TotalTime + withAI6TotalTime + withAI7TotalTime + withAI8TotalTime + withAI9TotalTime + withAI10TotalTime)/group1Num

#group2
noAI1TotalTime <- length(noAI1['text amount count']$`text amount count`$elapsedSec)
noAI2TotalTime <- length(noAI2['text amount count']$`text amount count`$elapsedSec)
noAI3TotalTime <- length(noAI3['text amount count']$`text amount count`$elapsedSec)
noAI4TotalTime <- length(noAI4['text amount count']$`text amount count`$elapsedSec)
noAI5TotalTime <- length(noAI5['text amount count']$`text amount count`$elapsedSec)
noAI6TotalTime <- length(noAI6['text amount count']$`text amount count`$elapsedSec)
noAI7TotalTime <- length(noAI7['text amount count']$`text amount count`$elapsedSec)
noAI8TotalTime <- length(noAI8['text amount count']$`text amount count`$elapsedSec)
noAI9TotalTime <- length(noAI9['text amount count']$`text amount count`$elapsedSec)
noAI10TotalTime <- length(noAI10['text amount count']$`text amount count`$elapsedSec)

noAIAvgTime <- (noAI1TotalTime + noAI2TotalTime + noAI3TotalTime + noAI4TotalTime + noAI5TotalTime + noAI6TotalTime + noAI7TotalTime + noAI8TotalTime + noAI9TotalTime + noAI10TotalTime)/group2Num

#group3
withSystem1TotalTime <- length(withSystem1['text amount count']$`text amount count`$elapsedSec)
withSystem2TotalTime <- length(withSystem2['text amount count']$`text amount count`$elapsedSec)
withSystem3TotalTime <- length(withSystem3['text amount count']$`text amount count`$elapsedSec)
withSystem4TotalTime <- length(withSystem4['text amount count']$`text amount count`$elapsedSec)
withSystem5TotalTime <- length(withSystem5['text amount count']$`text amount count`$elapsedSec)
withSystem6TotalTime <- length(withSystem6['text amount count']$`text amount count`$elapsedSec)
withSystem7TotalTime <- length(withSystem7['text amount count']$`text amount count`$elapsedSec)
withSystem8TotalTime <- length(withSystem8['text amount count']$`text amount count`$elapsedSec)
withSystem9TotalTime <- length(withSystem9['text amount count']$`text amount count`$elapsedSec)
withSystem10TotalTime <- length(withSystem10['text amount count']$`text amount count`$elapsedSec)

withSystemAvgTime <- (withSystem1TotalTime + withSystem2TotalTime + withSystem3TotalTime + withSystem4TotalTime + withSystem5TotalTime + withSystem6TotalTime + withSystem7TotalTime + withSystem8TotalTime + withSystem9TotalTime + withSystem10TotalTime)/group3Num

avgTime <- data.frame(group = c("group 1", "group 2", "group 3"), time = c(withAIAvgTime, noAIAvgTime, withSystemAvgTime))

avgTimePlot <- ggplot(avgTime, aes(x = group, y = time, fill = group)) + geom_bar(stat = "identity") +
  labs(title = "Average Input Time Among Groups", subtitle = "With AI(group 1) vs With No AI(group 2) vs With Dummy(group 3)") +
  xlab("Groups") +
  ylab("Total Input Time") +
  geom_text(label = round(avgTime$time, 2), position = position_stack(vjust = 0.80), size = textLabelSize) +
  theme_economist() + scale_fill_economist()


#-----------------------------------------------------------------------------------------
#P L O T 4
#Average Amount of Text Input

#group1 
withAIAvgText <- sum(group1TextLength) / group1Num

#group2
noAI1TextLength <- noAI1['text amount count']$`text amount count`$textNum[length(noAI1['text amount count']$`text amount count`$textNum)]
noAI2TextLength <- noAI2['text amount count']$`text amount count`$textNum[length(noAI2['text amount count']$`text amount count`$textNum)]
noAI3TextLength <- noAI3['text amount count']$`text amount count`$textNum[length(noAI3['text amount count']$`text amount count`$textNum)]
noAI4TextLength <- noAI4['text amount count']$`text amount count`$textNum[length(noAI4['text amount count']$`text amount count`$textNum)]
noAI5TextLength <- noAI5['text amount count']$`text amount count`$textNum[length(noAI5['text amount count']$`text amount count`$textNum)]
noAI6TextLength <- noAI6['text amount count']$`text amount count`$textNum[length(noAI6['text amount count']$`text amount count`$textNum)]
noAI7TextLength <- noAI7['text amount count']$`text amount count`$textNum[length(noAI7['text amount count']$`text amount count`$textNum)]
noAI8TextLength <- noAI8['text amount count']$`text amount count`$textNum[length(noAI8['text amount count']$`text amount count`$textNum)]
noAI9TextLength <- noAI9['text amount count']$`text amount count`$textNum[length(noAI9['text amount count']$`text amount count`$textNum)]
noAI10TextLength <- noAI10['text amount count']$`text amount count`$textNum[length(noAI10['text amount count']$`text amount count`$textNum)]

group2TextLength <- c(noAI1TextLength, noAI2TextLength, noAI3TextLength, noAI4TextLength, noAI5TextLength, noAI6TextLength, noAI7TextLength, noAI8TextLength, noAI9TextLength, noAI10TextLength)

noAIAvgText <- sum(group2TextLength) / group2Num

#group3
withSystemAvgText <- sum(group3TextLength) / group3Num

avgText <- data.frame(group = c("group 1", "group 2", "group 3"), time = c(withAIAvgText, noAIAvgText, withSystemAvgText))

avgTextPlot <- ggplot(avgText, aes(x = group, y = time, fill = group)) + geom_bar(stat = "identity") +
  labs(title = "Average Text Amount Among Groups", subtitle = "With AI(group 1) vs With No AI(group 2) vs With Dummy(group 3)") +
  xlab("Groups") +
  ylab("Total Text Amount") +
  geom_text(label = round(avgText$time), position = position_stack(vjust = 0.80), size = textLabelSize) +
  theme_economist() + scale_fill_economist()

#-----------------------------------------------------------------------------------------
#P L O T 5
#Avgerage Rate of Text Input / Second

withAIAvgRate <- withAIAvgText / withAIAvgTime
noAIAvgRate <- noAIAvgText / noAIAvgTime
withSystemAvgRate <- withSystemAvgText / withSystemAvgTime

avgRate <- data.frame(group = c("group 1", "group 2", "group 3"), rate = c(withAIAvgRate, noAIAvgRate, withSystemAvgRate))

avgRatePlot <- ggplot(avgRate, aes(x = group, y = rate, fill = group)) + geom_bar(stat = "identity") +
  labs(title = "Average Rate of Text Input per Second", subtitle = "With AI(group 1) vs With No AI(group 2) vs With Dummy(group 3)") +
  xlab("Groups") +
  ylab("Text Input Rate per Second") +
  geom_text(label = round(avgRate$rate, 2), position = position_stack(vjust = 0.80), size = textLabelSize) +
  theme_economist() + scale_fill_economist()

#avgRatePlot

#-----------------------------------------------------------------------------------------
#P L O T 6
#Self Reported Scores Among Groups

post_gr1_filtered <- filter(post_gr1_gr3, Group == 1)
post_gr3_filtered <- filter(post_gr1_gr3, Group == 3)

gr1_selfScorePlot <- filter(post_gr1_gr3, Group == 1) %>% 
  ggplot(aes(Self_Score)) + 
  labs(title = "Self Reported Scores of the Creative Output", subtitle = "With AI(Group 1)") +
  xlab("Score") +
  ylab("Count") +
  geom_bar(position = "stack") +
  scale_x_continuous(breaks=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
  xlim(histScoreMin, histScoreMax) +
  geom_vline(xintercept = mean(post_gr1_filtered$Self_Score), color = "red", size = 2) +
  geom_text(aes(x=mean(post_gr1_filtered$Self_Score) + 1.1, label="mean:", y=5), colour="red", angle=0, vjust = 1.2, size=textLabelSize) +
  geom_text(aes(x=mean(post_gr1_filtered$Self_Score) + 1, label= mean(post_gr1_filtered$Self_Score), y=4.6), colour="red", angle=0, vjust = 1.2, size=textLabelSize) +
  theme_economist() + scale_fill_economist()

#gr1_selfScorePlot
  
gr2_selfScorePlot <- ggplot(data = post_gr2, aes(Self_Score)) + 
  labs(title = "Self Reported Scores of the Creative Output", subtitle = "With No AI(Group 2)") +
  xlab("Score") +
  ylab("Count") +
  geom_bar(position = "stack") +
  scale_x_continuous(breaks=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
  xlim(histScoreMin, histScoreMax) +
  geom_vline(xintercept = mean(post_gr2$Self_Score), color = "red", size = 2) +
  geom_text(aes(x=mean(post_gr2$Self_Score) + 1.1, label="mean:", y=5), colour="red", angle=0, vjust = 1.2, size=textLabelSize) +
  geom_text(aes(x=mean(post_gr2$Self_Score) + 1, label= mean(post_gr2$Self_Score), y=4.6), colour="red", angle=0, vjust = 1.2, size=textLabelSize) +
  theme_economist() + scale_fill_economist()

#gr2_selfScorePlot

gr3_selfScorePlot <- filter(post_gr1_gr3, Group == 3) %>% 
  ggplot(aes(Self_Score)) + 
  labs(title = "Self Reported Scores of the Creative Output", subtitle = "With Dummy System(Group 3)") +
  xlab("Score") +
  ylab("Count") +
  geom_bar(position = "stack")  +
  scale_x_continuous(breaks=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
  xlim(histScoreMin, histScoreMax) +
  geom_vline(xintercept = mean(post_gr3_filtered$Self_Score), color = "red", size = 2) +
  geom_text(aes(x=mean(post_gr3_filtered$Self_Score) + 1.1, label="mean:", y=5), colour="red", angle=0, vjust = 1.2, size=textLabelSize) +
  geom_text(aes(x=mean(post_gr3_filtered$Self_Score) + 1, label= round(mean(post_gr3_filtered$Self_Score), 2), y=4.6), colour="red", angle=0, vjust = 1.2, size=textLabelSize) +
  theme_economist() + scale_fill_economist()

gr3_selfScorePlot

#-----------------------------------------------------------------------------------------
#P L O T 7
#Gen Results Score Between Group1 and 3
gr1_genResultScorePlot <- filter(post_gr1_gr3, Group == 1) %>%
  ggplot(aes(Gen_Score)) + 
  labs(title = "How Helpful Were the Generated Results?", subtitle = "With AI(Group 1)") +
  xlab("Score") +
  ylab("Count") +
  geom_bar(position = "stack") +
  scale_x_continuous(breaks=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
  xlim(histScoreMin, histScoreMax) +
  geom_vline(xintercept = mean(post_gr1_filtered$Gen_Score), color = "red", size = 2) +
  geom_text(aes(x=mean(post_gr1_filtered$Gen_Score) + 1.1, label="mean:", y=5), colour="red", angle=0, vjust = 1.2, size=textLabelSize) +
  geom_text(aes(x=mean(post_gr1_filtered$Gen_Score) + 1, label= round(mean(post_gr1_filtered$Gen_Score), 2), y=4.6), colour="red", angle=0, vjust = 1.2, size=textLabelSize) +
  theme_economist() + scale_fill_economist()

gr1_genResultScorePlot

gr3_genResultScorePlot <- filter(post_gr1_gr3, Group == 3) %>%
  ggplot(aes(Gen_Score)) + 
  labs(title = "How Helpful Were the Generated Results?", subtitle = "With Dummy System(Group 3)") +
  xlab("Score") +
  ylab("Count") +
  geom_bar(position = "stack") +
  scale_x_continuous(breaks=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
  xlim(histScoreMin, histScoreMax) +
  geom_vline(xintercept = mean(post_gr3_filtered$Gen_Score), color = "red", size = 2) +
  geom_text(aes(x=mean(post_gr3_filtered$Gen_Score) + 1.1, label="mean:", y=5), colour="red", angle=0, vjust = 1.2, size=textLabelSize) +
  geom_text(aes(x=mean(post_gr3_filtered$Gen_Score) + 1, label= round(mean(post_gr3_filtered$Gen_Score), 2), y=4.6), colour="red", angle=0, vjust = 1.2, size=textLabelSize) +
  theme_economist() + scale_fill_economist()

gr3_genResultScorePlot
  
#-----------------------------------------------------------------------------------------
#P L O T 8
#Colloborative Experience Score Between Group 1 and 3
gr1_genExpPlot <- filter(post_gr1_gr3, Group == 1) %>%
  ggplot(aes(Gen_Exp)) + 
  labs(title = "How Fun was it to Collaborate?", subtitle = "With AI(Group 1)") +
  xlab("Score") +
  ylab("Count") +
  geom_bar(position = "stack") +
  scale_x_continuous(seq(1,11, by = 1), limits=c(0, 10)) +
  xlim(histScoreMin, histScoreMax) +
  geom_vline(xintercept = mean(post_gr1_filtered$Gen_Exp), color = "red", size = 2) +
  theme_economist() + scale_fill_economist()

gr3_genExpPlot <- filter(post_gr1_gr3, Group == 3) %>%
  ggplot(aes(Gen_Exp)) + 
  labs(title = "How Fun was it to Collaborate?", subtitle = "With Dummy System(Group 3)") +
  xlab("Score") +
  ylab("Count") +
  geom_bar(position = "stack") +
  scale_x_continuous(breaks=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
  xlim(histScoreMin, histScoreMax) +
  geom_vline(xintercept = mean(post_gr3_filtered$Gen_Exp), color = "red", size = 2) +
  theme_economist() + scale_fill_economist()

#gr1_genExpPlot
#-----------------------------------------------------------------------------------------
#P L O T 9
#Subjective Scoring
group1_score <- filter(score, group == 1)
group2_score <- filter(score, group == 2)
group3_score <- filter(score, group == 3)

group1_mean <- (mean(group1_score$score1) + mean(group1_score$score2) + mean(group1_score$score3))/3
group2_mean <- (mean(group2_score$score1) + mean(group2_score$score2) + mean(group2_score$score3))/3
group3_mean <- (mean(group3_score$score1) + mean(group3_score$score2) + mean(group3_score$score3))/3

scoreAverage <- data.frame(group = c("group 1", "group 2", "group 3"), score = c(group1_mean, group2_mean, group3_mean))
scoreAveragePlot <- ggplot(scoreAverage, aes(x = group, y = score, fill = group)) + geom_bar(stat = "identity") +
  labs(title = "Final text output scored by 3 judges", subtitle = "With AI(group 1) vs With No AI(group 2) vs With Dummy(group 3)") +
  xlab("Groups") +
  ylab("Score") +
  geom_text(label = round(scoreAverage$score, 2), position = position_stack(vjust = 0.80), size = textLabelSize) +
  theme_economist() + scale_fill_economist()

#scoreAveragePlot
###############################
#                             #
#      P L O T T I N G        #
#                             #
###############################

group1_textNumPlot
group2_textNumPlot
group3_textNumPlot
grid.arrange(group1_textNumPlot, group2_textNumPlot, group3_textNumPlot, nrow = 3)
gr123_textNumPlot <- grid.arrange(group1_textNumPlot, group2_textNumPlot, group3_textNumPlot, ncol = 3)
gr123_textNumPlot

group1GenNumTextAmountPlot
group3GenNumTextAmountPlot
grid.arrange(group1GenNumTextAmountPlot, group3GenNumTextAmountPlot, nrow = 2)
gr13_GenNumTextAmountPlot <- grid.arrange(group1GenNumTextAmountPlot, group3GenNumTextAmountPlot, ncol = 2)
gr13_GenNumTextAmountPlot


avgTimePlot
avgTextPlot
avgRatePlot
grid.arrange(avgTimePlot, avgTextPlot, avgRatePlot, nrow = 3)
gr123_avgTimeTextRate <- grid.arrange(avgTimePlot, avgTextPlot, avgRatePlot, ncol = 3)
gr123_avgTimeTextRate

gr1_selfScorePlot
gr2_selfScorePlot
gr3_selfScorePlot
grid.arrange(gr1_selfScorePlot, gr2_selfScorePlot, gr3_selfScorePlot, nrow = 3)
gr123_selfScorePlot <- grid.arrange(gr1_selfScorePlot, gr2_selfScorePlot, gr3_selfScorePlot, ncol = 3)
gr123_selfScorePlot

gr1_genResultScorePlot
gr3_genResultScorePlot
grid.arrange(gr1_genResultScorePlot, gr3_genResultScorePlot, nrow = 2)
gr13_genResultScorePlot <- grid.arrange(gr1_genResultScorePlot, gr3_genResultScorePlot, ncol = 2)
gr13_genResultScorePlot

gr1_genExpPlot
gr3_genExpPlot
grid.arrange(gr1_genExpPlot, gr3_genExpPlot, nrow = 2)
gr13_genExpPlot <- grid.arrange(gr1_genExpPlot, gr3_genExpPlot, ncol = 2)
gr13_genExpPlot

scoreAveragePlot

###############################
#                             #
#  P L O T   S A V I N G      #
#                             #
###############################


# will use it later...
ggsave(plot = gr123_textNumPlot, width = 19, height = 8, dpi = 300, filename = "./output/gr123_textNumPlot.pdf")
#ggsave(plot = gr123_avgTextandRate, width = 19, height = 8, dpi = 300, filename = "./output/gr123_avgTextandRate.pdf")
ggsave(plot = gr123_avgTimeTextRate, width = 19, height = 8, dpi = 300, filename = "./output/gr123_avgTimeTextRate.pdf")
ggsave(plot = gr123_selfScorePlot, width = 19, height = 8, dpi = 300, filename = "./output/gr123_selfScorePlot.pdf")
ggsave(plot = gr13_GenNumTextAmountPlot, width = 19, height = 8, dpi = 300, filename = "./output/gr13_GenNumTextAmountPlot.pdf")
ggsave(plot = gr13_genResultScorePlot, width = 19, height = 8, dpi = 300, filename = "./output/gr13_genResultScorePlot.pdf")
ggsave(plot = gr13_genExpPlot, width = 19, height = 8, dpi = 300, filename = "./output/gr13_genExpPlot.pdf")
ggsave(plot = scoreAveragePlot, width = 19, height = 8, dpi = 300, filename = "./output/scoreAveragePlot.pdf")

