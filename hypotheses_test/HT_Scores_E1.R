"
Hypotheses tests on Experiment-1
"

source("C://Users//Christian//Documents//SVN_Giese//ModelsRobotSimulator//hypotheses_test//cleanup.r")

df1 <- cleanup_df1()
#--------------------------------
# NORMALITY TEST
shapiro.test(df1$Score_1) 
#data:  df1$Score_1 W = 0.8664, p-value = 0.002475
shapiro.test(df1$Score_2) 
#data:  df1$Score_2 W = 0.90935, p-value = 0.01652
shapiro.test(df1$Question_1) 
#data:  df1$Question_1 W = 0.85199, p-value = 0.0006827
shapiro.test(df1$Question_2)
#data:  df1$Question_2 W = 0.86339, p-value = 0.001198
shapiro.test(df1$Question_3)
#data:  df1$Question_3 W = 0.84765, p-value = 0.0005539
shapiro.test(df1$Question_4)
#data:  df1$Question_4 W = 0.93573, p-value = 0.06984*******

"Only question-4 has possibly normal distribution."

#------------------------------------------------------
#Hypotheses tests

#Score_1 Tool vs Paper
df1_tool_1st <-  df1[df1$Treament=="tool_1st",]
View(df1_tool_1st)

df1_paper_1st <-  df1[df1$Treament=="paper_1st",]
View(df1_paper_1st)

wilcox.test(df1_tool_1st$Score_1,df1_paper_1st$Score_1,alternative = "two.sided") #W = 131.5, p-value = 0.04911********
wilcox.test(df1_tool_1st$Score_1,df1_paper_1st$Score_1,alternative = "less") #W = 131.5, p-value = 0.9781
wilcox.test(df1_tool_1st$Score_1,df1_paper_1st$Score_1,alternative = "more") #W = 131.5, p-value = 0.9781

wilcox.test(df1_tool_1st$Score_2,df1_paper_1st$Score_2,alternative = "two.sided") #W = 83, p-value = 0.3628
wilcox.test(df1_tool_1st$Score_2,df1_paper_1st$Score_2,alternative = "less") #W = 83, p-value = 0.1814
wilcox.test(df1_tool_1st$Score_2,df1_paper_1st$Score_2,alternative = "more") #W = 83, p-value = 0.1814


wilcox.test(df1_tool_1st$Score_2,df1_paper_1st$Score_2,alternative = "two.sided") #W = 83, p-value = 0.3628
wilcox.test(df1_tool_1st$Question_1,df1_paper_1st$Question_1,alternative = "two.sided") #W = 98, p-value = 0.6004
wilcox.test(df1_tool_1st$Question_2,df1_paper_1st$Question_2,alternative = "two.sided") #W = 128.5, p-value = 0.4369
wilcox.test(df1_tool_1st$Question_3,df1_paper_1st$Question_3,alternative = "two.sided") #W = 137, p-value = 0.2737
wilcox.test(df1_tool_1st$Question_4,df1_paper_1st$Question_4,alternative = "two.sided") #W = 128.5, p-value = 0.4558

