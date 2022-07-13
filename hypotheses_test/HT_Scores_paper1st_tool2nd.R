"
Hypotheses tests on Experiment-1 paper_1st versus Experiment-2 tool_2nd
"

df1 <-  read.csv("C://Users//Christian//Documents//SVN_Giese//ModelsRobotSimulator//data//dataframe_1.csv")
summary(df1)
df2 <-  read.csv("C://Users//Christian//Documents//SVN_Giese//ModelsRobotSimulator//data//dataframe_2.csv")
summary(df2)



#------------------------------------------------------
#Hypotheses tests

#Score_1 paper vs Paper
df1_paper_1st <-  df1[df1$Treament=="paper_1st",]

wilcox.test(df1_paper_1st$Question_4,df2_tool_2nd$Question_4,alternative = "two.sided") #W = 96, p-value = 0.5418

View(df1_paper_1st)

df2_tool_2nd <-  df2[df2$Treament=="tool_2nd",]
View(df2_tool_2nd)

wilcox.test(df1_paper_1st$Score_1,df2_tool_2nd$Score_1,alternative = "two.sided") #W = 115, p-value = 0.1159
wilcox.test(df1_paper_1st$Score_2,df2_tool_2nd$Score_2,alternative = "two.sided") #W = 163.5, p-value = 2.77e-05 ***********
wilcox.test(df1_paper_1st$Question_1,df2_tool_2nd$Question_1,alternative = "two.sided") #W = 111, p-value = 0.1627
wilcox.test(df1_paper_1st$Question_2,df2_tool_2nd$Question_2,alternative = "two.sided") #W = 84, p-value = 1
wilcox.test(df1_paper_1st$Question_3,df2_tool_2nd$Question_3,alternative = "two.sided") #W = 109, p-value = 0.2135
wilcox.test(df1_paper_1st$Question_3,df2_tool_2nd$Question_3,alternative = "less") #W = 109, p-value = 0.9025