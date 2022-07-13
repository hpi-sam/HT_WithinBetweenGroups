"
Hypotheses tests on Experiment-1 tool_1st versus Experiment-2 tool_2nd
"

source("C://Users//Christian//Documents//SVN_Giese//ModelsRobotSimulator//hypotheses_test//cleanup.r")

df1 <- cleanup_df1()
df2 <- cleanup_df2()


#------------------------------------------------------
#Hypotheses tests

#Score_1 Tool vs Paper
df1_tool_1st <-  df1[df1$Treament=="tool_1st",]
View(df1_tool_1st)

df2_tool_2nd <-  df2[df2$Treament=="tool_2nd",]
View(df2_tool_2nd)

wilcox.test(df1_tool_1st$Score_1,df2_tool_2nd$Score_1,alternative = "two.sided") #W = 129.5, p-value = 0.01767*******
wilcox.test(df1_tool_1st$Score_1,df2_tool_2nd$Score_1,alternative = "less") #W = 129.5, p-value = 0.9923
wilcox.test(df1_tool_1st$Score_1,df2_tool_2nd$Score_1,alternative = "greater") #W = 129.5, p-value = 0.008836*******


#Score_2 not comparable between E1 and E2, so I am skipping it.

wilcox.test(df1_tool_1st$Question_1,df2_tool_2nd$Question_1,alternative = "two.sided") #W = 126, p-value = 0.2743
wilcox.test(df1_tool_1st$Question_2,df2_tool_2nd$Question_2,alternative = "two.sided") #W = 117, p-value = 0.5023
wilcox.test(df1_tool_1st$Question_3,df2_tool_2nd$Question_3,alternative = "two.sided") #W = 145, p-value = 0.05925
wilcox.test(df1_tool_1st$Question_4,df2_tool_2nd$Question_4,alternative = "two.sided") #W = 140, p-value = 0.09063
