"
Hypotheses tests on Experiment-1 tool_1st versus Experiment-2 paper_2nd
"

source("C://Users//Christian//Documents//SVN_Giese//ModelsRobotSimulator//hypotheses_test//cleanup.r")

df1 <- cleanup_df1()
df2 <- cleanup_df2()
#------------------------------------------------------
#Hypotheses tests

#Score_1 Tool vs Paper
df1_tool_1st <-  df1[df1$Treament=="tool_1st",]
View(df1_tool_1st)

df2_paper_2nd <-  df2[df2$Treament=="paper_2nd",]
View(df2_paper_2nd)

wilcox.test(df1_tool_1st$Score_1,df2_paper_2nd$Score_1,alternative = "two.sided") #W = 122, p-value = 0.01314 *************
wilcox.test(df1_tool_1st$Score_1,df2_paper_2nd$Score_1,alternative = "less") #W = 122, p-value = 0.9944 *************
wilcox.test(df1_tool_1st$Score_1,df2_paper_2nd$Score_1,alternative = "greater") #W = 122, p-value = 0.00657 *************

#Score_2 not comparable between E1 and E2, so I am skipping it.

wilcox.test(df1_tool_1st$Question_1,df2_paper_2nd$Question_1,alternative = "two.sided") #W = 86, p-value = 0.7317
wilcox.test(df1_tool_1st$Question_2,df2_paper_2nd$Question_2,alternative = "two.sided") #W = 107, p-value = 0.524
wilcox.test(df1_tool_1st$Question_3,df2_paper_2nd$Question_3,alternative = "two.sided") #W = 143.5, p-value = 0.01953 ***************
wilcox.test(df1_tool_1st$Question_3,df2_paper_2nd$Question_3,alternative = "less") #W = 143.5, p-value = 0.9914
wilcox.test(df1_tool_1st$Question_3,df2_paper_2nd$Question_3,alternative = "greater") #W = 143.5, p-value = 0.009766  ********

wilcox.test(df1_tool_1st$Question_4,df2_paper_2nd$Question_4,alternative = "two.sided") #W = 109, p-value = 0.2303
