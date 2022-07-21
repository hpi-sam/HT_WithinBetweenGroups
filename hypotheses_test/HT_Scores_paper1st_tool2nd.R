"
Hypotheses tests on Experiment-1 paper_1st versus Experiment-2 tool_2nd
"

source("C://Users//Christian//Documents//GitHub//HT_WithinBetweenGroups//hypotheses_test//cleanup.r")

df1 <- cleanup_task1()
df2 <- cleanup_task2()
#------------------------------------------------------
#Hypotheses tests

#Score_1 Tool vs Paper
df1_paper_1st <-  df1[df1$Treatment=="paper_1st",]
View(df1_paper_1st)

df2_tool_2nd <-  df2[df2$Treatment=="tool_2nd",]
View(df2_tool_2nd)

df1_f_matched <- df1_paper_1st[df1_paper_1st$Team %in% unique(df2_tool_2nd$Team),]
View(df1_f_matched)

wilcox.test(df1_f_matched$Score_1,df2_tool_2nd$Score_1,paired=TRUE,alternative = "two.sided") #V = 24, p-value = 0.2552
wilcox.test(df1_f_matched$Score_1,df2_tool_2nd$Score_1,paired=TRUE,alternative = "less") #V = 24, p-value = 0.1276
wilcox.test(df1_f_matched$Score_1,df2_tool_2nd$Score_1,paired=TRUE,alternative = "greater") #V = 24, p-value = 0.8881

#Score_2 not comparable between E1 and E2, so I am skipping it.

wilcox.test(df1_f_matched$Question_1,df2_tool_2nd$Question_1,paired=TRUE,alternative = "two.sided") #V = 45, p-value = 0.2918
wilcox.test(df1_f_matched$Question_2,df2_tool_2nd$Question_2,paired=TRUE, alternative = "two.sided") #V = 18.5, p-value = 0.6675

wilcox.test(df1_f_matched$Question_3,df2_tool_2nd$Question_3,paired=TRUE,alternative = "two.sided") #V = 65, p-value = 0.183
wilcox.test(df1_f_matched$Question_3,df2_tool_2nd$Question_3,paired=TRUE,alternative = "less") #V = 65, p-value = 0.9195
wilcox.test(df1_f_matched$Question_3,df2_tool_2nd$Question_3,paired=TRUE,alternative = "greater") #V = 65, p-value = 0.09151


#--------------------
#SCALED and CENTERED Score. No Change.

wilcox.test(df1_f_matched$Score_1_Scaled,df2_tool_2nd$Score_1_Scaled,paired=TRUE,alternative = "two.sided") #V = 24, p-value = 0.2552
wilcox.test(df1_f_matched$Score_1_Scaled,df2_tool_2nd$Score_1_Scaled,paired=TRUE,alternative = "less") #V = 24, p-value = 0.1276
wilcox.test(df1_f_matched$Score_1_Scaled,df2_tool_2nd$Score_1_Scaled,paired=TRUE,alternative = "greater") #V = 24, p-value = 0.8881
