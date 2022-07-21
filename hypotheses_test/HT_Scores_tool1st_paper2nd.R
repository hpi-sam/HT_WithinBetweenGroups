"
Hypotheses tests on Experiment-1 tool_1st versus Experiment-2 paper_2nd
"

source("C://Users//Christian//Documents//GitHub//HT_WithinBetweenGroups//hypotheses_test//cleanup.r")

df1 <- cleanup_task1()
df2 <- cleanup_task2()
#------------------------------------------------------
#Hypotheses tests

#Score_1 Tool vs Paper
df1_tool_1st <-  df1[df1$Treatment=="tool_1st",]
View(df1_tool_1st)

df2_paper_2nd <-  df2[df2$Treatment=="paper_2nd",]
View(df2_paper_2nd)

df1_f_matched <- df1_tool_1st[df1_tool_1st$Team %in% unique(df2_paper_2nd$Team),]
View(df1_f_matched)

wilcox.test(df1_f_matched$Score_1,df2_paper_2nd$Score_1,paired=TRUE,alternative = "two.sided") #V = 24, p-value = 0.4406
wilcox.test(df1_f_matched$Score_1,df2_paper_2nd$Score_1,paired=TRUE,alternative = "less") #V = 24, p-value = 0.8189
wilcox.test(df1_f_matched$Score_1,df2_paper_2nd$Score_1,paired=TRUE,alternative = "greater") #V = 24, p-value = 0.2203

#Score_2 not comparable between E1 and E2, so I am skipping it.

wilcox.test(df1_f_matched$Question_1,df2_paper_2nd$Question_1,paired=TRUE,alternative = "two.sided") #V = 32.5, p-value = 0.635
wilcox.test(df1_f_matched$Question_2,df2_paper_2nd$Question_2,paired=TRUE,alternative = "two.sided") #V = 8, p-value = 0.6733

wilcox.test(df1_f_matched$Question_3,df2_paper_2nd$Question_3,paired=TRUE,alternative = "two.sided") #V = 74.5, p-value = 0.006001 ***************
wilcox.test(df1_f_matched$Question_3,df2_paper_2nd$Question_3,paired=TRUE,alternative = "less") #V = 74.5, p-value = 0.9976
wilcox.test(df1_f_matched$Question_3,df2_paper_2nd$Question_3,paired=TRUE,alternative = "greater") #V = 74.5, p-value = 0.003  ********

wilcox.test(df1_f_matched$Question_4,df2_paper_2nd$Question_4,paired=TRUE,alternative = "two.sided") #V = 17, p-value = 0.9438
