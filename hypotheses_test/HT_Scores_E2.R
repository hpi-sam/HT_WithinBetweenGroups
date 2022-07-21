"
Hypotheses tests on Experiment-2 Between Group, hence independent groups, so using Wilcoxon rank sum test (paired=FALSE)
"
source("C://Users//Christian//Documents//GitHub//HT_WithinBetweenGroups//hypotheses_test//cleanup.r")

df2 <- cleanup_task2()
View(df2)

#--------------------------------
# NORMALITY TEST
shapiro.test(df2$Score_1) 
#data:  df2$Score_1 W = 0.87336, p-value = 0.007436
shapiro.test(df2$Score_2) 
#data:  df2$Score_2 W = 0.89737, p-value = 0.3955
shapiro.test(df2$Question_1) 
#data:  df2$Question_1 W = 0.87063, p-value = 0.00659
shapiro.test(df2$Question_2)
#data:  df2$Question_2 W = 0.82984, p-value = 0.001198
shapiro.test(df2$Question_3)
#data:  df2$Question_3 W = 0.92203, p-value = 0.07366 *************************
shapiro.test(df2$Question_4)
#data:  df2$Question_4 W = 0.8322, p-value = 0.001671

"All columns probably have not normal distributions"

#------------------------------------------------------
#Hypotheses tests

#Score_1 Tool vs Paper
df2_tool_2nd <-  df2[df2$Treatment=="tool_2nd",]
View(df2_tool_2nd)

df2_paper_2nd <-  df2[df2$Treatment=="paper_2nd",]
View(df2_paper_2nd)

wilcox.test(df2_tool_2nd$Score_1,df2_paper_2nd$Score_1,paired=FALSE,alternative = "two.sided") #W = 90, p-value = 0.139
wilcox.test(df2_tool_2nd$Score_1,df2_paper_2nd$Score_1,paired=FALSE,alternative = "less") #W = 90, p-value = 0.9385
wilcox.test(df2_tool_2nd$Score_1,df2_paper_2nd$Score_1,paired=FALSE,alternative = "greater") #W = 90, p-value = 0.06949

wilcox.test(df2_tool_2nd$Question_1,df2_paper_2nd$Question_1,paired=FALSE,alternative = "two.sided") #W = 63, p-value = 0.1667
wilcox.test(df2_tool_2nd$Question_2,df2_paper_2nd$Question_2,paired=FALSE,alternative = "two.sided") #W = 83, p-value = 0.7046
wilcox.test(df2_tool_2nd$Question_3,df2_paper_2nd$Question_3,paired=FALSE,alternative = "two.sided") #W = 85, p-value = 0.7217
wilcox.test(df2_tool_2nd$Question_4,df2_paper_2nd$Question_4,paired=FALSE,alternative = "two.sided") #W = 69.5, p-value = 0.4342


