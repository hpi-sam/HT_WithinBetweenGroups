"
Hypotheses tests on Experiment-1 paper_1st versus Experiment-2 paper_2nd
Between groups, independent groups
Wilcoxon Rank sum test (paired=FALSE)
"
source("C://Users//Christian//Documents//GitHub//HT_WithinBetweenGroups//hypotheses_test//cleanup.r")

df1 <- cleanup_df1()
df2 <- cleanup_df2()


#------------------------------------------------------
#Hypotheses tests

#Score_1 paper vs Paper
df1_paper_1st <-  df1[df1$Treatment=="paper_1st",]
View(df1_paper_1st)

df2_paper_2nd <-  df2[df2$Treatment=="paper_2nd",]
View(df2_paper_2nd)

wilcox.test(df1_paper_1st$Score_1,df2_paper_2nd$Score_1,paired=FALSE,alternative = "two.sided") #W = 125, p-value = 0.002051 ***********
wilcox.test(df1_paper_1st$Score_1,df2_paper_2nd$Score_1,paired=FALSE,alternative = "less") #W = 125, p-value = 0.9992
wilcox.test(df1_paper_1st$Score_1,df2_paper_2nd$Score_1,paired=FALSE,alternative = "greater") #W = 125, p-value = 0.001026 ***********
#NOT COMPARABLE Question 2 between E1 and E2, so I am skpping it.

wilcox.test(df1_paper_1st$Question_1,df2_paper_2nd$Question_1,paired=FALSE,alternative = "two.sided") #W = 76.5, p-value = 0.7749
wilcox.test(df1_paper_1st$Question_2,df2_paper_2nd$Question_2,paired=FALSE,alternative = "two.sided") #W = 69, p-value = 0.9012
wilcox.test(df1_paper_1st$Question_3,df2_paper_2nd$Question_3,paired=FALSE,alternative = "two.sided") #W = 100, p-value = 0.102
wilcox.test(df1_paper_1st$Question_4,df2_paper_2nd$Question_4,paired=FALSE,alternative = "two.sided") #W = 67, p-value = 0.921
