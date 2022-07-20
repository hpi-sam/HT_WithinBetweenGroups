"Do choice of tool versus papers is equally distributed among tutors?"

source("C://Users//Christian//Documents//GitHub//HT_WithinBetweenGroups//hypotheses_test//cleanup.r")

df2 <- cleanup_df2()

chisq.test(df2$Team,df2$Question_6)
# Pearson's Chi-squared test
# 
# data:  df2$Team and df2$Question_6
# X-squared = 39.205, df = 42, p-value = 0.5944



df2_tool <- df2[df2$Treatment=="tool_2nd",]

df2_tool_T3 <- df2_tool[df2_tool$Tutor=="T3",]
df2_tool_T4 <- df2_tool[df2_tool$Tutor=="T4",]
df2_tool_T5 <- df2_tool[df2_tool$Tutor=="T5",]

wilcox.test(df2_tool_T3$Question_4,df2_tool_T4$Question_4,paired=FALSE)
wilcox.test(df2_tool_T3$Question_4,df2_tool_T5$Question_4,paired=FALSE)
wilcox.test(df2_tool_T4$Question_4,df2_tool_T4$Question_4,paired=FALSE)
#Not significant

df1 <- cleanup_df1()
df1_tool <- df_all[df_all$Treatment=="Yakindu",]
df1_tool$Question_4 <- as.numeric(df1_tool$Question_4)

df1_tool_T3 <- df1_tool[df1_tool$Tutor=="Ben",]
df1_tool_T4 <- df1_tool[df1_tool$Tutor=="Julian",]
df1_tool_T5 <- df1_tool[df1_tool$Tutor=="Konrad",]

wilcox.test(df1_tool_T3$Question_4,df1_tool_T4$Question_4,paired=FALSE)
wilcox.test(df1_tool_T3$Question_4,df1_tool_T5$Question_4,paired=FALSE)
wilcox.test(df1_tool_T4$Question_4,df1_tool_T4$Question_4,paired=FALSE)
#Not Significant

df1_tool
df_bound <- cbind(df1_tool,df2_tool)

df_all <-  read.csv("C://Users//Christian//Documents//GitHub//HT_WithinBetweenGroups//data//Tutor_ToolChoice.csv")
chisq.test(df_all$Team,df_all$Question_6)
#X-squared = 25, df = 24, p-value = 0.4058
chisq.test(df_all$Tutor,df_all$Question_6)
#X-squared = 2.2589, df = 3, p-value = 0.5204


