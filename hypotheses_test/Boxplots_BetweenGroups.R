"Boxplots Between Group Comparisons for Task1 and Task2"

library(reshape2)
library(ggplot2)
library(data.table)

source("C://Users//Christian//Documents//GitHub//HT_WithinBetweenGroups//hypotheses_test//cleanup.r")



#Normalize data to plot in the same scale
normalize_features <- function(df){

  df$Score <- df$Score_1/100
  
  df$Time <- (df$Question_3 - min(df$Question_3,na.rm = TRUE))/
    (max(df$Question_3,na.rm = TRUE) - min(df$Question_3,na.rm = TRUE))
  
  df$Satisfaction <-  (df$Question_4 - min(df$Question_4,na.rm = TRUE))/
    (max(df$Question_4,na.rm = TRUE) - min(df$Question_4,na.rm = TRUE))
  df$Satisfaction <- 1- df$Satisfaction #reverses it, because low is good, high is bad...

  #keep only the columns that will be plotted
  df <- subset(df, select = c(Treatment,Score,Time,Satisfaction) )
  
  df[df$Treatment %like% "paper",]$Treatment <- "paper"
  df[df$Treatment %like% "tool",]$Treatment <- "tool"
  
  return(df)
}
#---------------------------------------

df1 <- cleanup_task1()
df <- normalize_features(df1)
data_long <- reshape2::melt(df, id <- "Treatment")
bp <- ggplot(data_long, aes(x=variable, y=value,fill=Treatment)) +
  geom_boxplot(width=1)+
  ggtitle("Task-1")+
  stat_summary(fun=mean, geom = "point", 
               position = position_dodge(1), shape=4, size=5)+
  theme_minimal()+
  theme(plot.title = element_text(size=32),
        axis.title=element_blank(),
        axis.text = element_text(size = 20),
        legend.title=element_blank(),
        legend.text = element_text(size=20),
        legend.key.size = unit(3,"line"))
bp + scale_fill_grey(start =1, end = 0.85) 

#----------------------------------------------------

df2 <- cleanup_task2()
df <- normalize_features(df2)
data_long <- reshape2::melt(df, id <- "Treatment")
  
bp <- ggplot(data_long, aes(x=variable, y=value,fill=Treatment)) +
    geom_boxplot(width=1)+
    ggtitle("Task-2")+
  stat_summary(fun=mean, geom = "point", 
               position = position_dodge(1), shape=4, size=5)+
  theme_minimal()+
  theme(plot.title = element_text(size=32),
        axis.title=element_blank(),
        axis.text = element_text(size = 20),
        legend.title=element_blank(),
        legend.text = element_text(size=20),
        legend.key.size = unit(3,"line"))
bp + scale_fill_grey(start =1, end = 0.85) 
 
