"Boxplots Between Group Comparisons for Task1 and Task2"

library(reshape2)
library(ggplot2)
library(data.table)
library(stringr)
library(tidyverse)
#library(extrafont)

source("C://Users//Christian//Documents//GitHub//HT_WithinBetweenGroups//hypotheses_test//cleanup.r")

#ggsave(file = foo.eps, plot = map, width = 15, height = 10, 
#       units = "cm", family='Linux Libertine Display')


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
  geom_boxplot(width=1,lwd=0.25)+
  ggtitle("Task-1")+
  stat_summary(fun=mean, geom = "point", 
               position = position_dodge(1), shape=4, size=2)+
  theme_minimal()+
  theme(plot.title = element_text(size=8),
        axis.title=element_blank(),
        axis.text = element_text(size = 6),
        legend.title=element_blank(),
        legend.text = element_text(size=6),
        legend.key.size = unit(1,"line"))
bb1 <- bp + scale_fill_grey(start =1, end = 0.85) 
bb1

path <- "C://Users//Christian//Documents//SVN_Giese//ModelsRobotSimulator//img//"
#GitHub//HT_WithinBetweenGroups//figures//"
ggsave(file = paste(path,"Boxplot_BetweenGroups_ask1.pdf"), plot = bb1, width = 10, height = 4, 
       units = "cm"); #family='Libertine Display')

#----------------------------------------------------

df2 <- cleanup_task2()
df <- normalize_features(df2)
data_long <- reshape2::melt(df, id <- "Treatment")
  
bp <- ggplot(data_long, aes(x=variable, y=value,fill=Treatment)) +
    geom_boxplot(width=1,lwd=0.25)+
    ggtitle("Task-2")+
  stat_summary(fun=mean, geom = "point", 
               position = position_dodge(1), shape=4, size=2)+
  theme_minimal()+
  theme(plot.title = element_text(size=8),
        axis.title=element_blank(),
        axis.text = element_text(size = 6),
        legend.title=element_blank(),
        legend.text = element_text(size=6),
        legend.key.size = unit(1,"line"))
bb2 <- bp + scale_fill_grey(start =1, end = 0.85) 
bb2
path <- "C://Users//Christian//Documents//SVN_Giese//ModelsRobotSimulator//img//"
ggsave(file = paste(path,"Boxplot_BetweenGroups_ask2.pdf"), plot = bb2, width = 10, height = 4, 
       units = "cm"); #family='Libertine Display')

