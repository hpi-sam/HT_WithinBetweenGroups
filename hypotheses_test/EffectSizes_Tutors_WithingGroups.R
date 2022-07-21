
"
Effect Sizes of Within Groups Analyzes Stratified by Tutors
 Treatment: Tool-Based task
 Control: Paper-Based task
 Outcome Variables: Scores in Tasks 1 and 2, Answers from Questions 1 to 4

Assumptions
 Groups are independent (groups with different students and different tutors, but same task)
 Distributions should have similar variance 
 
 Method Wilcoxon Sign-Rank Test which is executed for paired groups, because the compared groups are dependent, for instance multiple measures of the same unit.
 R-package rstatix, function: wilcox.test(data, paired=TRUE)
 
 We also compute the effect size using method wilcox_effsize in R-package rstatix by setting paired parameter to TRUE.
 
 Package source: https://rpkgs.datanovia.com/rstatix/reference/wilcox_effsize.html
 
"

#install.packages("rstatix") #https://rpkgs.datanovia.com/rstatix/reference/wilcox_effsize.html
#install.packages("coin")
library(rstatix)
library(coin)

source("C://Users//Christian//Documents//GitHub//HT_WithinBetweenGroups//hypotheses_test//cleanup.r")

df2 <- cleanup_task2()


tutor_list <- unique(df2$Tutor)[3:5] #removed tutor 1 and 2 they do not have enough data for the tests
question_list <- c("Score_1", "Question_1", "Question_2","Question_3", "Question_4")

df1_f <- df1[df1$Tutor %in% tutor_list,]
df2_f <- df2[df2$Tutor %in% tutor_list,]


"Compute rank sign test and effect sizes"
compute_wilcox_tests <- function(df){
  df <- df2
  p_values <- matrix(data=NA, nrow=length(tutor_list),ncol=length(question_list))
  effect_size <- matrix(data=NA, nrow=length(tutor_list),ncol=length(question_list))
  magnitude <- (matrix(data=c("-"), nrow=length(tutor_list),ncol=length(question_list)))
  i=0;
  for(tutor in tutor_list){
    i=i+1
    j=1
    df_tutor <-  df[df$Tutor==tutor,]
    
    p_values[i,j] <- rstatix::wilcox_test(data=df_tutor,Score_1 ~ Treatment, paired=TRUE, alternative = "two.sided")$p
    r <- rstatix::wilcox_effsize(data=df_tutor, Score_1 ~ Treatment, alternative = "two.sided")
    effect_size[i,j]<-r$effsize
    magnitude[i,j] <- levels(r$magnitude)[r$magnitude]
    j <- j+1
    
    #Skipping Question-2 because it is not comparable and very few datapoints
    #p_values[i,j] <- rstatix::wilcox_test(data=df_tutor, Score_2 ~ Treatment, paired=TRUE, alternative = "two.sided")$p
    #r <- rstatix::wilcox_effsize(data=df_tutor, Score_1 ~ Treatment, alternative = "two.sided")
    #effect_size[i,j]<-r$effsize
    #magnitude[i,j] <- levels(r$magnitude)[r$magnitude]
    #j <- j+1
    
    p_values[i,j] <- rstatix::wilcox_test(data=df_tutor, Question_1 ~ Treatment, paired=TRUE, alternative = "two.sided")$p
    r <- rstatix::wilcox_effsize(data=df_tutor, Score_1 ~ Treatment, alternative = "two.sided")
    effect_size[i,j]<-r$effsize
    magnitude[i,j] <- levels(r$magnitude)[r$magnitude]
    j <- j+1 
    
    p_values[i,j] <- rstatix::wilcox_test(data=df_tutor, Question_2 ~ Treatment, paired=TRUE, alternative = "two.sided")$p
    r <- rstatix::wilcox_effsize(data=df_tutor, Score_1 ~ Treatment, alternative = "two.sided")
    effect_size[i,j]<-r$effsize
    magnitude[i,j] <- levels(r$magnitude)[r$magnitude]
    j <- j+1 
    
    p_values[i,j] <- rstatix::wilcox_test(data=df_tutor, Question_3 ~ Treatment, paired=TRUE, alternative = "two.sided")$p
    r <- rstatix::wilcox_effsize(data=df_tutor, Score_1 ~ Treatment, alternative = "two.sided")
    effect_size[i,j]<-r$effsize
    magnitude[i,j] <- levels(r$magnitude)[r$magnitude]
    j <- j+1 
   
    p_values[i,j] <- rstatix::wilcox_test(data=df_tutor, Question_4 ~ Treatment, paired=TRUE, alternative = "two.sided")$p
    r <- rstatix::wilcox_effsize(data=df_tutor, Score_1 ~ Treatment, alternative = "two.sided")
    effect_size[i,j]<-r$effsize
    magnitude[i,j] <- levels(r$magnitude)[r$magnitude]
    
  }
  
  out <- list()
  
  df_p_values <- data.frame(p_values,row.names = tutor_list)
  colnames(df_p_values) <-  question_list
  out$p_values <- df_p_values
  
  df_effect_sizes <- data.frame(effect_size,row.names = tutor_list)
  colnames(df_effect_sizes) <-  question_list
  out$effect_sizes <- df_effect_sizes
  
  df_magnitude <- data.frame(magnitude,row.names = tutor_list)
  colnames(df_magnitude) <-  question_list
  out$magnitude <- df_magnitude
  
  return(out)
}

out <- compute_wilcox_tests(df2)

#P-Values
#    Score_1 Question_1 Question_2 Question_3 Question_4
#T3   1.000      1.000      0.586       1.00      0.346
#T4   0.346      0.149        NaN       0.50      1.000
#T5   0.250      0.750      0.500       0.75      1.000

out$effect_sizes
#      Score_1 Question_1 Question_2 Question_3 Question_4
#T3 0.09038769 0.09038769 0.09038769 0.09038769 0.09038769
#T4 0.64549722 0.64549722 0.64549722 0.64549722 0.64549722
#T5 0.85194275 0.85194275 0.85194275 0.85194275 0.85194275

out$magnitude
#    Score_1 Question_1 Question_2 Question_3 Question_4
#T3   small      small      small      small      small
#T4   large      large      large      large      large
#T5   large      large      large      large      large


