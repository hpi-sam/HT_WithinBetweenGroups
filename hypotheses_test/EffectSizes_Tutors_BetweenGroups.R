
"
Effect Sizes of Between Groups Analyzes Stratified by Tutors
 Treatment: Tool-Based task
 Control: Paper-Based task
 Outcome Variables: Scores in Tasks 1 and 2, Answers from Questions 1 to 4

Assumptions
 Groups are independent (groups with different students and different tutors, but same task)
 Groups have equal variance w.r.t. outcome variable (we used Levene test to verify that)
 
 Method Wilcoxon Rank Sum Test which is executed for unpaired groups, because are independent groups.
 R-package rstatix, function: wilcox.test(data, paired=FALSE)
 
 We also compute the effect size using method wilcox_effsize in R-package rstatix by setting paired parameter to FALSE.
 
 Package source: https://rpkgs.datanovia.com/rstatix/reference/wilcox_effsize.html
 
 
 TODO:
 - Check 
 
"

#install.packages("rstatix") #https://rpkgs.datanovia.com/rstatix/reference/wilcox_effsize.html
#install.packages("coin")
library(rstatix)
library(coin)
library(car)

source("C://Users//Christian//Documents//GitHub//HT_WithinBetweenGroups//hypotheses_test//cleanup.r")

df1 <- cleanup_task1()

tutor_list <- unique(df1$Tutor)[2:5] #removed first tutor as it has only row in the data (only treatment, no control)
question_list <- colnames(df1[4:9]) #("score_1, score_2,question_1, question_2,question_3, question_4")

tutor=tutor_list[4]
df_tutor <-  df1[df1$Tutor==tutor,]
#leveneTest(Score_1 ~ Treatment, df_tutor)


"Compute rank sign test and effect sizes"
compute_wilcox_tests <- function(df){

  #levene_test_equal_variance <- matrix(data=NA, nrow=4,ncol=length(question_list))
  p_values <- matrix(data=NA, nrow=4,ncol=length(question_list))
  effect_size <- matrix(data=NA, nrow=4,ncol=length(question_list))
  magnitude <- (matrix(data=c("-"), nrow=4,ncol=length(question_list)))
  i=0;
  for(tutor in tutor_list){
    i=i+1
    j=1
    df_tutor <-  df[df$Tutor==tutor,]
  
    #leveneTest(Score_1 ~ Treatment, df_tutor)
    p_values[i,j] <- rstatix::wilcox_test(data=df_tutor,Score_1 ~ Treatment, paired=FALSE, alternative = "two.sided")$p
    r <- rstatix::wilcox_effsize(data=df_tutor, Score_1 ~ Treatment, alternative = "two.sided")
    effect_size[i,j]<-r$effsize
    magnitude[i,j] <- levels(r$magnitude)[r$magnitude]
    j <- j+1
    
    p_values[i,j] <- rstatix::wilcox_test(data=df_tutor, Score_2 ~ Treatment, paired=FALSE, alternative = "two.sided")$p
    r <- rstatix::wilcox_effsize(data=df_tutor, Score_1 ~ Treatment, alternative = "two.sided")
    effect_size[i,j]<-r$effsize
    magnitude[i,j] <- levels(r$magnitude)[r$magnitude]
    j <- j+1
    
    p_values[i,j] <- rstatix::wilcox_test(data=df_tutor, Question_1 ~ Treatment, paired=FALSE, alternative = "two.sided")$p
    r <- rstatix::wilcox_effsize(data=df_tutor, Score_1 ~ Treatment, alternative = "two.sided")
    effect_size[i,j]<-r$effsize
    magnitude[i,j] <- levels(r$magnitude)[r$magnitude]
    j <- j+1 
    
    p_values[i,j] <- rstatix::wilcox_test(data=df_tutor, Question_2 ~ Treatment, paired=FALSE, alternative = "two.sided")$p
    r <- rstatix::wilcox_effsize(data=df_tutor, Score_1 ~ Treatment, alternative = "two.sided")
    effect_size[i,j]<-r$effsize
    magnitude[i,j] <- levels(r$magnitude)[r$magnitude]
    j <- j+1 
    
    p_values[i,j] <- rstatix::wilcox_test(data=df_tutor, Question_3 ~ Treatment, paired=FALSE, alternative = "two.sided")$p
    r <- rstatix::wilcox_effsize(data=df_tutor, Score_1 ~ Treatment, alternative = "two.sided")
    effect_size[i,j]<-r$effsize
    magnitude[i,j] <- levels(r$magnitude)[r$magnitude]
    j <- j+1 
   
    p_values[i,j] <- rstatix::wilcox_test(data=df_tutor, Question_4 ~ Treatment, paired=FALSE, alternative = "two.sided")$p
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

out <- compute_wilcox_tests(df1)

#P-Values
#    Score_1 Score_2 Question_1 Question_2 Question_3 Question_4
#T2  0.1590   0.806      0.711      0.115     0.0987      0.167
#T3  0.2220   0.282      0.282      1.000     0.4510      0.439
#T4  0.0765   0.400      1.000      0.554     0.2800      0.629
#T5  0.0636   0.582      0.696      0.693     0.4710      0.854

out$effect_sizes

out$magnitude



