"Effect Sizes of Between Groups Analyzes Stratified by Tutors
 Treatment: Tool-Based task
 Control: Paper-Based task
 Outcome Variables: Scores in Tasks 1 and 2, Answers from Questions 1 to 4
"

#install.packages("rstatix") #https://rpkgs.datanovia.com/rstatix/reference/wilcox_effsize.html
#install.packages("coin")
library(rstatix)
library(coin)

source("C://Users//Christian//Documents//GitHub//HT_WithinBetweenGroups//hypotheses_test//cleanup.r")

df1 <- cleanup_df1()

tutor_list <- unique(df1$Tutor)[2:5] #removed first tutor as it has only row in the data (only treatment, no control)
question_list <- colnames(df1[4:9]) #("score_1, score_2,question_1, question_2,question_3, question_4")

"Compute rank sign test and effect sizes"
compute_wilcox_tests <- function(df){

  p_values <- matrix(data=NA, nrow=4,ncol=length(question_list))
  effect_size <- matrix(data=NA, nrow=4,ncol=length(question_list))
  magnitude <- (matrix(data=c("-"), nrow=4,ncol=length(question_list)))
  i=0;
  for(tutor in tutor_list){
    i=i+1
    j=1
    df_tutor <-  df1[df1$Tutor==tutor,]
    
    p_values[i,j] <- rstatix::pairwise_wilcox_test(data=df_tutor, Score_1 ~ Treatment, alternative = "two.sided")$p
    r <- rstatix::wilcox_effsize(data=df_tutor, Score_1 ~ Treatment, alternative = "two.sided")
    effect_size[i,j]<-r$effsize
    magnitude[i,j] <- levels(r$magnitude)[r$magnitude]
    j <- j+1
    
    p_values[i,j] <- rstatix::pairwise_wilcox_test(data=df_tutor, Score_2 ~ Treatment, alternative = "two.sided")$p
    r <- rstatix::wilcox_effsize(data=df_tutor, Score_1 ~ Treatment, alternative = "two.sided")
    effect_size[i,j]<-r$effsize
    magnitude[i,j] <- levels(r$magnitude)[r$magnitude]
    j <- j+1
    
    p_values[i,j] <- rstatix::pairwise_wilcox_test(data=df_tutor, Question_1 ~ Treatment, alternative = "two.sided")$p
    r <- rstatix::wilcox_effsize(data=df_tutor, Score_1 ~ Treatment, alternative = "two.sided")
    effect_size[i,j]<-r$effsize
    magnitude[i,j] <- levels(r$magnitude)[r$magnitude]
    j <- j+1 
    
    p_values[i,j] <- rstatix::pairwise_wilcox_test(data=df_tutor, Question_2 ~ Treatment, alternative = "two.sided")$p
    r <- rstatix::wilcox_effsize(data=df_tutor, Score_1 ~ Treatment, alternative = "two.sided")
    effect_size[i,j]<-r$effsize
    magnitude[i,j] <- levels(r$magnitude)[r$magnitude]
    j <- j+1 
    
    p_values[i,j] <- rstatix::pairwise_wilcox_test(data=df_tutor, Question_3 ~ Treatment, alternative = "two.sided")$p
    r <- rstatix::wilcox_effsize(data=df_tutor, Score_1 ~ Treatment, alternative = "two.sided")
    effect_size[i,j]<-r$effsize
    magnitude[i,j] <- levels(r$magnitude)[r$magnitude]
    j <- j+1 
   
    p_values[i,j] <- rstatix::pairwise_wilcox_test(data=df_tutor, Question_4 ~ Treatment, alternative = "two.sided")$p
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

out <- compute_tests(df1)





