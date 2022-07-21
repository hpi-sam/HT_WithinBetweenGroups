

cleanup_task1 <- function(){
  df1 <-  read.csv("C://Users//Christian//Documents//GitHub//HT_WithinBetweenGroups//data//task_1.csv")
  #Convert fields do numeric
  cols.num <- colnames(df1)[4:9]
  sapply(df1, class)
  df1[cols.num] <- sapply(df1[cols.num],as.numeric)
  
  #Convert score to percentages of the maximum score
  df1$Score_1 <- (df1$Score_1/24 ) *100
  df1$Score_2 <- (df1$Score_2/24 ) *100
  
  #Replace all zeros for NA, so it does not distort the hypotheses tests
  df1[, 4:9][df1[, 4:9] == 0] <- NA 
  
  #Remove rows that have score for Score
  #df1 <- df1[!(is.na(df1$Score_1) | is.na(df1$Score_2)), ]
  
  return(df1)
}

cleanup_task2 <- function(){
  df2 <-  read.csv("C://Users//Christian//Documents//GitHub//HT_WithinBetweenGroups//data//task_2.csv")
  #Convert fields do numeric
  cols.num <- colnames(df2)[4:10]
  sapply(df2, class)
  df2[cols.num] <- sapply(df2[cols.num],as.numeric)
  
  #Convert score to percentages of the maximum score
  df2$Score_1 <- (df2$Score_1/18 ) *100
  df2$Score_2 <- (df2$Score_2/18 ) *100
  
  #Replace zeros with NA
  df2[, 4:10][df2[, 4:10] == 0] <- NA 
  
  #Remove rows that have score either zero or NA
  #df2 <- df2[!is.na(df2$Score_1), ]
  
  #Discarded Score-2 because it is not comparable with Task-1 (so not valid for within-group comparisons) and has too few data points for a between group comparsion
  df2 = subset(df2, select = -c(Score_2) )
  
  #Replace all zeros for NA, so it does not distort the hypotheses tests
  return(df2)
}

