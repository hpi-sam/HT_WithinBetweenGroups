


cleanup_df1 <- function(){
  df1 <-  read.csv("C://Users//Christian//Documents//GitHub//HT_WithinBetweenGroups//data//dataframe_1.csv")
  #Convert fields do numeric
  cols.num <- colnames(df1)[4:9]
  sapply(df1, class)
  df1[cols.num] <- sapply(df1[cols.num],as.numeric)
  
  #Remove rows that have score either zero or NA
  df1 <- df1[!(is.na(df1$Score_1) & is.na(df1$Score_2)), ]
  
  #Replace all zeros for NA, so it does not distort the hypotheses tests
  df1[, 4:9][df1[, 4:9] == 0] <- NA 
  return(df1)
}

cleanup_df2 <- function(){
  df2 <-  read.csv("C://Users//Christian//Documents//GitHub//HT_WithinBetweenGroups//data//dataframe_2.csv")
  #Convert fields do numeric
  cols.num <- colnames(df2)[4:10]
  sapply(df2, class)
  df2[cols.num] <- sapply(df2[cols.num],as.numeric)
  
  #Remove rows that have score either zero or NA
  df2 <- df2[!( (is.na(df2$Score_1) | df2$Score_1==0) & (is.na(df2$Score_2) | df2$Score_2==0) ), ]
  
  #Replace all zeros for NA, so it does not distort the hypotheses tests
  df2[, 4:10][df2[, 4:10] == 0] <- NA 
  return(df2)
}
