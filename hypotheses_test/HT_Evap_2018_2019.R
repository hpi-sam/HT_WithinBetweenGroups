"Anderson-Darling Test compare two distributions 
from evaluations in 2018 and 2019 courses, 
with respectively 65 and 51 participants"


"Result
We verified that the distributions compared in Fig.\ref{} are statistically significant distinct. Using Wilcoxon unpaired test we rejected the null hypothesis for 
all three evaluation questions: grade (W = 2150, p-value = 0.002361), 
time (W = 2432.5, p-value = 5.856e-06), and tutor (W = 2420, p-value = 8.961e-06).


"

library(twosamples)

rank <- c(1,2,3,4,5)
proportions_grade_2018 <- c(6.2,53.8,32.3,4.6,3.1)
proportions_grade_2019 <- c(23.5,58.8,9.8,5.9,2.0)

proportions_time_2018 <- c(12.3,49.2,27.7,6.2,4.6)
proportions_time_2019 <- c(62.7,21.9,0.0,9.8,5.9)

proportions_tutor_2018 <- c(23.1,29.2,24.6,13.8,9.2)
proportions_tutor_2019 <- c(60.8,23.5,11.8,2.0,2.0)


#--------------------------------------------------------
unroll_rank <- function(percentages, total_count){
  
  count_at_i_list <- round(percentages*total_count)
  vote_list <- rep(c(1,2,3,4,5),times=count_at_i_list)
  return(vote_list)
}

votes_grade_2018 <- unroll_rank(proportions_grade_2018/100,65)
votes_grade_2019 <- unroll_rank(proportions_grade_2019/100,51)
wilcox.test(votes_grade_2018,
            votes_grade_2019,unpaired=FALSE,alternative = "two.sided")

votes_time_2018 <- unroll_rank(proportions_time_2018/100,65)
votes_time_2019 <- unroll_rank(proportions_time_2019/100,51)
wilcox.test(votes_time_2018,
            votes_time_2019,unpaired=FALSE,alternative = "two.sided")

votes_tutor_2018 <- unroll_rank(proportions_tutor_2018/100,65)
votes_tutor_2019 <- unroll_rank(proportions_tutor_2019/100,51)
wilcox.test(votes_tutor_2018,
            votes_tutor_2019,unpaired=FALSE,alternative = "two.sided")


#--------------------------------------------------------

