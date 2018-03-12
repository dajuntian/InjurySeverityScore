# following code supresses note from R CMD check
if(getRversion() >= "2.15.1") {
  utils::globalVariables(c('severity', 'usubjid', 'issbr', 'score', 'score_seq',
                           'subj', 'code'))
} 
# sample code for testing
null_f <- function(){
pat_id <- c(2,2,2,2,2,1,2,1,2,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1)
icd9 <- c('874.2', '874.8', '900.81', '900.82', '900.89', '805.06', 
         'E966', '805.07', 'V14.0', '807.02', 'V70.4', '821.01', '823.20', 
           '860.0', '861.01', '861.21', '861.22', '863.84', '864.04', '865.04', 
           '865.09', '866.02', '868.04', '958.4')
sample_data <- data.frame(subj = pat_id, code = icd9, stringsAsFactors = FALSE)
injury_score(sample_data, subj, code)

# output from previous call
#  subj br_1 br_2 br_3 br_4 br_5 br_6 max_1 max_2 max_3 iss
#1    2    3    0    0    0    0    1     3     1     0  10
#2    1    2    0    3    5    3    0     5     3     3  43




}