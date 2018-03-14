# following code supresses note from R CMD check

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
pat_id <- c(2,2,2,2,2,1,2,1,2,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1)
icd9 <- c('874.2', '874.8', '900.81', '900.82', '900.89', '805.06', 
          'E966', '805.07', 'V14.0', '807.02', 'V70.4', '821.01', '823.20', 
          '860.0', '861.01', '861.21', '861.22', '863.84', '864.04', '865.04', 
          '865.09', '866.02', '868.04', '958.4')
sample_data <- data.frame(subj = pat_id, code = icd9, stringsAsFactors = FALSE)
injury_score(sample_data, subj, code)

sample_data1 <- sample_data[c("code", "subj")]
names(sample_data1) <- c("diag", "ptid1")
injury_score(sample_data1, ptid1, diag)

data_wide <- sample_data %>% arrange(subj) %>% 
  group_by(subj) %>% 
  mutate(seq = paste0("dx", row_number())) %>% 
  ungroup() %>% 
  spread(key = seq, value = code)
injury_score(data_wide, subj, dx, tall = F)

data_wide1 <- data_wide[c("dx1",'subj', "dx2")]
names(data_wide1) <- c("ds1", "refno", "ds2") 
injury_score(data_wide1, refno, ds, tall = F)

load("testdata.rda")
injury_score(indata_long, subj, code)


data.wide.big <- as.data.frame(data_wide)[rep(c(1,2),1000000),]
data.wide.big$subj <- 1:nrow(data.wide.big)
y2 <- injury_score(data.wide.big, subj, dx, tall = F)

## testing enque-euq branch
## at github


