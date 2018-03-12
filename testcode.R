pat_id <- c(2,2,2,2,2,1,2,1,2,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1)
icd9 <- c('874.2', '874.8', '900.81', '900.82', '900.89', '805.06', 
          'E966', '805.07', 'V14.0', '807.02', 'V70.4', '821.01', '823.20', 
          '860.0', '861.01', '861.21', '861.22', '863.84', '864.04', '865.04', 
          '865.09', '866.02', '868.04', '958.4')
sample_data <- data.frame(subj = pat_id, code = icd9, stringsAsFactors = FALSE)
injury_score(sample_data, subj, code)

data_wide <- sample_data %>% arrange(subj) %>% 
  group_by(subj) %>% 
  mutate(seq = paste0("dx", row_number())) %>% 
  spread(key = seq, value = code)
injury_score(data_wide, subj, dx, tall = F)

