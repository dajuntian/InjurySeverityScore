# InjurySeverityScore
Translate ICD-9 to Injury Severity Score. This is rewritten from STATA into R package based on ICDPIC. The reference was seen here:
https://ideas.repec.org/c/boc/bocode/s457028.html.

## Input Dataset
Function `injury_score` requires the input dataset has at least two variables. One is patient id and the other is the icd9 code. The icd9 code must be **character**. The icd9 code could be with or without dot and is indicated through `has_dot`.
## Output Dataset
Output dataset will use the patient id as the primary key and contains variable `iss` which is the injury severity score. For the definition of injury severity score, go to https://en.wikipedia.org/wiki/Injury_Severity_Score. Currently, the output dataset has some extra variables for debug purpose.
## Example Code
1. If you don't have the package, install through one of the following:
* `install.packages('InjurySeverityScore')`
* `devtools::install_github("dajuntian/InjurySeverityScore")`
2. Generate sample dataset  
``` R
pat_id <- c(2,2,2,2,2,1,2,1,2,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1)
icd9 <- c('874.2', '874.8', '900.81', '900.82', '900.89', '805.06', 
          'E966', '805.07', 'V14.0', '807.02', 'V70.4', '821.01', '823.20', 
          '860.0', '861.01', '861.21', '861.22', '863.84', '864.04', '865.04', 
          '865.09', '866.02', '868.04', '958.4')
sample_data <- data.frame(subj = pat_id, code = icd9, stringsAsFactors = FALSE)
````
3. Load the package and calculate injury severity score.
```R
library(InjurySeverityScore)
injury_score(sample_data, subj, code, has_dot = T)
```
