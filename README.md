# InjurySeverityScore
Translate ICD-9 to Injury Severity Score. This is rewritten from STATA into R package based on ICDPIC. The reference was seen here:
https://ideas.repec.org/c/boc/bocode/s457028.html

## Input Dataset
Function `injury_score` requires the input dataset has at least two variables. One is patient id and the other is the icd9 code. The icd9 code must be **character**.  
## Output Dataset
Output dataset will use the patient id as the primary key and contains variable `iss` which is the injury severity score. For the definition of injury severity score, go to https://en.wikipedia.org/wiki/Injury_Severity_Score.

