## Purpose: util function
#save(etab_s1, etab_s2, ntab_s1, ntab_s2, file = "sysdata.rda")
load("sysdata.rda")

#generate sample data
pt <- data.frame(patient_id = rep(1:1000, each = 10), row_id = sample(1:2036, 200))
icd9_w_id = cbind(ntab_s2, row_id = 1:2036)
pt <- dplyr::inner_join(pt, icd9_w_id)[c("dx", "patient_id")]

