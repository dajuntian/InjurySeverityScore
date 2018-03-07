
injury_score <- function(indata, id_var, dx_var){
  # create a new copy of data, get variable name as string
  idVar <- deparse(substitute(id_var))
  dxVar <- deparse(substitute(dx_var)) 
  cp_indata <- dplyr::select(indata, c(idVar, dxVar))
  
  #rename variable
  names(cp_indata) <- c("usubjid", "dx")
  
  #join with ntab_s2
  dx_dict <- ntab_s2
  
  pt_data_w_dxdict <- dplyr::inner_join(cp_indata, dx_dict, by = "dx")
  
  #get maximum score with 9
  pt_data_w_dxdict_w9 <- dplyr::filter(pt_data_w_dxdict, severity == 9)
  w_9 <- dplyr::summarise(dplyr::group_by(pt_data_w_dxdict_w9, usubjid, issbr), 
                   max_w_9 = max(severity))
  
  #get maximum score without 9
  pt_data_w_dxdict_wo9 <- dplyr::filter(pt_data_w_dxdict, severity != 9)
  wo_9 <- dplyr::summarise(dplyr::group_by(pt_data_w_dxdict_wo9, usubjid, issbr), 
                   max_wo_9 = max(severity))
  
  #join together
  dplyr::full_join(wo_9, w_9, by = c("usubjid", "issbr"))
}

print((injury_score(pt, patient_id, dx)))
