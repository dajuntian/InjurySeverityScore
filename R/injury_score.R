
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
  max_w9_wo9 <- dplyr::full_join(wo_9, w_9, by = c("usubjid", "issbr"))
  
  #create cross join for template with 0 for each body region
  iss_template <- merge(dplyr::distinct(cp_indata["usubjid"]), 
                        dplyr::distinct(dx_dict["issbr"]))
  iss_template$severity_default <- 0
  iss_br <- dplyr::full_join(max_w9_wo9, iss_template, by = c("usubjid", "issbr"))
  
  # get max score for each body region using the following order
  # 1-6 - non missing score
  # 9 - has valid trauma code but with unknow score
  # 0 - no valid trauma code for that region
  iss_br <- cbind(iss_br[c("usubjid", "issbr")],
                  score = dplyr::coalesce(iss_br$max_wo_9, 
                                          iss_br$max_w_9, 
                                          iss_br$severity_default))
  # change body region to be variable name
  iss_br$issbr <- paste0("br_", iss_br$issbr)
  
  # transpose data to be wide
  iss_br_wide <- tidyr::spread(iss_br, issbr, score)
}

x<-((injury_score(pt, patient_id, dx)))
