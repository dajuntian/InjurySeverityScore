
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
  
  #get the top 3 score after change 9 to 0
  
  nineTozero <- dplyr::mutate(iss_br, score = ifelse(score == 9, 0, score))
  nineTozeroMax <- 
      dplyr::mutate(
      dplyr::filter(dplyr::arrange(dplyr::group_by(nineTozero, usubjid), 
                                   usubjid, desc(score)),
                    dplyr::row_number(usubjid) <= 3), 
      score_seq = paste0("max_", row_number(usubjid)))
  #remove old label and spread
  nineTozeroMax$issbr <- NULL
  nineTozeroMaxWide <- tidyr::spread(nineTozeroMax, score_seq, score)
  
  iss_br_w_max <- dplyr::inner_join(iss_br_wide, nineTozeroMaxWide, by = 'usubjid')
  
  #calculattion based on top three score
  #if max(max1-3) = 6 & max(1-6) <= 0 then maxis = 0
  #if  max(max1-3) = 0
  iss_score <- dplyr::mutate(iss_br_w_max, iss = 
                dplyr::case_when(
                  pmax(br_1, br_2, br_3, br_4, br_5, br_6) == 0 ~ 0,
                  pmax(br_1, br_2, br_3, br_4, br_5, br_6) <= 5 ~ sum(c(max_1^2, max_2^2, max_3^2)),
                  pmax(br_1, br_2, br_3, br_4, br_5, br_6) == 6 ~ 75,
                  pmax(max_1, max_2, max_3) == 0 ~ 99,
                  pmax(max_1, max_2, max_3) <= 5 ~ sum(c(max_1^2, max_2^2, max_3^2)),
                  pmax(max_1, max_2, max_3) == 6 ~ 75
                  
                ))
  iss_result <- dplyr::left_join(cp_indata[1],
                   iss_score, by = "usubjid")
  names(iss_result)[1] <- idVar
  iss_result
}

x<-((injury_score(pt, patient_id, dx)))
