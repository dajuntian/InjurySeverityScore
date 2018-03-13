#' Calculate injury severity score from ICD-9
#' 
#' @param indata A data frame
#' @param id_var A variable for patient id
#' @param dx_var A character varaible for dx code
#' @param has_dot A logical varaible indicates whether ICD code has dot
#' @param tall A logical variable incates data is tall (T) or wide (F)
#' @export
#' @return A data frame contains iss score
#' @examples
#' pat_id <- c(2,2,2,2,2,1,2,1,2,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1)
#' icd9 <- c('874.2', '874.8', '900.81', '900.82', '900.89', '805.06', 
#'           'E966', '805.07', 'V14.0', '807.02', 'V70.4', '821.01', '823.20', 
#'           '860.0', '861.01', '861.21', '861.22', '863.84', '864.04', '865.04', 
#'           '865.09', '866.02', '868.04', '958.4')
#' sample_data <- data.frame(subj = pat_id, code = icd9, stringsAsFactors = FALSE)
#' injury_score(sample_data, subj, code)
#' 
#' data2 <- data.frame(pid = c(1,2), diag1 = c('900.89', '805.06'),
#'                     diag2 = c('863.84', '865.04'))
#' injury_score(data2, pid, diag, tall = FALSE)
## quiets concerns of R CMD check re: the .'s that appear in pipelines
#' @seealso \url{https://github.com/dajuntian/InjurySeverityScore/blob/master/README.md}
injury_score <- function(indata, id_var, dx_var, has_dot = TRUE, tall = TRUE){
  # make local variables
  dx <- NULL
  severity <- NULL
  usubjid <- NULL
  issbr <- NULL
  score_seq <- score <- NULL
  subj <- code <- NULL
  # create a new copy of data, get variable name as string
  idVar <- deparse(substitute(id_var))
  dxVar <- deparse(substitute(dx_var)) 
  if (tall){cp_indata <- dplyr::select(indata, c(idVar, dxVar))}
  else {
    cp_indata <- as.data.frame(.helper.wide2tall(indata, idVar, dxVar))
    }
  
  #rename variable
  names(cp_indata) <- c("usubjid", "dx")
  if (has_dot){dx_dict <- ntab_s2}
  else {dx_dict <- ntab_s1}
  

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
                                   usubjid, dplyr::desc(score)),
                    dplyr::row_number(usubjid) <= 3), 
      score_seq = paste0("max_", dplyr::row_number(usubjid)))
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
  iss_result <- dplyr::left_join(dplyr::distinct(cp_indata["usubjid"]),
                   iss_score, by = "usubjid")
  names(iss_result)[1] <- idVar
  iss_result
}

.helper.wide2tall <- function(df, id, prefix){
  dx <- NULL
  #define a helper function to transpose data
  df_wide <- dplyr::select(as.data.frame(df), id, dplyr::starts_with(prefix))
  tall <- tidyr::gather(df_wide, "code_seq", "dx", 2:ncol(df_wide))
  #remove NA and code_seq column
  dplyr::ungroup(dplyr::filter(dplyr::select(tall, id, dx), !is.na(dx)))
}



