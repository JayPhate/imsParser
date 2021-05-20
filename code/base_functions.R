options(stringsAsFactors = FALSE)

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

get_vals_dts <- function(records, position){
  
  if(length(position)>1){
    print(records[position])
  }
  
  total_elements <- trim(unlist(strsplit(records[position+1], "\t")))
  
  total_elements <- total_elements[2: length(total_elements)]
  total_elements <- total_elements[total_elements!=""]
  total_elements_count <-  length(total_elements)+ 1
  
  dts <- paste(records[(position - total_elements_count) : (position-1)], 
               collapse =" ")
  dts <- trim(unlist(strsplit(dts, "\t")))
  dts <- dts[dts!=""]
  dts <- paste(dts, collapse ="|")
  
  vals_n_dts = c( paste(total_elements, collapse ="|"), dts)
  return(vals_n_dts)
}

get_alt <- function(records, pattern){
  
  idxs <- grep(pattern, records)    
  matched_part <- records[idxs]
  
  val_n_dt <- ""
  if (length(matched_part) == 1){
    val_n_dt <- get_vals_dts(records, idxs)
  }else if(length(matched_part) > 1){
    # select part which has char len < 4
    selcted_idx = idxs[trim(nchar(matched_part)) < 6 ]
    if (length(selcted_idx) == 1){
      val_n_dt <- get_vals_dts(records, selcted_idx)
    }
  }
  return(val_n_dt)
}

get_creatinine <- function(records, pattern){
  idxs <- grep(pattern, records)    
  matched_part <- trim(records[idxs])
  
  #check two patterns
  part1 = matched_part[matched_part=="Renal Panel\tUrea \tCreatinine"]
  part2 = matched_part[matched_part=="Creatinine*"]
  
  
  val_n_dt <- c("","")
  
  if (length(part1) == 1){
    
    part1_idx = idxs[matched_part=="Renal Panel\tUrea \tCreatinine"]
    counter = 1
    res = NULL
    while(!grepl("^[a-zA-Z].*", records[part1_idx + counter])) {
      res <- c(res, records[part1_idx + counter])
      counter = counter + 1
    }
    
    dts <- paste(trim(sapply(strsplit(res, "\t"),'[', 1)), sep = "|", collapse = "|")
    vals <- paste(trim(sapply(strsplit(res, "\t"),'[', 3)), sep = "|", collapse = "|")
    val_n_dt <- c(vals, dts)
    
  }
  if(length(part2) == 1){
    part2_idx = idxs[matched_part=="Creatinine*"]
    v_n_d <- get_vals_dts(records, part2_idx)
    
    if(val_n_dt[1] != ""){
      val_n_dt[1] = paste(val_n_dt[1], v_n_d[1], sep = "|", collapse = "|")
      val_n_dt[2] = paste(val_n_dt[2], v_n_d[2], sep = "|", collapse = "|")
      
    }else{val_n_dt = v_n_d}
  }
  return(val_n_dt)
}

get_HaematologicTestResults <- function(records, pattern){
  idxs <- grep(pattern, records)    
  matched_part <- trim(records[idxs])
  
  val_n_dt = ""
  
  if(length(idxs)==1){
    counter = 2
    res = NULL
    while(!grepl("^[a-zA-Z].*", records[idxs + counter])) {
      res <- c(res, records[idxs + counter])
      counter = counter + 1
    }
    
    dts <- paste(trim(sapply(strsplit(res, "\t"),'[', 1)), sep = "|", collapse = "|")
    HB <- paste(trim(sapply(strsplit(res, "\t"),'[', 2)), sep = "|", collapse = "|")
    RBC <- paste(trim(sapply(strsplit(res, "\t"),'[', 3)), sep = "|", collapse = "|")
    WBC <- paste(trim(sapply(strsplit(res, "\t"),'[', 4)), sep = "|", collapse = "|")
    PLT <- paste(trim(sapply(strsplit(res, "\t"),'[', 5)), sep = "|", collapse = "|")
    PCV <- paste(trim(sapply(strsplit(res, "\t"),'[', 6)), sep = "|", collapse = "|")
    MCV <- paste(trim(sapply(strsplit(res, "\t"),'[', 7)), sep = "|", collapse = "|")
    RDW_cv <- paste(trim(sapply(strsplit(res, "\t"),'[', 8)), sep = "|", collapse = "|")
    LY <- paste(trim(sapply(strsplit(res, "\t"),'[', 9)), sep = "|", collapse = "|")
    Mixed <- paste(trim(sapply(strsplit(res, "\t"),'[', 10)), sep = "|", collapse = "|")
    GR <- paste(trim(sapply(strsplit(res, "\t"),'[', 11)), sep = "|", collapse = "|")
    
    vals <- c(HB, RBC, WBC, PLT, PCV, MCV, RDW_cv, LY, Mixed, GR)
    val_n_dt <- c(vals, dts)
  }
  
  return(val_n_dt)
}

get_urea <- function(records, pattern){
  
  idxs <- grep(pattern, records)    
  matched_part <- trim(records[idxs])
  #check two patterns
  part1 = matched_part[matched_part=="Renal Panel\tUrea \tCreatinine"]
  part2 = matched_part[matched_part=="Urea*"]
  
  val_n_dt <- c("","")
  if (length(part1) == 1){
    part1_idx = idxs[matched_part=="Renal Panel\tUrea \tCreatinine"]
    counter = 1
    res = NULL
    while(!grepl("^[a-zA-Z].*", records[part1_idx + counter])) {
      res <- c(res, records[part1_idx + counter])
      counter = counter + 1
    }
    dts <- paste(trim(sapply(strsplit(res, "\t"),'[', 1)), sep = "|", collapse = "|")
    vals <- paste(trim(sapply(strsplit(res, "\t"),'[', 2)), sep = "|", collapse = "|")
    val_n_dt <- c(vals, dts)
  }
  
  if(length(part2) == 1){
    part2_idx = idxs[matched_part=="Urea*"]
    v_n_d <- get_vals_dts(records, part2_idx)
    
    if(val_n_dt[1] != ""){
      val_n_dt[1] = paste(val_n_dt[1], v_n_d[1], sep = "|", collapse = "|")
      val_n_dt[2] = paste(val_n_dt[2], v_n_d[2], sep = "|", collapse = "|")
      
    }else{val_n_dt = v_n_d}
  }
  return(val_n_dt)
}

get_X_ray_report <- function(records, pattern){
  
  #records = file_data$V1 
  #pattern = clinical_attributes[a]
  
  pos <- grep(pattern, records)
  last_elment = length(records)
  
  impressions = ""
  if (length(pos) > 0 ){
    for(p in 1:length(pos)){
      
      if( nchar(records[pos[p]] ) <18 ){
        
        pat = "INTERPRETATION|IMPRESSION|Interpretation|Impression" 
        pointer_pos = grep(pat, records[pos[p]:last_elment])
        
        str_to_match = ""
        if(length(pointer_pos) >= 1 ){
          check = TRUE
          start = pos[p] + pointer_pos[1] - 1
          while (check) {
            m = grep("Reported By", records[start])
            if(length(m) >= 1 ){
              check = FALSE
            }
            str_to_match = paste(str_to_match, records[start], sep=" ", collapse = "")
            start = start + 1
          }
        }
        
        if(str_to_match != ""){
          #pat_to_grab <- paste( "(",pat,")\\s*(.*?)\\s*Reported By", collapse = "",sep="")
          #pat_to_grab <- paste( "(",pat,")(.*?)(Reported By)", collapse = "",sep="")
          #result <- regmatches(str_to_match, regexec(pat_to_grab, str_to_match))
          impressions = c(impressions , str_to_match)
        } 
      }
    }  
  }
  
  return(impressions)
  
}

generate_clinical_row  <- function(cln_pat, records){
  pos <- grep(cln_pat, records)    
  cln_row = c("", "")
  if(length(pos) > 0){
    matched <- records[pos]
    values_n_dates = get_vals_dts(records, pos)
    cln_row = c(values_n_dates[1], values_n_dates[2])
  }
  return(cln_row)
}

get_Liver_Panel_AST_n_ALT <- function(records, pattern){
  
  idxs <- grep(pattern, records)
  act_pat <- trim("Liver Panel\tProtein \tAlbumin \tGlobulin \tBilirubin(T) \tBilirubin(C) \tA L T \tA S T \tAlk phosphatase ")
  matched_part <- trim(records[idxs])
  part1 = matched_part[matched_part==act_pat]
  part1_idx = idxs[matched_part==act_pat]
  res <- c("", "", "", "")
  if (length(part1) > 0){
    counter = 1
    r = NULL
    while(!grepl("^[a-zA-Z].*", records[idxs + counter])) {
      r <- c(r, records[idxs + counter])
      counter = counter + 1
    }
    
    dts = paste(trim(sapply(strsplit(r, "\t"), '[', 1)), sep = "|", collapse = "|")
    alt_vals <- paste(trim(sapply(strsplit(r, "\t"), '[', 7)), sep = "|", collapse = "|")
    ast_vals <- paste(trim(sapply(strsplit(r, "\t"), '[', 8)), sep = "|", collapse = "|")
    res <- c(alt_vals, dts, ast_vals, dts)
  }
   return(res) 
  }
  
get_HS_Crp <- function(records, pattern){
  idxs <- grep(pattern, records)
  act_pat <- trim("Hs-Crp\tC R P \tHS C R P ")
  matched_part <- trim(records[idxs])
  part1 = matched_part[matched_part==act_pat]
  part1_idx = idxs[matched_part==act_pat]
  res <- c("", "", "", "")
  if (length(part1) > 0){
    counter = 1
    r = NULL
    while(!grepl("^[a-zA-Z].*", records[idxs + counter])) {
      r <- c(r, records[idxs + counter])
      counter = counter + 1
    }
    
    dts = paste(trim(sapply(strsplit(r, "\t"), '[', 1)), sep = "|", collapse = "|")
    crp_vals <- paste(trim(sapply(strsplit(r, "\t"), '[', 2)), sep = "|", collapse = "|")
    hs_crp_vals <- paste(trim(sapply(strsplit(r, "\t"), '[', 3)), sep = "|", collapse = "|")
    res <- c(crp_vals, dts, hs_crp_vals, dts)
  }
  return(res) 

}


get_BloodGasAnalysis <- function(records, pattern){
  
  idxs <- grep(pattern, records)
  act_pat <- trim("A B G (Blood Gas Analysis)\tpH \tpCO2 \tHCO3 \tNa \tK \tCl \tCa \tAG \tPaO2 \tSaO2 \tCaO2 \tAAO2 \tBEB ")
  matched_part <- trim(records[idxs])
  part1 = matched_part[matched_part==act_pat]
  part1_idx = idxs[matched_part==act_pat]
  res <- c("", "", "", "", "")
  if (length(part1) > 0){
    counter = 1
    r = NULL
    while(!grepl("^[a-zA-Z].*", records[idxs + counter])) {
      r <- c(r, records[idxs + counter])
      counter = counter + 1
    }
    dts = paste(trim(sapply(strsplit(r, "\t"), '[', 1)), sep = "|", collapse = "|")
    pH_vals <- paste(trim(sapply(strsplit(r, "\t"), '[', 2)), sep = "|", collapse = "|")
    pCO2_vals <- paste(trim(sapply(strsplit(r, "\t"), '[', 3)), sep = "|", collapse = "|")
    HCO3_vals <- paste(trim(sapply(strsplit(r, "\t"), '[', 3)), sep = "|", collapse = "|")
    Na_vals <- paste(trim(sapply(strsplit(r, "\t"), '[', 3)), sep = "|", collapse = "|")
    K_vals <- paste(trim(sapply(strsplit(r, "\t"), '[', 3)), sep = "|", collapse = "|")
    Cl_vals <- paste(trim(sapply(strsplit(r, "\t"), '[', 3)), sep = "|", collapse = "|")
    Ca_vals <- paste(trim(sapply(strsplit(r, "\t"), '[', 3)), sep = "|", collapse = "|")
    AG_vals <- paste(trim(sapply(strsplit(r, "\t"), '[', 3)), sep = "|", collapse = "|")
    PaO2_vals <- paste(trim(sapply(strsplit(r, "\t"), '[', 3)), sep = "|", collapse = "|")
    SaO2_vals <- paste(trim(sapply(strsplit(r, "\t"), '[', 3)), sep = "|", collapse = "|")
    CaO2_vals <- paste(trim(sapply(strsplit(r, "\t"), '[', 3)), sep = "|", collapse = "|")
    AAO2_vals <- paste(trim(sapply(strsplit(r, "\t"), '[', 3)), sep = "|", collapse = "|")
    BEB_vals <- paste(trim(sapply(strsplit(r, "\t"), '[', 3)), sep = "|", collapse = "|")  
    
    res <- c(pH_vals, pCO2_vals, PaO2_vals, SaO2_vals, HCO3_vals, AG_vals )
  }
  return(res) 
}


