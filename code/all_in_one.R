options(stringsAsFactors = FALSE)

get_HaematologicTestResults <- function(records, pattern){
  idxs <- grep(pattern, records)    
  matched_part <- trim(records[idxs])
  
  val_n_dt = ""
  
  if(length(idxs)==1){
    counter = 2
    res = NULL
    while( (!grepl("^[a-zA-Z].*", records[idxs + counter])) && (idxs + counter) <= length(records)) {
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

fetch_attributes <- function(basic_att, 
                             basic_details, 
                             clinical_fields,
                             clinical_details,
                             Haematologic_details,
                             root_files_path){
  
  all_files <- list.files(root_files_path)
  
  for (x in 1:length(all_files)){
    cat("#### File No:", x, "####\n")
    
    file_data <- read.csv(paste( c(root_files_path, "\\", all_files[x]), collapse=""), sep = "\n", header=F)
    #print(all_files[x])
    one_row <- paste("~",all_files[x], sep="")
    for (a in 2:length(basic_att)){
      pos <- grep(named_att[a], file_data$V1)
      
      if (length(pos) > 0) {
        one_row <- c(one_row,file_data$V1[ pos + 1])
      }else{
        one_row <- c(one_row, NA)}
    }
    
    basic_details <- rbind(basic_details, one_row)
    rm(one_row)
    
    clinical_row <- NULL
    Haematologic_row <- NULL
    
    for (a in 1:length(clinical_fields)){
      print(clinical_fields[a])
      
      if (clinical_fields[a] == "Urea"){
        values_n_dates = get_urea(file_data$V1, clinical_fields[a])
        clinical_row = c(clinical_row, values_n_dates[1], values_n_dates[2])
        
      }else if(clinical_fields[a] == "Creatinine"){
        values_n_dates = get_creatinine(file_data$V1, clinical_fields[a])
        clinical_row = c(clinical_row, values_n_dates[1], values_n_dates[2])
        
      }else if(clinical_fields[a] == "Sodium"){
        
        pos <- grep(clinical_fields[a], file_data$V1)
        if (length(pos)>0){
          matched <- file_data$V1[pos]
          values_n_dates = get_vals_dts(file_data$V1, pos)
          clinical_row = c(clinical_row, values_n_dates[1], values_n_dates[2])  
        }else{clinical_row = c(clinical_row, "", "")}
        
        
      }else if(clinical_fields[a] == "Potassium"){
        pos <- grep(clinical_fields[a], file_data$V1)    
        if(length(pos) > 0 ){
          matched <- file_data$V1[pos]
          values_n_dates = get_vals_dts(file_data$V1, pos)
          clinical_row = c(clinical_row, values_n_dates[1], values_n_dates[2])  
          
        }else{clinical_row = c(clinical_row, "", "")}
        
        
      }else if(clinical_fields[a] == "Serum L D H"){
        pos <- grep(clinical_fields[a], file_data$V1)    
        
        if(length(pos) > 0){
          matched <- file_data$V1[pos]  
          values_n_dates = get_vals_dts(file_data$V1, pos)
          clinical_row = c(clinical_row, values_n_dates[1], values_n_dates[2])
        }else{clinical_row = c(clinical_row, "", "")}
        
      }else if(clinical_fields[a] == "Lactate"){
        pos <- grep(clinical_fields[a], file_data$V1)    
        if(length(pos) > 0){
          matched <- file_data$V1[pos]
          values_n_dates = get_vals_dts(file_data$V1, pos)
          clinical_row = c(clinical_row, values_n_dates[1], values_n_dates[2])
        }else{clinical_row = c(clinical_row, "", "")}
        
      }else if(clinical_fields[a] == "A L T"){
        #values_n_dates = get_alt(file_data$V1, clinical_fields[a])
        #clinical_row = c(clinical_row, values_n_dates[1], values_n_dates[2])
        pos <- grep(clinical_fields[a], file_data$V1)    
        if(length(pos) > 0 ){
          #matched <- file_data$V1[pos]
          values_n_dates = get_vals_dts(file_data$V1, pos[1])
          clinical_row = c(clinical_row, values_n_dates[1], values_n_dates[2])  
          
        }else{clinical_row = c(clinical_row, "", "")}
        
        
      }else if(clinical_fields[a] == "Troponin T"){
        pos <- grep(clinical_fields[a], file_data$V1)    
        
        if(length(pos) > 0){
          matched <- file_data$V1[pos]
          values_n_dates = get_vals_dts(file_data$V1, pos)
          clinical_row = c(clinical_row, values_n_dates[1], values_n_dates[2])
        }else{clinical_row = c(clinical_row, "", "")}
        
      }else if(clinical_fields[a] == "Glucose Random"){
        
        r = generate_clinical_row(clinical_fields[a], file_data$V1)
        clinical_row = c(clinical_row, r)
        
      }else if (clinical_fields[a]=="Ferritin"){
        
        r = generate_clinical_row(clinical_fields[a], file_data$V1)
        clinical_row = c(clinical_row, r)
        
      }else if(clinical_fields[a]=="D-Dimer"){
        
        r = generate_clinical_row(clinical_fields[a], file_data$V1)
        clinical_row = c(clinical_row, r)
        
      }else if(clinical_fields[a]=="P C T"){
        
        r = generate_clinical_row(clinical_fields[a], file_data$V1)
        clinical_row = c(clinical_row, r)
        
      } else if(clinical_fields[a] == "Liver Panel"){
        #c(alt_vals, dts, ast_vals, dts)
        values_n_dates = get_Liver_Panel_AST_n_ALT(file_data$V1, clinical_fields[a])
        clinical_row = c(clinical_row, values_n_dates[1], values_n_dates[2], values_n_dates[3], values_n_dates[4])
        
      }else if(clinical_fields[a] == "Hs-Crp"){
        # c(crp_vals, dts, hs_crp_vals, dts)
        values_n_dates = get_HS_Crp(file_data$V1, clinical_fields[a])
        clinical_row = c(clinical_row, values_n_dates[1], values_n_dates[2], values_n_dates[3], values_n_dates[4])
        
      }else if(clinical_fields[a] == "A B G (Blood Gas Analysis)"){
        # c(pH_vals, pCO2_vals, PaO2_vals, SaO2_vals, HCO3_vals, AG_vals )
        values_n_dates = get_BloodGasAnalysis(file_data$V1, "A B G \\(Blood Gas Analysis\\)")
        clinical_row = c(clinical_row, values_n_dates[1], values_n_dates[2],
                         values_n_dates[3], values_n_dates[4],
                         values_n_dates[5], values_n_dates[6])
        
      }else if(clinical_fields[a] == "Haematologic test results"){
        values_n_dates = get_HaematologicTestResults(file_data$V1, clinical_fields[a])
        Haematologic_row = c(Haematologic_row, values_n_dates)
        
      }else if(clinical_fields[a] == "X-Ray Chest"){
        x_rays = get_X_ray_report(file_data$V1, clinical_fields[a])
        x_rays = paste(x_rays, collapse = "|")
        clinical_row = c(clinical_row, x_rays)
      }
      else {
        Haematologic_row = c(Haematologic_row, rep("", length(Haematologic_test_cols))) 
        clinical_row = c(clinical_row, "", "")
      }
    }
    
    clinical_details <- rbind(clinical_details, clinical_row)
    rm(clinical_row)
    
    Haematologic_details <- rbind(Haematologic_details, Haematologic_row)
    rm(Haematologic_row)
  }
  
  result_file <- cbind(basic_details, clinical_details, Haematologic_details)
  return(result_file)
  
}

###################


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

get_HaematologicTestResults1 <- function(records, pattern){
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
  
  impressions = NULL
  if (length(pos) > 0 ){
    for(p in 1:length(pos)){
      
      if( nchar(records[pos[p]] ) <18 ){
        
        pat = "INTERPRETATION|IMPRESSION|Interpretation|Impression" 
        pointer_pos = grep(pat, records[pos[p]:last_elment])
        
        str_to_match = ""
        if(length(pointer_pos) >= 1 ){
          check = TRUE
          start = pos[p] + pointer_pos[1] - 1
          reported_by_counter = 1
          
          while (check) {
            m = grep("Reported By", records[start])
            if(length(m) >= 1 ){
              check = FALSE
            }
            str_to_match = paste(str_to_match, records[start], sep=" ", collapse = "")
            start = start + 1
            
            
            if (reported_by_counter>=15){
              check = FALSE
              #str_to_match = "X-Ray details not found"
              
            }
            reported_by_counter = reported_by_counter + 1
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


####################

print(getwd())

#!/usr/bin/env Rscript
# args = commandArgs(trailingOnly=TRUE)
# print(args)
# 
# if (length(args)==0) {
#   stop("At least one argument must be supplied (input file).n", call.=FALSE)
# } else if (length(args)==1) {
#   # default output file
#   #args[2] = "out.txt"
#   print(length(args))
#   wd_path = args[1]
# }else{
#   stop("Pass working directpry path")
# }



wd_path <- "C:\\ML\\PreetamWork\\imsParser"
setwd(wd_path)
options(stringsAsFactors = FALSE)

#source("code\\base_functions.R")
#source("code\\design_main_call.R")

clinical_attributes <- read.table("input\\attri2.txt", sep="\n")
clinical_attributes <- clinical_attributes$V1

clinical_attributes <- read.csv("input\\attributeInfo.csv")
colnames(clinical_attributes) <- c("Attributes", "AttributesTotalColumns")

#

named_att <-  c("FileName","CR No",
                "Name",
                "Age",
                "Sex",
                "Admission Date/Time",
                #"Discharge Date/Time",
                "DIAGNOSIS",
                "CASE SUMMARY")

basic_data <- data.frame(matrix(ncol=length(named_att), nrow=2))
colnames(basic_data) <- named_att

#clinical_col = gsub("\\s+", "_", clinical_attributes)
#dt_cols = paste(gsub("\\s+", "_", clinical_attributes), "date", sep="_")

actual_headers = clinical_attributes$AttributesTotalColumns
actual_headers = unlist(strsplit(actual_headers, "\\|"))



clinical_col = gsub("\\s+", "_", actual_headers)
dt_cols = paste(gsub("\\s+", "_", actual_headers), "date", sep="_")
# A B G (Blood Gas Analysis),pH|pCO2|PaO2|SaO2|HCO3|AG

col_names = NULL
for (y in 1:length(dt_cols)) { col_names <- c(col_names, clinical_col[y], dt_cols[y]) }

# Add Blood Gas Analysis columns, which are wt dates
BloodGasAnalysis_cols <- unlist(strsplit("pH|pCO2|PaO2|SaO2|HCO3|AG", "\\|"))

Haematologic_test_cols = c("HB",	"RBC",	"WBC",	"PLT",	"PCV",	"MCV",	"RDW-cv","LY%","Mixed%","GR%", "Haematologic_test_date")

#total_cols = length(col_names)
#col_names = col_names[1:  (total_cols- 2)]

col_names <- c(col_names, BloodGasAnalysis_cols, "xRay")
clinical_data <- data.frame(matrix(ncol=length(col_names), nrow=2))
colnames(clinical_data) <- col_names

Haematologic_test_cols = c("HB",	"RBC",	"WBC",	"PLT",	"PCV",	"MCV",	"RDW-cv","LY%","Mixed%","GR%", "Haematologic_test_date")
Haematologic_data <- data.frame(matrix(ncol=length(Haematologic_test_cols), nrow=2))
colnames(Haematologic_data) <- Haematologic_test_cols

fields_to_process = c(clinical_attributes$Attributes,
                      "X-Ray Chest",
                      "Haematologic test results")
data <- fetch_attributes(basic_att = named_att, 
                         basic_details = basic_data,
                         clinical_fields = fields_to_process,
                         clinical_details = clinical_data,
                         Haematologic_details = Haematologic_data,
                         root_files_path = "samples")

#print(head(data))

col_names <- grep("date", colnames(data), value=TRUE, ignore.case = TRUE, invert = TRUE)
data <- data[, col_names]


res_df <- data[, col_names[1:7]]
remaining_cols <- col_names[-c(1:7)]
for (x in 1: length(remaining_cols)){
  res_df[[remaining_cols[x]]] <- sapply(strsplit(
    data[,remaining_cols[x]],"\\|"), 
    "[",1)
}

res_df <- res_df[-(1:2),]
write.csv(res_df, file="output\\res_file10.csv", row.names = FALSE)



