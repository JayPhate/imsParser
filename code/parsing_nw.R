setwd("D:\\Personal\\PriAnalysis")
options(stringsAsFactors = FALSE)

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

named_att <-  c("CR No",
                "Name",
                "Age",
                "Sex",
                "Admission Date/Time")

one <- read.csv("samples\\one", sep = "\n", header=F)
two <- read.csv("samples\\two", sep = "\n", header=F)

get_vals_dts <- function(records, position){
  
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


clinical_attributes <- read.table("attri.txt", sep="\n")
clinical_attributes <- clinical_attributes$V1
#clinical_attributes <- c(clinical_attributes, "Urea")


dummy <- data.frame(matrix(ncol=length(named_att), nrow=2))
colnames(dummy) <- named_att

clinical_col = gsub("\\s+", "_", clinical_attributes)
dt_cols = paste(gsub("\\s+", "_", clinical_attributes), "date", sep="_")
col_names = NULL
for (y in 1:length(dt_cols)) { col_names <- c(col_names, clinical_col[y], dt_cols[y]) }
Haematologic_test_cols = c("HB",	"RBC",	"WBC",	"PLT",	"PCV",	"MCV",	"RDW-cv","LY%","Mixed%","GR%", "Haematologic_test_date")

#col_names = c(col_names[1:14],Haematologic_test_cols)
col_names = col_names[1:18]  

clinical_dummy <- data.frame(matrix(ncol=length(col_names), nrow=2))
colnames(clinical_dummy) <- col_names

Haematologic_test_cols = c("HB",	"RBC",	"WBC",	"PLT",	"PCV",	"MCV",	"RDW-cv","LY%","Mixed%","GR%", "Haematologic_test_date")
Haematologic_dummy <- data.frame(matrix(ncol=length(Haematologic_test_cols), nrow=2))
colnames(Haematologic_dummy) <- Haematologic_test_cols


all_files <- list.files("samples")

for (x in 1:length(all_files)){
  
  file_data <- read.csv(paste( c("samples\\", all_files[x]), collapse=""), sep = "\n", header=F)
  
  one_row <- NULL
  for (a in 1:length(named_att)){
    pos <- grep(named_att[a], file_data$V1)
    
    if (pos > 0) {
      one_row <- c(one_row, file_data$V1[ pos + 1])
    }else{
      one_row <- c(one_row, NA)}
  }
  
  dummy <- rbind(dummy, one_row)
  rm(one_row)
  
  clinical_row <- NULL
  Haematologic_row <- NULL
  
  for (a in 1:length(clinical_attributes)){
    print(clinical_attributes[a])
    
    if (clinical_attributes[a] == "Urea"){
      values_n_dates = get_urea(file_data$V1, clinical_attributes[a])
      clinical_row = c(clinical_row, values_n_dates[1], values_n_dates[2])

    }else if(clinical_attributes[a] == "Creatinine"){
      values_n_dates = get_creatinine(file_data$V1, clinical_attributes[a])
      clinical_row = c(clinical_row, values_n_dates[1], values_n_dates[2])
      
    }else if(clinical_attributes[a] == "Sodium"){
      
      pos <- grep(clinical_attributes[a], file_data$V1)
      if (length(pos)>0){
        matched <- file_data$V1[pos]
        values_n_dates = get_vals_dts(file_data$V1, pos)
        clinical_row = c(clinical_row, values_n_dates[1], values_n_dates[2])  
      }else{clinical_row = c(clinical_row, "", "")}
      
      
    }else if(clinical_attributes[a] == "Potassium"){
      pos <- grep(clinical_attributes[a], file_data$V1)    
      if(length(pos) > 0 ){
        matched <- file_data$V1[pos]
        values_n_dates = get_vals_dts(file_data$V1, pos)
        clinical_row = c(clinical_row, values_n_dates[1], values_n_dates[2])  
      }else{clinical_row = c(clinical_row, "", "")}
      
      
    }else if(clinical_attributes[a] == "Serum L D H"){
      pos <- grep(clinical_attributes[a], file_data$V1)    
      
      if(length(pos) > 0){
        matched <- file_data$V1[pos]  
        values_n_dates = get_vals_dts(file_data$V1, pos)
        clinical_row = c(clinical_row, values_n_dates[1], values_n_dates[2])
      }else{clinical_row = c(clinical_row, "", "")}
      
    }else if(clinical_attributes[a] == "Lactate"){
      pos <- grep(clinical_attributes[a], file_data$V1)    
      if(length(pos) > 0){
        matched <- file_data$V1[pos]
        values_n_dates = get_vals_dts(file_data$V1, pos)
        clinical_row = c(clinical_row, values_n_dates[1], values_n_dates[2])
      }else{clinical_row = c(clinical_row, "", "")}
      
    }else if(clinical_attributes[a] == "A L T"){
      values_n_dates = get_alt(file_data$V1, clinical_attributes[a])
      clinical_row = c(clinical_row, values_n_dates[1], values_n_dates[2])
    }else if(clinical_attributes[a] == "Troponin T"){
      pos <- grep(clinical_attributes[a], file_data$V1)    
      
      if(length(pos) > 0){
        matched <- file_data$V1[pos]
        values_n_dates = get_vals_dts(file_data$V1, pos)
        clinical_row = c(clinical_row, values_n_dates[1], values_n_dates[2])
      }else{clinical_row = c(clinical_row, "", "")}
      
    }else if(clinical_attributes[a] == "Haematologic test results"){
      values_n_dates = get_HaematologicTestResults(file_data$V1, clinical_attributes[a])
      Haematologic_row = c(Haematologic_row, values_n_dates)
      
    }else if(clinical_attributes[a] == "X-Ray Chest"){
      x_rays = get_X_ray_report(file_data$V1, clinical_attributes[a])
      x_rays = paste(x_rays, collapse = "|")
      clinical_row = c(clinical_row, x_rays, "")
    }
    else {
      Haematologic_row = c(Haematologic_row, rep("", length(Haematologic_test_cols))) 
      clinical_row = c(clinical_row, "", "")
    }
  }
  
  clinical_dummy <- rbind(clinical_dummy, clinical_row)
  rm(clinical_row)
  
  Haematologic_dummy <- rbind(Haematologic_dummy, Haematologic_row)
  rm(Haematologic_row)
}

# Creatinine
View(cbind(dummy, clinical_dummy, Haematologic_dummy))
write.csv(cbind(dummy, clinical_dummy, Haematologic_dummy), file="res_file2.csv", row.names = FALSE)

x <- cbind(dummy, clinical_dummy, Haematologic_dummy)

cols = grep("date", colnames(x), invert = TRUE, value = TRUE)
View(x[,cols])


