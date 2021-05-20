options(stringsAsFactors = FALSE)

fetch_attributes <- function(basic_att, 
                             basic_details, 
                             clinical_attributes,
                             clinical_details,
                             Haematologic_details,
                             root_files_path){
  
  all_files <- list.files(root_files_path)
  
  for (x in 1:length(all_files)){
    
    file_data <- read.csv(paste( c(root_files_path, "\\", all_files[x]), collapse=""), sep = "\n", header=F)
    
    one_row <- paste("~",all_files[x], sep="")
    for (a in 2:length(basic_att)){
      pos <- grep(named_att[a], file_data$V1)
      
      if (pos > 0) {
        one_row <- c(one_row,file_data$V1[ pos + 1])
      }else{
        one_row <- c(one_row, NA)}
    }
    
    basic_details <- rbind(basic_details, one_row)
    rm(one_row)
    
    clinical_row <- NULL
    Haematologic_row <- NULL
    
    for (a in 1:length(clinical_attributes)){
      #print(clinical_attributes[a])
      
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
    
    clinical_details <- rbind(clinical_details, clinical_row)
    rm(clinical_row)
    
    Haematologic_details <- rbind(Haematologic_details, Haematologic_row)
    rm(Haematologic_row)
  }
  
  result_file <- cbind(basic_details, clinical_details, Haematologic_details)
  return(result_file)
  
}