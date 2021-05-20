setwd("D:\\Personal\\PriAnalysis")
options(stringsAsFactors = FALSE)

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

named_att <-  c("CR No",
                "Name",
                "Age",
                "Sex",
                "Admission Date/Time")

one <- read.csv("samples\\one", sep = "\n", header=F)
#one <- read.csv("samples\\two", sep = "\n", header=F)
two <- read.csv("samples\\two", sep = "\n", header=F)


clinical_attributes <- read.table("attri.txt", sep="\n")
clinical_attributes <- clinical_attributes$V1


a = 1
one_pos <- grep(clinical_attributes[a], one$V1)    
one$V1[one_pos]

two_pos <- grep(clinical_attributes[a], two$V1)    
two$V1[two_pos]

total_elements <- trim(unlist(strsplit(one$V1[one_pos[1]+1], "\t")))

total_elements <- total_elements[2: length(total_elements)]
total_elements <- total_elements[total_elements!=""]
total_elements_count <-  length(total_elements)+ 1

dts <- paste(one$V1[(one_pos[1] - total_elements_count) : (one_pos[1]-1)], 
             collapse =" ")
dts <- trim(unlist(strsplit(dts, "\t")))
dts <- dts[dts!=""]
dts <- paste(dts, collapse ="|")


print( paste(total_elements, collapse ="|"))
print(dts)


################
cr_no <- two$V1[pos + 1]

dummy <- data.frame(matrix(ncol=length(named_att), nrow=2))
colnames(dummy) <- named_att

clinical_col = gsub("\\s+", "_", clinical_attributes)
dt_cols = paste(gsub("\\s+", "_", clinical_attributes), "date", sep="_")
col_names = NULL
for (y in 1:length(dt_cols)) { col_names <- c(col_names, clinical_col[y], dt_cols[y]) }
Haematologic_test_cols = c("HB",	"RBC",	"WBC",	"PLT",	"PCV",	"MCV",	"RDW-cv","LY%","Mixed%","GR%", "Haematologic_test_date")

#col_names = c(col_names[1:14],Haematologic_test_cols)
col_names = col_names[1:16]  

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
      values_n_dates = ""
      clinical_row = c(clinical_row, "", "")
      
    }else if(clinical_attributes[a] == "Sodium"){
      
      pos <- grep(clinical_attributes[a], file_data$V1)    
      matched <- file_data$V1[pos]
      values_n_dates = get_vals_dts(file_data$V1, pos)
      clinical_row = c(clinical_row, values_n_dates[1], values_n_dates[2])
      
    }else if(clinical_attributes[a] == "Potassium"){
      pos <- grep(clinical_attributes[a], file_data$V1)    
      matched <- file_data$V1[pos]
      values_n_dates = get_vals_dts(file_data$V1, pos)
      clinical_row = c(clinical_row, values_n_dates[1], values_n_dates[2])
      
    }else if(clinical_attributes[a] == "Serum L D H"){
      pos <- grep(clinical_attributes[a], file_data$V1)    
      matched <- file_data$V1[pos]
      values_n_dates = get_vals_dts(file_data$V1, pos)
      clinical_row = c(clinical_row, values_n_dates[1], values_n_dates[2])
      
    }else if(clinical_attributes[a] == "Lactate"){
      pos <- grep(clinical_attributes[a], file_data$V1)    
      matched <- file_data$V1[pos]
      values_n_dates = get_vals_dts(file_data$V1, pos)
      clinical_row = c(clinical_row, values_n_dates[1], values_n_dates[2])
      
    }else if(clinical_attributes[a] == "A L T"){
      values_n_dates = get_alt(file_data$V1, clinical_attributes[a])
      clinical_row = c(clinical_row, values_n_dates[1], values_n_dates[2])
    }else if(clinical_attributes[a] == "Troponin T"){
      pos <- grep(clinical_attributes[a], file_data$V1)    
      matched <- file_data$V1[pos]
      values_n_dates = get_vals_dts(file_data$V1, pos)
      clinical_row = c(clinical_row, values_n_dates[1], values_n_dates[2])
      
    }else if(clinical_attributes[a] == "Haematologic test results"){
      values_n_dates = get_HaematologicTestResults(file_data$V1, clinical_attributes[a])
      Haematologic_row = c(Haematologic_row, values_n_dates)
    }else {
      Haematologic_row = c(Haematologic_row, rep("", length(Haematologic_test_cols))) 
      clinical_row = c(clinical_row, "", "")
    }
  }
  
  clinical_dummy <- rbind(clinical_dummy, clinical_row)
  rm(clinical_row)
  
  Haematologic_dummy <- rbind(Haematologic_dummy, Haematologic_row)
  rm(Haematologic_row)
}

View(dummy)
View(clinical_dummy)
View(Haematologic_dummy)
View(cbind(dummy, clinical_dummy, Haematologic_dummy))

write.csv(cbind(dummy, clinical_dummy, Haematologic_dummy), file="res_file1.csv", row.names = FALSE)
