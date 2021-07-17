print(getwd())

#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
print(args)

if (length(args)==0) {
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
} else if (length(args)==1) {
  # default output file
  #args[2] = "out.txt"
  print(length(args))
  wd_path = args[1]
}else{
  stop("Pass working directpry path")
}


#wd_path <- "C:\\ML\\PreetamWork\\imsParser"
setwd(wd_path)
options(stringsAsFactors = FALSE)

source("code\\base_functions.R")
source("code\\design_main_call.R")

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
                         root_files_path = "all_files")

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
write.csv(res_df, file="output\\res_file9.csv", row.names = FALSE, na="")





