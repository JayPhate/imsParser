# https://statsandr.com/blog/fisher-s-exact-test-in-r-independence-test-for-a-small-sample/
# https://stats.stackexchange.com/questions/81483/warning-in-r-chi-squared-approximation-may-be-incorrect
# https://stackoverflow.com/questions/53055077/chi-square-test-error-chi-squared-approximation-may-be-incorrect

setwd("D:\\Personal\\PriAnalysis")
options(stringsAsFactors = FALSE)
options(digits = 5)  

trim <- function(x) gsub("^\\s+|\\s+$", "", x)

all_data <- read.csv( file.path("refreshedData","Covid_refreshedData3_V2.csv"))

all_data$XrayTags <- trim(all_data$XrayTags)

all_data$DOA <- gsub("~", "", all_data$DOA)   
all_data$Discharge.date <- gsub("~", "", all_data$Discharge.date)   
all_data$Discharge.date[all_data$Discharge.date==""] <- "01-01"


DOA_dayNmonth <- data.frame(do.call('rbind', 
                                    strsplit(as.character(all_data$DOA),'-',fixed=TRUE)))
DischargeDate_dayNmonth <- data.frame(do.call('rbind', 
                                              strsplit(as.character(all_data$Discharge.date),'-',fixed=TRUE)))
colnames(DOA_dayNmonth) <- c("day", "month")
colnames(DischargeDate_dayNmonth) <- c("day", "month")

head(DOA_dayNmonth)
head(DischargeDate_dayNmonth)

dim(DOA_dayNmonth)
dim(DischargeDate_dayNmonth)

head(all_data[, c("DOA", "Discharge.date" ,"nw_DOA", "nw_DischargeDate", "HospitalStay")])

all_data$Age <- as.numeric(all_data$Age)


## Basic Summary table
base_details <- data.frame("FieldName"=c("Total Patients",
                                         "Median Age",
                                         "Died",
                                         "Discharged",
                                         "Median Hospital Stay",
                                         "Median Death Time"),
                           
                           "FieldValue" =c(nrow(all_data),
                                           median(all_data$Age),
                                           sum(all_data$IsDeath),
                                           sum(!all_data$IsDeath),
                                           median(all_data$HospitalStay),
                                           median(all_data$HospitalStay[ all_data$Death !="~" ]) ))


base_details

b = c(1, 13, 35, 55, 95)
all_data$age_groups = cut(all_data$Age, breaks = round(b), include.lowest =TRUE)
head(all_data$age_groups)
table(all_data$age_groups)

all_data$IsDeath <- rep(0, nrow(all_data))
all_data$IsDeath[gsub("~","", all_data$Death)!=""] = 1
table(all_data$IsDeath)

all_data$aGroups <- as.character(all_data$age_groups) 
table(all_data$aGroups)

all_data$HospitalStay_7days = ifelse(all_data$HospitalStay > 7, ">7", "<7")
table(all_data$HospitalStay_7days)

clinicalStatusTag = c(0,3,5,10,15, 100)
all_data$clinicalStatus = cut(all_data$HospitalStay, breaks = round(clinicalStatusTag), include.lowest =TRUE)
table(all_data$clinicalStatus)

##########################################

library(dplyr)
library(reshape2)

table(all_data$Sex, all_data$age_groups)

bySex <- dcast(all_data, Sex ~ age_groups, length, 
               value.var = "age_groups")


# Groups By Religion

all_data$Religion <- tolower(all_data$Religion)
unique(all_data$Religion)

all_data$Religion <- gsub("hindu", "Hindu", all_data$Religion)
all_data$Religion <- gsub("sindhi|sindhiya|juni", "Sindhi", all_data$Religion)
all_data$Religion <- gsub("muslim", "Muslim", all_data$Religion)

all_data$Religion <- gsub("buddhist|buddist", "Buddhist", all_data$Religion)
all_data$Religion <- gsub("sikh", "Sikh", all_data$Religion)
unique(all_data$Religion)
byReligion <- dcast(all_data, Religion ~ age_groups, length, 
                    value.var = "age_groups")
print(byReligion)

# Groups By Hospital Stay
hospitalStayF <- ifelse(all_data$HospitalStay < 3, "<3",
                        ifelse( (all_data$HospitalStay >=3) && 
                                  (all_data$HospitalStay <=7), "3-7",
                                ">7"))
byHospitalStay = dcast(all_data, HospitalStay_7days ~ age_groups, length,
                       value.var = "age_groups")
print(byHospitalStay)

# Groups By Clinical Status
byClinicalStatus = dcast(all_data, clinicalStatus ~ age_groups, length,
                         value.var = "age_groups")
print(byClinicalStatus)

#######################################################

all_data$Urea <- as.numeric(all_data$Urea)
apply(is.na(all_data), 2, sum)

all_data_features <- all_data[, c("Name", "Age","age_groups","IsDeath",  "Religion" ,"Sex", "HospitalStay_7days")]

all_data_features$urea_5_0plus <- ifelse(all_data$Urea > 50, 1, 0)
all_data_features$creatinine_1_p_5plus <- ifelse(all_data$Creatinine > 1.5, 1, 0)

all_data_features$hyponatremia_130less <- ifelse(all_data$Sodium < 130, 1, 0)
all_data_features$hyponatremia_150plus <- ifelse(all_data$Sodium > 150, 1, 0)
all_data_features$hypokalemia_3_p_5_less <- ifelse(all_data$Potassium < 3.5, 1, 0)
all_data_features$hyperkalemia_5_p_5_plus <- ifelse(all_data$Potassium > 5.5, 1, 0)

all_data_features$ldh_3OO_plus <- ifelse(all_data$LDH > 50, 1, 0)
all_data_features$lactate_2_plus <- ifelse(all_data$Lactate > 2, 1, 0) 

all_data_features$D_dimmer_0_p_5_plus <- ifelse(all_data$Ddimer > 0.5, 1, 0) 
all_data_features$Ferrtin_3OO_plus <- ifelse(all_data$Ferritin > 300, 1, 0) 
all_data_features$Trop_T_O_p_OO1_plus <- ifelse(all_data$Trop.T > 0.001, 1, 0)

all_data_features$Anaemia_HB_F_10_less <- ifelse((all_data$Hb < 10) & (all_data$Sex =="F"), 1, 0)
all_data_features$Anaemia_HB_M_12_less <-   ifelse((all_data$Hb < 12) & (all_data$Sex =="M"), 1, 0)

all_data_features$WBCs_4_less <- ifelse(all_data$WBC < 4, 1, 0)
all_data_features$WBCs_11_plus <- ifelse(all_data$WBC > 11, 1, 0)

all_data_features$platelets_1OO_less <- ifelse(all_data$Plt < 100, 1, 0)
all_data_features$PCV_5O_plus <- ifelse(all_data$PCV > 50, 1, 0)

all_data_features$MCV_8O_less <- ifelse(all_data$MCV < 80, 1, 0)
all_data_features$MCV_1O5_plus <- ifelse(all_data$MCV > 105, 1, 0)

all_data_features$RDW_1_4_plus <- ifelse(all_data$RDW > 14, 1, 0)

all_data_features$Lymphocyte_2O_per_less <- ifelse(all_data$L < 20, 1, 0)
all_data_features$Lymphocyte_40_per_plus <- ifelse(all_data$L > 20, 1, 0)

all_data_features$granulocyte_6O_per_plus <- ifelse(all_data$Gr > 60, 1, 0)
all_data_features$ALT_50_plus <- ifelse(all_data$ALT > 50, 1, 0)

all_data_features$XrayTags = all_data$XrayTags 

col_to_process <- colnames(all_data_features)[5:ncol(all_data_features)]  

#################

summary1 <- data.frame()

for (one_col in col_to_process) {
  print(one_col)
  obj <- dcast(all_data_features, eval(parse(text=one_col)) ~ age_groups, length, 
               value.var = "age_groups")
  print(obj)
  obj$header <- one_col
  
  summary1 = rbind(summary1, obj)
  
}

colnames(summary1)
colnames(summary1) <- c("Filter", colnames(summary1)[2:6])

summary1_sub <- summary1[!is.na(summary1$Filter),]
summary1_sub <- summary1_sub[summary1_sub$Filter != "0",]

write.csv(summary1_sub, "refreshedData//6//summary_AgeGroup6.csv", row.names = FALSE)

rm(one_col, obj)


#####################

summary2 <- data.frame()
summary3 <- data.frame()

for (one_col in col_to_process[-c(1:3, 28)]) { # -c(1,2): removing Religion, Sex,HospitalStay_7days, XrayTags 
  print(one_col)
  sub = all_data_features[all_data_features[,one_col] == 1,]
  obj <- dcast(sub, eval(parse(text=one_col)) ~ IsDeath, length, 
               value.var = "age_groups")
  print(obj)
  
  obj_hospitalStay <- dcast(sub, eval(parse(text=one_col)) ~ HospitalStay_7days, length, 
                            value.var = "age_groups")
  
  if( ! "1" %in% colnames(obj)){
    obj$"1" <- 0
  }
  
  if(! "NA" %in% colnames(obj)){
    obj$"NA" <- 0
  }
  
  obj <- obj[, c("eval(parse(text = one_col))", "0", "1" ,"NA")]  
  
  obj$header <- one_col
  
  ################
  if( ! ("<7" %in% colnames(obj_hospitalStay) )){
    obj_hospitalStay$"<7" <- 0
  }
  
  if( ! (">7" %in% colnames( obj_hospitalStay) )){
    obj_hospitalStay$">7" <- 0
  }
  
  if(! "NA" %in% colnames( obj_hospitalStay)){
    obj_hospitalStay$"NA" <- 0
  }
  
  obj_hospitalStay <-  obj_hospitalStay[, c("eval(parse(text = one_col))", "<7", ">7" ,"NA")]  
  
  obj_hospitalStay$header <- one_col
  
  ##############
  summary2 = rbind(summary2, obj)
  summary3 = rbind(summary3, obj_hospitalStay)
  
}


colnames(summary2)
colnames(summary2) <- c("Filter", colnames(summary2)[2:5])

summary2_sub <- summary2[!is.na(summary2$Filter),]
summary2_sub <- summary2_sub[summary2_sub$Filter != "0",]

colnames(summary3)
colnames(summary3) <- c("Filter", colnames(summary3)[2:5])

summary3_sub <- summary3[!is.na(summary3$Filter),]
summary3_sub <- summary3_sub[summary3_sub$Filter != "0",]



########## P-values #########


# one_header <- "urea_5_0plus"
# dat = table(all_data_features$urea_5_0plus, all_data_features$IsDeath)
# test <- fisher.test(dat)
# p_value <- test$p.value
# test_name <- test$method

pVals = NULL
test_Names = NULL

for(p in 1:length(summary2_sub$header)){
  
  one_col <- summary2_sub$header[p]
  dat <- table(all_data_features[,one_col], all_data_features$IsDeath)
  
  if(dim(dat)[1] == 2){
    test <- fisher.test(dat)
    pVals <- c(pVals, test$p.value)
    test_Names <- c(test_Names, test$method)
    rm(test);rm(dat);rm(one_col)
  }else{
    pVals <- c(pVals, NA)
    test_Names <- c(test_Names, NA)
  }
  
}


summary2_sub$p_vals = pVals
summary2_sub$testNames = test_Names
write.csv(summary2_sub, "refreshedData/6/summary_DisNDeath6.csv", row.names = FALSE)

#########################
options(digits = 5) 
pVals3 = NULL
test_Names3 = NULL

for(p in 1:length(summary3_sub$header)){
  
  one_col <- summary3_sub$header[p]
  dat <- table(all_data_features[,one_col], all_data_features$HospitalStay_7days)
  
  if(dim(dat)[1] == 2){
    test <- fisher.test(dat)
    pVals3 <- c(pVals3, test$p.value)
    test_Names3 <- c(test_Names3, test$method)
    rm(test);rm(dat);rm(one_col)
  }else{
    pVals3 <- c(pVals3, NA)
    test_Names3 <- c(test_Names3, NA)
  }
  
}

summary3_sub$p_vals = format(round(pVals3, 3), nsmall = 3) 
summary3_sub$testNames = test_Names3

write.csv(summary3_sub, "refreshedData/6/summary_HospitalStay6.csv", row.names = FALSE)

############

obj_xray <- dcast(all_data_features, XrayTags ~ IsDeath, length, 
                  value.var = "age_groups")



pVals_Xrays = NULL
test_Names_Xrays = NULL
chTest <- c(4,5,6)

for(p in 1:length(obj_xray$XrayTags)){
  
  x = colSums(obj_xray[p,-1])
  y = colSums(obj_xray[-p,-1])
  
  #one_col <- obj_xray$XrayTags[p]
  #one_xray_data = all_data_features[all_data_features$XrayTags == one_col,]
  
  xray_dat <- (rbind(x, y)) #t
  
  if(dim(xray_dat)[1] == 2){
    
    if(sum(p == chTest) > 0){
      test <- chisq.test(xray_dat)
      pVals_Xrays <- c(pVals_Xrays, test$p.value)
      test_Names_Xrays <- c(test_Names_Xrays, test$method)
      rm(test);rm(xray_dat);
    }else{
      test <- fisher.test(xray_dat)
      pVals_Xrays <- c(pVals_Xrays, test$p.value)
      test_Names_Xrays <- c(test_Names_Xrays, test$method)
      rm(test);rm(xray_dat);  
    }
    
  }else{
    pVals_Xrays <- c(pVals_Xrays, NA)
    test_Names_Xrays <- c(test_Names_Xrays, NA)
  }
  
}

obj_xray$p_vals <- format(round(pVals_Xrays, 3), nsmall = 3) 
obj_xray$testNames = test_Names_Xrays



write.csv(obj_xray, "refreshedData/6/xray_tags6.csv", row.names = FALSE)
rm(one_col)








