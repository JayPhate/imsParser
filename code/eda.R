#R3434815
# https://www.r-bloggers.com/reshape-and-aggregate-data-with-the-r-package-reshape2/


setwd("D:\\Personal\\PriAnalysis")
options(stringsAsFactors = FALSE)

#data1 = read.csv(file.path("providedData", "data_sheet1.csv"))
#data2 = read.csv(file.path("providedData", "data_sheet2.csv"))

data1 = read.csv(file.path("providedData", "COVID 19 - Sheet1.csv"))
data2 = read.csv(file.path("providedData", "COVID 19 - Sheet2.csv"))

dim(data1)
data1 = data1[data1$Name != "",]
dim(data1)

dim(data2)
data2 = data2[data2$Name != "",]
dim(data2)

colnames(data1)
colnames(data2)


colnames(data1)[which(!(colnames(data1) %in% colnames(data2)))]
data1$Hospital.stay <- NULL

all_data <- rbind(data1, data2)
dim(all_data)

all_data$DOA <- gsub("[[:punct:]]", "-", all_data$DOA)
all_data$Discharge.date <- gsub("[[:punct:]]", "-", all_data$Discharge.date)

all_data$DOA <- gsub( "\\-\\d{4,}$", "", all_data$DOA) #c("8-Oct","8-09-","10-8-2020"))
all_data$DOA <- gsub( "\\-$", "", all_data$DOA)

all_data$Discharge.date <- gsub( "\\-\\d{4,}$", "", all_data$Discharge.date) #c("8-Oct","8-09-","10-8-2020"))
all_data$Discharge.date <- gsub( "\\-$", "", all_data$Discharge.date)

all_data$DOA <- paste( "~", all_data$DOA, sep = "")
all_data$Discharge.date <- paste( "~",all_data$Discharge.date, sep = "")

write.csv(all_data, file = "all_data.csv", row.names = FALSE  )

###########################################

#all_data <- read.csv("all_refreshed_data_modified.csv")

all_data <- read.csv( file.path("refreshedData","Covid_refreshedData3_V2.csv"))


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


#all_data$nw_DOA <- paste("2020", DOA_dayNmonth$month, DOA_dayNmonth$day, sep = "/")
#all_data$nw_DischargeDate <- paste("2020", DischargeDate_dayNmonth$month, DischargeDate_dayNmonth$day, sep = "/")

#all_data$HospitalStay <- as.Date(as.character(all_data$nw_DischargeDate), format="%Y/%m/%d") - 
#        as.Date(as.character(all_data$nw_DOA), format="%Y/%m/%d")
  

head(all_data[, c("DOA", "Discharge.date" ,"nw_DOA", "nw_DischargeDate", "HospitalStay")])

###########

#all_data[all_data$Death !="",]

all_data$Age <- as.numeric(all_data$Age)
#hist(all_data$Age)


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

#View(all_data[all_data$Death !="~",])
#all_data$HospitalStay[ all_data$Death !="~" ]


#b = quantile(all_data$Age, probs = seq(0, 1, by = 0.25))
#b = c(1, 15, 30, 55, 95)
b = c(1, 13, 35, 55, 95)


all_data$age_groups = cut(all_data$Age, breaks = round(b), include.lowest =TRUE)
head(all_data$age_groups)


table(all_data$age_groups)

all_data$IsDeath <- rep(0, nrow(all_data))
all_data$IsDeath[gsub("~","", all_data$Death)!=""] = 1
#all_data = all_data[-c(6, 89),] 
#write.csv(all_data, file = 'all_data_res.csv', row.names = FALSE)

View(all_data[all_data$IsDeath==1,])
View(all_data[c(6, 89),])
all_data$aGroups <- as.character(all_data$age_groups) 

all_data$HospitalStay_7days = ifelse(all_data$HospitalStay > 7, ">7", "<7")


clinicalStatusTag = c(0,3,5,10,15, 100)
all_data$clinicalStatus = cut(all_data$HospitalStay, breaks = round(clinicalStatusTag), include.lowest =TRUE)

table(all_data$clinicalStatus)


##############
library(dplyr)
library(reshape2)

table(all_data$Sex, all_data$age_groups)

bySex <- dcast(all_data, Sex ~ age_groups, length, 
               value.var = "age_groups")

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


hospitalStayF <- ifelse(all_data$HospitalStay < 3, "<3",
       ifelse( (all_data$HospitalStay >=3) && 
                 (all_data$HospitalStay <=7), "3-7",
       ">7"))

#all_data$HospitalStay_7days

byHospitalStay = dcast(all_data, HospitalStay_7days ~ age_groups, length,
                       value.var = "age_groups")

byClinicalStatus = dcast(all_data, clinicalStatus ~ age_groups, length,
                         value.var = "age_groups")


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
#col_to_process <- c("", "", col_to_process)  

summary1 <- data.frame()
  
for (one_col in col_to_process) {
  print(one_col)
  obj <- dcast(all_data_features, eval(parse(text=one_col)) ~ age_groups, length, 
                 value.var = "age_groups")
  print(obj)
  obj$header <- one_col
  
  summary1 = rbind(summary1, obj)
  
}

write.csv(summary1, "refreshedData//6//summary_AgeGroup6.csv", row.names = FALSE)

rm(one_col, obj)

summary2 <- data.frame()
summary3 <- data.frame()

for (one_col in col_to_process[-c(1:3)]) { # -c(1,2): removing Religion, Sex
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
  
  #if( ! "0" %in% colnames(obj)){
  #  obj$"0" <- 0
  #}
  
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

write.csv(summary2, "refreshedData/6/summary_DisNDeath6.csv", row.names = FALSE)
write.csv(summary3, "refreshedData/6/summary_HospitalStay6.csv", row.names = FALSE)

obj_xray <- dcast(all_data_features, XrayTags ~ IsDeath, length, 
                  value.var = "age_groups")

write.csv(obj_xray, "refreshedData/6/xray_tags6.csv", row.names = FALSE)

##############

features = colnames(all_data)[10:21]
features = features[ !features %in% c("RBS", "LFT") ]
target_col = "IsDeath"

res_df = data.frame()

for (one_f in features) {
  independent = as.numeric(all_data[,c(one_f)] )
  dependent = as.numeric(all_data[,c(target_col)])
  
  #cat(a, b, "\n")
  
  output <- kruskal.test(independent ~ dependent, data = all_data)
  
  demographics = one_f
  all_patients = sum(!is.na(independent))
  discharged = sum(!is.na( independent[dependent == 0]))
  died = sum(!is.na( independent[dependent == 1]))
  
  all_patients_m = median(independent, na.rm = TRUE)
  discharged_m = median(independent[dependent == 0], na.rm = TRUE)
  died_m = median  (independent[dependent == 1], na.rm = TRUE)
  p.value = output$p.value
  test = output$method
  
  dummy = data.frame("Demographics" = demographics,
                     "All_Patients" = all_patients,
                     "Discharged" = discharged,
                     "Died" = died,
                     
                     "All_Patients_m" = all_patients_m,
                     "Discharged_m" = discharged_m,
                     "Died_m" = died_m,
                     
                     "p_value" = p.value,
                     "Method" = test
                     
                     )
  res_df = rbind(res_df, dummy) 
  
}


write.csv(res_df, "refreshedData//6//StatisticalComparison6.csv", row.names = FALSE)
write.csv(base_details, "refreshedData//6//base_details6.csv", row.names = FALSE)





