
setwd("D:\\Personal\\PriAnalysis")
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
options(stringsAsFactors = FALSE)


data1 = read.csv(file.path("refreshedData\\main_file", "COVID19_sheet1.csv"))
data2 = read.csv(file.path("refreshedData\\main_file", "COVID19_sheet2.csv"))


colnames(data1) <- c("Name", colnames(data1)[-1])
colnames(data2) <- c("Name", colnames(data2)[-1])

cutoff_values = data1[1:29,]


dim(data1) #1001
data1 <- data1[-c(1:29),]
dim(data1)

data1 = data1[ trim(data1$Name) != "",]
dim(data1)

dim(data2)
data2 = data2[ trim(data2$Name) != "",]
dim(data2)

colnames(data1)[which(!(colnames(data1) %in% colnames(data2)))]
data1$Hospital.stay <- NULL

data1$X <- NULL
data2$X.1 <- NULL
data2$X <- NULL


all_data <- rbind(data1, data2)
dim(all_data)

#all_data[c(355,356,357),]
#View(all_data[c(355,356,357),])
all_data$CR.no <- paste("~", format(all_data$CR.no, scientific=F), sep="")

all_data$DOA <- gsub("[[:punct:]]", "-", all_data$DOA)
all_data$Discharge.date <- gsub("[[:punct:]]", "-", all_data$Discharge.date)

all_data$DOA <- gsub( "\\-\\d{4,}$", "", all_data$DOA) #c("8-Oct","8-09-","10-8-2020"))
all_data$DOA <- gsub( "\\-$", "", all_data$DOA)

all_data$Discharge.date <- gsub( "\\-\\d{4,}$", "", all_data$Discharge.date) #c("8-Oct","8-09-","10-8-2020"))
all_data$Discharge.date <- gsub( "\\-$", "", all_data$Discharge.date)

all_data$DOA <- paste( "~", all_data$DOA, sep = "")
all_data$Discharge.date <- paste( "~",all_data$Discharge.date, sep = "")

all_data$Death <- paste( "~", all_data$Death, sep = "")

write.csv(all_data, file = "all_refreshed_data.csv", row.names = FALSE  )

###########################################


all_data <- read.csv("all_refreshed_data_modified.csv")

all_data <- refreshed_data1


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


all_data$nw_DOA <- paste("2020", DOA_dayNmonth$month, DOA_dayNmonth$day, sep = "/")
all_data$nw_DischargeDate <- paste("2020", DischargeDate_dayNmonth$month, DischargeDate_dayNmonth$day, sep = "/")

all_data$HospitalStay <- as.Date(as.character(all_data$nw_DischargeDate), format="%Y/%m/%d") - 
  as.Date(as.character(all_data$nw_DOA), format="%Y/%m/%d")


head(all_data[, c("DOA", "Discharge.date" ,"nw_DOA", "nw_DischargeDate", "HospitalStay")])

###########

all_data[all_data$Death !="",]
dim(all_data[all_data$Death !="",])


all_data$Age <- as.numeric(all_data$Age)
hist(all_data$Age)

#b = quantile(all_data$Age, probs = seq(0, 1, by = 0.25))
#b = c(1, 15, 30, 55, 95)
b = c(1, 13, 35, 55, 95)


all_data$age_groups = cut(all_data$Age, breaks = round(b), include.lowest =TRUE)
head(all_data$age_groups)



table(all_data$age_groups)

all_data$IsDeath <- rep(0, nrow(all_data))
all_data$IsDeath[gsub("~", "",all_data$Death)!=""] = 1

all_data$DOA <- paste( "~", all_data$DOA, sep = "")
all_data$Discharge.date <- paste( "~",all_data$Discharge.date, sep = "")
#all_data$Death <- paste( "~", all_data$Death, sep = "")

all_data$nw_DOA <- paste( "~", all_data$nw_DOA, sep = "")
all_data$nw_DischargeDate <- paste( "~", all_data$nw_DischargeDate, sep = "")

View(all_data)

write.csv(all_data, file = 'all_refreshed_data_modified_res.csv', row.names = FALSE)

#all_data$aGroups <- as.character(all_data$age_groups) 

#########################################


refreshed_data1 = read.csv(file.path("refreshedData", "Covid_refreshedData3.csv"))


refreshed_data1$Discharge.date <- gsub( "^~","",refreshed_data1$Discharge.date)
refreshed_data1$Discharge.date <- gsub("[[:punct:]]", "-", refreshed_data1$Discharge.date)

refreshed_data1$Discharge.date <- gsub( "\\-\\d{4,}$", "", refreshed_data1$Discharge.date) #c("8-Oct","8-09-","10-8-2020"))
refreshed_data1$Discharge.date <- gsub( "\\-$", "", refreshed_data1$Discharge.date)

refreshed_data1$DOA <- gsub( "^~","",refreshed_data1$DOA)

#########
refreshed_data1$Death <- gsub( "^~","",refreshed_data1$Death)
refreshed_data1$Death <- gsub("[[:punct:]]", "-", refreshed_data1$Death)

refreshed_data1$Death <- gsub( "\\-\\d{4,}$", "", refreshed_data1$Death) #c("8-Oct","8-09-","10-8-2020"))
refreshed_data1$Death <- gsub( "\\-$", "", refreshed_data1$Death)

refreshed_data1$Discharge.date[refreshed_data1$Death != ""] = refreshed_data1$Death[refreshed_data1$Death != "" ]

#Validate

View(refreshed_data1[refreshed_data1$Death != "", ])
sum(refreshed_data1$Discharge.date == "01-01")
sum(refreshed_data1$Death != "" )

refreshed_data1$Death = paste( "~", refreshed_data1$Death, sep = "")
#########



DOA_dayNmonth <- data.frame(do.call('rbind', 
                                    strsplit(as.character(refreshed_data1$DOA),'-',fixed=TRUE)))

DischargeDate_dayNmonth <- data.frame(do.call('rbind', 
                                              strsplit(as.character(refreshed_data1$Discharge.date),'-',fixed=TRUE)))

colnames(DOA_dayNmonth) <- c("day", "month")
colnames(DischargeDate_dayNmonth) <- c("day", "month")

head(DOA_dayNmonth)
head(DischargeDate_dayNmonth)

dim(DOA_dayNmonth)
dim(DischargeDate_dayNmonth)


refreshed_data1$nw_DOA <- paste("2020", DOA_dayNmonth$month, DOA_dayNmonth$day, sep = "/")
refreshed_data1$nw_DischargeDate <- paste("2020", DischargeDate_dayNmonth$month, DischargeDate_dayNmonth$day, sep = "/")

refreshed_data1$HospitalStay <- as.Date(as.character(refreshed_data1$nw_DischargeDate), format="%Y/%m/%d") - 
  as.Date(as.character(refreshed_data1$nw_DOA), format="%Y/%m/%d")

View(refreshed_data1)

View(refreshed_data1[refreshed_data1$HospitalStay<0,])

# Delete, records where no discharge date and no death info

idx = which(refreshed_data1$Death == "~" & refreshed_data1$Discharge.date == "01-01")

if (length(idx) > 0){
  refreshed_data1 = refreshed_data1[-idx, ]
  
}

write.csv(refreshed_data1, file.path("refreshedData", "Covid_refreshedData3.csv"), row.names = FALSE) 
#all_refreshed_data_modified.csv
