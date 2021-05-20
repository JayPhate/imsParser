
library("readxl")
options(digits=2)

path = "D:\\Personal\\PriAnalysis\\refreshedData\\5"
file_name = "StatisticalComparison5_final.xlsx"

characteristics = read_excel(paste(path,file_name, sep  = "\\" ),
                             sheet=1)

char_cols = colnames(characteristics)[3:6]

char_per = (characteristics[,char_cols]/ characteristics$Total) * 100

characteristics$nwTotal <- paste(characteristics$Total, "(", format(round((characteristics$Total/433) * 100, 2), nsmall=2) ,")", sep="")

   
characteristics$GroupA = paste(characteristics$`Group A`, "(", 
      format(round(char_per$`Group A`,2), nsmall=2),
       "%)", sep="")

characteristics$GroupB = paste(characteristics$`Group B`, "(", 
                               format(round(char_per$`Group B`,2), nsmall=2),
                               "%)", sep="")
characteristics$GroupC = paste(characteristics$`Group C`, "(", 
                               format(round(char_per$`Group C`,2), nsmall=2),
                               "%)", sep="")
characteristics$GroupD = paste(characteristics$`Group D`, "(", 
                               format(round(char_per$`Group D`,2), nsmall=2),
                               "%)", sep="")

View(characteristics)

write.csv(characteristics, file ="refreshedData//5//characteristics.csv" , row.names = FALSE )

###########



characteristics2 = read_excel(paste(path,file_name, sep  = "\\" ),
                              sheet=2)

char_cols_V2 = colnames(characteristics2)[3:6]

char_per_V2 = (characteristics2[,char_cols_V2]/ characteristics2$Total) * 100


characteristics2$nwTotal <- paste(characteristics2$Total, "(", format(round((characteristics2$Total/433) * 100, 2), nsmall=2) ,")", sep="")

characteristics2$GroupA = paste(characteristics2$`Group A`, "(", 
                                format(round(char_per_V2$`Group A`,2), nsmall=2),
                                "%)", sep="")

characteristics2$GroupB = paste(characteristics2$`Group B`, "(", 
                                format(round(char_per_V2$`Group B`,2), nsmall=2),
                                "%)", sep="")
characteristics2$GroupC = paste(characteristics2$`Group C`, "(", 
                                format(round(char_per_V2$`Group C`,2), nsmall=2),
                                "%)", sep="")
characteristics2$GroupD = paste(characteristics2$`Group D`, "(", 
                                format(round(char_per_V2$`Group D`,2), nsmall=2),
                                "%)", sep="")

write.csv(characteristics2, file ="refreshedData//5//characteristics2.csv" , row.names = FALSE )

############

# Discharge and Death

dischargeNdeath <- read_excel(paste(path,file_name, sep  = "\\" ),
                              sheet=3)

char_cols_V3 = colnames(dischargeNdeath)[3:4]

char_per_V3 = (dischargeNdeath[,char_cols_V3]/ dischargeNdeath$Total) * 100

dischargeNdeath$nwTotal <- paste(dischargeNdeath$Total, "(", format(round((dischargeNdeath$Total/433) * 100, 2), nsmall=2) ,")", sep="")

dischargeNdeath$DischargePer <- paste(dischargeNdeath$Discharge, "(", 
      format(round(char_per_V3$Discharge,2), nsmall=2),
      "%)", sep="")

dischargeNdeath$DeathPer <- paste(dischargeNdeath$Death, "(", 
      format(round(char_per_V3$Death,2), nsmall=2),
      "%)", sep="")


write.csv(dischargeNdeath, file ="refreshedData//5//dischargeNdeath.csv" , row.names = FALSE )

############

#Hospital Stay


hospitalStay <- read_excel(paste(path,file_name, sep  = "\\" ),
                              sheet=4)

char_cols_V4 = colnames(hospitalStay)[3:4]

char_per_V4 = (hospitalStay[,char_cols_V4]/ hospitalStay$Total) * 100

hospitalStay$nwTotal <- paste(hospitalStay$Total, "(", format(round((hospitalStay$Total/433) * 100, 2), nsmall=2) ,")", sep="")

hospitalStay$`HospitalStayPer < 7` <- paste(hospitalStay$`HospitalStay < 7`, "(", 
                                      format(round(char_per_V4$`HospitalStay < 7`,2), nsmall=2),
                                      "%)", sep="")

hospitalStay$`HospitalStayPer > 7` <- paste(hospitalStay$`HospitalStay > 7`, "(", 
                                  format(round(char_per_V4$`HospitalStay > 7`,2), nsmall=2),
                                  "%)", sep="")



write.csv(hospitalStay, file ="refreshedData//5//hospitalStay.csv" , row.names = FALSE )


