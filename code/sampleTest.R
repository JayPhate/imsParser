# http://www.utstat.toronto.edu/~brunner/oldclass/312f12/lectures/312f12FisherWithR.pdf
# https://www.sheffield.ac.uk/polopoly_fs/1.714563!/file/stcp-karadimitriou-MannWhitR.pdf
# https://stats.idre.ucla.edu/r/whatstat/what-statistical-analysis-should-i-usestatistical-analyses-using-r/
# https://statistics.laerd.com/spss-tutorials/kruskal-wallis-h-test-using-spss-statistics.php
# http://www.r-tutor.com/elementary-statistics/non-parametric-methods/kruskal-wallis-test



sample_matrix1 <- matrix(c(31,2,54,18),2,2, byrow = TRUE)
fisher.test(sample_matrix1)


sample_matrix2 <- matrix(c(2,31,18, 54),2,2, byrow = TRUE)
fisher.test(sample_matrix2)




head(airquality)

kruskal.test(Ozone ~ Month, data = airquality)
kruskal.test(Month ~ Ozone, data = airquality)
output <- kruskal.test(Ozone ~ Month, data = airquality)


######################

library(tidyverse)
library(ggpubr)
library(rstatix)

set.seed(1234)
PlantGrowth %>% sample_n_by(group, size = 1)


############

df_test <- data.frame("Male" = c(rep(1, 240), rep(0, 5)), 
                      "target" = c(rep(0, 134), rep(1, 111)))

df_test$Gender <- as.factor(df_test$Gender)
df_test$target <- as.factor(df_test$target)

kruskal.test(Male ~ target, data = df_test)
t.test(Male ~ target, data = df_test)
##############


sex <- c(rep(1, 48), rep(0, 57))
young_old <- c(rep(1, 38), rep(0,10), 
               rep(1,47), rep(0,10))
               
  
df_test2 <- data.frame(sex, young_old)
head(df_test2)

kruskal.test(sex ~ young_old, data = df_test2)
kruskal.test( young_old ~ sex, data = df_test2)
t.test(young_old ~ sex, data = df_test2)

wilcox.test(sex ~ young_old, data = df_test2)
################


hsb2 <- within(read.csv("https://stats.idre.ucla.edu/stat/data/hsb2.csv"), {
  race <- as.factor(race)
  schtyp <- as.factor(schtyp)
  prog <- as.factor(prog)
})
attach(hsb2)
kruskal.test(write ~ female)
t.test(write ~ female)


###############


library(RCurl)

myfile <- getURL('https://sakai.unc.edu/access/content/group/3d1eb92e-7848-4f55-90c3-7c72a54e7e43/public/data/bycatch.csv', ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)

u1 <- "http://172.16.1.4:8181/AHIMSWeb/investigation/reports/ipd_AllResultCnt.jsp" 
 
f1 <- getURL(u1,  ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)

write.table(f1, "f1.txt") 


library(xml2)
library(rvest)
library(stringr)
f2 <- read_html(u1) 

library(XML)

f2 = readHTMLTable(u1)
names(f2)

f2 <- readLines(u1)

f2 <- read.csv(url(u1))
write.table(f2,"f2.txt")
#  201512000602


library(pdftools)

pdf_text("sample1.pdf") 

pdf_text("COVID reference.pdf")
pdf_text(pdf = "http://arxiv.org/pdf/1403.2805.pdf")

library(tm)

doc <- readPDF(control = list(text = "-layout"))(elem = list(uri = "sample1.pdf"),
                                                 language = "en",
                                                 id = "id1")

fName = "sample1_html.html"

rawHTML <- paste(readLines(fName), collapse="\n")


library(XML)
library(stringr)
namespaces=c(xmlns="http://www.xbrl.org/2008/inlineXBRL")
parse.html <- htmlTreeParse(fName, useInternalNodes=TRUE)
tb <- xpathApply(parse.html, '//table', namespaces=namespaces)

tb_v <- xpathApply(parse.html, '//html//body//form//table[1]', xmlValue)

tb_v <- xpathApply(parse.html, count('//html//body//table'), xmlValue)


doc = xmlParse(fName)
root = xmlRoot(doc)
xml.data = xmlToList(doc)


View(tb_v)

write.table(tb_v,"tb_v.txt")

tb_vcontent <- as.data.frame(matrix(tb_v, ncol = 8, byrow = TRUE))


library(rvest)
theurl <- "http://en.wikipedia.org/wiki/Brazil_national_football_team"
file<-read_html(fName)
tables<-html_nodes(file, "table")
table1 <- html_table(tables[1], fill = TRUE)


