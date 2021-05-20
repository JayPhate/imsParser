options(stringsAsFactors = FALSE)

get_Neu_LymRatio <- function(records, pattern="Peripheral Smear"){
  
  idx = grep(pattern, records)
  res = ""
  if(length(idx) > 0){
    sec_pattern = "Normocytic normochromic RBCs. N:L ratio"
    while (length(grep(sec_pattern, records[idx + 1])) > 0) {
      x =gsub("^[^-]+=\\s?|(\\.?\\s?Platelets).*", "", records[idx + 1])
      if (res[1] != ""){
        res <- c(res, x)
      }else{
        res <- x
      }
      idx = idx + 1
    }
  } 
  return(res)
}


file_path <- "D:\\Personal\\PriAnalysis\\samples"
file_name <- "202008041350"
prepare_file_path <- paste( c(file_path, "\\", file_name), collapse="")
sample_data1 <- read.csv(prepare_file_path, sep = "\n", header=F)

get_Neu_LymRatio(sample_data1$V1)
grep("Normocytic normochromic RBCs. N:L ratio", sample_data1$V1[130])


p <- "Glucose Random"
p <- "Ferritin" 
p <- "D-Dimer"
p <- "P C T"
p <- "Liver Panel"
p <- "Hs-Crp"
p <- "A B G \\(Blood Gas Analysis\\)"

pos <- grep(p, sample_data1$V1)    
matched <- sample_data1$V1[pos]
values_n_dates = get_vals_dts(sample_data1$V1, pos)
values_n_dates

!grepl("^[a-zA-Z].*", sample_data1$V1[81:85])
sample_data1$V1[81:85]


r = get_Liver_Panel_AST_n_ALT( sample_data1$V1, "Liver Panel")

dts <- paste(trim(sapply(strsplit(res, "\t"),'[', 1)), sep = "|", collapse = "|")
vals <- paste(trim(sapply(strsplit(res, "\t"),'[', 2)), sep = "|", collapse = "|")



sapply(strsplit(act_pat, "\t"), '[', 7)
sapply(strsplit(act_pat, "\t"), '[', 8)

dts = paste(trim(sapply(strsplit(r, "\t"), '[', 1)), sep = "|", collapse = "|")
alt_vals <- paste(trim(sapply(strsplit(r, "\t"), '[', 7)), sep = "|", collapse = "|")
ast_vals <- paste(trim(sapply(strsplit(r, "\t"), '[', 8)), sep = "|", collapse = "|")


