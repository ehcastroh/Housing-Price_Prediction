---
title: "IEOR 142 -- Ames Housing Analysis"
author: "Paul Grigas"
date: "November 2017"
output:
  html_document: default
  html_notebook: default
  pdf_document: default
  word_document: default
---
### Load Libraries and Packages ###
  
#install.packages("tidyverse")
library(tidyverse)

# read data set (note: previously cleaned)
ames <- read.csv("qtr_ames.csv")

#convert time sold to factor and data frame to tibble
ames$YrSold <- as.factor(ames$YrSold)
ames$MoSold <- as.factor(ames$MoSold)
qtr_ames <- as_tibble(ames)

#Find columns with NAs.
naTF <- sapply(qtr_ames, anyNA)
amesNaCols <- colnames(qtr_ames[,naTF])
amesNaCols

# convert monthly prices to quarterly averages
mean_qa <- function(x, y, m){
  qa_y <- x %>% filter(YrSold %in% c(y))
  qb_y <- qa_y %>% filter(MoSold %in% c(m))
  qc_y <- mean(qb_y$SalePrice)
}

# function calls
q01_y06 <- mean_qa(qtr_ames, y = "2006",m = c("1","2","3"))
q02_y06 <- mean_qa(qtr_ames, y = "2006",m = c("4","5","6"))
q03_y06 <- mean_qa(qtr_ames, y = "2006",m = c("7","8","9"))
q04_y06 <- mean_qa(qtr_ames, y = "2006",m = c("10","11","12"))
q01_y07 <- mean_qa(qtr_ames, y = "2007",m = c("1","2","3"))
q02_y07 <- mean_qa(qtr_ames, y = "2007",m = c("4","5","6"))
q03_y07 <- mean_qa(qtr_ames, y = "2007",m = c("7","8","9"))
q04_y07 <- mean_qa(qtr_ames, y = "2007",m = c("10","11","12"))
q01_y08 <- mean_qa(qtr_ames, y = "2008",m = c("1","2","3"))
q02_y08 <- mean_qa(qtr_ames, y = "2008",m = c("4","5","6"))
q03_y08 <- mean_qa(qtr_ames, y = "2008",m = c("7","8","9"))
q04_y08 <- mean_qa(qtr_ames, y = "2008",m = c("10","11","12"))
q01_y09 <- mean_qa(qtr_ames, y = "2009",m = c("1","2","3"))
q02_y09 <- mean_qa(qtr_ames, y = "2009",m = c("4","5","6"))
q03_y09 <- mean_qa(qtr_ames, y = "2009",m = c("7","8","9"))
q04_y09 <- mean_qa(qtr_ames, y = "2009",m = c("10","11","12"))
q01_y10 <- mean_qa(qtr_ames, y = "2010",m = c("1","2","3"))
q02_y10 <- mean_qa(qtr_ames, y = "2010",m = c("4","5","6"))
q03_y10 <- mean_qa(qtr_ames, y = "2010",m = c("7","8","9"))
q04_y10 <- mean_qa(qtr_ames, y = "2010",m = c("10","11","12"))

# row bind quartely averages
qtr_resp_ames <- rbind(q01_y06, q02_y06, q03_y06, q04_y06,
                       q01_y07, q02_y07, q03_y07, q04_y07,
                       q01_y08, q02_y08, q03_y08, q04_y08,
                       q01_y09, q02_y09, q03_y09, q04_y09,
                       q01_y10, q02_y10, q03_y10, q04_y10)

# write out to disk, include row names, and ommit NaN's
write.csv(qtr_resp_ames, file = "Ames Quarterly Responses.csv", row.names=TRUE, na="")
