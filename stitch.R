###------------------------------------------------------------------###
# title: "IEOR 142 Group Project -- Fed/Ames combine monthly data sets"
# author: "Elias Castro Hernandez and Darren Fang"
# date: "December 2017"
# purpose: clean and convert Ames and macro data for processing 
###------------------------------------------------------------------###

### Load Libraries and Packages ###

#install.packages("tidyverse")
library(tidyverse)

### read and cleand data for processing ###
# macro level csv files
temp_macro = list.files(pattern="*.csv")
temp_Data <- lapply(temp_macro, read.csv) %>% bind_cols()
colnames(temp_Data)[1] <- "Darkwing Duck" 
macro_Data <- temp_Data  %>% select(-starts_with("DAT")) #not case sensitive
colnames(macro_Data)[1] <- "Date"

# write out to disk, include row names, and ommit NaN's
write.csv(macro_Data, file = "Ames Macro Monthly Predictors.csv", row.names=TRUE, na="")

