############# FAST DATA CLEANING #############

# @LannaCox
# @JagathJaiKumar

############# IMPORT #########################
library(readr) # reading data
source("../utils/DCF.R")
############# INIT ###########################

## USER SET

#export filename
new.filename <- "RENN_Cleaned.Rda"
#data filename
raw.data <- read_csv("FAST_Renn_v3.2_June 21, 2018_09.32.csv") 
#name table filename
name.table <- read_csv("Renn_NameTable.csv") 
#any test rows or rows that need to be dropped
additional.dropped.rows <- c(3,4)

############# CLEANING ########################

#Ladder Predict, Alias, Age, Drop Cols, Drop Rows
cleaned.data <- CleanDataFull(raw.data,name.table, new.filename, additional.dropped.rows, export.csv = TRUE)

#One hot encode data for adjaceny matrices
one.hot.data <- one.hot.all.columns(new.filename, name.table)

#Rename sentiment questions with name table data
rename.sentiment.data <- RenameSentiment(new.filename, name.table)

#Save question wordings for data dictionary (contstructed separately)
questions <- rename.sentiment.data[1,]
save(questions,file="questions.rda")

#Drop Questions from dataset
rename.sentiment.data <- DropRows(rename.sentiment.data, name.table, new.filename,1)

#Drop extraneous levels from dataset
renn.cleaned.data <- DropLevels(rename.sentiment.data)

#Save Data
save(renn.cleaned.data, file=new.filename)




