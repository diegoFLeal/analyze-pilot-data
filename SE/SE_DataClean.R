############# FAST DATA CLEANING #############

# @LannaCox
# @JagathJaiKumar

############# IMPORT #########################
library(readr) # 
source("DataCleaningFunction.R")
source("OneHot.R")
source("RenameSentiment.R")
############# INIT ###########################

new.filename <- "SE_Cleaned.Rda"
raw.data <- read_csv("FAST_SouthEnd_v3.1_June 21, 2018_09.34.csv") 
name.table <- read_csv("SE_NameTable.csv") 
additional.dropped.rows <- c()

cleaned.data <- CleanDataFull(raw.data,name.table, new.filename, additional.dropped.rows, export.csv = TRUE)
one.hot.data <- one.hot.all.columns(new.filename, name.table)
rename.sentiment.data <- RenameSentiment(new.filename, name.table)

