---
title: "Data Cleaning Pipeline"
author: "Lanna Cox, Jagath Jai Kumar"
date: "7/16/2018"
output: word_document
---

## Overview

This document contains the current FAST data cleaning strategy and code review. While many areas are still under development, this document should serve as an overview for the entire set of scripts. This document will delve in detail into the following scripts:

* RENN_DataClean.R - Driver script that will be school specific
* DCF.R - A collection of data cleaning functions that will be sourced by the driver
* data_dict.R - A script for generating summary statistics in a presentable word document, **STILL IN EARLY DEVELOPMENT**

An important step that is currently missing from this pipeline is the robust alias creation and implementation that we are still developing. The alias generation will be a separate script that will be run before any of the cleaning steps are executed. For now, we have a *minimal aliasing function that uses a pre-generated lookup table* to replace every instance of a student name with a user defined value.

### DataClean.R - Driver Script

As currently constructed, the code for executing the data cleaning functions is separated into 3 scripts, but ultimately users will only ever need to interface with 1 script (the driver). The driver is a piece of code that contains **school specific** information, and a new driver will need to be made for each school visit. The school specific information that will need to be supplied by the user is the following:

* Import filename - the filename for the 
* Export filename - the filename for the save Rda/RData file
* Name table filename - the filename for the alias lookup table
* Additional drop rows - any test or extra rows in the data that need to be removed



```{r eval=FALSE}
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
```

After setting the initalization variables, the rest of the script calls the functions from the DCF suite. The driver also produces questions.rda, a data frame containing the question wordings which is used later in data dictionary generation.

``` {r eval=FALSE}
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
```

### DCF.R - Data Cleaning Function Suite

The DCF.R file is a collection of functions that can be called for any school dataset. The file currently contains the following functions:

* PredictLadder()
* AliasNames() 
* CalculateAge()
* DropCols()
* removeRows()
* DropRows()
* ExportRda()
* CleanDataFull()
* one.hot.all.columns()
* RenameSentiment()

The CleanDataFull() function was written to keep the driver file uncluttered: it simply calls the functions above it in order and returns a semi-cleaned dataset. The driver then calls one.hot.all.columns(), and RenameSentiment(). Again, this makes it easy for users to write new driver files and call these methods.

```{r eval=FALSE}
############# FAST DATA CLEANING #############

# @LannaCox
# @JagathJaiKumar


library(tidyverse) # Data manipulation tools
library(onehot) # One hot tools
library(lubridate) # date reading tools
library(qdapTools) # One hot tools


################## DATA CLEANING FUNCTIONS ###

PredictLadder <- function(raw.data, name.table, new.filename) {
    ## Ladder Cleaning
    
    ## find missing values by iterating through ladder question
    l = c()
    h <- 3
    for (i in raw.data$Q_ses_loc_ladder[-(1:2)]){
        if (is.na(i)){
            raw.data$Q_ses_loc_ladder[h] <- -1
        } else if (i == "Other") {
            l = c(l, h)
        } 
        h <- h+1
    }
    ## list l now contains missing ladder indeces
    
    ## Separate into training and test data
    raw.data.missingladder <- raw.data[l,c("Q_ses_loc_ladder_1_x","Q_ses_loc_ladder_1_y","Q_ses_loc_ladder")]
    raw.data.hasladder <- raw.data[-c((1:2),l),c("Q_ses_loc_ladder_1_x","Q_ses_loc_ladder_1_y","Q_ses_loc_ladder")]
    
    nmissing = nrow(raw.data.missingladder)
    nhas = nrow(raw.data.hasladder)
    
    ## predict missing values with single nearest neighbor approach
    predictions <- c()
    for (i in 1:nmissing){
        mx <- as.numeric(raw.data.missingladder[i,][1])
        my <- as.numeric(raw.data.missingladder[i,][2])
        
        min_dist <- 1000000
        min_dist_index <- 0
        for (j in 1:nhas){
            hx <- as.numeric(raw.data.hasladder[j,][1])
            hy <- as.numeric(raw.data.hasladder[j,][2])
            
            dist <- sqrt( (mx - hx) ^2 + (my - hy) ^2)
            if(dist < min_dist){
                min_dist <- dist
                min_dist_index <- j
            }
        }
        predictions <- c(predictions, as.numeric(raw.data.hasladder[min_dist_index, "Q_ses_loc_ladder"]))
    }
    
    ## add predicted values to existing column
    
    for (i in 1:length(predictions)){
        raw.data$Q_ses_loc_ladder[l[i]] <- predictions[i]
    }
    raw.data
}

## alias names according to name table
AliasNames <- function(raw.data, name.table, new.filename) {
    n <-  nrow(name.table)
    ## iterate through data and replace every instance of names with name table value
    for (i in 1:n){
        raw.data <- data.frame(lapply(raw.data, function(x) {
            gsub(name.table[[i, 1]], name.table[[i, 2]], x)
        }))
    }
    raw.data
}
```

We calculated age by subtracting the birthdate from the recorded start time of the survey. This gives age in weeks that we convert to age in years by dividing by 52.25.

```{r eval=FALSE}
## calculate age in years
CalculateAge <- function(raw.data, name.table, new.filename) {
    ## Birthdate Column
    raw.data$birthdate <- as.Date(with(raw.data, paste(Q_age_1, Q_age_2, Q_age_3,sep="-")), "%B-%d-%Y")
    
    ## Age Column
    age_temp<-as.numeric(difftime(as.Date(as.character(as.POSIXct(raw.data$StartDate[-(1:2)]))), as.Date(as.character(as.POSIXct(raw.data$birthdate[-(1:2)], tz="UTC"))), units="weeks"))/52.25
    
    ## This happens because there are 2 rows at the beginning of the data that are not student responses
    ## They will be dropped in a later step
    age_temp<-c(NA, age_temp)
    age_temp<-c(NA, age_temp)
    
    raw.data$CurrentAge <- age_temp  
    
    raw.data
}

## drop specific columns
DropCols <- function(raw.data, name.table, new.filename) {
    raw.data$StartDate <- NULL
    raw.data$EndDate <- NULL
    raw.data$Q_role_friend <- NULL
    raw.data$Finished <- NULL
    raw.data$Status <- NULL
    raw.data$IPAddress <- NULL
    raw.data$Progress <- NULL
    raw.data$RecordedDate <- NULL
    raw.data$RecipientLastName <- NULL
    raw.data$RecipientFirstName <- NULL
    raw.data$RecipientEmail <- NULL
    raw.data$ExternalReference <- NULL
    raw.data$LocationLatitude <- NULL
    raw.data$LocationLongitude <- NULL
    raw.data$DistributionChannel <- NULL
    raw.data$ResponseId <- NULL
    raw.data$UserLanguage <- NULL
    raw.data$Q_assent_staff <- NULL
    raw.data$Q_assent_staff_12_TEXT <- NULL
    raw.data$Q_assent_sss <- NULL
    raw.data$Q_assent_sss_new <- NULL
    raw.data$Q_assent_sss_new_1_TEXT <- NULL
    raw.data$Q_assent_type <- NULL
    raw.data$Q_assent_type_1_TEXT <- NULL
    raw.data$Q_assent_sig_Id <- NULL
    raw.data$Q_assent_sig_Name <- NULL
    raw.data$Q_assent_sig_Size <- NULL
    raw.data$Q_assent_sig_Type <- NULL
    raw.data$Q_id_missing <- NULL
    raw.data$Q_id_missing_4_TEXT <- NULL
    raw.data$Duration..in.seconds. <- NULL
    
    raw.data
}

## drop specifc rows
removeRows <- function(rowNum, data) {
    newData <- data[-rowNum, , drop = FALSE]
    rownames(newData) <- NULL
    newData
}

## Drop row 2 (question ID) and any other user specified rows
DropRows <- function(raw.data, name.table, new.filename, additional.dropped.rows){
    raw.data <- removeRows(c(2,additional.dropped.rows), raw.data)
    raw.data
}

## export RDA
ExportRda <- function(raw.data, new.filename){
    save(raw.data, file = new.filename)
}

## Drop extraneous levels (just dropping rows will not also remove extraneous levels)
DropLevels <- function(raw.data){
    for (col in colnames(raw.data)){
        if (!is.Date(raw.data[[col]]) && !is.numeric(raw.data[[col]]) && !is.double(raw.data[[col]])){
            raw.data[[col]] <- droplevels(raw.data[[col]])
        } 
    }
    raw.data
}

## call all above methods
CleanDataFull <- function(raw.data, name.table, new.filename, additional.dropped.rows = c(), export.csv = FALSE) {
    print("Predicting missing Ladder values...")
    ladder.data <- PredictLadder(raw.data, name.table, new.filename)
    print("Aliasing names ...")
    aliased.data <- AliasNames(ladder.data, name.table, new.filename)
    print("Calculating ages ...")
    age.data <- CalculateAge(aliased.data, name.table, new.filename)
    print("Dropping Columns ...")
    drop.cols <- DropCols(age.data, name.table, new.filename)
    print("Dropping Rows ...")
    drop.rows <- DropRows(drop.cols, name.table, new.filename, additional.dropped.rows)
    cleaned.data <- drop.rows
    if (export.csv){
        ExportRda(cleaned.data, new.filename) 
    } 
    cleaned.data
}
```

One-hot encoding is the process of transforming a column containing a list of data into an adjacency matrix for easier processing.

![Easy example of one-hot encoding](onehot.png)

In cleaning this data, we need to one-hot encode the questions that required the students to list the people they interacted with in different situations.


```{r eval=FALSE}
########### ONE HOT COLUMNS FUNCTION #########
## input: rda filename, name.table
## output: expanded dataframe with one-hot columns for network questions

## Naming convention - Question_id_studentid

one.hot.all.columns <- function (filename, name.table) {
    
    load(filename)
    renn.data <- raw.data

    ## F2F.ONLINE
    one.hot.columns <- c("Q_sn_f2finteract", "Q_sn_onlineinteract")
    
    for (cols in one.hot.columns){
        ## one hot
        temp <- mtabulate(strsplit(as.character(renn.data[[cols]]), ","))
        '%!in%' <- function(x,y)!('%in%'(x,y))
        n <- nrow(temp)
        ## add missing columns
        l <- c()
        for (i in name.table$Value){
            if (i %!in% colnames(temp)){
                l<-c(l,i)
            }
        }
        for (i in l){
            temp[[i]] <- rep(0, n)
        }
        new.names <- c()
        ## give correct names
        for (i in colnames(temp)){
            colname <- paste(cols,i, sep = "_", collapse = NULL)
            new.names <- c(new.names,colname)
        }
        new.names
        colnames(temp) <- new.names
        renn.data <- cbind(renn.data, temp)
    }
    
    ## FOOD
    ## onehot
    food1 <- mtabulate(strsplit(as.character(renn.data[["Q_sn_comb_food1"]]), ","))
    food2 <- mtabulate(strsplit(as.character(renn.data[["Q_sn_comb_food2"]]), ","))
    
    ## combine
    food <- cbind(food1, food2)
    food <- as.data.frame(do.call(cbind, by(t(food),INDICES=names(food),FUN=colSums)))
    
    n <- nrow(food)
    ## add missing columns
    l <- c()
    for (i in name.table$Value){
        if (i %!in% colnames(food)){
            l<-c(l,i)
        }
    }
    for (i in l){
        food[[i]] <- rep(0, n)
    }
    new.names <- c()
    for (i in colnames(food)){
        colname <- paste("Q_sn_comb_food",i, sep = "_", collapse = NULL)
        new.names <- c(new.names,colname)
    }
    new.names
    colnames(food) <- new.names
    
    renn.data <- cbind(renn.data, food)
    ## PA
    
    ## one hot
    pa1 <- mtabulate(strsplit(as.character(renn.data[["Q_sn_comb_pa1"]]), ","))
    pa2 <- mtabulate(strsplit(as.character(renn.data[["Q_sn_comb_pa2"]]), ","))
    
    ## combine
    pa <- cbind(pa1, pa2)
    pa <- as.data.frame(do.call(cbind, by(t(pa),INDICES=names(pa),FUN=colSums)))
    
    ## add missing columns
    n <- nrow(pa)
    l <- c()
    for (i in name.table$Value){
        if (i %!in% colnames(pa)){
            l<-c(l,i)
        }
    }
    for (i in l){
        pa[[i]] <- rep(0, n)
    }
    new.names <- c()
    for (i in colnames(pa)){
        colname <- paste("Q_sn_comb_pa",i, sep = "_", collapse = NULL)
        new.names <- c(new.names,colname)
    }
    new.names
    colnames(pa) <- new.names
    
    renn.data <- cbind(renn.data, pa)
    
    
    ## SCREEN
    
    ## one hot
    screen1 <- mtabulate(strsplit(as.character(renn.data[["Q_sn_comb_screen1"]]), ","))
    screen2 <- mtabulate(strsplit(as.character(renn.data[["Q_sn_comb_screen2"]]), ","))
    
    ## combine
    screen <- cbind(screen1, screen2)
    screen <- as.data.frame(do.call(cbind, by(t(screen),INDICES=names(screen),FUN=colSums)))
    
    ## add missing columns
    n <- nrow(screen)
    l <- c()
    for (i in name.table$Value){
        if (i %!in% colnames(screen)){
            l<-c(l,i)
        }
    }
    for (i in l){
        screen[[i]] <- rep(0, n)
    }
    new.names <- c()
    for (i in colnames(screen)){
        colname <- paste("Q_sn_comb_screen",i, sep = "_", collapse = NULL)
        new.names <- c(new.names,colname)
    }
    new.names
    colnames(screen) <- new.names
    
    renn.data <- cbind(renn.data, screen)
    
    raw.data <- renn.data
    
    save(raw.data,file = filename)
    
    raw.data
}

```

Finally, the sentiment question data needs to be renamed in accordance with the name-table that was previously generated. Otherwise, the sentiment questions will be named with Qualtrics' internal naming scheme. 

*Example: Qualtrics column: Q_sentiment_x1 ===> Renamed column Q_sentiment_alias*

```{r eval=FALSE}

########### RENAME SENTIMENT FUNCTION ########

## input: RDA filename, name.table
## output: saved RDA w/ renamed sentiment questions
RenameSentiment <- function (filename, name.table){
    
    load(filename)
    
    ## extract sentiment
    sentiment <- raw.data[ , grepl( "Q_sn_sentiment" , names( raw.data ) ) ]
    
    ## delete original sentiment
    raw.data[ , grepl( "Q_sn_sentiment" , names( raw.data ) ) ] <- NULL
    
    
    ## Produce list of new names
    new.names <- c()
    
    for (i in name.table$Value){
        new.names <- c(new.names, paste("Q_sn_sentiment", i, sep = "_"))
    }
    new.names
    
    
    ## Rename sentiment 
    colnames(sentiment) <- new.names
    
    
    ## Reattach sentiment
    raw.data <- cbind(raw.data, sentiment)
    
    ## Save rda
    save(raw.data,file = filename)
    
    ## Return dataframe
    raw.data
}
```

### data_dict.R - Script for Generating Summary Statistics Document

The data_dict.R script is still in early development, but we were excited by the potential for aesthetically pleasing auto-generated summary statistics. This script uses the question data that was extracted when the driver script was run and the cleaned data frame to build a set of summary statistics for each data frame. We will ultimately turn this script into a function and add it to the DCF suite once we are fully satisfied with its construction.


We begin by importing plyr for counts and ReporteRs for word doc generation. We also define helper functions for later use.

```{r eval=FALSE}
######################
### IN DEVELOPMENT ###
######################

library(plyr)   # count tool
library(readr)  # read csv
library(ReporteRs) # word doc
library(flextable) # flextable

# helper function for formatting percents
percent <- function(x, digits = 2, format = "f", ...) {
    paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

## helper function for combining tables (ignore warning)
cbind.fill<-function(...)
{
    nm <- list(...) 
    nm<-lapply(nm, as.matrix)
    n <- max(sapply(nm, nrow)) 
    as.data.frame(do.call(cbind, lapply(nm, function (x) 
        rbind(x, matrix(, n-nrow(x), ncol(x))))))
}
```

Here we identify which columns we are going to produce summary stastics for. The columns we select can easily be changed for development, but it should remain the same between schools.

```{r eval=FALSE}
# load cleaned data & questions
load("RENN_Cleaned.Rda")
load("questions.rda")

# identify columns that need summary stats (will be same for all tables)
factor.colnums <- c(2,3,7,8,9,10,13,26:55, 59:91)
```


We then go on to build a large table containing the summary statistics for every question.

```{r eval=FALSE}
# extract questions
questionsc <- questions[1, factor.colnums]

# df will contain all summary stats as 1 table
df <- data.frame()
# output folder will contain new csv for each table (separate files)
filebase <- "output/rennstat"

# iterate through selected columns and produce full statistics table
for (col in factor.colnums){
    #extract columname
    c <- colnames(renn.cleaned.data)[col]
    #make filename
    filen <- paste(filebase, c,".csv", sep = "")
    #count levels
    temp <- count(renn.cleaned.data, renn.cleaned.data[[c]])
    #calculate percent
    perc <- c()
    s <- sum(temp$n)
    for (i in temp$n){
        perc <- c(perc, i/s)
    }
    perc <- percent(perc)
    #add percent column
    temp$percentage <- perc
    #rename columns
    colnames(temp) <- c(c, "freq", "percentage")
    #bind to large table
    df <- cbind.fill(df,temp)
}
# Write out to csv
write.csv(df, file = "renn_fullstatisticstable.csv")
```

Finally, we use ReporteRs to fortmat and export each question and its corresponding summary as a word document.

```{r eval=FALSE}

# Create a word document to contain R outputs
doc <- docx()
options("ReporteRs-fontsize"=11, "ReporteRs-default-font"="Arial")
# Add a title to the document
doc <- addTitle(doc, "Rennaisance Summary Stats", level=1)
# Produce summary statistics from large table
j <- 1
for (i in seq(1, length(df) - 3, by=3)){
    # Add question from question list
    doc <- addParagraph(doc, as.character(questionsc[[j]]))
    # Add space
    doc <- addParagraph(doc, "")
    #Extract table from fullstats table 
    startindex <- i
    endindex <- i+2
    temp <- vanilla.table(na.omit(df[,startindex:endindex]))
    #Set zebra style
    temp <- setZebraStyle(temp, odd = '#eeeeee', even = 'white')
    #Set header color
    temp <- setFlexTableBackgroundColors(temp, i = 1, colors = '#FFFFFF', to = 'header')
    #Add to doc
    doc <- addFlexTable(doc, temp, vertical.align = 'center')
    # Add space
    doc <- addParagraph(doc, "")
    j <- j+1
}
# Write out document
writeDoc(doc, file = "rennstats.docx")
```

The output of this code is attached to the end of the packet.

### Wrapping up and Moving Forward

As the code is setup currently, it is easy to debug and runs in less than a minute. However, an important step that we haven't constructed fully yet is the alias generation. That will be our top priority going forward. In addition, we will continue to trim and optimize the existing code, and work on generalizing it so that the driver can be a modular "plug and play" type system for anyone to use.

