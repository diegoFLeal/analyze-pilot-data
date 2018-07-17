library(tidyverse) 
library(ggplot2)   
library(ggthemes)  
library(onehot)

PredictLadder <- function(raw.data, name.table, new.filename) {
    ## Ladder Cleaning
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
    
    raw.data.missingladder <- raw.data[l,c("Q_ses_loc_ladder_1_x","Q_ses_loc_ladder_1_y","Q_ses_loc_ladder")]
    raw.data.hasladder <- raw.data[-c((1:2),l),c("Q_ses_loc_ladder_1_x","Q_ses_loc_ladder_1_y","Q_ses_loc_ladder")]
    
    nmissing = nrow(raw.data.missingladder)
    nhas = nrow(raw.data.hasladder)
    
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
        #print(min_dist_index)
        predictions <- c(predictions, as.numeric(raw.data.hasladder[min_dist_index, "Q_ses_loc_ladder"]))
    }
    
    for (i in 1:length(predictions)){
        raw.data$Q_ses_loc_ladder[l[i]] <- predictions[i]
    }
    raw.data
}

AliasNames <- function(raw.data, name.table, new.filename) {
    n <-  nrow(name.table)
    for (i in 1:n){
        raw.data <- data.frame(lapply(raw.data, function(x) {
            gsub(name.table[[i, 1]], name.table[[i, 2]], x)
        }))
    }
    raw.data
}

CalculateAge <- function(raw.data, name.table, new.filename) {
    ## Birthdate Column
    raw.data$birthdate <- as.Date(with(raw.data, paste(Q_age_1, Q_age_2, Q_age_3,sep="-")), "%B-%d-%Y")
    
    ## Age Column
    age_temp<-as.numeric(difftime(as.Date(as.character(as.POSIXct(raw.data$StartDate[-(1:2)]))), as.Date(as.character(as.POSIXct(raw.data$birthdate[-(1:2)], tz="UTC"))), units="weeks"))/52.25
    age_temp<-c(NA, age_temp)
    age_temp<-c(NA, age_temp)
    
    raw.data$CurrentAge <- age_temp  
    
    raw.data
}

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

removeRows <- function(rowNum, data) {
    newData <- data[-rowNum, , drop = FALSE]
    rownames(newData) <- NULL
    newData
}

DropRows <- function(raw.data, name.table, new.filename, additional.dropped.rows){
    raw.data <- removeRows(c(1,2,additional.dropped.rows), raw.data)
}

MoveColumns <- function(raw.data, name.table, new.filename){
    cleaned.1 <- raw.data[,c(1:13,124:193)]
    cleaned.2 <- raw.data[,-c(1:13,124:193)]
    clean.data <- data.frame(cleaned.1,cleaned.2)
    clean.data
}

ExportRda <- function(raw.data, new.filename){
    save(raw.data, file = new.filename)
}
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
    print("Moving Columns ...")
    cleaned.data <- MoveColumns(drop.rows, name.table, new.filename)
    if (export.csv){
       ExportRda(cleaned.data, new.filename) 
    } 
    cleaned.data
}
