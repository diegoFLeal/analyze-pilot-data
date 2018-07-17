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