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

# load cleaned data & questions
load("RENN_Cleaned.Rda")
load("questions.rda")

# identify columns that need summary stats (will be same for all tables)
factor.colnums <- c(2,3,7,8,9,10,13,26:55, 59:91)

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






