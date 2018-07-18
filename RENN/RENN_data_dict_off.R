library(officer)
library(plyr)
library(magrittr)
library(flextable)
library(magrittr)
library(ggplot2)
library(devEMF)
library(ggthemes)
library(scales)

#wd <- setwd("/Users/mfcox/Box/FAST-Clean Data/FAST_DataScience/RENN")
load("RENN_Cleaned.Rda")
load("questions.rda")

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


# identify columns that need summary stats (will be same for all tables)
factor.colnums <- c(2,3,7,8,9,10,13,26:55, 59:91)

# extract questions
questionsc <- questions[1, factor.colnums]


# df will contain all summary stats as 1 table
df <- data.frame()

# output folder will contain new csv for each table (separate files)
filebase <- "output/rennstat"


for (col in factor.colnums){
    filen <- paste(filebase, col,".csv", sep = "")
    print(file)
    temp <- count(renn.cleaned.data, col)
    perc <- c()
    s <- sum(temp$freq)
    for (i in temp$freq){
        perc <- c(perc, i/s)
    }
    perc <- percent(perc)
    temp$percentage <- perc
    df <- cbind.fill(df,temp)
}


write.csv(df, file = "renn_fullstatisticstable.csv")

# Create a word document to contain R outputs
doc <- read_docx()


freq <- as.numeric(temp$freq)

j <- 1


for (i in seq(1, length(df) - 3, by=3)) {
    startindex <- i
    endindex <- i+2
    temp <- body_add_par(doc, as.character(questionsc[[j]]))
    doc <- body_add_table(doc, na.omit(df[,(startindex:endindex)]), style = 'Table Professional', pos = "after")
    doc <- body_add_par(doc, "")
    t <- na.omit(df[,(startindex:endindex)])
    p <- ggplot(t, 
                aes_q(x = as.name(names(t)[1]), 
                      y = as.name(names(t)[2]))) + 
        geom_bar(stat = "identity") + 
        theme(axis.text.x = element_text(angle = 290, hjust = 0)) + 
        theme(axis.title.x=element_blank()) +
        ggtitle(colnames(t)[1])
    doc <- body_add_gg(doc,p, width = 3)
    j <- j+1
}

doc <- body_end_section(doc, landscape = TRUE)

print(doc, target = "~/Desktop/rennstatstest.docx")