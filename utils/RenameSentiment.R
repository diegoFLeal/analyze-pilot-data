
RenameSentiment <- function (filename, name.table){

load("RENN_Cleaned.Rda")

sentiment <- raw.data[ , grepl( "Q_sn_sentiment" , names( raw.data ) ) ]

raw.data[ , grepl( "Q_sn_sentiment" , names( raw.data ) ) ] <- NULL


new.names <- c()

for (i in name.table$Value){
    new.names <- c(new.names, paste("Q_sn_sentiment", i, sep = "_"))
}
new.names

colnames(sentiment) <- new.names

raw.data <- cbind(raw.data, sentiment)

save(raw.data,file = filename)

raw.data

}