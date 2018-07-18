load("SE_Cleaned.Rda")

library(igraph)

####################################################################################
####################################################################################
####################################################################################

f2f <- renn.cleaned.data[ , grepl( "Q_sn_f2finteract" , names( renn.cleaned.data ) ) ]
f2f <- f2f[,5:length(f2f)]
f2f$id <- as.character(renn.cleaned.data$Q_id)
f2f<-f2f[ , order(names(f2f))]
f2f<-f2f[,c(which(colnames(f2f)=="id"),which(colnames(f2f)!="id"))]

subjectnames <- c()

for (i in seq(0,length(f2f)-2)){
    if (i < 10){
        name <- paste("x60",i,sep="")
    } else {
        name <- paste("x6",i,sep="")
    }
    subjectnames <- c(subjectnames, name)
}
need <- c()
for (s in subjectnames){
    if (!(s %in% f2f$id)){
        need <- c(need,s)
    }
}
f2f<- f2f[!duplicated(f2f$id),]
for (n in need){
    f2f <- rbind(f2f,c(n, rep(0, length(f2f)-1)))
}


f2f <- f2f[order(f2f$id),]
f2fc <- f2f

f2fc$id <- NULL

colnames(f2fc) <- seq(0, length(f2fc)-1)
f2fm <- as.matrix(f2fc)

network=graph_from_adjacency_matrix(f2fm , mode='directed', diag=F )


png(filename="~/Desktop/f2f_se.png", heigh=4000, width=6000)
plot(network, 
     layout=layout.fruchterman.reingold, 
     main="fruchterman.reingold", 
     vertex.size = degree(network)*.1, 
     vertex.label.cex = 5,)
dev.off()
####################################################################################
####################################################################################
####################################################################################

on <- renn.cleaned.data[ , grepl( "Q_sn_online" , names( renn.cleaned.data ) ) ]
on <- on[,c(5:length(on))]
on$id <- as.character(renn.cleaned.data$Q_id)
on<-on[ , order(names(on))]
on<-on[,c(which(colnames(on)=="id"),which(colnames(on)!="id"))]

subjectnames <- c()

for (i in seq(0,length(on)-3)){
    if (i < 10){
        name <- paste("x60",i,sep="")
    } else {
        name <- paste("x6",i,sep="")
    }
    subjectnames <- c(subjectnames, name)
}
need <- c()
for (s in subjectnames){
    if (!(s %in% on$id)){
        need <- c(need,s)
    }
}
on<- on[!duplicated(on$id),]
for (n in need){
    on <- rbind(on,c(n, rep(0, length(on)-1)))
}


on <- on[order(on$id),]
onc <- on

onc$id <- NULL

colnames(onc) <- seq(0, length(onc)-1)
onm <- as.matrix(onc)

network=graph_from_adjacency_matrix(onm , mode='directed', diag=F )


png(filename="~/Desktop/onlineinteract_se.png", heigh=4000, width=6000)
plot(network, 
     layout=layout.fruchterman.reingold, 
     main="fruchterman.reingold", 
     vertex.size = degree(network)*.1, 
     vertex.label.cex = 2)
dev.off()

####################################################################################
####################################################################################
####################################################################################

sent <- renn.cleaned.data[ , grepl( "Q_sn_sentiment" , names( renn.cleaned.data ) ) ]
sent$id <- as.character(renn.cleaned.data$Q_id)
sent<-sent[ , order(names(sent))]
sent<-sent[,c(which(colnames(sent)=="id"),which(colnames(sent)!="id"))]

subjectnames <- c()

for (i in seq(0,length(sent)-2)){
    if (i < 10){
        name <- paste("x60",i,sep="")
    } else {
        name <- paste("x6",i,sep="")
    }
    subjectnames <- c(subjectnames, name)
}
need <- c()
for (s in subjectnames){
    if (!(s %in% sent$id)){
        need <- c(need,s)
    }
}
sent<- sent[!duplicated(sent$id),]
for (n in need){
    sent <- rbind(sent,c(n, rep(0, length(sent)-1)))
}


sent <- sent[order(sent$id),]
sentc <- sent
sentc$id <- NULL

colnames(sentc) <- seq(0, length(sentc)-1)
sentm <- as.matrix(sentc)

network=graph_from_adjacency_matrix(sentm , mode='directed', diag=F )


png(filename="~/Desktop/sentiment_se.png", heigh=4000, width=6000)
plot(network, 
     layout=layout.fruchterman.reingold, 
     main="fruchterman.reingold", 
     vertex.size = degree(network)*.1, 
     vertex.label.cex = 2)
dev.off()
