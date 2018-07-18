
one.hot.all.columns <- function (filename, name.table) {

load(filename)
renn.data <- raw.data

library(qdapTools)
library(dplyr)


## F2F.ONLINE
one.hot.columns <- c("Q_sn_f2finteract", "Q_sn_onlineinteract")

for (cols in one.hot.columns){
    temp <- mtabulate(strsplit(as.character(renn.data[[cols]]), ","))
    '%!in%' <- function(x,y)!('%in%'(x,y))
    n <- nrow(temp)
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
    for (i in colnames(temp)){
        colname <- paste(cols,i, sep = "_", collapse = NULL)
        new.names <- c(new.names,colname)
    }
    new.names
    colnames(temp) <- new.names
    renn.data <- cbind(renn.data, temp)
}

## FOOD
food1 <- mtabulate(strsplit(as.character(renn.data[["Q_sn_comb_food1"]]), ","))
food2 <- mtabulate(strsplit(as.character(renn.data[["Q_sn_comb_food2"]]), ","))

food <- cbind(food1, food2)
food <- as.data.frame(do.call(cbind, by(t(food),INDICES=names(food),FUN=colSums)))

n <- nrow(food)
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

pa1 <- mtabulate(strsplit(as.character(renn.data[["Q_sn_comb_pa1"]]), ","))
pa2 <- mtabulate(strsplit(as.character(renn.data[["Q_sn_comb_pa2"]]), ","))

pa <- cbind(pa1, pa2)
pa <- as.data.frame(do.call(cbind, by(t(pa),INDICES=names(pa),FUN=colSums)))

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

screen1 <- mtabulate(strsplit(as.character(renn.data[["Q_sn_comb_screen1"]]), ","))
screen2 <- mtabulate(strsplit(as.character(renn.data[["Q_sn_comb_screen2"]]), ","))

screen <- cbind(screen1, screen2)
screen <- as.data.frame(do.call(cbind, by(t(screen),INDICES=names(screen),FUN=colSums)))

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
