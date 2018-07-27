### International Migration Flows in the Americas (1960-2000): A Story of Network Inequalities.
### code written by Diego F. Leal (www.diegoleal.info)
### Last update: 3/5/2018

#install.packages("devtools")
require(devtools)

remove.packages("statnet")
remove.packages("ergm")
remove.packages("xergm")
remove.packages("latticeExtra")
remove.packages("statnet")
remove.packages("tergm")
remove.packages("Rcpp")
remove.packages("dplyr")
remove.packages("reshape2")
remove.packages("network")
remove.packages("migest")

install.packages("network")
install.packages("sna")
install.packages("migest")
install.packages("snow")

install_version("statnet", version = "2015.11.0", repos = "http://cran.us.r-project.org")
install_version("ergm", version = "3.6.0", repos = "http://cran.us.r-project.org")
install_version("latticeExtra", version = "0.6-26", repos = "http://cran.us.r-project.org")
install_version("tergm", version = "3.3.1", repos = "http://cran.us.r-project.org")
install_version("Rcpp", version = "0.12.11", repos = "http://cran.us.r-project.org")
install_version("dplyr", version = "0.7.1", repos = "http://cran.us.r-project.org")
install_version("reshape2", version = "1.4.2", repos = "http://cran.us.r-project.org")
install_version("migest", version = "1.7.3", repos = "http://cran.us.r-project.org")
install_version("xergm", version = "1.3", repos = "http://cran.us.r-project.org")


library(reshape2)         # version 1.4.2
library(network)          # version 1.13.0
library(ergm)             # version 3.6.0
library(statnet)          # version 2015.11.0
library(sna)              # version 2.4
library(latticeExtra)     # version 0.6-26
library(xergm)            # version 1.3
library(migest)           # version 1.7.3
library(dplyr)            # version 0.7.1
library(Rcpp)             # version 0.12.11 
library(tergm)            # version 3.3.1
library(snow)             # version 0.4-2


sessionInfo()

# R version 3.4.1 (2017-06-30)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows Server >= 2012 x64 (build 9200)
# 
# Matrix products: default
# 
# locale:
#   [1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252   
# [3] LC_MONETARY=English_United States.1252 LC_NUMERIC=C                          
# [5] LC_TIME=English_United States.1252    
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] xergm_1.3            bindrcpp_0.2         gplots_3.0.1         gtools_3.5.0        
# [5] snow_0.4-2           Rcpp_0.12.11         dplyr_0.7.1          migest_1.7.4        
# [9] latticeExtra_0.6-26  lattice_0.20-35      RColorBrewer_1.1-2   statnet_2015.11.0   
# [13] ergm.count_3.2.2     tergm_3.3.1          sna_2.4              networkDynamic_0.9.0
# [17] ergm_3.6.0           statnet.common_4.0.0 network_1.13.0       reshape2_1.4.2      
# [21] devtools_1.13.5     
# 
# loaded via a namespace (and not attached):
# [1] lpSolve_5.6.13     splines_3.4.1      stats4_3.4.1       mgcv_1.8-17       
# [5] rlang_0.2.0        nloptr_1.0.4       pillar_1.2.1       glue_1.2.0        
# [9] withr_2.1.1        trust_0.1-7        plyr_1.8.4         bindr_0.1         
# [13] robustbase_0.92-8  stringr_1.3.0      caTools_1.17.1     coda_0.19-1       
# [17] memoise_1.1.0      knitr_1.17         permute_0.9-4      parallel_3.4.1    
# [21] curl_3.1           DEoptimR_1.0-8     ROCR_1.0-7         KernSmooth_2.23-15
# [25] gdata_2.18.0       vegan_2.4-6        texreg_1.36.23     lme4_1.1-15       
# [29] digest_0.6.12      stringi_1.1.6      grid_3.4.1         tools_3.4.1       
# [33] bitops_1.0-6       magrittr_1.5       tibble_1.4.2       cluster_2.0.6     
# [37] pkgconfig_2.0.1    MASS_7.3-47        Matrix_1.2-10      minqa_1.2.4       
# [41] assertthat_0.2.0   httr_1.3.1         boot_1.3-19        R6_2.2.2          
# [45] igraph_1.1.2       nlme_3.1-131       compiler_3.4.1  

############################# Import data#############################################################

setwd("~/diss/c3/input_data_files")
## import data
load("all_data_migration_Americas.Rdata")

####################### cleaning WB migrant stock data ###############################################

#get rid of unimportant columns
WB.stock<- subset(WB.stock, select=-c(Migration.by.Gender.Code,Migration.by.Gender.Name))

#get rid of unimportant rows
WB.stock<-WB.stock[1:53592,]

#relabel columns
labels.WB.stock<-c("orig","iso_orig","dest","iso_dest","s_1960s","s_1970s","s_1980s","s_1990s","s_2000s")
colnames(WB.stock)<-labels.WB.stock

#replace ".." with NAs
for(i in 5:ncol(WB.stock))
{
  for (j in 1:nrow(WB.stock))
  {
    if(WB.stock[j,i]== "..")
    {WB.stock[j,i]<-NA}
  }
}

#transforming columns with migrant stock data from type ch to type numeric 
WB.stock<-as.data.frame(WB.stock)

for (i in 5:ncol(WB.stock))
{
  WB.stock[,i] <-as.numeric(gsub(" ", "",WB.stock[,i], fixed = TRUE)) 
}  


######################## cleaning UN population data ############################

#extract column labels
labels.UN.pop<-unlist(UN.pop[16,])

#get rid of unimportant rows
UN.pop<-UN.pop[-(1:16),]

#attach labels to original object
colnames(UN.pop)<-labels.UN.pop
colnames(UN.pop)[3]<-"country_UN"
colnames(UN.pop)[5]<-"code_UN"

#get rid of unimportant columns
UN.pop    <-as.data.frame(UN.pop[,c("country_UN","code_UN","1960","1970","1980","1990","2000")])

#transforming columns with pop data from type ch to type numeric 
for (i in 3:ncol(UN.pop))
{
  UN.pop[,i] <-as.numeric(gsub(" ", "",UN.pop[,i], fixed = TRUE)) 
}  

#raw UN.pop data in thousands, so each cell that contains pop data is * by 3
Z<-UN.pop[,3:ncol(UN.pop)]
Y<-UN.pop[,1:2]
UN.pop<-cbind(Y,Z)

########################### cleaning UN births data ##############################

#extract column labels
labels.UN.births<-unlist(UN.births[16,])

#get rid of unimportant rows
UN.births<-UN.births[-(1:16),]

#attach labels to original object
colnames(UN.births)     <-labels.UN.births
colnames(UN.births)[3]  <-"country_UN"
colnames(UN.births)[5]  <-"code_UN"

#get rid of unimportant columns
UN.births <-as.data.frame(UN.births[,c("country_UN","code_UN","1960-1965",
                                       "1965-1970","1970-1975","1975-1980","1980-1985","1985-1990",
                                       "1990-1995","1995-2000")])

#transforming columns with pop data from type ch to type numeric 
for (i in 3:ncol(UN.births))
{
  UN.births[,i] <-as.numeric(gsub(" ", "",UN.births[,i], fixed = TRUE)) 
}  

#create new columns to store decades-long births
UN.births[,c("b_1960s","b_1970s","b_1980s","b_1990s")]<-9999

#compute decades-long births 
for (i in 1:nrow(UN.births))
{
  UN.births[i,"b_1960s"] <-(UN.births[i,"1960-1965"] + UN.births[i,"1965-1970"]) 
  UN.births[i,"b_1970s"] <-(UN.births[i,"1970-1975"] + UN.births[i,"1975-1980"]) 
  UN.births[i,"b_1980s"] <-(UN.births[i,"1980-1985"] + UN.births[i,"1985-1990"]) 
  UN.births[i,"b_1990s"] <-(UN.births[i,"1990-1995"] + UN.births[i,"1995-2000"]) 
}


#raw UN.pop data in thousands, so each cell that contains pop data is * by 3
Z<-UN.births[,11:ncol(UN.births)]
Y<-UN.births[,1:2]
UN.births<-cbind(Y,Z)


########################### cleaning UN deaths data ##############################

#extract column labels
labels.UN.deaths<-unlist(UN.deaths[16,])

#get rid of unimportant rows
UN.deaths<-UN.deaths[-(1:16),]

#attach labels to original object
colnames(UN.deaths)<-labels.UN.deaths
colnames(UN.deaths)[3]<-"country_UN"
colnames(UN.deaths)[5]<-"code_UN"

#get rid of unimportant columns
UN.deaths <-as.data.frame(UN.deaths[,c("country_UN","code_UN","1960-1965",
                                       "1965-1970","1970-1975","1975-1980","1980-1985","1985-1990",
                                       "1990-1995","1995-2000")])

#transforming columns with pop data from type ch to type numeric 
for (i in 3:ncol(UN.deaths))
{
  UN.deaths[,i] <-as.numeric(gsub(" ", "",UN.deaths[,i], fixed = TRUE)) 
}  

#create new columns to store decade-long deaths
UN.deaths[,c("d_1960s","d_1970s","d_1980s","d_1990s")]<-9999

#compute decade-long deaths 
for (i in 1:nrow(UN.deaths))
{
  UN.deaths[i,"d_1960s"] <-(UN.deaths[i,"1960-1965"] + UN.deaths[i,"1965-1970"]) 
  UN.deaths[i,"d_1970s"] <-(UN.deaths[i,"1970-1975"] + UN.deaths[i,"1975-1980"]) 
  UN.deaths[i,"d_1980s"] <-(UN.deaths[i,"1980-1985"] + UN.deaths[i,"1985-1990"]) 
  UN.deaths[i,"d_1990s"] <-(UN.deaths[i,"1990-1995"] + UN.deaths[i,"1995-2000"]) 
} 

#keep onlyv relevabt columns
Z<-UN.deaths[,11:ncol(UN.deaths)]
Y<-UN.deaths[,1:2]
UN.deaths<-cbind(Y,Z)

########################### cleaning TWN data ###############################

## select important cols and rows, labeling them
TWN.data<-TWN.data[c(2:6),c(2:4)]
colnames(TWN.data)<-c("population","births","deaths")
rownames(TWN.data)<-c("Y1960","Y1970","Y1980","Y1990", "Y2000")

#transforming columns with pop data from type ch to type numeric 
for (i in 1:ncol(TWN.data))
{
  TWN.data[,i] <-as.numeric(gsub( ",", "",TWN.data[,i], fixed = TRUE)) 
}  

######## storing TWN data in the deaths, births, and pop data frames ##################################
# remember: code_UN = 900 (i.e. the first row in the data frames) is a place holder for TWN  ##########

for (i in 3:ncol(UN.births))
{
  (UN.births[1,i]<-TWN.data[i-2,2])
  (UN.deaths[1,i]<-TWN.data[i-2,3])
}

for (i in 3:ncol(UN.pop))
{
  (UN.pop[1,i]<-TWN.data[i-2,1])
}

################ merge demographic data (births, deaths, population) ###########

UN.deaths  <-subset(UN.deaths, select= -country_UN)
UN.births  <-subset(UN.births, select= -country_UN)
UN.demo    <-merge(UN.pop, UN.deaths, by="code_UN")
UN.demo    <-merge(UN.demo, UN.births, by="code_UN")
UN.demo    <-merge(IDs,UN.demo, by="code_UN",sort=T)
UN.demo    <-UN.demo[with(UN.demo, order(id)), ]
colnames(UN.demo)[9:13]<-c("p_1960","p_1970","p_1980","p_1990","p_2000")


########## keep migrant stock data only for the 192 under analysis ##############
########## merge migrant stock data & demographic data             ##############

Z                <-as.data.frame(UN.demo[,4:ncol(UN.demo)])
colnames(Z)[3]   <-c("iso_dest")
ALL.data         <-merge(Z,WB.stock)
ALL.data         <-ALL.data[with(ALL.data, order(id)), ]

Z                <-as.data.frame(IDs[,c("id", "iso_abel")])
colnames(Z)      <-c("id_o","iso_orig")
ALL.data         <-merge(Z,ALL.data)
ALL.data         <-ALL.data[with(ALL.data, order(id_o,id)), ]
ALL.data         <-subset(ALL.data, select=-c(id_o))

## compute native population (UN pop ith country - total stock ith country) ######
## store native pop in the main diagonal (i.e. where iso_org = iso_dest)    ######

## sum of all migrants in each country in a given decade
V<-aggregate(ALL.data$s_1960s,by=list(Category=ALL.data$id),FUN=sum)
W<-aggregate(ALL.data$s_1970s,by=list(Category=ALL.data$id),FUN=sum)
X<-aggregate(ALL.data$s_1980s,by=list(Category=ALL.data$id),FUN=sum)
Y<-aggregate(ALL.data$s_1990s,by=list(Category=ALL.data$id),FUN=sum)
Z<-aggregate(ALL.data$s_2000s,by=list(Category=ALL.data$id),FUN=sum)

## store native pop in the main diagonal
U<-1
for (i in 1:nrow(ALL.data))
{
  if (ALL.data[i,"iso_orig"]==ALL.data[i,"iso_dest"])
  {
    ALL.data[i,"s_1960s"] <-ALL.data[i,"p_1960"] - V[U,"x"]
    ALL.data[i,"s_1970s"] <-ALL.data[i,"p_1970"] - W[U,"x"]
    ALL.data[i,"s_1980s"] <-ALL.data[i,"p_1980"] - X[U,"x"]
    ALL.data[i,"s_1990s"] <-ALL.data[i,"p_1990"] - Y[U,"x"]
    ALL.data[i,"s_2000s"] <-ALL.data[i,"p_2000"] - Z[U,"x"]
    U                     <-U + 1
  } 
}

################ save decades-long edgelists (with native pop) ##################

STOCK.60s <- subset(ALL.data, select=c(iso_orig, iso_dest, s_1960s))
STOCK.70s <- subset(ALL.data, select=c(iso_orig, iso_dest, s_1970s))
STOCK.80s <- subset(ALL.data, select=c(iso_orig, iso_dest, s_1980s))
STOCK.90s <- subset(ALL.data, select=c(iso_orig, iso_dest, s_1990s))
STOCK.00s <- subset(ALL.data, select=c(iso_orig, iso_dest, s_2000s))

############### merge edgelist with CEPII dist data #############################

Z                    <-as.data.frame(UN.demo[,4:ncol(UN.demo)])
colnames(Z)[4]       <-c("iso_d")
CEPII.dist           <-merge(Z,CEPII.all,by="iso_d")
CEPII.dist           <-CEPII.dist[with(CEPII.dist, order(id)), ]

Z                    <-as.data.frame(IDs[,c("id", "iso_cepii")])
colnames(Z)          <-c("id_o","iso_o")
CEPII.dist           <-merge(Z,CEPII.dist,by="iso_o")
CEPII.dist           <-CEPII.dist[with(CEPII.dist, order(id_o,id)), ]
CEPII.dist           <-subset(CEPII.dist, select=c(id_o, iso_o,id,iso_d,distcap))
colnames(CEPII.dist) <-c("id_o","iso_orig","id_d","iso_dest","distcap") 


#################################################################################
#################################################################################
############### PREPARE DATA FOR ESTIMATON OF MIGRATION FLOWS ###################
#################################################################################
#################################################################################


##### births and deaths by decade in one (long) data frame #####################

W             <-as.data.frame(UN.demo[,c("iso_abel","b_1960s","d_1960s")])
W$period      <-"1960-1970"            
X             <-as.data.frame(UN.demo[,c("iso_abel","b_1970s","b_1970s")])
X$period      <-"1970-1980"  
Y             <-as.data.frame(UN.demo[,c("iso_abel","b_1980s","d_1980s")])
Y$period      <-"1980-1990"  
Z             <-as.data.frame(UN.demo[,c("iso_abel","b_1990s","b_1990s")])
Z$period      <-"1990-2000"  
colnames(W)   <-colnames(X)<-colnames(Y)<-colnames(Z)<-c("iso3","b","d","period")
df0           <-rbind(W,X,Y,Z)
rownames(df0) <-NULL
df0           <- df0[,c("iso3","period","b","d")]

############## labels ##########################################################

df1           <-subset(IDs,select=c(iso_abel,country,region,region_code))
colnames(df1) <-c("iso3","country","region","reg")

############## distances: from edgelist to matrix ##############################

dd            <-subset(CEPII.dist,select=c(id_o,id_d,distcap))
colnames(dd)  <-c("iso_abel_o","iso_abel_d","distcap")
dd            <-acast(dd, iso_abel_o ~ iso_abel_d, value.var="distcap")
Z             <-unlist(UN.demo[,"iso_abel"])     
colnames(dd)  <-rownames(dd)  <-Z

############## migrant stock with native pop in long form #####################

V             <-STOCK.60s
V$decade      <-1960
W             <-STOCK.70s
W$decade      <-1970
X             <-STOCK.80s
X$decade      <-1980
Y             <-STOCK.90s
Y$decade      <-1990
Z             <-STOCK.00s
Z$decade      <-2000
colnames(V)   <-colnames(W)<-colnames(X)<-colnames(Y)<-colnames(Z) <-c("orig","dest","stock","decade")
stw           <-rbind(V,W,X,Y,Z)
rownames(stw) <-NULL
stw           <- stw[,c("decade","dest","orig","stock")]

############## sorting stw so that it is ordered as all other data sets ##########

Z                 <-as.data.frame(IDs[,c("id", "iso_abel")])
colnames(Z)       <-c("id","dest")
stw               <-merge(Z,stw,by="dest")
stw               <-stw[with(stw, order(decade, id)), ]

Z                 <-as.data.frame(IDs[,c("id", "iso_abel")])
colnames(Z)       <-c("id_o","orig")
stw               <-merge(Z,stw,by="orig")
stw               <-stw[with(stw, order(decade,id,id_o)), ]
stw               <-subset(stw, select=c(decade, orig,dest,stock))
stw               <-stw[,c("decade","dest","orig","stock")]
rownames(stw)     <-NULL

setwd("~/diss/c3/output_data_files")
write.csv(x=stw,file="Global_stocks_with_native_pop_1960-2000_edgelist.csv",row.names=TRUE)

#################################################################################
#################################################################################
#####################Esimating migration flows ##################################
#################################################################################
#################################################################################

#set up empty data.frame (df2) and array (st) to store results
decades  <- unique(df0$period)
years    <- c("1960", "1970", "1980", "1990", "2000" )
iso      <- df1$iso3
df2      <- expand.grid(orig = iso, dest = iso, pob=iso, period=decades)
df2$flow <- NA
## arrays dims nrows = 192, and five time periods (stw$decade)
st <- array(stw$stock, c(nrow(IDs),nrow(IDs),length(unique(stw$decade))), dimnames=list(pob=iso, por=iso, time=years))

################
## Estimation  #
################

#offset for orig X dest X pob tables
g_dist     <- array(NA, c(nrow(IDs),nrow(IDs),nrow(IDs)), dimnames=list(orig=iso, dest=iso, pob=iso))
g_dist[,,] <- 1/dd

## locate position of the the variables "b_1960s" and "d_1960s", 
## the first variable where births and deaths are recorder in WB.demo, respectively
allColLabels<-colnames(UN.demo[,])

for (i in 1:length(allColLabels))
{
  if (allColLabels[i]=="b_1960s")
  {count_b<-i } 
  if (allColLabels[i]=="d_1960s")
  {count_d<-i } 
}

## Main Loop to carry out the estimation using the flows-from-stocks methodology 

count_p <-1

s<-Sys.time()                                              ### keep track of time
for(p in decades)                                          ### for each period (1960-1970, 1970-1980, etc)
{  
  t0 <- years[count_p]                                     ### store t0 in the ith decade (e.g. 1960 if period is 1960-1970)
  t1 <- years[count_p + 1]                                 ### store t1 in the ith decade (e.g. 1970 if period is 1960-1970)
  message("t0 = P1 = ", t0)                                ### print t0
  message("t1 = P2 = ", t1)                                ### print t1
  message("births data = ",colnames(UN.demo)[count_b])     ### print name of birth data for the ith decade (see UN.demo object)
  message("deaths data = ",colnames(UN.demo)[count_d])     ### print name of death data for the ith decade (see UN.demo object)
  gf <- ffs(                                               ### use fss function, flows from stocks methodology
    P1 = st[,,t0],                                         ### P1= stock data at time t (e.g. 1960)
    P2 = st[,,t1],                                         ### P2= stock data at t1me + 1 (e.g. 1970)
    b = UN.demo[,count_b],                                 ### births in decade p
    d = UN.demo[,count_d],                                 ### deaths in decade p
    m = g_dist,                                            ### geo distances between all countries
    method = "outside"                                     ### this method reproduces the results in Abel 2013  
  )
  df2$flow[df2$period==p] <- round(c(gf$mu))               ### save estimates in df in column "flow"
  count_b <- count_b + 1                                   ### go to births in next decade
  count_d <- count_d + 1                                   ### go to deaths in next decade
  count_p <- count_p + 1                                   ### go to migrant stock in next decade   
}

#Edgelsit: country to country migration flows by decade
FLOWS.all_edgelist      <-df2 %>% group_by(period, orig, dest) %>% summarise(flow = sum(flow)) %>% mutate(flow = ifelse(orig!=dest, flow, 0))

#Export all the data
setwd("~/diss/c3/output_data_files")
write.csv(x=FLOWS.all_edgelist,file="Global_flows_1960-2000_edgelist.csv",row.names=TRUE)

#################################################################################
#################################################################################
################# Extra Data Preparation  #######################################
#################################################################################
#################################################################################

## working directory
setwd("~/diss/c3/input_data_files")

#from edgelist to array od matrices
FLOWS.all_matrix        <-array(FLOWS.all_edgelist$flow, c(nrow(IDs),nrow(IDs),length(unique(FLOWS.all_edgelist$period))), dimnames=list(pob=iso, por=iso, time=decades))

#create a list to store to store migration flows within the americas
FLOWS.americas_matrix   <-vector("list",nrow(IDs.americas)) ## egonets in matrix format with relationship type and including NAs

for (i in 1:length(decades))
{  
  Z<-as.data.frame(t(FLOWS.all_matrix[,,decades[i]]))     ## extract one matrix                           
  Y<-cbind(subset(IDs,select=iso_abel),Z)                 ## add country labels as a column in the matrix  
  X<-merge(IDs.americas,Y,by="iso_abel",sort=F)           ## merge in order to leave on countries in the Americas
  rownames(X)<-X[,"iso_2"]                                ## give row names the names of the countries in the Americas
  X<-X[,(ncol(IDs.americas)+1):ncol(X)]                   ## remove all columns that do not contain flow data
  Z<-t(X)                                                 ## transpose the matrix and do the same as above to get a rectangular matrix
  Y<-cbind(subset(IDs,select=iso_abel),Z)
  X<-merge(IDs.americas,Y,by="iso_abel",sort=F) 
  rownames(X)<-X[,"iso_2"]
  X<-X[,(ncol(IDs.americas)+1):ncol(X)]
  X<-t(X)
  FLOWS.americas_matrix[[i]] <-X                         ## store the resulting matrix in the list FLOWS.americas_matrix 
}

##save the decades-long migration flow matrices in csv and txt format (uncomment the write functions to export the data)
for(i in 1:length(decades))
{
  net  <-(FLOWS.americas_matrix[[i]])
  setwd("~/diss/c3/output_data_files")
  write.csv(x=net,file=paste("Americas_flows",decades[i],"matrix.csv",sep="_"),row.names=T)
  setwd("~/diss/c3/input_data_files")
  #write.table(x=FLOWS.americas_matrix[[i]],file=paste("Americas_flows",decades[i],"Circos.txt",sep="_"), sep="\t")
  setwd("~/diss/c3/output_data_files")
  #NOT US
  net2 <-net[-c(36),-c(36)]
  #write.table(x=net2,file=paste("No_US_flows",decades[i],"Circos.txt",sep="_"), sep="\t")
  #NOT US-CA
  net2 <-net[-c(9, 36),-c(9, 36)]
  #write.table(x=net2,file=paste("No_US-CA_flows",decades[i],"Circos.txt",sep="_"), sep="\t")
  #ANDEAN
  net2 <-net[c(7,11,15,30,38),c(7,11,15,30,38)]
  #write.table(x=net2,file=paste("Andean_flows",decades[i],"Circos.txt",sep="_"), sep="\t")
  #CARIBBEAN
  net2 <-net[c(1,3,4,5,12,13,14,17,18,19,21,22,24,25,31,32,33,34,35),c(1,3,4,5,12,13,14,17,18,19,21,22,24,25,31,32,33,34,35)]
  #write.table(x=net2,file=paste("Caribbean_flows",decades[i],"Circos.txt",sep="_"), sep="\t")
  #CENTRAL AMERICAN
  net2 <-net[c(6,16,20,23,27,28),c(6,16,20,23,27,28)]
  #write.table(x=net2,file=paste("Central_American_flows",decades[i],"Circos.txt",sep="_"), sep="\t")
  net2 <-net[c(6,16,20,23,27,28,1,3,4,5,12,13,14,17,18,19,21,22,24,25,31,32,33,34,35),c(6,16,20,23,27,28,1,3,4,5,12,13,14,17,18,19,21,22,24,25,31,32,33,34,35)]
  #write.table(x=net2,file=paste("Central_American_&_Caribbean_flows",decades[i],"Circos.txt",sep="_"), sep="\t")
  #Southern Cone
  net2 <-net[c( 2,8,10,29,37),c( 2,8,10,29,37)]
  #write.table(x=net2,file=paste("Southern_Cone",decades[i],"Circos.txt",sep="_"), sep="\t")
  net2 <-net[c( 2,8,10,29,37,7,11,15,30,38),c( 2,8,10,29,37,7,11,15,30,38)]
  #write.table(x=net2,file=paste("South_American",decades[i],"Circos.txt",sep="_"), sep="\t")
}

#################################################################
#################################################################
############## TERGMS: data processing                ###########
#################################################################
#################################################################

set.seed(5)

###### this loops binarizes trhe flow matrices based on top-destinations per country ####

top_destinations <-15

for (d in 1:length(decades))                          ## loop through each flow network
{
  for (a in 1:nrow(FLOWS.americas_matrix[[d]]))        ## loop through each row (i.e. sending country)
  {
    for (b in 1:top_destinations)                       ## select top_destination per sending country
    {
      flow<-max(FLOWS.americas_matrix[[d]][a,])          ## store the bth top destination 
      for (c in 1:nrow(FLOWS.americas_matrix[[d]]))      ## loop thorugh each column  
      {
        if ((FLOWS.americas_matrix[[d]][a,c] == flow))    ## if the cell [a,c] == flow
        {
          FLOWS.americas_matrix[[d]][a,c]<- FLOWS.americas_matrix[[d]][a,c] * -1  ## flag that cell (that top destination) with a -1
        } 
      }
    }
  }
  X<-which(FLOWS.americas_matrix[[d]][,]>0)                            ## get the indices/positions of all cell > 0   
  FLOWS.americas_matrix[[d]]<-replace(FLOWS.americas_matrix[[d]],X,0)  ## make the cells identified above = 0
  X<-which(FLOWS.americas_matrix[[d]][,]<0)                            ## get the indices/positions of all cells < 0 (i.e. the top destinations of each country)
  FLOWS.americas_matrix[[d]]<-replace(FLOWS.americas_matrix[[d]],X,1)  ## make the ceels idientifies above = 1
}

#### Import, clean Income Adjusted Index (AICI) data

AICI             <-AICI[-c(1,3),]   ## get rid of unnecessary cells
AICI[1,2]        <-"country"        ## change some labels
X                <-unlist(AICI[1,]) ## create a list of col labels
X[1]             <-"iso_abel"       ## change the label of the first column for future merging
colnames(AICI)   <-X                ## assign X to AICI
AICI             <-AICI[-c(1),]     ## get rid of unnecessary rows
AICI             <-AICI[,-c(3:13)]  ## get rid of unnecessary cols
X                <-colnames(AICI)   ## extract the col labels of AICI

for (i in 2:length(X))              ## append _AICI to each col label in AICI
{
  Y<-X[i]
  Y<-paste(Y,"AICI",sep="_")
  X[i]<-Y
}
colnames(AICI)   <-X                ## attach the new vector of labels to AICI

### merging AICI data with the original nodel-level data set (i.e. IDs.americas)
IDs.americas<- merge(IDs.americas,AICI,by="iso_abel",sort=F) ## merge countries IDs object (i.e. node-level attributes) and AICI

## get labels that are repeated in the original flow matrices and the node-level attribute data set
ZZ<-intersect(colnames(FLOWS.americas_matrix[[1]]),IDs.americas$iso_2)

## drop columns and row that are not in ZZ
for (i in 1:length(decades))
{
  FLOWS.americas_matrix[[i]]<-FLOWS.americas_matrix[[i]][(rownames(FLOWS.americas_matrix[[i]]) %in% ZZ), (colnames(FLOWS.americas_matrix[[i]]) %in% ZZ)]
}

############# clean and process poltitical conflict data ################

### getting rid of unimporant variables
war <-subset(war,select=c(iso_code,year,civtot))

## relabeling first columns for merging
colnames(war)[1] <-"iso_abel"

## merge war data and node-level attributes
war<-merge(IDs.americas,war,by="iso_abel",sort=F)

## sort the merged object
war<-war[with(war, order(id, year)), ]

## create a vector with the needed years of conflict data
Z<-seq(from=1950,to=1999,by=1)

## get rid of rows (i.e. years) that are in Z
for (i in 1:nrow(war))
{
  war<-war[(war$year %in% Z),]
}

## create a new variable in the data frame to store the conflict data
war$conflict<-NA

## sum the number of conflicts (i.e."civtot) for each country in a given decade
for (b in 1:(nrow(war)/10))  
{
  X                <-10*b                         ## year in which a decade ends
  Y                <-X -9                         ## year in which a decade begins  
  war[b*10,"conflict"] <- sum(war[Y:X,"civtot"]) ## sum conflicts in the decade
}

## get rids of NAs (i.e. years different from those who corresponde to the end of a given decade)
war<-war[complete.cases(war$conflict),]

## order the war object
war<-war[with(war, order(year, id)), ]

## create labels for the war object
Z <- c("conflict_50s", "conflict_60s", "conflict_70s", "conflict_80s", "conflict_90s")

## assignt the nnumber of conflicts to each country
for (i in 1:length(Z))
{
  X <- nrow(IDs.americas) * i                            ## row corresponding to a new country
  Y <- X - (nrow(IDs.americas) - 1)                      ## row corresponding to the same counry as above
  IDs.americas<-cbind(war[Y:X,"conflict"],IDs.americas)  ## cbind war (i.e. conflicts) for the ith decade with the node-level data set (IDs.americas)
  colnames(IDs.americas)[1]<-Z[i]                        ## label the new conflict variable
}

#region, language, adjusted GDP per capita, & political violence 
region         <-IDs.americas$region_num
language       <-IDs.americas$language_num
aici           <-vector("list",nrow(IDs.americas))
conflict       <-vector("list",nrow(IDs.americas))

aici_60        <-as.data.frame(as.numeric((IDs.americas[,"1960_AICI"])))
aici_60        <-aici_60*100
aici_65        <-as.data.frame(as.numeric((IDs.americas[,"1965_AICI"])))
aici_65        <-aici_65*100
aici_70        <-as.data.frame(as.numeric((IDs.americas[,"1970_AICI"])))
aici_70        <-aici_70*100 
aici_75        <-as.data.frame(as.numeric((IDs.americas[,"1975_AICI"])))
aici_75        <-aici_75*100 
aici_80        <-as.data.frame(as.numeric((IDs.americas[,"1980_AICI"])))
aici_80        <-aici_80*100 
aici_85        <-as.data.frame(as.numeric((IDs.americas[,"1985_AICI"])))
aici_85        <-aici_85*100 
aici_90        <-as.data.frame(as.numeric((IDs.americas[,"1990_AICI"])))
aici_90        <-aici_90*100 
aici_95        <-as.data.frame(as.numeric((IDs.americas[,"1995_AICI"])))
aici_95        <-aici_95*100
aici_00        <-as.data.frame(as.numeric((IDs.americas[,"2000_AICI"])))
aici_00        <-aici_00*100

aici_60s       <-((aici_60)+(aici_65)+(aici_70)) / 3 # aici in the 1960s
aici_70s       <-((aici_70)+(aici_75)+(aici_80)) / 3 # aici in the 1970s
aici_80s       <-((aici_80)+(aici_85)+(aici_90)) / 3 # aici in the 1980s
aici_90s       <-((aici_90)+(aici_95)+(aici_00)) / 3 # aici in the 1990s

#put the aici variables in the aici list
aici[[1]]      <-aici_60s
aici[[2]]      <-aici_70s
aici[[3]]      <-aici_80s
aici[[4]]      <-aici_90s

#put the conflict variables in the conflict list
conflict[[1]]  <-IDs.americas$conflict_60s
conflict[[2]]  <-IDs.americas$conflict_70s
conflict[[3]]  <-IDs.americas$conflict_80s
conflict[[4]]  <-IDs.americas$conflict_90s

#create empty lists to store results
results        <-vector("list",length(decades)) 
degeneracy     <-vector("list",length(decades)) 
AllNets        <-vector("list",length(decades))

#export node levelcovars
setwd("~/diss/c3/output_data_files")
write.csv(x=IDs.americas,file="node_level_covars.csv",row.names=T)

############################################################
############################################################
#################### Create "network" Objects ##############
############################################################
############################################################

setwd("~/diss/c3/output_data_files/ERGM_GOF")

#create the network
for (i in 1:length(decades))
{
  net <-as.network.matrix(FLOWS.americas_matrix[[i]],matrix.type = "adjacency",directed=T)
  network.vertex.names(net)<-IDs.americas$iso_2
  odegsqrt<-sqrt(degree(net,cmode="outdegree"))
  idegsqrt<-sqrt(degree(net,cmode="indegree"))
  
  # setting vertex attributes
  set.vertex.attribute(net,"region",region)
  set.vertex.attribute(net,"language",language)
  set.vertex.attribute(net,"odegsqrt",odegsqrt)
  set.vertex.attribute(net,"idegsqrt",idegsqrt)
  set.vertex.attribute(net,"aici",aici[i])
  set.vertex.attribute(net,"conflict",conflict[i])
  
  # save the network in the object AllNets
  AllNets[[i]]<-net
}

################################################################################
################## descriptive network stats ###################################
################################################################################

#Describe the networks
gden(AllNets)  # density
centralization(AllNets,degree,mode="digraph",normalize=T) #degree centralization
AllNets[1]     # number of ties
AllNets[2]     # number of ties
AllNets[3]     # number of ties
AllNets[4]     #number of ties
dyad.census(AllNets)  #dyad census
triad.census(AllNets) #triad census

#indegree distributions
indegree1<-(degree(AllNets, g=1, gmode="digraph", cmode="indegree"))
indegree2<-(degree(AllNets, g=2, gmode="digraph", cmode="indegree"))
indegree3<-(degree(AllNets, g=3, gmode="digraph", cmode="indegree"))
indegree4<-(degree(AllNets, g=4, gmode="digraph", cmode="indegree"))

#mean indegree
mean(degree(indegree1))
mean(degree(indegree2))
mean(degree(indegree3))
mean(degree(indegree4))

#sd of degree
sd(degree(indegree1))
sd(degree(indegree2))
sd(degree(indegree3))
sd(degree(indegree4))

#plot degree distributions
hist(indegree1, xlab="Indegree", main="Indegree distribution, 1960s") 
hist(indegree2, xlab="Indegree", main="Indegree distribution, 1970s") 
hist(indegree3, xlab="Indegree", main="Indegree distribution, 1980s") 
hist(indegree4, xlab="Indegree", main="Indegree distribution, 1990s") 

#ploting the networks
plotCoordinates   <-vector("list",length(decades))

par(mfrow = c(2,2), mar = c(0,0,1,0))
for (i in 1:length(AllNets)) 
{
  plotCoordinates[[i]]<-plot(network(AllNets[[i]]),main=paste("t =",decades[i]),
                             usearrows = T,edge.col="azure2",vertex.cex = 1.5)
}

############## simulated 500 dyad census conditioned-graphs for each network and compute a triad census in each simulated network

#create a list to store the simulatd nets
g        <-vector("list",500) 

#network 1: simulate networks

for (i in 1:length(g))
{
  g[[i]]<-as.sociomatrix(rguman(n=1, 32, mut = 0.25, asym = 0.40, null = 0.35,method = "probability", return.as.edgelist = FALSE))
}

#network 1: compute triad census

triads<-triad.census(AllNets[[1]], mode="digraph")

for(i in 1:length(g))
{
  triads.i<-triad.census(g[[i]], mode="digraph")
  triads<-rbind(triads,triads.i)
}

#labeling the triads object
colnames(triads)<-c("T-003","T-012","T-102","T-021D","T-021U",   
                    "T-021C", "T-111D", "T-111U", "T-030T","T-030C",
                    "T-201", "T-120D", "T-120U", "T-120C", "T-210",
                    "T-300")

#network 1: means and sd of relevant triadic configurations of simulated networks

mean(triads[2:nrow(triads),"T-030T"])
sd(triads[2:nrow(triads),"T-030T"])
triads[1,"T-030T"] # observed triad

mean(triads[2:nrow(triads),"T-021U"])
sd(triads[2:nrow(triads),"T-021U"])
triads[1,"T-021U"] # observed triad

mean(triads[2:nrow(triads),"T-030C"])
sd(triads[2:nrow(triads),"T-030C"])
triads[1,"T-030C"] # observed triad

mean(triads[2:nrow(triads),"T-021C"])
sd(triads[2:nrow(triads),"T-021C"])
triads[1,"T-021C"] # observed triad

#########################################

#create a list to store the simulatd nets
g        <-vector("list",500) 

#network 2: simulate networks

for (i in 1:length(g))
{
  g[[i]]<-as.sociomatrix(rguman(n=1, 32, mut = 0.25, asym = 0.38, null = 0.37,method = "probability", return.as.edgelist = FALSE))
}

#network 2: compute triad census

triads<-triad.census(AllNets[[2]], mode="digraph")

for(i in 1:length(g))
{
  triads.i<-triad.census(g[[i]], mode="digraph")
  triads<-rbind(triads,triads.i)
}

#labeling the triads object
colnames(triads)<-c("T-003","T-012","T-102","T-021D","T-021U",   
                    "T-021C", "T-111D", "T-111U", "T-030T","T-030C",
                    "T-201", "T-120D", "T-120U", "T-120C", "T-210",
                    "T-300")

#network 2: means and sd of relevant triadic configurations of simulated networks
mean(triads[2:nrow(triads),"T-030T"])
sd(triads[2:nrow(triads),"T-030T"])
triads[1,"T-030T"] # observed triad

mean(triads[2:nrow(triads),"T-021U"])
sd(triads[2:nrow(triads),"T-021U"])
triads[1,"T-021U"] # observed triad

mean(triads[2:nrow(triads),"T-030C"])
sd(triads[2:nrow(triads),"T-030C"])
triads[1,"T-030C"] # observed triad

mean(triads[2:nrow(triads),"T-021C"])
sd(triads[2:nrow(triads),"T-021C"])
triads[1,"T-021C"] # observed triad

###########################################

#create a list to store the simulatd nets
g        <-vector("list",500) 

#network 3: simulate networks

for (i in 1:length(g))
{
  g[[i]]<-as.sociomatrix(rguman(n=1, 32, mut = 0.24, asym = 0.41, null = 0.35,method = "probability", return.as.edgelist = FALSE))
}

#network 3: compute triad census
triads<-triad.census(AllNets[[3]], mode="digraph") #empirical network

for(i in 1:length(g))
{
  triads.i<-triad.census(g[[i]], mode="digraph")   #simulated network
  triads<-rbind(triads,triads.i)
}

#labeling the triads object
colnames(triads)<-c("T-003","T-012","T-102","T-021D","T-021U",   
                    "T-021C", "T-111D", "T-111U", "T-030T","T-030C",
                    "T-201", "T-120D", "T-120U", "T-120C", "T-210",
                    "T-300")

#network 3: means and sd of relevant triadic configurations of simulated networks
mean(triads[2:nrow(triads),"T-030T"])
sd(triads[2:nrow(triads),"T-030T"])
triads[1,"T-030T"] # observed triad

mean(triads[2:nrow(triads),"T-021U"])
sd(triads[2:nrow(triads),"T-021U"])
triads[1,"T-021U"] # observed triad

mean(triads[2:nrow(triads),"T-030C"])
sd(triads[2:nrow(triads),"T-030C"])
triads[1,"T-030C"] # observed triad

mean(triads[2:nrow(triads),"T-021C"])
sd(triads[2:nrow(triads),"T-021C"])
triads[1,"T-021C"] # observed triad

#############################################

#create a list to store the simulatd nets
g        <-vector("list",500) 

#network 4: simulate networks

for (i in 1:length(g))
{
  g[[i]]<-as.sociomatrix(rguman(n=1, 32, mut = 0.27, asym = 0.38, null = 0.35,method = "probability", return.as.edgelist = FALSE))
}

#network 4: compute triad census

triads<-triad.census(AllNets[[4]], mode="digraph")

for(i in 1:length(g))
{
  triads.i<-triad.census(g[[i]], mode="digraph")
  triads<-rbind(triads,triads.i)
}

#labeling the triads object
colnames(triads)<-c("T-003","T-012","T-102","T-021D","T-021U",   
                    "T-021C", "T-111D", "T-111U", "T-030T","T-030C",
                    "T-201", "T-120D", "T-120U", "T-120C", "T-210",
                    "T-300")

#network 1: means and sd of relevant triadic configurations of simulated networks
mean(triads[2:nrow(triads),"T-030T"])
sd(triads[2:nrow(triads),"T-030T"])
triads[1,"T-030T"] # observed triad

mean(triads[2:nrow(triads),"T-021U"])
sd(triads[2:nrow(triads),"T-021U"])
triads[1,"T-021U"] # observed triad

mean(triads[2:nrow(triads),"T-030C"])
sd(triads[2:nrow(triads),"T-030C"])
triads[1,"T-030C"] # observed triad

mean(triads[2:nrow(triads),"T-021C"])
sd(triads[2:nrow(triads),"T-021C"])
triads[1,"T-021C"] # observed triad

################################################################
#### extract/identify every cyclic triads or transitive triad
################################################################

## select a matrix
XX<-as.matrix.network(AllNets[[3]],matrix.type = "adjacency")

## flag (local variables)
l <-0
j <-1

##create a matrix (store) to store every single triad of a given type (400 is just a random number)
### since i kn ow there there no more than 400 cyclic or transitive triads in each decade
store    <-matrix(nrow=400,ncol=3)
store    <-as.data.frame(store)
store[,] <-999999

##loop through every triad

for (u in 1:nrow(XX))
{
  for (v in 1:nrow(XX))
  {
    for (w in 1:nrow(XX))
    {
      #uncomment first line to extact cyclic triads. Uncomment second line to extract transitive triads 
      if ((XX[u,v]==1) & (XX[v,w]==1) & (XX[w,u]==1) & (XX[v,u]==0) & (XX[w,v]==0) & (XX[u,w]==0))
        #if ((XX[u,v]==1) & (XX[v,w]==1) & (XX[w,u]==0) & (XX[v,u]==0) & (XX[w,v]==0) & (XX[u,w]==1))  
      {
        #classify the triad
        l<-triad.classify(AllNets[[3]], tri=c(u, v, w), mode="digraph")
        print(c(u,v,w,l))
        #sort the nodes in the triad 
        triad<-sort(c(u,v,w))
        #put the three (ordered) nodes of the triad in the data frame "store" 
        store[j,1]<-triad[1]
        store[j,2]<-triad[2]
        store[j,3]<-triad[3]
        j <- j + 1
      }
    }
  }
}

#sort the data frame
sort(store)
#eliminate repeated triads
unique.data <- unique( store[ ,] )
#relabel rows in ascending order
row.names(unique.data)<-NULL
# show all the triads
unique.data

##################################################################
#################### TERGMs:  ####################################
##################################################################


##transforming data objects into binary (flow) matrices, flows that are < 50 = 0
t1<-FLOWS.americas_matrix[[1]]
t2<-FLOWS.americas_matrix[[2]]
t3<-FLOWS.americas_matrix[[3]]
t4<-FLOWS.americas_matrix[[4]]

#stroring the matrices in a list
flows<-mget(paste0("t",1:4))

conflict_60s<-as.numeric(IDs.americas$conflict_60s)
conflict_70s<-as.numeric(IDs.americas$conflict_70s)
conflict_80s<-as.numeric(IDs.americas$conflict_80s)
conflict_90s<-as.numeric(IDs.americas$conflict_90s)

aici_60s<-((as.numeric(IDs.americas$`1960_AICI`) * 1000) + (as.numeric(IDs.americas$`1965_AICI`) * 1000) ) / 2
aici_70s<-((as.numeric(IDs.americas$`1970_AICI`) * 1000) + (as.numeric(IDs.americas$`1975_AICI`) * 1000) ) / 2
aici_80s<-((as.numeric(IDs.americas$`1980_AICI`) * 1000) + (as.numeric(IDs.americas$`1985_AICI`) * 1000) ) / 2
aici_90s<-((as.numeric(IDs.americas$`1990_AICI`) * 1000) + (as.numeric(IDs.americas$`1995_AICI`) * 1000) ) / 2


#list of dependent flow networks
dep<- preprocess(flows,aici_60s,aici_70s,aici_80s,aici_90s,
                 conflict_60s,conflict_70s,conflict_80s,conflict_90s,
                 language,region, 
                 lag=T,covariate=F,na=NA,na.method="fillmode")

#checking the resutls, the dependnet networks shouls start at time 2 because
# the estimation is contidioned on the first network
length(dep)
sapply(flows,dim)
sapply(dep,dim)
rownames(dep[[3]])

#creating a lagged dependent network to use as a covariate because current flows
#are probably the result of previous ones.
mem.stability<- preprocess(flows,region,aici_60s,
                           lag=T,covariate=T,memory="stability", na=NA,na.method="fillmode")

#checking the resutls. The last time step should be removed and dimensions
#should be adjusted.
length(mem.stability)
sapply(mem.stability,dim)
sapply(mem.stability,dim)
rownames(mem.stability[[3]])

#covariate: region of the world
region.cov<-preprocess(region, flows,
                       lag=F,covariate=T)

#covariate: region of the world
language.cov<-preprocess(language, flows,
                         lag=F,covariate=T)

#single-period delayed reciprocity:
#transpose the flow matrices and create a logged covariate
delrecip<-lapply(flows,t)
delrecip<-preprocess(delrecip,aici_60s,region,
                     lag=T,covariate=T,
                     na=NA,na.method="fillmode")

#adding, node attributes to the dependnet network: region, 
# indegree, outdegree, income, language, conflict

for (i in 1:length(dep))
{
  dep[[i]] <-network(dep[[i]])
  odegsqrt<-sqrt(degree(dep[[i]],cmode="outdegree"))
  idegsqrt<-sqrt(degree(dep[[i]],cmode="indegree"))
  dep[[i]]<-set.vertex.attribute(dep[[i]],"odegsqrt",odegsqrt)
  dep[[i]]<-set.vertex.attribute(dep[[i]],"idegsqrt",idegsqrt)
  dep[[i]]<-set.vertex.attribute(dep[[i]],"region",region.cov[[i]])
  dep[[i]]<-set.vertex.attribute(dep[[i]],"language",language.cov[[i]])
}

dep[[1]]<-set.vertex.attribute(dep[[1]],"aici",aici_60s)
dep[[2]]<-set.vertex.attribute(dep[[2]],"aici",aici_70s)
dep[[3]]<-set.vertex.attribute(dep[[3]],"aici",aici_80s)

dep[[1]]<-set.vertex.attribute(dep[[1]],"conflict",conflict_60s)
dep[[2]]<-set.vertex.attribute(dep[[2]],"conflict",conflict_70s)
dep[[3]]<-set.vertex.attribute(dep[[3]],"conflict",conflict_80s)

#checking the dependent networks
#first extract the networks
dependent.t2<-dep$t2
dependent.t3<-dep$t3
dependent.t4<-dep$t4

#sumarize the networks
dependent.t2
dependent.t3
dependent.t4

#checking income attribute 
get.vertex.attribute(dependent.t2,attrname="aici")
get.vertex.attribute(dependent.t3,attrname="aici")
get.vertex.attribute(dependent.t4,attrname="aici")

#checking conflict attribute 
get.vertex.attribute(dependent.t2,attrname="conflict")
get.vertex.attribute(dependent.t3,attrname="conflict")
get.vertex.attribute(dependent.t4,attrname="conflict")

model <-btergm(dep ~
                 ctriple+
                 ttriple+
                 mutual +
                 edgecov(delrecip) +
                 edgecov(mem.stability)+
                 idegreepopularity +       ## dispersion of indegrees
                 odegreepopularity +       ## dispersion of outdegrees
                 absdiff("odegsqrt")+
                 nodematch("region") +
                 nodematch("language") +
                 nodeicov("aici")+
                 nodeocov("aici")+
                 nodeicov("conflict")+
                 nodeocov("conflict")+
                 edges,
                 parallel="snow",ncpus=35,R=250000)

#summary TERGM
summary(model,level=0.90)
summary(model,level=0.95)
summary(model,level=0.995)
summary(model,level=0.999)

#check goodness-of-fit: ROC curves, all periods
dev.off()
X               <-paste(top_destinations,"gof-ROC.pdf",sep="_")
gof.tergm1       <-gof(model,nsim=500,MCM.interval=50,classicgof=F,
                       MCMC.burnin=10000,MCM.samplesize=5000,ncpus=30)
dev.off()
plot(gof.tergm1)  #GOF plots

#check degeneracy, all periods
gof.tergm1

#ckech goodness-of-fit: classic statnet measures by period
# 1990s
dev.off()
X               <-paste(top_destinations,"gof-classic.pdf",sep="_")
gof.tergmt4       <-gof(model,nsim=500,MCM.interval=50,
                        predict.period =4,
                        MCMC.burnin=10000,MCM.samplesize=5000,ncpus=35,
                        classicgof=T
                        ,simulate(model,sequential=F))
plot(gof.tergmt4)  #GOF plots
dev.off()

#1980s
dev.off()
X               <-paste(top_destinations,"gof-classic.pdf",sep="_")
gof.tergmt3       <-gof(model,nsim=500,MCM.interval=50,
                        predict.period =3,
                        MCMC.burnin=10000,MCM.samplesize=5000,ncpus=35,
                        classicgof=T
                        ,simulate(model,sequential=F))
plot(gof.tergmt3)  #GOF plots
dev.off()

#1970s
dev.off()
X               <-paste(top_destinations,"gof-classic.pdf",sep="_")
gof.tergmt2     <-gof(model,nsim=500,MCM.interval=50,
                        predict.period =2,
                        MCMC.burnin=10000,MCM.samplesize=5000,ncpus=8,
                        classicgof=T
                        ,simulate(model,sequential=F))
plot(gof.tergmt2)  #GOF plots
dev.off()

##### Check all GOF plots
plot(gof.tergm1)   
plot(gof.tergmt4)  
plot(gof.tergmt3)  
plot(gof.tergmt2) 

### save image
setwd("~/diss/c3/output_data_files/ERGM_GOF")
#save.image(file = "ch3_TERGM.RData")