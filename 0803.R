## clear all
rm(list=ls(all=TRUE))
setwd("~/Documents/Dropbox/0-James-RA")

library(officer)
library(plyr)
library(magrittr)
library(flextable)
library(magrittr)
library(ggplot2)
library(devEMF)
library(ggthemes)
library(scales)

load("RENN_Cleaned.Rda")
load("questions.rda")
data<-renn.cleaned.data

# setting network attributes

nettitle = "Network of Interaction"
netq = "Question: Pick the 6th graders that you chose to spend the most time with during these last 5 months. \nData: Pilot study \nDate: 08/02/18 \nSchool: Renn\nNo answer: A person who is a respondent but doesn't answer the question
       \nclustering: In R's sna package it's called transitivity (commend: gtrans).\nmean path length: In sna I use (mean) closeness \ndegree centralization: In sna I use mean Eigenvector_centrality."
# clean the network==================================================
#read in data and create gender variable
data<-renn.cleaned.data
data$male = "1"
data$male[data$Q_gender == '-99'] <- NA
data$male[data$Q_gender == 'Girl'] <- "0"

data$ID<-as.character(data$Q_id)
n <- dim(data)[1]

# code from previous quests. not needed now.
nomination_count = list()
race_count = list()
egolist = list()
alterlist = list()

# create the race category James defined on 07/31/18
data$netrace = "multiracial"
for (i in seq(1, n,1)){
  a1 = data$Q_sn_f2finteract[i]
  a2 = unlist(strsplit(as.character(a1),","))
  egolist = append(egolist, rep(paste(data$Q_id[i]),length(a2))) 
  alterlist = append(alterlist, unlist(a2))
  r1 = data$Q_race_eth[i]
  r2 = unlist(strsplit(as.character(r1),","))
  nomination_count = append(nomination_count,length(a2))
  race_count = append(race_count,length(r2))
  
  if(r1 == "White")data$netrace[i] ="white"
  if(r1 == "Hispanic or Latino")data$netrace[i] ="Hispanic or Latino"
  if(r1 == "Black or African American")data$netrace[i] ="Black or African American"
}
data$sn2_nomicount <- unlist(nomination_count)
data$race_count <- as.character(unlist(race_count))

library(gridExtra)

# create edgelist
edgelist = data.frame(ego = unlist(egolist), alter = unlist(alterlist))

# create a network with NA
net<-as.network.matrix(edgelist,matrix.type = "edgelist", directed = TRUE)
nodelist = net %v% 'vertex.names'

## check if all the nomination are in the data
in_data = list()
notin_data = list()

for (i in nodelist){
  if (i %in% data$Q_id){
    in_data = append(in_data, i)
  }else{
    notin_data = append(notin_data, i)
  }
}

# create an edgelist with no NAs
cnetedge <- edgelist[which(edgelist$alter %in% in_data),]

# create a network with NO NA
cnet<-as.network.matrix(cnetedge,matrix.type = "edgelist", directed = TRUE)
set.seed(315)
codeg<-(degree(cnet,cmode="outdegree"))
cideg<-(degree(cnet,cmode="indegree"))

# make in and outdegree node-level attirbutes
set.vertex.attribute(cnet,"odeg",codeg)
set.vertex.attribute(cnet,"ideg",cideg)

# adding more attributes
data$gender  <- "male"
data$gender[data$male==0] <- "female"
data$gender[is.na(data$male)] <- "No answer"

cnet %v% 'gender' = as.character(data$gender[match(cnet %v% 'vertex.names' ,data$Q_id)])
cnet %v% 'race' = as.character(data$netrace[match(cnet %v% 'vertex.names' ,data$Q_id)])
cnet %v% 'gender'  <- ifelse(is.na(cnet %v% 'gender'),"Nonattendee",cnet %v% 'gender' )
cnet %v% 'race'  <- ifelse(is.na(cnet %v% 'race'),"Nonattendee",cnet %v% 'race' )

set.seed(315)
gcnet <- ggnet2(cnet, node.color = cnet %v% 'gender', arrow.size = 10,
                palette = c('female'= '#6a67ce','male' = '#1aafd0','Nonattendee'='black','No answer'='#C04759'), 
                shape = cnet %v% 'race' ,shape.palette =c('Black or African American' = 15, 'Hispanic or Latino' = 16, 'multiracial' = 17, 'white' =18), shape.legend = "race",alpha = 0.8,
                edge.color = "#d0d2d3",node.size = cnet %v% 'race',
                size.palette =c('Black or African American' = 4, 'Hispanic or Latino' = 4, 'multiracial' = 4, 'Nonattendee' = 1, 'white' = 4)) +
  labs(title = nettitle,
       subtitle = netq) +
  labs(color = 'gender', shape = 'race') +guides(size = FALSE)

cnetinfo = data.frame(mean_degree = round(mean(degree(cnet)),digits = 2), 
                      clustering = round(gtrans(cnet),digits = 2),
                      mean_path_length  = round(mean(closeness(cnet)),digits = 2),
                      degree_centralization = round(mean(evcent(cnet)),digits = 2))

net <- cnet

data$g1 = "non-white-non-white/asian"
data$g2 = "non-black-non-black/latino-non-black/hawaiian"
data$g3 = "non-latino"
#+ theme(legend.position = "bottom")
for (i in seq(1, n,1)){
  r1 = data$Q_race_eth[i]
  #r2 = unlist(strsplit(as.character(r1),","))
  
  if(r1 == "White"){data$g1[i] ="white, white/aisan or white/latino"}
  if(r1 == "Asian,White"){data$g1[i] ="white, white/aisan or white/latino"}
  if(r1 == "Hispanic or Latino,White"){data$g1[i] ="white, white/aisan or white/latino"}
  if(r1 == "Black or African American")
  {data$g2[i] ="black, black/latino, or black/hawaiian"}
  if(r1 == "Black or African American,Hispanic or Latino")
  {data$g2[i] ="black, black/latino, or black/hawaiian"}
  if(r1 == "Black or African American,Native Hawaiian or other Pacific Islander")
  {data$g2[i] ="black, black/latino, or black/hawaiian"}
  if(r1 == "Hispanic or Latino"){data$g3[i] ="Hispanic or Latino"}
}

net %v% 'g1' = as.character(data$g1[match(net %v% 'vertex.names' ,data$Q_id)])
net %v% 'g2' = as.character(data$g2[match(net %v% 'vertex.names' ,data$Q_id)])
net %v% 'g3' = as.character(data$g3[match(net %v% 'vertex.names' ,data$Q_id)])

netdata <- data.frame(ID = net %v% 'vertex.names', ideg = net %v% 'ideg',
                      odeg  = net %v% 'odeg', white  = net %v% 'g1', black = net %v% 'g2', 
                      latino = net %v% 'g3', gender = net %v% 'gender')
net_g1 = netdata[netdata$white == "white, white/aisan or white/latino",]
net_g2 = netdata[netdata$black == "black, black/latino, or black/hawaiian",]
net_g3 = netdata[netdata$latino == "Hispanic or Latino",]


g1 <- qplot(net %v% 'ideg', binwidth = 0.5) + 
  labs(title = "Distribution of indegree",
       subtitle = netq, 
       x = "Indegree",y="Frequency") 

t1 <-as.data.frame(t(data.frame(table(net %v% 'ideg', useNA = "no"))))
t1 <- cbind (c('indegree','frequency'),t1)

g2 <- qplot(net_g1$ideg, binwidth = 0.5, ylim = c(0,5)) + 
  labs(title = "Distribution of indegree -white, white/aisan or white/latino only",
       subtitle = netq, 
       x = "indegree",y="Frequency") 
t2 <- as.data.frame(t(data.frame(table(net_g1$ideg))))
t2 <- cbind (c('indegree','frequency'),t2)

## plot the indegree - black
g3 <- qplot(net_g2$ideg, binwidth = 0.5, ylim = c(0,5)) + 
  labs(title = "Distribution of indegree - black, black/latino and black/hawaiian only",
       subtitle = netq, 
       x = "indegree",y="Frequency") 
t3 <- as.data.frame(t(data.frame(table(net_g2$ideg))))
t3 <- cbind (c('indegree','frequency'),t3)

## plot the indegree - latino
g4 <- qplot(net_g3$ideg, binwidth = 0.5, ylim = c(0,5)) + 
  labs(title = "Distribution of indegree - latino only",
       subtitle = netq, 
       x = "indegree",y="Frequency") 
t4 <- as.data.frame(t(data.frame(table(net_g3$ideg))))
t4 <- cbind (c('indegree','frequency'),t4)


g5 <- qplot(net %v% 'odeg', binwidth = 0.5) + 
  labs(title = "Distribution of outdegree",
       subtitle = netq, 
       x = "outdegree",y="Frequency") 

t5 <-as.data.frame(t(data.frame(table(net %v% 'odeg', useNA = "no"))))
t5 <- cbind (c('outdegree','frequency'),t5)

g6 <- qplot(net_g1$odeg, binwidth = 0.5, ylim = c(0,5)) + 
  labs(title = "Distribution of outdegree - white, white/aisan or white/latino only",
       subtitle = netq, 
       x = "outdegree",y="Frequency") 
t6 <- as.data.frame(t(data.frame(table(net_g1$odeg))))
t6 <- cbind (c('outdegree','frequency'),t6)

## plot the outdegree - black
g7 <- qplot(net_g2$odeg, binwidth = 0.5, ylim = c(0,5)) + 
  labs(title = "Distribution of outdegree - black, black/latino and black/hawaiian only",
       subtitle = netq, 
       x = "outdegree",y="Frequency") 
t7 <- as.data.frame(t(data.frame(table(net_g2$odeg))))
t7 <- cbind (c('outdegree','frequency'),t7)

## plot the outdegree - latino
g8 <- qplot(net_g3$odeg, binwidth = 0.5, ylim = c(0,5)) + 
  labs(title = "Distribution of outdegree - latino only",
       subtitle = netq, 
       x = "outdegree",y="Frequency") 
t8 <- as.data.frame(t(data.frame(table(net_g3$odeg))))
t8 <- cbind (c('outdegree','frequency'),t8)

net_boy = netdata[netdata$gender == "male",]
net_girl = netdata[netdata$gender == "female",]

## plot the indegree - boy
g9 <- qplot(net_boy$ideg, binwidth = 0.5, ylim = c(0,5)) + 
  labs(title = "Distribution of indegree - boy only",
       subtitle = netq, 
       x = "indegree",y="Frequency") 
t9 <- as.data.frame(t(data.frame(table(net_boy$ideg))))
t9 <- cbind (c('indegree','frequency'),t9)

## plot the indegree - girl
g10 <- qplot(net_girl$ideg, binwidth = 0.5, ylim = c(0,5)) + 
  labs(title = "Distribution of indegree - girls' only",
       subtitle = netq, 
       x = "indegree",y="Frequency") 
t10 <- as.data.frame(t(data.frame(table(net_girl$ideg))))
t10 <- cbind (c('indegree','frequency'),t10)


## plot the outdegree - boy
g11 <- qplot(net_boy$odeg, binwidth = 0.5, ylim = c(0,5)) + 
  labs(title = "Distribution of outdegree - boys' only",
       subtitle = netq, 
       x = "outdegree",y="Frequency") 
t11 <- as.data.frame(t(data.frame(table(net_boy$odeg))))
t11 <- cbind (c('outdegree','frequency'),t11)

## plot the outdegree - girl
g12 <- qplot(net_girl$odeg, binwidth = 0.5, ylim = c(0,5)) + 
  labs(title = "Distribution of outdegree - girls' only",
       subtitle = netq, 
       x = "outdegree",y="Frequency") 
t12 <- as.data.frame(t(data.frame(table(net_girl$odeg))))
t12 <- cbind (c('outdegree','frequency'),t12)


doc <- read_docx()
doc <- body_add_gg(doc,gcnet, width = 10, height = 7)
doc <- body_add_table(doc, cnetinfo, style = "table_template", pos = "after")

doc <- body_add_gg(doc,g1, width = 10, height = 7)
doc <- body_add_table(doc, t1, style = "table_template", pos = "after",header = FALSE)
doc <- body_add_break(doc, pos = "after")
doc <- body_add_gg(doc,g2, width = 10, height = 7)
doc <- body_add_table(doc, t2, style = "table_template", pos = "after",header = FALSE)
doc <- body_add_break(doc, pos = "after")
doc <- body_add_gg(doc,g3, width = 10, height = 7)
doc <- body_add_table(doc, t3, style = "table_template", pos = "after",header = FALSE)
doc <- body_add_break(doc, pos = "after")
doc <- body_add_gg(doc,g4, width = 10, height = 7)
doc <- body_add_table(doc, t4, style = "table_template", pos = "after",header = FALSE)

doc <- body_add_gg(doc,g5, width = 10, height = 7)
doc <- body_add_table(doc, t5, style = "table_template", pos = "after",header = FALSE)
doc <- body_add_break(doc, pos = "after")
doc <- body_add_gg(doc,g6, width = 10, height = 7)
doc <- body_add_table(doc, t6, style = "table_template", pos = "after",header = FALSE)
doc <- body_add_break(doc, pos = "after")
doc <- body_add_gg(doc,g7, width = 10, height = 7)
doc <- body_add_table(doc, t7, style = "table_template", pos = "after",header = FALSE)
doc <- body_add_break(doc, pos = "after")
doc <- body_add_gg(doc,g8, width = 10, height = 7)
doc <- body_add_table(doc, t8, style = "table_template", pos = "after",header = FALSE)
doc <- body_add_break(doc, pos = "after")

doc <- body_add_gg(doc,g9, width = 10, height = 7)
doc <- body_add_table(doc, t9, style = "table_template", pos = "after",header = FALSE)
doc <- body_add_break(doc, pos = "after")
doc <- body_add_gg(doc,g10, width = 10, height = 7)
doc <- body_add_table(doc, t10, style = "table_template", pos = "after",header = FALSE)
doc <- body_add_break(doc, pos = "after")
doc <- body_add_gg(doc,g11, width = 10, height = 7)
doc <- body_add_table(doc, t11, style = "table_template", pos = "after",header = FALSE)
doc <- body_add_break(doc, pos = "after")
doc <- body_add_gg(doc,g12, width = 10, height = 7)
doc <- body_add_table(doc, t12, style = "table_template", pos = "after",header = FALSE)

doc<- body_end_section(doc, landscape = TRUE)


print(doc, target = "sn_interact.docx")
