library(rstan)
library(plyr)
library(ggplot2)
setwd("C:/Users/Colin/Documents/GitHub/533-proj")
d <- readRDS("unit_summaries_full.rds")
s<-readRDS("B01_t.rds")

data.fit <- as.data.frame(summary(s))
par.data<-data.fit$summary
parms <- sapply(data.fit[c("eta", "beta")], mean)

#Extract 2.5, 50, 97.5% of param etimates for plotting

outputb <- matrix(ncol=3, nrow=21)
for (i in 1:21){
num=i
sbeta <- paste0("beta[",num,"]",collapse="")
betal <- summary(s)$summary[sbeta,"2.5%"]
betam <- summary(s)$summary[sbeta,"50%"]
betah <- summary(s)$summary[sbeta,"97.5%"]
outputb[i,1] <- betal
outputb[i,2] <- betam
outputb[i,3]<- betah
}

outb.dat<-as.data.frame(outputb)
colnames(outb.dat) <- c("lb", "med","ub")

outputa <- matrix(ncol=3, nrow=21)
for (i in 1:21){
  num=i
  seta <- paste0("eta[",num,"]",collapse="")
  etal <- summary(s)$summary[seta,"2.5%"]
  etam <- summary(s)$summary[seta,"50%"]
  etah <- summary(s)$summary[seta,"97.5%"]
  outputa[i,1] <- etal
  outputa[i,2] <- etam
  outputa[i,3]<- etah
}

outa.dat<-as.data.frame(outputa)
colnames(outa.dat) <- c("lb", "med","ub")
