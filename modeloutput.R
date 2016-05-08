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
#Get Beta Values
outputb <- matrix(ncol=4, nrow=21)
for (i in 1:21){
num=i
sbeta <- paste0("beta[",num,"]",collapse="")
betal <- summary(s)$summary[sbeta,"2.5%"]
betam <- summary(s)$summary[sbeta,"50%"]
betah <- summary(s)$summary[sbeta,"97.5%"]
outputb[i,1] <- betal
outputb[i,2] <- betam
outputb[i,3]<- betah
outputb[i,4]<-i
}

outb.dat<-as.data.frame(outputb)
colnames(outb.dat) <- c("lb", "median","ub","model")

#look at log_tp.01 quantile estimates
outputa <- matrix(ncol=4, nrow=21)
for (i in 1:21){
  num=i
  seta <- paste0("log_tp[",num,"]",collapse="")
  etal <- summary(s)$summary[seta,"2.5%"]
  etam <- summary(s)$summary[seta,"50%"]
  etah <- summary(s)$summary[seta,"97.5%"]
  outputa[i,1] <- etal
  outputa[i,2] <- etam
  outputa[i,3]<- etah
  outputa[i,4]<-i
}

outa.dat<-as.data.frame(outputa)
colnames(outa.dat) <- c("lb", "median","ub","model")

#Get Eta Values
output_eta <- matrix(ncol=4, nrow=21)
for (i in 1:21){
  num=i
  seta <- paste0("eta[",num,"]",collapse="")
  etal <- summary(s)$summary[seta,"25%"]
  etam <- summary(s)$summary[seta,"50%"]
  etah <- summary(s)$summary[seta,"75%"]
  output_eta[i,1] <- etal
  output_eta[i,2] <- etam
  output_eta[i,3]<- etah
  output_eta[i,4]<-i
}

eta.dat<-as.data.frame(output_eta)
colnames(eta.dat) <- c("lb", "median","ub","model")

#Untransform Log for B01, and Convert to Days
cols <- c("lb","median","ub")
outa.dat[cols] <- exp(outa.dat[cols])
outa.dat[cols] <- (outa.dat[cols])/24


#Make Plots
#for tp.01 quantile
theme_set(theme_bw(base_size=18))
ggplot(outa.dat, aes(x=model, y=median)) + 
  geom_errorbar(aes(ymin=lb, ymax=ub)) +
  geom_point()+
  ggtitle("95% Credible Interval for B01")+
  ylab("B01")+xlab("Model")

#for beta quantile
theme_set(theme_bw(base_size=18))
ggplot(outb.dat, aes(x=model, y=median)) + 
  geom_errorbar(aes(ymin=lb, ymax=ub)) +
  geom_point()+
  geom_hline(yintercept=1,linetype="dotted") +
  ggtitle("95% Credible Interval for Beta") +
  ylab(expression(beta))+xlab("Model")
