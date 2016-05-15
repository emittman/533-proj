library(SPREDA)
d <- readRDS("unit_summaries_full.rds")
names(d) <- c("serial_number", "model", "start_time", "end_time", "failed")
d$model <- as.factor(d$model)


#locate entries for models with few drives
o <- summary(d$model)
id_100plus <- which(o>100)
o[id_100plus]

id_suff <- d$model %in% names(o[id_100plus])

id_clean <- d$end_time != d$start_time & d$end_time != 0

id_infant <- d$end_time/24 < 20

id_inc <- id_suff & id_clean & !id_infant

#subset and refactor d!
d <- d[id_inc,]

d <- droplevels(d)

saved_levels <- levels(d$model)

d$model <- as.integer(d$model)

#Get Data Ready for RSplida, Export to .txt files by model
mod_list=seq(1,21)
for (i in 1:21){
s<-subset(d,model==i)
#For Splida 2=Censor and 1 is Failure
s$failed[s$failed==0] <- 2
s$left <- "Left"
s <- s[c(-1)]
write.table(file=paste("model",mod_list[i],".txt",sep=""),sep="\t",s,row.names=TRUE)
}

#Commands to Run Weibull Model in R Splida, Make sure to Put Files in RSplidaTextData Folder

mod.mle<-list()
for (i in 1:21){
name=paste("model",i,".txt",sep="")
life.dat <- frame.to.ld(file=SplidaDataName(name),response.column=3,censor.column = 4,truncation.response.column=2, truncation.type.column=5, data.title = name, time.units = "Hours")
weib<-mlest(life.dat,"Weibull")
mu=weib$theta[1]
sigma=weib$theta[2]
vcv=weib$vcv
df<-data.frame(mu,sigma,vcv)
mod.mle[[i]]=df
}

#Models 1,2,4,16 couldn't estimate a covariance matrix
#Extract Beta
test=data.frame(beta=sapply(mod.mle, function(x) beta=1/x$sigma[1]))

#Try NOT Using Rsplida; Check with Model 1

#log-likelihood function in terms of mu and sigma
lliktruncr <- function (theta, y, d, trunctime) {
  mu <- theta[1]
  sigma <- theta[2]
  
  z <- (log(y)-mu)/sigma
  ztrunc <- (log(trunctime)-mu)/sigma
    sum(log(( (( 1/(sigma*y) * dsev(z))/(1-psev(ztrunc)) )^d)*
                 (( (1-psev(z))/(1-psev(ztrunc)))^(1-d) )))
    }


#Set Initial values for Optim
mu <- 12.55
names(mu) <- "mu"
sigma <- .65
names(sigma) <- "sigma"

weibull.mle <- optim(c(mu, sigma), lliktruncr,
                     method='BFGS', control=list(fnscale=-1),
                     y=s$end_time, d=s$failed,
                     trunctime=s$start_time,
                     hessian=TRUE)

#log-likelihood function in terms of sigma and tp.01
lliktruncr2 <- function (theta, y, d, trunctime) {
  tp <- theta[1]
  sigma <- theta[2]
  
  beta=1/sigma
  eta=tp/((-log(.99))^(1/beta))
  sum(log(( ((dweibull(y,beta,eta)/(1-pweibull(trunctime,beta,eta)) ))^d)*
            (( (1-pweibull(y,beta,eta))/(1-pweibull(trunctime,beta,eta)))^(1-d) )))
}

#Pick starting values based on Bayes Project Results
tp<-2500
names(mu) <- "tp"
sigma <- 16
names(sigma) <- "sigma"

weibull.mle2 <- optim(c(tp, sigma), lliktruncr2,
                     method='BFGS', control=list(fnscale=-1),
                     y=s$end_time, d=s$failed,
                     trunctime=s$start_time,
                     hessian=TRUE)
weibull.mle2$par

#The above seems very sensitive to starting values.
