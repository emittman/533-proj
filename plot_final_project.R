library(rstan)
library(plyr)
library(ggplot2)
#setwd("C:/Users/Colin/Documents/GitHub/533-proj")
d <- readRDS("unit_summaries_full.rds")
s<-readRDS("B01_t.rds")
#small change test

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

t <- ddply(d, .(model), summarize,
            ends  = sort(unique(end_time[failed > 0]))) #sorted failure times for each model
            


table <- ldply(1:21, function(i){
                 model = i
                 ti  = subset(t, model == i)
                 di  = subset(d, model == i)
                 ldply(1:nrow(ti), function(j) {
                   tij = ti$ends[j]
                   minst <- min(di$start_time) #min start time for that model
                   nij = sum( di$start_time < tij & di$end_time >= tij ) #number at risk
                   dij = sum( di$start_time < tij & di$end_time == tij & di$failed > 0) #number failed
                   ret = data.frame(ti = tij, ni = nij, di = dij, model = model, min_st=minst)
                 })
})


S <- ldply(1:21, function(i)
  {
    tab <- subset(table, model == i)
    pi <- (tab$ni - tab$di) / tab$ni #conditional survival probability
    S <- cumprod(pi) #Survival probabilities
    data.frame(model = rep(i,length(S)),minstart=tab$min_st,Si = S, ni = tab$ni, di = tab$di, ti = tab$di, Fi = 1-S, time=tab$ti)
  }
)

#Add Ribbon
samples <- extract(s) #extracts MCMC samples to list of data.frames

#Get Posterior Quantiles
get_quantiles <- function(begin, end, length, model, alpha){ #pointwise intervals for proportion failing
  seq <-seq(begin, end, length.out = length) #grid over time
  par <- data.frame(beta = samples$beta[,model], eta = samples$eta[,model]) 
  q <- ldply(seq, function(t){
    probs <- sapply(1:4000, function(i) pweibull(t, par[i,1], par[i,2])) #get distribution of probability for one point on grid
    data.frame(time = t, lower = quantile(probs, alpha/2), upper = quantile(probs, 1-alpha/2))
  })
  return(q)
}

weibull_plot <- function(df, mod, alpha){
  x<- subset(df,model==mod) #df contains K-M estimates
  sbeta1 <- paste0("beta[",mod,"]",collapse="")
  seta1 <- paste0("eta[",mod,"]",collapse="")
  beta1 <- summary(s)$summary[sbeta1,"50%"]
  eta1 <- summary(s)$summary[seta1,"50%"]
  p<-(x$Fi)*(1 - pweibull(x$minstart, beta1, eta1)) + pweibull(x$minstart,beta1,eta1)
  x2<-cbind(x,p)
  l<-unique(x2$minstart)
  ul<-max(x2$time)+200
  z<-get_quantiles(l,ul,100,mod, alpha)

  plot <- ggplot(data = x2, aes(x=time, y=p)) +
    geom_point()+
    geom_ribbon(data = z, aes(time, ymin=lower, ymax=upper),fill="grey70",alpha=.5,inherit.aes = FALSE)
  return(plot)
}

#Plot

plot1<-weibull_plot(S,1,.20)+ggtitle("Posterior 80% Quantile Ribbon for Model 1")
plot2<-weibull_plot(S,11,.20)+ggtitle("Posterior 80% Quantile Ribbon for Model 11")
plot3<-weibull_plot(S,15,.20)+ggtitle("Posterior 80% Quantile Ribbon for Model 15")
plot4<-weibull_plot(S,21,.20)+ggtitle("Posterior 80% Quantile Ribbon for Model 21")
#plot_grid(plot1, plot2, align='h')
#plot_grid(plot3, plot4, align='h')

head_to_head <- function(num1, num2){
  d_m1 <- d[d$model==num1,]
  d_m1$cens <- d_m1$failed == 0
  d_m1$trunc <- d_m1$start_time>24
  sbeta1 <- paste0("beta[",num1,"]",collapse="")
  seta1 <- paste0("eta[",num1,"]",collapse="")
  beta1 <- summary(s)$summary[sbeta1,"50%"]
  eta1 <- summary(s)$summary[seta1,"50%"]
  df1 <- with(d_m1, weibull_points(start_time, end_time, cens, trunc, beta1, eta1))
  df1$model <- num1
  d_m2 <- d[d$model==num2,]
  d_m2$cens <- d_m2$failed == 0
  d_m2$trunc <- d_m2$start_time>24
  sbeta2 <- paste0("beta[",num2,"]",collapse="")
  seta2 <- paste0("eta[",num2,"]",collapse="")
  beta2 <- summary(s)$summary[sbeta2,"50%"]
  eta2 <- summary(s)$summary[seta2,"50%"]
  df2 <- with(d_m2, weibull_points(start_time, end_time, cens, trunc, beta2, eta2))
  df2$model <- num2
  DF <- rbind(df1,df2)
  require(ggplot2)
  plot <- ggplot(data = DF, aes(x=time, y=p)) +
    geom_point(aes(color = factor(model), shape=truncated))
    plot <- plot + stat_function(fun = pweibull,
                                 args = list(shape = beta1, scale = eta1), color="red")
    plot <- plot + stat_function(fun = pweibull,
                                 args = list(shape = beta2, scale = eta2), color="cyan")
    return(plot)
}


#assumes d is data and s is stanfit
plot_cpu_model <- function(num, line){
  d_m <- d[d$model==num,]
  d_m$cens <- d_m$failed == 0
  d_m$trunc <- d_m$start_time>24
  sbeta <- paste0("beta[",num,"]",collapse="")
  seta <- paste0("eta[",num,"]",collapse="")
  beta <- summary(s)$summary[sbeta,"50%"]
  eta <- summary(s)$summary[seta,"50%"]
  df<- with(d_m, weibull_points(start_time, end_time, cens, trunc, beta, eta))
  p <- weibull_plot(df, line, beta, eta)
  print(p)
}

plot_complete <- function(num, line){
  d_m <- d[d$model==num & d$start_time<24,]
  d_m$cens <- d_m$failed==0
  sbeta <- paste0("beta[",num,"]",collapse="")
  seta <- paste0("eta[",num,"]",collapse="")
  beta <- summary(s)$summary[sbeta,"50%"]
  eta <- summary(s)$summary[seta,"50%"]
  df<- with(d_m, weibull_points(start_time, end_time, cens, rep(F, nrow(d_m)), beta, eta))
  p <- weibull_plot(df, line, beta, eta)
  print(p)
}

select_pairs = function(fit, indices, p = c(1,2)){
  tp <- sapply(indices, function(x) paste(c("log_tp[",x,"]"),collapse=""))
  beta <- sapply(indices, function(x) paste(c("beta[",x,"]"),collapse=""))
  eta <- sapply(indices, function(x) paste(c("eta[",x,"]"),collapse=""))
  if(1 %in% p) pairs(fit, pars = c(beta, tp, "m1", "C1", "m2", "C2"))
  if(2 %in% p) plot(fit, pars = c(tp))
  if(3 %in% p) plot(fit, pars = c(eta))
}
select_pairs(s, 1, 1)
