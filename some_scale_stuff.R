curve(pweibull(x,10,2),from=0,to=10)
x <- 1:10
y <- pweibull(x,1,2)

require(scales) # trans_new() is in the scales library
inv_cdf_trans = function() trans_new("inv_cdf", function(x) log(qweibull(x,1,1)),
                               function(x) pweibull(exp(x),1,1))


ggplot(data=NULL,aes(x=x,y=y))+geom_point() + coord_trans(x = "log",y = "inv_cdf")

#Weibull Paper with ggplot
weib_trans = function() trans_new("weib_cdf", function(x) log(-log(1-x)),
                                     function(x) 1-exp(-exp(x)))
ggplot(data=NULL,aes(x=x,y=y))  +
  geom_point() +
  coord_trans(x = "log", y = "weib")

S$days <- S$time/24


#Get Median for Eta and Beta and Plug into Weibull CDF
fit<-function(mod){
  eta=eta.dat[mod,2]
  beta=outb.dat[mod,2]
  line<-function(x){1-exp(-(x*24/eta)^beta)}
  return(line)
}

#Fitted CDf does OK for Model 11, but terrible for Model 1,15,21
theme_set(theme_bw(base_size=18))
ggplot(data=subset(S,model==11), aes(x=days, y=Fi)) + geom_point() +
  coord_trans(x = "log",y="inv_cdf") + 
  scale_x_continuous(breaks=c(20,40,60,80,100,200,400,800)) +
  scale_y_continuous(breaks=c(.001,.002,.005,.01,.02,.04,.06))+
  stat_function(fun=fit(11))
