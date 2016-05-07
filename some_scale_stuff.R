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

#plot on Weibull paper, base R
plot(mod11$days, -log(1-mod11$Fi), log="xy",
     xlab="Time", ylab="ln(1/(1-F(t)))",
     main = "Weibull Q-Q Plot")
mod11=subset(S,model==11)

S$days <- S$time/24

#add fitted line from median of posterior eta and beta
eta11=eta.dat[11,2]
beta11=outb.dat[11,2]

test<-function(x){1-exp(-(x*24/eta11)^beta11)}
theme_set(theme_bw(base_size=18))
ggplot(data=subset(S,model==11), aes(x=days, y=Fi)) + geom_point() +
  coord_trans(x = "log",y="inv_cdf") + 
  scale_x_continuous(breaks=c(20,40,60,80,100,200,400,800)) +
  scale_y_continuous(breaks=c(.001,.002,.005,.01,.02,.04,.06))+
  stat_function(fun=test)
