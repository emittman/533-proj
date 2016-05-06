curve(pweibull(x,10,2),from=0,to=10)
x <- 1:10
y <- pweibull(x,1,2)

require(scales) # trans_new() is in the scales library
inv_cdf_trans = function() trans_new("inv_cdf", function(x) log(qweibull(x,1,1)),
                               function(x) pweibull(exp(x),1,1))

ggplot(data=NULL,aes(x=x,y=y))+geom_point() + coord_trans(x = "log",y = "inv_cdf")



S$days <- S$time/24

theme_set(theme_bw(base_size=18))
ggplot(data=subset(S,model==11), aes(x=days, y=Fi)) + geom_point() +
  coord_trans(x = "log",y="inv_cdf") + 
  scale_x_continuous(breaks=c(20,40,60,80,100,200,400,800)) +
  scale_y_continuous(breaks=c(.001,.002,.005,.01,.02,.04,.06))
