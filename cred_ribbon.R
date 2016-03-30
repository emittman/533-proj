#calculate credible intervals for cdf
require(rstan)
require(plyr)
s <- readRDS("B01_t.rds")
samples <- extract(s)

#head(samples$beta)

get_quantiles <- function(begin, end, length, model, alpha){
  seq <-seq(begin, end, length.out = length)
  par <- data.frame(beta = samples$beta[,model], eta = samples$eta[,model])
  q <- ldply(seq, function(t){
    probs <- sapply(1:4000, function(i) pweibull(t, par[i,1], par[i,2]))
    data.frame(time = t, lower = quantile(probs, alpha/2), upper = quantile(probs, 1-alpha/2))
  })
  return(q)
}

# p <- ggplot(data = points, aes(x=time, y = p)) +
#      geom_points() +
#      geom_ribbon(data = ribbon, aes(time, ymin=lower, ymax=upper))