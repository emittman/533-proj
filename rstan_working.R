d <- readRDS("proj/unit_summaries_full.rds")
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


#break things up for rstan
id_t <- d$start_time>24

id_c <- d$failed==0 & d$end_time != 0

id_f <- (d$failed==1 | d$failed==2) & d$end_time != 0

#untruncated data
cens <- d$end_time[id_c & !id_t]
x_cens <- d$model[id_c & !id_t]
failed <- d$end_time[id_f & !id_t]
x_failed <- d$model[id_f & !id_t]

#truncated data
tr_cens <- d$end_time[id_c & id_t]
x_tr_cens <- d$model[id_c & id_t]
tr_failed <- d$end_time[id_f & id_t]
x_tr_failed <- d$model[id_f & id_t]

#truncation times
t_cens <- d$start_time[id_c & id_t]
t_failed <- d$start_time[id_f & id_t]

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

data <- list(N_obs = length(failed),
              N_cens = length(cens),
              N_tr_obs = length(tr_failed),
              N_tr_cens = length(tr_cens),
              M = length(saved_levels),
              y_obs = failed,
              y_cens = cens,
              y_tr_obs = tr_failed,
              y_tr_cens = tr_cens,
              t_obs = t_failed,
              t_cens = t_cens,
              x_obs = x_failed,
              x_cens = x_cens,
              x_tr_obs = x_tr_failed,
              x_tr_cens = x_tr_cens,
              p = .01)

source("proj/models.R")

compiled_model <- stan_model(model_code = hier_mu_and_beta3)
s <- sampling(compiled_model, data = data, chains = 4, iter=2000)
saveRDS(s, file = "trans_hier2000.rds")


