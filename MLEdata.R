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

#Get Data Ready for RSplida, Test with Model 11
s11<-subset(d,model==11)
s11$failed[s11$failed==0] <- 2
s11$failed[s11$failed==1] <- 1
s11$left <- "Left"
s11 <- s11[c(-1)]
write.table(s11, file="model11.txt",sep="\t",row.names=TRUE)

#Commands to Run in R Splida
dat <- read.table("C:/Users/Colin/Documents/Stat533/model11.txt", header = TRUE)
mod11 <- frame.to.ld(file=SplidaDataName("model11.txt"),response.column=3,censor.column = 4,truncation.response.column=2, truncation.type.column=6, data.title = "Mod11", time.units = "Hours")
weib11<-mlest(mod11,"Weibull")