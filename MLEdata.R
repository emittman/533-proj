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

for (i in 11:21){
name=paste("model",i,".txt",sep="")
mod.i <- frame.to.ld(file=SplidaDataName(name),response.column=3,censor.column = 4,truncation.response.column=2, truncation.type.column=5, data.title = name, time.units = "Hours")
weib.i<-mlest(mod.i,"Weibull")
}
