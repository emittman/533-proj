library(RSQLite)
name13 <- "C:/Users/emittman/GitHub/533/docs_2013/docs_2013/drive_stats.db"
name14 <- "C:/Users/emittman/GitHub/533/docs_2014/docs_2014/drive_stats.db"
name15 <- "C:/Users/emittman/GitHub/533/docs_2015/docs_2015/drive_stats.db"

#need to check how unit was withdrawn
q <- "SELECT serial_number, model, MIN(Date), MAX(Date), MIN(smart_9_raw), MAX(smart_9_raw), MAX(failure)   
      FROM drive_stats   
      GROUP BY serial_number;  
      "

conn <- dbConnect(drv = SQLite(),
                  dbname = name13)
temp <- dbGetQuery(conn, q)

subset <- temp

#disconnect from database
dbDisconnect(conn)

conn <- dbConnect(drv = SQLite(),
                  dbname = name14)
temp <- dbGetQuery(conn, q)

subset <- merge(subset, temp, )

subset[,3] <- as.Date(subset[,3])
subset[,4] <- as.Date(subset[,4])

#disconnect from database
dbDisconnect(conn)
saveRDS(subset, file="unit_summaries_full.rds")


