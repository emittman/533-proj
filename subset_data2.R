library(RSQLite)
name <- "C:/Users/emittman/GitHub/533/docs_2015/docs_2015/drive_stats.db"

#?dbConnect
conn <- dbConnect(drv = SQLite(),
          dbname = name)


q <- "SELECT serial_number, model, MIN(smart_9_raw), MAX(smart_9_raw), MAX(failure)   
      FROM drive_stats   
      GROUP BY serial_number;  
      "

# q2 <- paste(c('SELECT *
#               FROM drive_stats
#               WHERE serial_number IN ("',
#               paste(x[,1],collapse='","'),
#               '")'), collapse="")


subset <- dbGetQuery(conn, q)
subset[,3] <- as.Date(subset[,3])
subset[,4] <- as.Date(subset[,4])

#disconnect from database
dbDisconnect(conn)
saveRDS(subset, file="unit_summaries_full.rds")
