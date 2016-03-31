library(plotly)
library(xlsx)
library(scales)
library(readr)

setwd("e:/R/cdh_log")

# about 5 times slower:
# system.time(f.weblog1 <- read.fwf(
#   "../cdh_log_data/web.log",
#   widths = c(5, 24, 7, 9, 100),
#   col.names = c("One", "Timestamp", "Thread",
#                 "Severity", "Message"),
#   comment.char = "",
#   stringsAsFactors = FALSE
# ))

system.time({
  f.tmp <- read_lines("../cdh_log_data/web.log")
  l.tmp <- lapply(f.tmp, function(x) substring(x, 
                                               c(1, 6, 30, 37, 46), 
                                               c(4, 28, 35, 45, 150)))
  f.weblog <- as.data.frame(matrix(unlist(l.tmp), ncol = 5, byrow = TRUE),
                            stringsAsFactors = FALSE)
  colnames(f.weblog) <- c("One", "Timestamp", "Thread", "Severity", "Message")
})


cleans <- grepl("No changes detected:|Record saved:", f.weblog$Message)
f.weblog <- f.weblog[cleans,]

f.weblog$Timestamp <- gsub(",", ".", f.weblog$Timestamp)
f.weblog$Timestamp <- strptime(f.weblog$Timestamp,
                               format = "%Y-%m-%d %H:%M:%OS")

packagesize <- 1000



# Restart from here

weblog <- f.weblog

groups <- rep(1:5000, each = packagesize)

weblog <- cbind(weblog, groups[1:nrow(weblog)])

splitted.weblog <- split(weblog, weblog[, 6])

min.ts <- do.call(c, lapply(splitted.weblog, function(x) x[1, "Timestamp"]))
max.ts <- do.call(c, lapply(splitted.weblog, function(x) x[nrow(x), "Timestamp"]))
min.ts.c <- as.character(min.ts, format = "%H:%M:%OS3")
max.ts.c <- as.character(max.ts, format = "%H:%M:%OS3")
diff.ts <- as.numeric(difftime(max.ts, min.ts, units = "secs"))
rows.log <- unlist(lapply(splitted.weblog, nrow))
rps.log <- rows.log / diff.ts
sav.log <- do.call(c, lapply(splitted.weblog, 
                  function(x) sum(grepl("Record saved:", x$Message))))
noc.log <- do.call(c, lapply(splitted.weblog, 
                  function(x) sum(grepl("No changes detected:", x$Message))))

df <- as.data.frame(cbind.data.frame(min.ts, min.ts.c, max.ts.c, diff.ts, rows.log, 
                                     rps.log, sav.log, noc.log))
colnames(df) <- c("Timestamp", "Start", "End", "Time", "Rows", "RowsPerSec", 
                  "Updates", "NoChange")

p3 <- ggplot(data = df, aes(x = Timestamp, y = RowsPerSec)) +
  geom_point(aes(text = paste("Start time:", Start)), 
             size = 2, 
             colour = "darkgreen") +
  scale_x_datetime(labels = date_format("%H:%M:%S")) +
  xlab("Zeit") + ylab("Records pro Sekunde") +
  geom_smooth(aes(colour = RowsPerSec, fill = RowsPerSec)) +
  theme(plot.margin = unit(c(1,1,2,2), units = "lines"))
p3

layout(ggplotly(p3),
       margin = list(t = 20,
                     r = 20,
                     b = 80,
                     l = 80),
       xaxis = list(title = "Zeit"),
       yaxis = list(title = "Records pro Sekunde"))

df.xls <- df[,-1]
write.xlsx(df.xls, file = "../cdh_log_data/runtimes.xlsx")
