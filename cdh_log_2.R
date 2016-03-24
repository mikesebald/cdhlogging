library(plotly)
library(xlsx)
#library(readr)

setwd("~/R/cdhlogging")

# fr.weblog <- read_fwf(
#   file = "../cdhlogging_data/server-2016-03-21.log",
#   fwf_widths(c(5, 24, 7, 9, 0))
# )

system.time(f.weblog <- read.fwf(
  "../cdhlogging_data/server-2016-03-21.log",
  widths = c(5, 24, 7, 9, 100),
  col.names = c("One", "Timestamp", "Thread",
                "Severity", "Message"),
  comment.char = "",
  stringsAsFactors = FALSE
))


cleans <- grepl("No changes detected:|Record saved:", f.weblog$Message)
f.weblog <- f.weblog[cleans,]

f.weblog$Timestamp <- gsub(",", ".", f.weblog$Timestamp)
f.weblog$Timestamp <- strptime(f.weblog$Timestamp,
                               format = "%Y-%m-%d %H:%M:%OS")


# Restart from here

weblog <- f.weblog

groups <- rep(1:5000, each = 1000)

weblog <- cbind(weblog, groups[1:nrow(weblog)])

splitted.weblog <- split(weblog, weblog[, 6])

min.ts <- do.call(c, lapply(splitted.weblog, function(x) min(x$Timestamp)))
max.ts <- do.call(c, lapply(splitted.weblog, function(x) max(x$Timestamp)))
min.ts.c <- as.character(min.ts, format = "%H:%M:%OS3")
max.ts.c <- as.character(max.ts, format = "%H:%M:%OS3")
diff.ts <- as.numeric(difftime(max.ts, min.ts, units = "secs"))
rows.log <- unlist(lapply(splitted.weblog, nrow))
rps.log <- rows.log / diff.ts
sav.log <- do.call(c, lapply(splitted.weblog, 
                  function(x) sum(grepl("Record saved:", x$Message))))
noc.log <- do.call(c, lapply(splitted.weblog, 
                  function(x) sum(grepl("No changes detected:", x$Message))))

df <- as.data.frame(cbind(min.ts.c, max.ts.c, diff.ts, rows.log, rps.log, 
                          sav.log, noc.log))
colnames(df) <- c("Start", "End", "Time", "Rows", "RowsPerSec", 
                  "Updates", "NoChange")

# x <- list(title = "Paket")
# y <- list(title = "Records pro Sekunde")
# p <- plot_ly(data = df, y = RowsPerSec, mode = "markers") %>%
#   layout(xaxis = x, yaxis = y)
# p

df$RowsPerSec <- as.numeric(df$RowsPerSec)
p2 <- ggplot(data = df, aes(x = as.numeric(rownames(df)), 
                            y = RowsPerSec)) +
  xlab("Paket") + ylab("Records pro Sekunde") +
  geom_point(aes(text = paste("Start time:", Start)), size = 2) +
  geom_smooth(aes(colour = RowsPerSec, fill = RowsPerSec))
ggplotly(p2)

write.xlsx(df, file = "../cdh_log_data/runtimes.xlsx")
