# This script parses a CDH admin log file. The intention is to report the time
# taken by the several load steps. It looks for START and END entries for 
# different loads steps. The script takes the assumption that the steps are not 
# processed in parallel so every START entry has to be succeeded by an END 
# entry. As input the script reads a file "startend.txt" which has to be created
# using grep 'START\|END' file1 file2 ... > startend.txt

library(ggplot2)
library(scales)

terms <- c(
  "LoadInit",
  "LoadValidate",
  "LoadExportDescriptors",
  "LoadDedup",
  "LoadStore",
  "CreateMatchIndex",
  "LoadPossibleMatches",
  "UpdateHistReports"
)
termstring <- paste(terms, collapse = "|")

file <- readLines("c:/temp/p01/startend.txt")
if (length(file) == 0) {
  cat("\nWarning! Reading emmpty file!")
  stop()
}

df.tmp <- data.frame(
  ts = substr(file, 27, 49),
  cat = substr(file, 58, 66),
  msg = substr(file, 67, 100),
  stringsAsFactors = FALSE
)
df <- data.frame(df.tmp[grepl(termstring, df.tmp$msg), ])
rm(df.tmp)

if (nrow(df) < 2) {
  stop("No reasonable logging information found. Exiting...")
}

i <- 1
while (grepl("START", df$msg[i]) == FALSE) {
  if (i == nrow(df)) {
    stop("No reasonable logging information found. Exiting...")
  }
  i <- i + 1
}

cat("Skipping lines which do not begin with \"START\". Beginning in line ", i)

properline <- c()
while (i < nrow(df)) {
  if (df$msg[i + 1] == df$msg[i]) {
    cat("\nNext line, same step. Leaving out line", i)
    properline[i] <- FALSE
  } else if ((grepl("START", df$msg[i]) == TRUE) &&
             (grepl("START", df$msg[i + 1]) == TRUE)) {
    cat("\nTwo start events after each other, skipping line", i)
    properline[i] <- FALSE
  } else if ((grepl("END", df$msg[i]) == TRUE) &&
             (grepl("END", df$msg[i + 1]) == TRUE)) {
    cat("\nTwo end events after each other, skipping line", i)
    properline[i] <- FALSE
  } else {
    properline[i] <- TRUE
  }
  i <- i + 1
  if (i == nrow(df)) {
    if (grepl("START", df$msg[i]) == TRUE) {
      cat("\nLast line is a start event, skipping", i)
      properline[i] <- FALSE
    }
  }
}

df.tmp <- df[properline, ]

seq(1, nrow(df.tmp), 2)
seq(2, nrow(df.tmp), 2)

res.df <- data.frame(cbind(ts1 = df.tmp$ts[seq(1, nrow(df.tmp), 2)],
                           ts2 = df.tmp$ts[seq(2, nrow(df.tmp), 2)],
                           msg1 = df.tmp$msg[seq(1, nrow(df.tmp), 2)],
                           msg2 = df.tmp$msg[seq(2, nrow(df.tmp), 2)]),
                     stringsAsFactors = FALSE)
rm(df.tmp)

# Make this an ordered factor so it will be plotted in the right order
step <- sub("START ", "", res.df$msg1)
step <- factor(step, levels = terms)

res.df$ts1 <- strptime(res.df$ts1,
                       format = "%Y-%m-%d %H:%M:%OS")
res.df$ts2 <- strptime(res.df$ts2,
                       format = "%Y-%m-%d %H:%M:%OS")
timediff.s <- as.numeric(difftime(res.df$ts2, 
                                  res.df$ts1, 
                                  units = "secs"))
timediff.m <- as.numeric(difftime(res.df$ts2, 
                                  res.df$ts1, 
                                  units = "mins"))
res.df <- cbind(res.df, step, timediff.m, timediff.s)

i <- 1
r <- 0
run <- character()
while(i <= nrow(res.df)) {
  if (res.df$step[i] == "LoadInit") {
    r <- r + 1
  }
  run[i] <- r
  i <- i + 1
}

res.df <- cbind(res.df, run)
exclude <- c() #c("1", "2", "5")
final.df <- res.df[!(run %in% exclude),]


p <- ggplot(final.df, aes(x = step, y = timediff.m, fill = run)) +
  geom_bar(position = "dodge", stat = "identity") +
  xlab("Step") +
  ylab("Duration in minutes")
p
ggplotly(p)
