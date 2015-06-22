library(readxl)

read_ysk_xls <- function(fname) {
  data <- read_excel(fname, skip=1)

  # Convert everything that should be numeric to numeric
  data[, c(-3, -4)] <- lapply(data[, c(-3, -4)], function(col)as.numeric(sub(".", "", col, fixed=T)))

  TOPLAM <- rowSums(data[, 5:ncol(data)])

  # Return
  cbind(data[, 1:4], TOPLAM, data[, c("MHP", "HDP", "SAADET", "CHP", "AK PARTİ")])
}

read_all_ysk_xls <- function(dirname) {
  all <- NULL
  for (xls in dir(dirname, pattern="*xls", full.names = T)) {
    all <- rbind(all, read_ysk_xls(xls))
  }
  # There are 2 duplicate lines, remove them
  unique(all)
}

write.csv(read_all_ysk_xls("results/2015/raw"), "results/2015/processed.csv", row.names=F)