source("../../Util/utils.R")

part1 <- function(d = ri("../data/everybody_codes_e2025_q04_p1.txt")) {
  d2 <- d[1:(length(d)-1)] / d[2:length(d)]
  trunc(2025 * prod(d2))
}

part2 <- function(d = ri("../data/everybody_codes_e2025_q04_p2.txt")) {
  t <- 10000000000000
  for (i in length(d) : 2) {
    t <- t / (d[i - 1] / d[i])
  }
  ceiling(t)
}

part3 <- function(d = rl("../data/everybody_codes_e2025_q04_p3.txt")) {
  tn <- 100
  for (s in 1:(length(d) - 1)) {
    c1 <- as.integer(d[s])
    c2 <- d[s + 1]
    if (grepl("\\|", d[s + 1])) c2 <- strsplit(d[s + 1], "\\|")[[1]]
    c2 <- as.integer(c2)
    if (length(c2) == 1) {
      tn <- tn * (c1 / c2)
    } else {
      tn <- tn * (c1 / c2[1])
      d[s + 1] <- c2[2]
    }
  }
  floor(tn)
}

part1()
part2()
part3()
