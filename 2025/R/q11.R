source("../../Util/utils.R")

part1 <- function(d = ri("../data/everybody_codes_e2025_q11_p1.txt"),
                  part = 1) {
  r <- 0
  while(TRUE) {
    cont <- FALSE
    for (i in 1:(length(d) - 1)) {
      if (d[i] > d[i + 1]) {
        d[i + 1] <- d[i + 1] + 1
        d[i] <- d[i] - 1
        cont <- TRUE
      }
    }
    if (!cont) break
    r <- r + 1
    if ((part == 1) && (r == 10)) return(sum(d * seq_along(d)))
  }
  while(TRUE) {
    cont <- FALSE
    for (i in 1:(length(d) - 1)) {
      if (d[i + 1] > d[i]) {
        d[i + 1] <- d[i + 1] - 1
        d[i] <- d[i] + 1
        cont <- TRUE
      }
    }
    if (!cont) break
    r <- r + 1
    if ((part == 1) && (r == 10)) return(sum(d * seq_along(d)))
  }
  r
}

part2 <- function(d = ri("../data/everybody_codes_e2025_q11_p2.txt")) {
  part1(d, part = 2)
}

part3 <- function(d = rl("../data/everybody_codes_e2025_q11_p3.txt")) {
  d <- as.numeric(d)
  stopifnot(all(diff(d) > 0)) # It's all increasing.
  mn <- mean
  sum(abs(d - mn) / 2)
}

part1()
part2()
part3()
