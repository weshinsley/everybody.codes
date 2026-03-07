source("../../Util/utils.R")

part1 <- function(d = rl("../data/everybody_codes_e2024_q01_p1.txt")) {
  d <- utf8ToInt(d)
  d[d == 67] <- 68
  sum(d - 65)
}

conv <- function(s) {
  if (s == "A") 0 else if (s == "B") 1 else if (s == "C") 3 else 5
}

part2 <- function(d = rl("../data/everybody_codes_e2024_q01_p2.txt")) {
  starts <- seq(1, nchar(d), 2)
  pairs <- substring(d, starts, starts + 1)
  extra <- c(0, 2)
  sum(unlist(lapply(pairs, function(x) {
    x <- strsplit(x, "")[[1]]
    x <- unlist(lapply(x[x != "x"], conv))
    sum(x) + extra[length(x)]
  })))
}

part3 <- function(d = rl("../data/everybody_codes_e2024_q01_p3.txt")) {
  starts <- seq(1, nchar(d), 3)
  trios <- substring(d, starts, starts + 2)
  extra <- c(0, 2, 6)
  sum(unlist(lapply(trios, function(x) {
    x <- strsplit(x, "")[[1]]
    x <- unlist(lapply(x[x != "x"], conv))
    sum(x) + extra[length(x)]
  })))
}

part1()
part2()
part3()
