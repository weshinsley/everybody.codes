install.packages("numbers")
source("../../Util/utils.R")

part1 <- function(d = rkv("../data/everybody_codes_e1_q03_p1.txt")) {
  d$diag <- (d$x + d$y) - 1
  d$moves <- 100 %% d$diag
  d$x2 <- d$x + d$moves
  d$y2 <- d$y - d$moves
  d$out <- d$y2 <= 0
  d$x2[d$out] <- d$x2[d$out] - d$diag[d$out]
  d$y2[d$out] <- d$y2[d$out] + d$diag[d$out]
  sum(d$x2 + 100 * d$y2)
}

part2 <- function(d = rkv("../data/everybody_codes_e1_q03_p2.txt")) {
  d$diag <- (d$x + d$y) - 1
  numbers::chinese(d$y, d$diag) - 1
}

part3 <- function(d = rkv("../data/everybody_codes_e1_q03_p3.txt")) {
  part2(d)
}

part1()
part2()
part3()
