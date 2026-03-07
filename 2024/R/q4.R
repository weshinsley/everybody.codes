source("../../Util/utils.R")

part1 <- function(d = ri("../data/everybody_codes_e2024_q04_p1.txt")) {
  sum(d - min(d))
}

part2 <- function(d = ri("../data/everybody_codes_e2024_q04_p2.txt")) {
  part1(d)
}

part3 <- function(d = ri("../data/everybody_codes_e2024_q04_p3.txt")) {
  mn <- median(d)
  sum(abs(d - mn))
}

part1()
part2()
part3()
