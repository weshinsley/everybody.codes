source("../../Util/utils.R")

thing <- function(d, s) {
  sum(unlist(lapply(d, function(x) s[which(s >= x)[1]])) - d)
}

part1 <- function(d = ri("../data/everybody_codes_e2026_q01_p1.txt", ",")) {
  thing(d, unlist(lapply(1:as.integer(sqrt(max(d))) + 1, function(x) x * x)))
}

part2 <- function(d = ri("../data/everybody_codes_e2026_q01_p2.txt", ",")) {
  thing(d, unlist(lapply(3:max(d), function(x) (2 * x) + (2 * (x - 2)))))
}

part3 <- function(d = ri("../data/everybody_codes_e2026_q01_p3.txt", ",")) {
  sizes <- seq(from = 3, to = as.integer(sqrt(max(d))) + 1, by = 2)
  thing(d, unlist(lapply(sizes, function(x) (x * x) - 1)))
}

part1()
part2()
part3()
