source("../../Util/utils.R")

part1 <- function(d = rl("../data/everybody_codes_e2025_q16_p1.txt")) {
  v <- as.integer(strsplit(d,",")[[1]])
  sum(90 %/% v)
}

spell <- function(d) {
  target <- as.integer(strsplit(d,",")[[1]])
  size <- length(target)
  i <- 1
  used <- c()
  while (i <= size) {
    while (target[i] > 0) {
      used <- c(used, i)
      indexes <- seq(from = i, to = size, by = i)
      target[indexes] <- target[indexes] - 1
    }
    i <- i + 1
  }
  used
}

part2 <- function(d = rl("../data/everybody_codes_e2025_q16_p2.txt")) {
  prod(spell(d))
}

part3 <- function(d = rl("../data/everybody_codes_e2025_q16_p3.txt")) {
  s <- spell(d)
  target <- 202520252025000
  guess <- 10
  while (sum(guess %/% s) < target) guess <- guess * 10
  left <- guess / 10
  right <- guess
  while (TRUE) {
    if (left > right) break
    mid <- left + ((right - left) %/% 2)
    v <- sum(mid %/% s)
    if (v < target) { left <- mid + 1 }
    else if (v > target) { right <- mid - 1 }
    else return(mid)
  }
  (mid - 1)
}

part1()
part2()
part3()
