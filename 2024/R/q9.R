source("../../Util/utils.R")

part1 <- function(d = ri("../data/everybody_codes_e2024_q09_p1.txt")) {
  count <- function(x) {
    b <- (x %/% 10)
    x <- x %% 10
    remainder <- c(1, 2, 1, 2, 1, 2, 3, 2, 3)
    if (x != 0) b <- b + remainder[x]
    b
  }
  sum(unlist(lapply(d, count)))
}

solve_lin <- function(x, stamps) {
  obj <- rep(1, length(stamps))
  mat <- matrix(stamps, nrow = 1)
  dir <- "="

  res <- lpSolve::lp(
    direction = "min",
    objective.in = obj,
    const.mat = mat,
    const.dir = "=",
    const.rhs = x,
    all.int = TRUE
  )
  sum(res$solution)
}

part2 <- function(d = ri("../data/everybody_codes_e2024_q09_p2.txt")) {
  stamps <- c(1, 3, 5, 10, 15, 16, 20, 24, 25, 30)
  sum(unlist(lapply(d, function(x) solve_lin(x, stamps))))
}

part3 <- function(d = ri("../data/everybody_codes_e2024_q09_p3.txt")) {
  stamps <- c(1, 3, 5, 10, 15, 16, 20, 24, 25, 30, 37, 38, 49, 50, 74, 75,
              100, 101)
  tot <- 0
  for (num in d) {
    if (num %% 2 == 1) {
      targ <- num %/% 2
      range <- (targ - 49):(targ + 50)
      res <- unlist(lapply(range, function(x) solve_lin(x, stamps)))
      tot <- tot + min(res[1:100]+res[100:1])
    } else {
      targ <- num %/% 2
      range <- (targ - 50):(targ + 50)
      res <- unlist(lapply(range, function(x) solve_lin(x, stamps)))
      tot <- tot + min(res[1:101]+res[101:1])
    }
  }
  tot
}

part1()
part2()
part3()
