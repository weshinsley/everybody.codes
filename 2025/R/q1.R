source("../../Util/utils.R")

part1 <- function(d = rl("../data/everybody_codes_e2025_q01_p1.txt"),
                  p = 1) {
  ns <- strsplit(d[1], ",")[[1]]
  i <- 0
  moves <- strsplit(d[3], ",")[[1]]
  for (move in moves) {
    dir <- if (substr(move, 1, 1) == "L") -1 else 1
    steps <- as.integer(substring(move, 2))
    if (p <= 2) {
      i <- i + dir * steps
      if (p == 1) i <- min(max(i, 0), length(ns) - 1)
      else if (p == 2) i <- i %% length(ns)
    } else {
      j <- (dir * steps) %% length(ns)
      swap <- ns[j + 1]
      ns[j + 1] <- ns[1]
      ns[1] <- swap
    }

  }
  ns[i + 1]
}


part2 <- function(d = rl("../data/everybody_codes_e2025_q01_p2.txt")) {
  part1(d, 2)
}

part3 <- function(d = rl("../data/everybody_codes_e2025_q01_p3.txt")) {
  part1(d, 3)
}

part1()
part2()
part3()
