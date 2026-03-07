source("../../Util/utils.R")

part1 <- function(d = rl("../data/everybody_codes_e2025_q19_p1.txt")) {
  q <- data.frame(y = 0, f = 0)
  oldx <- 0
  i <- 1
  while (i <= length(d)) {
    valid <- c()
    best <- rep(Inf, 2000000)
    passage <- as.integer(strsplit(d[i], ",")[[1]])
    nx <- passage[1]
    steps <- nx - oldx
    while ((passage[1] == nx) && (i <= length(d))) {
      valid <- unique(c(valid, passage[2]:(passage[2] + passage[3] - 1)))
      i <- i + 1
      if (i <= length(d)) {
        passage <- as.integer(strsplit(d[i], ",")[[1]])
      }
    }

    for (j in seq_len(nrow(q))) {
      state <- q[j, ]
      flaps <- ((valid - state$y) + steps)
      flaps <- flaps[(flaps %% 2) == 0] / 2
      flaps <- flaps[flaps >= 0]
      flaps <- unique(flaps[flaps <= steps])

      if (length(flaps) > 0) {
        nys <- state$y + ((2 * flaps) - steps) + 1
        best[nys] <- pmin(best[nys], state$f + flaps)
      }

    }
    good <- !is.infinite(best)
    q <- data.frame(y = which(good) - 1, f = best[good])
    oldx <- nx
  }
  min(q$f)
}

part2 <- function(d = rl("../data/everybody_codes_e2025_q19_p2.txt")) {
  part1(d)
}

part3 <- function(d = rl("../data/everybody_codes_e2025_q19_p3.txt")) {
  part1(d)
}

part1()
part2()
part3()
