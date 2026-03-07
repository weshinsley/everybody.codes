source("../../Util/utils.R")

pg <- function(d) {
  d[d == 0] <- "."
  d[d == 1] <- "#"
  for (i in 1:(nrow(d))) {
    message(paste0(d[i, 1:(ncol(d))]), collapse="")
  }
}
turn <- function(d) {
  nc <- ncol(d)
  nr <- nrow(d)
  diags <- matrix(0, ncol = nc, nrow = nr)
  diags[2:(nr - 1), 2:(nc - 1)] <-
    (d[1:(nr - 2), 1:(nc - 2)] + d[3:(nr), 1:(nc - 2)] +
       d[1:(nr - 2), 3:(nc)] + d[3:(nr), 3:(nc)]) %% 2
  d2 <- d
  d2[d == 1] <- diags[d == 1] == 1
  d2[d == 0] <- diags[d == 0] == 0
  d2[1,] <- 0
  d2[, 1] <- 0
  d2[nr, ] <- 0
  d2[, nc] <- 0
  d2
}

part1 <- function(d = rig("../data/everybody_codes_e2025_q14_p1.txt"),
                  n = 10) {
  tot <- 0
  for (i in 1:n) {
    message(i)
    d <- turn(d)
    tot <- tot + sum(d)
  }
  tot
}

part2 <- function(d = rig("../data/everybody_codes_e2025_q14_p2.txt")) {
  part1(d, 2025)
}

part3 <- function(d = rig("../data/everybody_codes_e2025_q14_p3.txt", pad = 0)) {
  m <- matrix(0, 36, 36)
  i <- 0
  df <- data.frame()
  while(TRUE) {
    m <- turn(m)
    i <- i + 1
    if (all(m[15:22, 15:22] == d)) {
      tot <- sum(m)
      df <- rbind(df, data.frame(round = i, tot = tot))
      if (sum(df$tot == tot) == 2) break
    }
  }

  burnin <- df$round[1]
  cycle_length <- df$round[nrow(df)] - df$round[1]
  df <- df[1:(nrow(df) - 1), ]
  df$round <- df$round - df$round[1]

  cycles <- 1000000000 - burnin # burn-in.
  complete <- cycles %/% cycle_length
  remain <- cycles %% cycle_length
  tot <- complete * sum(df$tot) + sum(df$tot[df$round <= remain])
  tot
}

part1()
part2()
part3()
