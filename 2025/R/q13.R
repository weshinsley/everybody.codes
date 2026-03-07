source("../../Util/utils.R")

part1 <- function(d = ri("../data/everybody_codes_e2025_q13_p1.txt")) {
  dial <- rep(0, length(d) + 1)
  dial[1] <- 1
  i <- 2
  j <- length(dial)
  n <- 1
  while (j >= i) {
    dial[i] <- d[n]
    if (n + 1 <= length(d)) dial[j] <- d[n + 1]
    n <- n + 2
    i <- i + 1
    j <- j - 1
  }
  dial[1 + (2025 %% (1 + length(d)))]
}

part2 <- function(d = rl("../data/everybody_codes_e2025_q13_p2.txt"),
                  mod = 20252025) {
  d <- strsplit(d, "-")
  df <- data.frame(from = as.integer(unlist(lapply(d, `[[`, 1))),
                   to = as.integer(unlist(lapply(d, `[[`, 2))))
  df$range <- 1 + (df$to - df$from)
  dial <- rep(0, sum(df$range) + 1)
  dial[1] <- 1
  i <- length(dial)
  j <- 2
  for (n in seq_len(nrow(df))) {
    r <- df[n, ]
    if ((n %% 2) == 1) { # cw
      dial[j:(j + (r$range - 1))] <- r$from:r$to
      j <- j + r$range
    } else { # acw
      dial[i:(i - (r$range - 1))] <- r$from:r$to
      i <- i - r$range
    }
  }
  dial[1 + (mod %% length(dial))]
}

part3 <- function(d = rl("../data/everybody_codes_e2025_q13_p3.txt")) {
  # It's quick enough and I am lazy.
  part2(d, mod = 202520252025)
}

part1()
part2()
part3()
