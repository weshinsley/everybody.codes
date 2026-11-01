source("../../Util/utils.R")

thing <- function(d, trunc = 5) {
  lapply(strsplit(substring(d, trunc), " "), function(x) {
    res <- list(left = as.numeric(strsplit(x[1], "")[[1]]),
                right = as.numeric(strsplit(x[2], "")[[1]]))
    orig <- res$left
    i <- length(res$left)
    while (length(res$left) < length(res$right)) {
      res$left <- c(orig[i], res$left)
      i <- if (i == 1) length(orig) else (i - 1)
    }
    res$left <- res$left ^ seq(from = length(res$left) - 1, to = 0, by = -1)
    while (length(res$left) > length(res$right)) res$left <- res$left[-1]
    res$sum <- sum(res$left * res$right)
    res
  })
}

part1 <- function(d = rl("../data/everybody_codes_e2026_q03_p1.txt")) {
  sum(unlist(lapply(thing(d), `[[`, "sum")))
}

part2 <- function(d = rl("../data/everybody_codes_e2026_q03_p2.txt")) {
  part1(d)
}

part3 <- function(d = rl("../data/everybody_codes_e2026_q03_p3.txt")) {
  sum(unlist(lapply(thing(d, 6), function(x) {
    while (any(x$right > 0)) {
      i <- length(x$right)
      while(TRUE) {
        x$right[i] <- x$right[i] - 1
        if (x$right[i] >= 0) break
        x$right[i] <- 9
        i <- i - 1
      }
      if (sum(x$left * x$right) == x$sum) return(0)
    }
    x$sum
  })))
}

part1()
part2()
part3()
