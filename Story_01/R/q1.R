source("../../Util/utils.R")

parse <- function(s) {
  res <- list()
  s <- strsplit(s, " ")[[1]]
  for (a in s) {
    a <- strsplit(a, "=")[[1]]
    res[[a[1]]] <- as.numeric(a[2])
  }
  res
}

eni <- function(n, exp, mod) {
  rem <- c()
  score <- 1
  for (j in 1:exp) {
    score <- (score * n) %% mod
    rem <- c(rem, score %% mod)
  }
  as.numeric(paste(rev(rem), collapse = ""))
}

part1 <- function(d = rl("../data/everybody_codes_e1_q01_p1.txt"),
                  func = eni) {

  best <- 0
  for (s in d) {
    x <- parse(s)
    res <- func(x$A, x$X, x$M) + func(x$B, x$Y, x$M) + func(x$C, x$Z, x$M)
    best <- max(best, res)
  }
  best
}

eni2 <- function(n, exp, mod) {
  rem <- c()
  for (e in max(0, exp - 4):exp) {
    res <- powmod(n, e, mod)
    rem <- c(res, rem)
  }
  as.numeric(paste0(rem, collapse = ""))
}

part2 <- function(d = rl("../data/everybody_codes_e1_q01_p2.txt")) {
  part1(d, func = eni2)
}

part3 <- function(d = rl("../data/everybody_codes_e1_q01_p3.txt")) {
  eni <- function(n, exp, mod) {
    i <- exp
    cycle <- NULL
    while (TRUE) {
      val <- powmod(n, i, mod)
      if (val %in% cycle) break
      cycle <- c(cycle, val)
      i <- i - 1
    }
    start <- exp %% length(cycle)
    while(TRUE) {
      x <- powmod(n, start, mod)
      if (x == cycle[1])  break
      start <- start + length(cycle)
    }
    tot <- 0

    for (i in 1:start) {
      tot <- tot + powmod(n, i, mod)
    }
    cycles <- exp - start
    stopifnot((cycles %% length(cycle)) == 0)
    cycles <- cycles %/% length(cycle)
    tot <- tot + (sum(cycle) * cycles)
    tot
  }


  best <- 0
  for (s in d) {
    x <- parse(s)
    res <- eni(x$A, x$X, x$M) + eni(x$B, x$Y, x$M) + eni(x$C, x$Z, x$M)
    best <- max(best, res)
  }
  best
}

options(digits=16)
part1()
part2()
part3()
