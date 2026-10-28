source("../../Util/utils.R")

go <- function(d, part) {
  crosses <- function(left, right, df, layer, range) {
    c1 <- any(df$ud[range] %in% layer & df$a[range] < left & df$b[range] > left
              & df$b[range] < right)
    if (isTRUE(c1)) return(TRUE)
    c1 <- any(df$ud[range] %in% layer & df$a[range] > left & df$a[range] < right
              & df$b[range] > right)
    isTRUE(c1)
  }

  sum(unlist(lapply(d, function(x) {
    moves <- as.integer(strsplit(x, ",")[[1]])
    df <- data.frame(a = rep(NA, 1000), b = rep(NA, 1000), ud = rep(NA, 1000))
    i <- 1
    pos <- 0
    layer <- 2

    for (move in moves) {
      range <- (1:(i-1))
      if (i == 1) range <- 0
      bwd <- pos - move
      if ((bwd > 0) && (!(bwd %in% c(df$a[range], df$b[range])))) {
        if ((part < 3) || (!crosses(bwd, pos, df, layer, range))) {
          df$a[i] <- min(bwd, pos)
          df$b[i] <- max(bwd, pos)
          df$ud[i] <- layer
          i <- i + 1
          layer <- 3 - layer
          pos <- bwd
          next
        }
      }

      fwd <- pos + move
      if (part >= 2) {
        while (fwd %in% c(df$a[range], df$b[range])) fwd <- fwd + 1
      }
      fail <- FALSE
      if (part == 3) {
        mx <- if (all(range == 0)) 2 else 1 + max(c(df$a[range], df$b[range]))
        while (TRUE) {
          if ((fwd %in% c(df$a[range], df$b[range])) ||
              crosses(pos, fwd, df, layer, range)) {
            fwd <- fwd + 1
            if (fwd > mx) {
              fail <- TRUE
              break
            }
            next
          }
          break
        }
        if (fail) next
      }

      df$a[i] <- min(fwd, pos)
      df$b[i] <- max(fwd, pos)
      df$ud[i] <- layer
      i <- i + 1
      layer <- 3 - layer
      pos <- fwd
    }
    pos
  })))
}

part1 <- function(d = rl("../data/everybody_codes_e4_q01_p1.txt")) {
  go(d, 1)
}

part2 <- function(d = rl("../data/everybody_codes_e4_q01_p2.txt")) {
  go(d, 2)
}

part3 <- function(d = rl("../data/everybody_codes_e4_q01_p3.txt")) {
  go(d, 3)
}

part1()
part2()
part3()
