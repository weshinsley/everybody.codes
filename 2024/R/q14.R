source("../../Util/utils.R")

part1 <- function(d = rl("../data/everybody_codes_e2024_q14_p1.txt")) {
  d <- strsplit(d, ",")[[1]]
  x <- 0
  y <- 0
  z <- 0
  h <- 0
  for (grow in d) {
    ch <- substr(grow, 1, 1)
    amount <- as.integer(substring(grow, 2))
    if (ch == "U") y <- y + amount
    if (ch == "D") y <- y - amount
    if (ch == "F") z <- z - amount
    if (ch == "B") z <- z + amount
    if (ch == "L") x <- x - amount
    if (ch == "R") x <- x + amount
    h <- max(h, y)
  }
  h
}

part2 <- function(d = rl("../data/everybody_codes_e2024_q14_p2.txt"),
                  p3 = FALSE) {
  set <- new.env(parent = emptyenv())
  if (p3) {
    leaves <- data.frame()
    trunk <- c()
  }
  d <- strsplit(d, ",")
  for (row in d) {
    x <- 0
    y <- 0
    z <- 0
    for (grow in row) {
      x2 <- x
      y2 <- y
      z2 <- z
      ch <- substr(grow, 1, 1)
      amount <- as.integer(substring(grow, 2))
      if (ch == "U") y2 <- y + amount
      if (ch == "D") y2 <- y - amount
      if (ch == "F") z2 <- z - amount
      if (ch == "B") z2 <- z + amount
      if (ch == "L") x2 <- x - amount
      if (ch == "R") x2 <- x + amount
      for (xx in x:x2) {
        for (yy in max(1, y):y2) {
          for (zz in z:z2) {
            assign(sprintf("x_%d_%d_%d", xx, yy, zz), 1, envir = set)
            if ((p3) && (xx == 0) && (zz == 0)) {
              trunk <- unique(c(trunk, yy))
            }
          }
        }
      }
      x <- x2
      y <- y2
      z <- z2
    }
    if (p3) leaves <- rbind(leaves, data.frame(x = x, y = y, z = z))
  }
  if (!p3) return(length(ls(set)))
  min_murk <- Inf
  leaves <- unique(leaves)
  trunk <- sort(trunk)
  avg <- floor(mean(leaves$y))
  trunk <- trunk[order(abs(avg - trunk))]
  dx <- c(-1, 1, 0, 0, 0, 0)
  dy <- c(0, 0, -1, 1, 0, 0)
  dz <- c(0, 0, 0, 0, -1, 1)
  best <- Inf
  points <- ls(set)
  for (y in trunk) {
    message(y)
    for (point in points) {
      assign(point, Inf, envir = set)
    }
    x <- 0
    z <- 0
    queue <- list(list(x = 0, y = y, z = 0, d = 0))
    i <- 1
    while (i <= length(queue)) {
      state <- queue[[i]]
      i <- i + 1
      hash <- sprintf("x_%d_%d_%d", state$x, state$y, state$z)
      bestd <- get(hash, envir = set)
      if (bestd <= state$d) next
      assign(hash, state$d, envir = set)

      for (dir in 1:6) {
        nx <- state$x + dx[dir]
        ny <- state$y + dy[dir]
        nz <- state$z + dz[dir]
        hash <- sprintf("x_%d_%d_%d", nx, ny, nz)
        if (exists(hash, envir = set)) {
          bestd <- get(hash, envir = set)
          if (bestd > state$d) {
            newstate <- list(x = nx, y = ny, z = nz, d = state$d + 1)
            queue[[length(queue) + 1]] <- newstate
          }
        }
      }
    }
    murk <- 0
    for (i in seq_len(nrow(leaves))) {
      leaf <- leaves[i, ]
      hash <- sprintf("x_%d_%d_%d", leaf$x, leaf$y, leaf$z)
      murk <- murk + get(hash, envir = set)
    }
    min_murk <- min(min_murk, murk)
  }
  min_murk
}

part3 <- function(d = rl("../data/everybody_codes_e2024_q14_p3.txt")) {
  part2(d, TRUE)
}

part1()
part2()
part3()
