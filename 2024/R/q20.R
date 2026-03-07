source("../../Util/utils.R")

part1 <- function(d = rcg("../data/everybody_codes_e2024_q20_p1.txt")) {

  cache <- new.env(parent = emptyenv())
  alt <- 1000
  start <- as.data.frame(which(d == "S", arr.ind = TRUE))
  dx <- c(0, -1, 0, 1)
  dy <- c(1, 0, -1, 0)

  explore <- function(px, py, x, y, time_left) {
    hash <- sprintf("%d_%d_%d_%d_%d", px, py, x, y, time_left)
    if (exists(hash, envir = cache)) {
      return(get(hash, envir = cache))
    }
    ch <- d[y, x]
    if (ch == "#") {
      assign(hash, -Inf, envir = cache)
      return(-Inf)
    }
    delta <- (ch == "+") - (2 * (ch == "-")) - (ch == ".")
    if (time_left == 0) {
      assign(hash, delta, envir = cache)
      return(delta)
    }
    best <- (-Inf)
    for (dir in 1:4) {
      nx <- x + dx[dir]
      ny <- y + dy[dir]
      if ((nx < 1) || (ny < 1) || (nx > ncol(d)) || (ny > nrow(d))) next
      if ((nx == px) && (ny == py)) next
      best <- max(best, explore(x, y, nx, ny, time_left - 1))
    }
    assign(hash, delta + best, envir = cache)
    return(delta + best)
  }

  1000 + explore(-99, -99, start$col, start$row, 100)
}

part2 <- function(d = rcg("../data/everybody_codes_e2024_q20_p2.txt")) {
  dx <- c(0, -1, 0, 1)
  dy <- c(1, 0, -1, 0)
  q1 <- 1
  q2 <- 2
  qs <- list()
  cache <- new.env(parent = emptyenv())
  qs[[q1]] <- new.env(parent = emptyenv())
  qs[[q2]] <- new.env(parent = emptyenv())

  X <- 1
  Y <- 2
  DIR <- 3
  CP <- 4

  start <- as.data.frame(which(d == "S", arr.ind = TRUE))
  hash <- sprintf("_%d_%d_%d_%d", start$col, start$row, 1, 0)
  assign(hash, 10000, envir = qs[[q1]])
  best_time <- Inf
  time <- 0
  while(TRUE) {
    states <- ls(qs[[q1]])
    for (state in states) {
      alt <- get(state, envir = qs[[q1]])
      state <- as.integer(strsplit(substring(state, 2), "_")[[1]])
      if ((state[CP] == 3) && (state[X] == start$col) &&
          (state[Y] == start$row) && (alt >= 10000)) {
        return(time)
      }
      hash <- sprintf("_%d_%d_%d_%d", state[X], state[Y], state[DIR], state[CP])
      if (exists(hash, envir = cache)) {
        prev <- get(hash, envir = cache)
        if (prev >= alt) next
      }
      assign(hash, alt, envir = cache)
      left <- if (state[DIR] == 1) 4 else state[DIR] - 1
      right <- if (state[DIR] == 4) 1 else state[DIR] + 1
      for (ndir in c(left, state[DIR], right)) {
        nx <- state[X] + dx[ndir]
        ny <- state[Y] + dy[ndir]
        if ((nx < 1) || (ny < 1) || (nx > ncol(d)) || (ny > nrow(d))) next
        ch <- d[ny, nx]
        if (ch == '#') next
        ncp <- state[CP]
        if ((ch == 'A') && (ncp == 0)) ncp <- 1
        else if ((ch == 'B') && (ncp == 1)) ncp <- 2
        else if ((ch == 'C') && (ncp == 2)) ncp <- 3

        nalt <- alt
        if (ch %in% c(".", "A", "B" ,"C", "S")) nalt <- nalt - 1
        else if (ch == "-") nalt <- nalt - 2
        else if (ch == "+") nalt <- nalt + 1

        hash2 <- sprintf("_%d_%d_%d_%d", nx, ny, ndir, ncp)
        if (exists(hash2, envir = qs[[q2]])) {
          oldalt <- get(hash2, envir = qs[[q2]])
          if (nalt > oldalt) assign(hash2, nalt, envir = qs[[q2]])
        } else {
          assign(hash2, nalt, envir = qs[[q2]])
        }
      }
    }
    qs[[q1]] <- new.env(parent = emptyenv())
    if (length(ls(qs[[q2]])) == 0) break
    q1 <- 3 - q1
    q2 <- 3 - q2
    time <- time + 1
    message(time)
  }
}

part3 <- function(d = rcg("../data/everybody_codes_e2024_q20_p3.txt"),
                  alt = 384400) {

  # So if my data is reasonably like everyone else's, there's one
  # best column we want to camp in.

  tots <- matrix(0, ncol = ncol(d), nrow = nrow(d))
  tots[d == '#'] <- -999
  tots[d == '+'] <- 1
  tots[d == '-'] <- -2
  tots[d == '.'] <- -1
  sums <- colSums(tots)
  best <- abs(max(sums))

  # There might be multiple similar ones - we want to choose the
  # nearest.

  opts <- which(sums == -best)
  start <- as.data.frame(which(d == "S", arr.ind = TRUE))$col
  pick <- opts[which(abs(opts - start) == min(abs(opts - start)))]
  steps <- abs(pick - start)

  # Got it. For me, steps = 2, pick = 32 (start = 30)
  # Take (steps) steps, which changes alt by 2, but not dist.
  alt <- alt - steps

  # Now do as many complete cycle as we can, which all increase dist
  # by nrow(d). Each cycle reduces alt by `best`

  repeats <- alt %/% best
  dist <- repeats * nrow(d)

  # And the leftovers - 3 for me.

  remain <- alt %% best

  i <- 1
  while (remain != 0) {
    dist <- dist + 1
    remain <- remain + tots[i, pick]
    i <- i + 1
  }
  dist
}

part1()
part2()
part3()
