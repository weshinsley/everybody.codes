source("../../Util/utils.R")

part1 <- function(d = rcg("../data/everybody_codes_e2024_q13_p1.txt"),
                  start = "S", end = "E") {
  startxy <- as.data.frame(which(d == start, arr.ind = TRUE))
  best <- matrix(Inf, ncol = ncol(d), nrow = nrow(d))
  dlevs <- d
  dlevs[dlevs == start] <- "0"
  dlevs[dlevs == end] <- "0"
  best[startxy$row, startxy$col] <- 0L
  dx <- c(0, 1, 0, -1)
  dy <- c(-1, 0, 1, 0)
  queue <- list(list(x = startxy$col, y = startxy$row, dist = 0))
  i <- 1
  while (i <= length(queue)) {
    state <- queue[[i]]
    i <- i + 1
    for (dir in 1:4) {
      nx <- state$x + dx[dir]
      ny <- state$y + dy[dir]
      if ((nx <= 0) || (ny <= 0) || (nx > ncol(d)) || (ny > nrow(d))) next
      if (dlevs[ny, nx] == '#') next
      fromlev <- as.integer(dlevs[state$y, state$x])
      nextlev <- as.integer(dlevs[ny, nx])
      levs <- sort(c(fromlev, nextlev))
      levdiff <- min(levs[2] - levs[1], (10 + levs[1]) - levs[2])
      newdist <- state$dist + 1 + levdiff
      if (newdist < best[ny, nx]) {
        best[ny, nx] <- newdist
        newstate <- list(x = nx, y = ny, dist = newdist)
        queue[[length(queue) + 1]] <- newstate
      }
    }
  }
  min(best[which(d == end)])
}


part2 <- function(d = rcg("../data/everybody_codes_e2024_q13_p2.txt")) {
  part1(d)
}

part3 <- function(d = rcg("../data/everybody_codes_e2024_q13_p3.txt")) {
  part1(d, start = "E", end = "S")
}

part1()
part2()
part3()
