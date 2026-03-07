source("../../Util/utils.R")

dx <- c(1, 0, -1, 0)
dy <- c(0, 1, 0, -1)

explode <- function(d, sx, sy) {
  visited <- matrix(0, ncol = ncol(d), nrow = nrow(d))
  q <- list()
  q[[1]] <- c(sx, sy)
  i <- 1
  visited[sy, sx] <- 1
  while (i <= length(q)) {
    state <- q[[i]]
    current <- d[state[2], state[1]]
    i <- i + 1
    for (dir in 1:4) {
      nx <- state[1] + dx[dir]
      ny <- state[2] + dy[dir]
      if ((nx < 1) || (ny < 1) || (nx > ncol(d)) || (ny > nrow(d))) next
      if (visited[ny, nx] == 1) next
      newnum <- d[ny, nx]
      if (newnum > current) next
      q[[length(q) + 1]] <- c(nx, ny)
      visited[ny, nx] <- 1
    }
  }
  visited
}

part1 <- function(d = rig("../data/everybody_codes_e2025_q12_p1.txt", pad = 0)) {
  return(sum(explode(d, 1, 1)))
}

part2 <- function(d = rig("../data/everybody_codes_e2025_q12_p2.txt", pad = 0)) {
  v1 <- explode(d, 1, 1)
  v2 <- explode(d, ncol(d), nrow(d))
  sum(v1 > 0 | v2 > 0)
}

part3 <- function(d = rig("../data/everybody_codes_e2025_q12_p3.txt", pad = 0)) {
  gtotal <- 0
  for (run in 1:3) {
    biggest <- max(d[d < Inf])
    best <- 0
    bestv <- 0
    while(TRUE) {
      starts <- as.data.frame(which(d == biggest, arr.ind = TRUE))
      for (i in seq_len(nrow(starts))) {
        v <- explode(d, starts$col[i], starts$row[i])
        if (sum(v) > best) {
          best <- sum(v)
          bestv <- v
        }
      }
      biggest <- biggest - 1
      if (sum(d <= biggest) < best) break
    }
    gtotal <- gtotal + best
    d[bestv == 1] <- Inf
  }
  gtotal
}

part1()
part2()
part3()
