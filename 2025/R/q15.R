source("../../Util/utils.R")

dx <- c(0, 1, 0, -1)
dy <- c(1, 0, -1, 0)

solve_path <- function(m, xs = NULL, ys = NULL) {
  dist <- matrix(Inf, ncol = ncol(m), nrow = nrow(m))
  start <- as.data.frame(which(m == "S", arr.ind = TRUE))
  end <- as.data.frame(which(m == "E", arr.ind = TRUE))
  q <- list(c(start$col, start$row, 0))
  dist[start$y, start$x] <- 0
  i <- 1
  best <- Inf
  while (i <= length(q)) {
    state <- q[[i]]
    i <- i + 1
    x <- state[1]
    y <- state[2]
    steps <- state[3]
    if (steps >= best) next
    if (steps > dist[y, x]) next
    if ((x == end$col) && (y == end$row)) {
      best <- min(best, dist[y, x])
      next
    }
    for (dir in 1:4) {
      nx <- x + dx[dir]
      ny <- y + dy[dir]
      if ((nx == 0) || (ny == 0) || (nx > ncol(m)) || (ny > nrow(m))) next
      if (m[ny, nx] == '#') next
      nd <- 1
      if (!is.null(xs)) {
        x1 <- xs[x]
        x2 <- xs[nx]
        y1 <- ys[y]
        y2 <- ys[ny]
        nd <- abs(x1 - x2) + abs(y1 - y2)

      }
      if (dist[ny, nx] <= (steps + nd)) next
      dist[ny, nx] <- (steps + nd)
      q[[length(q) + 1]] <- c(nx, ny, steps + nd)
    }
  }
  best

}

part1 <- function(d = rl("../data/everybody_codes_e2025_q15_p1.txt")) {
  d <- strsplit(d, ",")[[1]]
  x <- 500
  y <- 500
  m <- matrix(" ", ncol = 1000, nrow = 1000)
  m[y, x] <- "S"
  dir <- 1
  for (move in d) {
    if (substr(move, 1, 1) == "L") dir <- if (dir == 1) 4 else dir - 1
    else dir <- if (dir == 4) 1 else dir + 1
    for (z in 1:as.integer(substring(move, 2))) {
      x <- x + dx[dir]
      y <- y + dy[dir]
      m[y, x] <- "#"
    }
  }
  m[y, x] <- "E"
  sq <- as.data.frame(which(m != " ", arr.ind = TRUE))
  m <- m[min(sq$row):max(sq$row), min(sq$col):max(sq$col)]
  solve_path(m)
}

part2 <- function(d = rl("../data/everybody_codes_e2025_q15_p2.txt")) {
  part1(d)
}

part3 <- function(d = rl("../data/everybody_codes_e2025_q15_p3.txt")) {
  df <- data.frame()
  d <- strsplit(d, ",")[[1]]
  x <- 0
  y <- 0
  dir <- 1
  for (move in d) {
    if (substr(move, 1, 1) == "L") dir <- if (dir == 1) 4 else dir - 1
    else dir <- if (dir == 4) 1 else dir + 1
    dist <- as.integer(substring(move, 2))
    nx <- x + dist * dx[dir]
    ny <- y + dist * dy[dir]
    df <- rbind(df, data.frame(x1 = x, y1 = y, x2 = nx, y2 = ny, dist = dist))
    x <- nx
    y <- ny
  }

  df <- rbind(df, data.frame(x1 = x, y1 = y, x2 = x, y2 = y, dist = 0))
  xs <- sort(unique(c(df$x1 - 1, df$x1, df$x1 + 1)))
  ys <- sort(unique(c(df$y1 - 1, df$y1, df$y1 + 1)))
  m <- matrix(" ", ncol = length(xs), nrow = length(ys))
  for (n in seq_len(nrow(df))) {
    row <- df[n, ]
    for (i in which(xs == row$x1):which(xs == row$x2)) {
      for (j in which(ys == row$y1):which(ys == row$y2)) {
        m[j, i] <- "#"
      }
    }
  }
  m[which(ys == df$y1[1]), which(xs == df$x1[1])] <- "S"
  m[which(ys == df$y1[nrow(df)]), which(xs == df$x1[nrow(df)])] <- "E"
  solve_path(m, xs, ys)
}

part1()
part2()
part3()
