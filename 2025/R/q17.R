source("../../Util/utils.R")

gen_circle <- function(r) {
  m <- matrix("", ncol = (2 * r) + 1, nrow = (2 * r) + 1)
  Xc <- r + 1
  Yc <- r + 1
  for (Xv in 1:ncol(m)) {
    for (Yv in 1:nrow(m)) {
      if ((Xv - Xc) * (Xv - Xc) + (Yv - Yc) * (Yv - Yc) <= r * r) {
        m[Yv, Xv] <- "X"
      }
    }
  }
  m[r+1, r+1] <- "@"
  m
}

part1 <- function(d = rcg("../data/everybody_codes_e2025_q17_p1.txt")) {
  m <- gen_circle(10)
  vol <- as.data.frame(which(d == "@", arr.ind = TRUE))
  d <- d[(vol$row - 10):(vol$row + 10), (vol$col - 10):(vol$col + 10)]
  select <- d[which(m != "")]
  select[select == "@"] <- 0
  sum(as.integer(select))
}

part2 <- function(d = rcg("../data/everybody_codes_e2025_q17_p2.txt")) {
  mx <- 0
  score <- 0
  vol <- as.data.frame(which(d == "@", arr.ind = TRUE))
  d2 <- matrix("", ncol = ncol(d), nrow = nrow(d))
  maxr <- (ncol(d) %/% 2) - 1
  for (r in 1:maxr) {
    m <- gen_circle(r)
    d2[(vol$row - r):(vol$row + r), (vol$col - r):(vol$col + r)] <- m
    lava <- which(d2 == "X")
    vals <- d[lava]
    vals <- vals[vals != ""]
    d[lava] <- ""
    v <- sum(as.integer(vals))
    if (v > mx) {
      mx <- v
      score <- v * r
    }
  }
  score
}

explore <- function(map, sx, sy, voly, limit) {
  nc <- ncol(map)
  nr <- nrow(map)
  dx <- c(0, 1, 0, -1)
  dy <- c(1, 0, -1, 0)
  visit <- list()
  visit[[1]] <- matrix(Inf, ncol = nc, nrow = nr)
  visit[[2]] <- matrix(Inf, ncol = nc, nrow = nr)
  visit[[1]][sy, sx] <- 0
  visit[[1]][voly, sx:nc] <- 0
  visit[[2]][voly, 1:sx] <- 0

  q <- list()
  q[[1]] <- c(sx, sy, 0, 1)
  best <- Inf
  while (length(q) > 0) {
    q2 <- list()
    for (i in seq_along(q)) {
      state <- q[[i]]
      for (dir in 1:4) {
        nx <- state[1] + dx[dir]
        ny <- state[2] + dy[dir]
        if ((nx < 1) || (ny < 1) || (nx > nc) || (ny > nr)) next
        if ((map[ny, nx] == "X") || (map[ny, nx] == "@")) next

        ns <- state[3]
        if (map[ny, nx] != "S") ns <- ns + as.integer(map[ny, nx])
        if (ns > limit) next

        M <- state[4]
        if (visit[[M]][ny, nx] <= ns) next

        if ((nx == sx) && (ny == sy) && (M == 2)) {
          best <- min(best, ns)
          next
        }
        if (ns >= best) next

        visit[[M]][ny, nx] <- min(visit[[M]][ny, nx], ns)
        if ((ny > voly) && (nx == sx) && (M == 1)) {
          visit[[2]][ny, nx] <- min(visit[[2]][ny, nx], ns)
          q2[[length(q2) + 1]] <- c(nx, ny, ns, 2)
        } else {
          q2[[length(q2) + 1]] <- c(nx, ny, ns, M)
        }
      }
    }
    q <- q2
  }
  best
}

part3 <- function(d = rcg("../data/everybody_codes_e2025_q17_p3.txt")) {
  r <- 0 # 38
  sec <- 30 # 1170
  vol <- as.data.frame(which(d == "@", arr.ind = TRUE))
  start <- as.data.frame(which(d == "S", arr.ind = TRUE))
  while(TRUE) {
    d2 <- d
    if (r >= 1) {
      m <- gen_circle(r)
      d2[(vol$row - r):(vol$row + r), (vol$col - r):(vol$col + r)] <- m
      d2[d2 == ""] <- d[d2 == ""]
    }
    best <- explore(d2, start$col, start$row, vol$row, sec)
    if (!is.infinite(best)) return(best * r)
    r <- r + 1
    sec <- sec + 30
  }
}

part1()
part2()
part3()
