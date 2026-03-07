source("../../Util/utils.R")
part1 <- function(d = rcg("../data/everybody_codes_e3_q02_p1.txt"),
                  part = 1) {
  start <- as.data.frame(which(d == "@", arr.ind = TRUE))
  end <- as.data.frame(which(d == "#", arr.ind = TRUE))
  visits <- new.env(parent = emptyenv())
  surrounded <- function(x, y) {
    (exists(hash(x - 1, y), envir = visits) &&
     exists(hash(x + 1, y), envir = visits) &&
     exists(hash(x, y - 1), envir = visits) &&
     exists(hash(x, y + 1), envir = visits))
  }
  hash <- function(x, y) { sprintf("_%s_%s", x, y) }
  assign(hash(start$col, start$row), 1, envir = visits)
  if (part == 2) assign(hash(end$col, end$row), 1, envir = visits)
    #urdl
  dx <- c(0, 1, 0, -1)
  dy <- c(-1, 0, 1, 0)
  steps <- 0
  x <- start$col
  y <- start$row
  dir <- 1
  while(TRUE) {
    nx <- x + dx[dir]
    ny <- y + dy[dir]
    dir <- if (dir == 4) 1 else dir + 1
    if ((nx == end$col) && (ny == end$row) && (part == 1)) return(steps + 1)
    if (exists(hash(nx, ny), envir = visits)) next
    x <- nx
    y <- ny
    steps <- steps + 1
    assign(hash(nx, ny), 1, envir = visits)
    if (part == 2) {
      for (k in 1:4) {
        sx <- x + dx[k]
        sy <- y + dy[k]
        h <- hash(sx, sy)
        if ((!exists(h, envir = visits)) && (surrounded(sx, sy))) {
          assign(h, 1, envir = visits)
        }
      }
      if (surrounded(end$col, end$row)) return(steps)
    }
  }
}

part2 <- function(d = rcg("../data/everybody_codes_e3_q02_p2.txt")) {
  part1(d, 2)
}

flood <- function(d, x, y, n) {
  dx <- c(0, 1, 0, -1)
  dy <- c(1, 0, -1, 0)
  q <- list()
  q[[1]] <- c(x, y)
  d[y, x] <- n
  while(length(q) > 0) {
    q2 <- list()
    for (i in 1:length(q)) {
      p <- q[[i]]
      for (dir in 1:4) {
        nx <- p[1] + dx[dir]
        ny <- p[2] + dy[dir]
        if ((nx < 1) || (ny < 1) || (nx > ncol(d)) || (ny > nrow(d))) next
        if (d[ny, nx] != 0) next
        d[ny, nx] <- n
        q2[[length(q2) + 1]] <- c(nx, ny)
      }
    }
    q <- q2
  }
  d
}


part3 <- function(d = rig("../data/everybody_codes_e3_q02_p3.txt",
                          chars = c(".", "@", "#"), pad = 20)) {
  # 0 = empty (.)
  # 1 = "@"
  # 2 = "#"
  # 3 = visited / enclosed

  d2 <- flood(d, 1, 1, 3) # Padding lets us fill from the outside.
  d[d2 == 0] <- 3         # Fill the enclosed.
  bones <- as.data.frame(which(d == 2, arr.ind = TRUE))
  for (j in nrow(bones):1) {
    bone <- bones[j, ]
    if (d[bone$row - 1, bone$col] == 0) next
    if (d[bone$row + 1, bone$col] == 0) next
    if (d[bone$row, bone$col - 1] == 0) next
    if (d[bone$row, bone$col + 1] == 0) next
    bones <- bones[-j, ]
  }
  dx <- c(0,0,0,1,1,1,0,0,0,-1,-1,-1)
  dy <- c(-1,-1,-1,0,0,0,1,1,1,0,0,0)
  start <- as.data.frame(which(d == 1, arr.ind = TRUE))

  steps <- 0
  x <- start$col
  y <- start$row
  dir <- 1
  plot(start$col, -start$row, xlim = c(1, ncol(d)), ylim = c(1, -nrow(d)), type="p")
  points(3, -5, col="red")
  while (nrow(bones) > 0) {
    nx <- x + dx[dir]
    ny <- y + dy[dir]
    dir <- if (dir == 12) 1 else dir + 1
    if (d[ny, nx] != 0) next
    x <- nx
    y <- ny
    steps <- steps + 1
    message(steps)
    d[ny, nx] <- 3
    points(nx, ny)
    browser()
    for (k in c(1,4,7,10)) {
      sx <- x + dx[k]
      sy <- y + dy[k]
      if (d[sy, sx] == 0) {
        d2 <- flood(d, sx, sy, 4)
        if (d2[1,1] != 4) {
          d[d2 == 4] <- 3
        }
      }
    }
    j <- nrow(bones)
    while (j > 0) {
      bone <- bones[j, ]
      if (d[bone$row - 1, bone$col] == 0) break
      if (d[bone$row + 1, bone$col] == 0) break
      if (d[bone$row, bone$col - 1] == 0) break
      if (d[bone$row, bone$col + 1] == 0) break
      j <- j - 1
    }
    if (j > 0) bones <- bones[1:j, ]
    else return(steps)
    plot()
  }
  steps
}

options(digits=16)
part1()
part2()
part3()
