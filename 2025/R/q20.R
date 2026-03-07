source("../../Util/utils.R")

part1 <- function(d = rcg("../data/everybody_codes_e2025_q20_p1.txt")) {
  tr <- 0
  last <- ncol(d) - 1
  start <- 1
  for (j in 1:(nrow(d) - 1)) {
    for (i in start:last) {
      if (d[j, i] != "T") next
      if (d[j, i + 1] == "T") tr <- tr + 1
      if (((i %% 2) != (j %% 2)) && (d[j + 1, i] == "T")) tr <- tr + 1
    }
    start <- start + 1
    last <- last - 1
  }
  tr
}

part2 <- function(d = rcg("../data/everybody_codes_e2025_q20_p2.txt")) {
  part3(d, FALSE)
}

rotate_matrix <- function(nr) {
  nc <- (2 * nr) - 1
  trans <- matrix(0, nrow = nr, ncol = nc)

  for (y in 1:nr) {
    sx <- ((nc + 1) / 2) + (y-1)
    sy <- (nr + 1) - y
    for (x in seq(from = y, to = (nc + 1) - y, by = 2)) {
      trans[y, x] <- (nr * (sx - 1)) + sy
      sy <- sy - 1
      if (x == (nc + 1) - y) break
      trans[y, x + 1] <- (nr * (sx - 1)) + sy
      sx <- sx - 1
    }
  }
  trans[trans == 0] <- which(trans == 0)
  trans
}

part3 <- function(d = rcg("../data/everybody_codes_e2025_q20_p3.txt"),
                  rotate = TRUE) {
  nc <- ncol(d)
  nr <- nrow(d)
  trans <- rotate_matrix(nrow(d))
  start <- as.data.frame(which(d == "S", arr.ind = TRUE))
  d[start$row, start$col] <- "T"

  maps <- list()
  maps[[1]] <- d
  maps[[2]] <- matrix(d[trans], ncol = nc, nrow = nr)
  maps[[3]] <- matrix(d[trans][trans], ncol = nc, nrow = nr)

  visits <- list()
  ends <- list()
  for (v in 1:3) {
    visits[[1]] <- matrix(Inf, nrow = nr, ncol = nc)
    visits[[2]] <- matrix(Inf, nrow = nr, ncol = nc)
    visits[[3]] <- matrix(Inf, nrow = nr, ncol = nc)
    ends[[v]] <- as.data.frame(which(maps[[v]] == "E", arr.ind = TRUE))
    maps[[v]][ends[[v]]$row, ends[[v]]$col] <- "T"
  }

  best <- Inf
  q <- list()
  M <- 1
  q[[1]] <- c(start$col, start$row, 0)
  visits[[1]][start$row, start$col] <- 0

  while(length(q) > 0) {
    q2 <- list()
    if (rotate) M <- if (M == 3) 1 else M + 1
    for (i in seq_along(q)) {
      here <- q[[i]]
      if (here[3] >= best) next

      left <- c(here[1] - 1, here[2], here[3] + 1)
      right <- c(here[1] + 1, here[2], here[3] + 1)
      if ((here[2] %% 2) == 1) {
        if ((here[1] %% 2) == 1) upd <- c(here[1], here[2] - 1, here[3] + 1)
        else upd <- c(here[1], here[2] + 1, here[3] + 1)
      } else {
        if ((here[1] %% 2) == 1) upd <- c(here[1], here[2] + 1, here[3] + 1)
        else upd <- c(here[1], here[2] - 1, here[3] + 1)
      }
      here[3] <- here[3] + 1
      moves <- list(left, right, upd, here)
      for (m in 1:4) {
        move <- moves[[m]]
        if ((move[1] < 1) || (move[1] > nc)) next
        if ((move[2] < 1) || (move[2] > nr)) next
        if (maps[[M]][move[2], move[1]] != "T") next
        if (visits[[M]][move[2], move[1]] <= move[3]) next
        if (move[3] > best) next
        if ((move[1] == ends[[M]]$col) && (move[2] == ends[[M]]$row)) {
          best <- min(best, move[3])
          next
        }
        visits[[M]][move[2], move[1]] <- move[3]
        q2[[length(q2) + 1]] <- move
      }
    }
    q <- q2

  }
  best
}

part1()
part2()
part3()
