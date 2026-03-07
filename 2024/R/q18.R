source("../../Util/utils.R")

part1 <- function(d = rcg("../data/everybody_codes_e2024_q18_p1.txt"),
                  part3 = FALSE, palmx = NA, palmy = NA) {
  palms <- which(d == "P")
  if (!part3) {
    entry <- as.data.frame(which(d == ".", arr.ind = TRUE))
    entry <- entry[entry$row == 1 | entry$row == nrow(d) |
                   entry$col == 1 | entry$col == ncol(d), ]
    queue <- list()
    for (i in seq_len(nrow(entry))) {
      queue[[i]] <- list(x = entry$col[i], y = entry$row[i], time = 0)
    }
  } else {
    queue <- list(list(x = palmx, y = palmy, time = 0))
  }

  dist <- matrix(0, ncol = ncol(d), nrow = nrow(d))
  dx <- c(0, 1, 0, -1)
  dy <- c(-1, 0, 1, 0)
  i <- 1
  while (i <= length(queue)) {
    state <- queue[[i]]
    i <- i + 1
    for (dir in 1:4) {
      nx <- state$x + dx[dir]
      ny <- state$y + dy[dir]
      if ((nx == 0) || (ny == 0) || (nx > ncol(dist)) || (ny > nrow(dist))) next
      if (d[ny, nx] == '#') next
      if ((dist[ny, nx] > 0) && (dist[ny, nx] <= state$time + 1)) next
      dist[ny, nx] <- state$time + 1
      queue[[length(queue) + 1]] <- list(x = nx, y = ny, time = state$time + 1)
    }
  }
  if (!part3) return(max(dist[palms]))
  dist
}

part2 <- function(d = rcg("../data/everybody_codes_e2024_q18_p2.txt")) {
  part1(d)
}

part3 <- function(d = rcg("../data/everybody_codes_e2024_q18_p3.txt")) {
  palms <- as.data.frame(which(d == "P", arr.ind = TRUE))
  dist <- matrix(0, ncol = ncol(d), nrow = nrow(d))
  for (i in seq_len(nrow(palms))) {
    cat("\r", i," ")
    dist2 <- part1(d, part3 = TRUE, palmx = palms$col[i], palmy = palms$row[i])
    dist2[dist2 == 0] <- Inf
    dist2[palms$row[i], palms$col[i]] <- 0
    dist <- dist + dist2
  }
  start <- as.data.frame(which(dist == min(dist), arr.ind = TRUE))
  dist <- part1(d, part3 = TRUE, palmx = start$col[1], palmy = start$row[1])
  sum(dist[which(d == "P")])
}

part1()
part2()
part3()
