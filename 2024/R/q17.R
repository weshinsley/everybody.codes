source("../../Util/utils.R")

prims <- function(stars) {
  dist <- matrix(0, nrow = nrow(stars), ncol = nrow(stars))
  for (i in seq_len(nrow(stars))) {
    dist[i, ] <-  abs(stars$row[i] - stars$row) + abs(stars$col[i] - stars$col)
  }
  diag(dist) <- Inf
  visited <- 1
  not_visited <- 2:nrow(stars)
  total <- 0
  while (length(visited) < nrow(stars)) {
    best_dist <- Inf
    best_from <- Inf
    best_to <- Inf
    for (from in visited) {
      for (to in not_visited) {
        d <- dist[from, to]
        if (d < best_dist) {
          best_dist <- d
          best_from <- from
          best_to <- to
        }
      }
    }
    total <- total + best_dist
    dist[best_from, best_to] <- Inf
    dist[best_to, best_from] <- Inf
    visited <- c(visited, best_to)
    not_visited <- not_visited[not_visited != best_to]
  }
  total
}

part1 <- function(d = rcg("../data/everybody_codes_e2024_q17_p1.txt")) {
  stars <- as.data.frame(which(d == "*", arr.ind = TRUE))
  prims(stars) + nrow(stars)
}

part2 <- function(d = rcg("../data/everybody_codes_e2024_q17_p2.txt")) {
  part1(d)
}

part3 <- function(d = rcg("../data/everybody_codes_e2024_q17_p3.txt")) {
  stars <- as.data.frame(which(d == "*", arr.ind = TRUE))
  dist <- matrix(0, nrow = nrow(stars), ncol = nrow(stars))
  for (i in seq_len(nrow(stars))) {
    dist[i, ] <-  abs(stars$row[i] - stars$row) + abs(stars$col[i] - stars$col)
  }
  diag(dist) <- Inf
  dist[dist >= 6] <- Inf
  df <- as.data.frame(data.table::rbindlist(
    lapply(seq_len(nrow(dist)), function(i) {
      row <- dist[i, ]
      if (any(!is.infinite(row))) {
        links <- which(!is.infinite(row))
        dists <- row[links]
      data.frame(from = i, to = links, dist = dists)
      } else NULL
    })
  ))

  cons <- df[, c("from", "to")]
  score <- c()
  while (nrow(cons) > 0) {
    superset <- c(cons$from[1], cons$to[1])
    while(TRUE) {
      more <- c(cons$from[cons$to %in% superset], cons$to[cons$from %in% superset])
      more <- more[!more %in% superset]
      if (length(more) == 0) break
      superset <- sort(unique(c(superset, more)))
    }
    score <- c(score, prims(stars[superset, ]) + length(superset))
    cons <- cons[!cons$from %in% superset, ]
    cons <- cons[!cons$to %in% superset, ]
  }
  score <- sort(score, decreasing = TRUE)
  score[1] * score[2] * score[3]
}

part1()
part2()
part3()
