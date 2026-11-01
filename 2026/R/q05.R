source("../../Util/utils.R")

grow <- function(d, seed, soil, res, valid, delta, one_step = FALSE) {
  while (TRUE) {
    i <- which(d == seed)
    edges <- unique(as.vector(outer(i, delta, "+")))
    edges <- edges[d[edges] %in% soil]
    fill <- unlist(lapply(edges, function(x) valid(d[x + delta])))
    if (sum(fill) == 0) break
    d[edges[fill]] <- seed
    if (one_step) {
      d[edges[fill]] <- res
      break
    }
    d[i] <- res
  }
  d
}

solve <- function(f, part = 1) {
  d <- rig(f, pad = 2)
  nc <- ncol(d)
  nr <- nrow(d)
  sx <- (nc %/% 2)
  sy <- (nr %/% 2)
  d <- as.integer(t(d))
  expl <- d * 0
  delta <- c(-nc - 1, -nc, -nc + 1, -1, 1, nc - 1, nc, nc + 1)
  d[1 + sx + (sy * nc)] <- 2
  d <- grow(d, 2, c(0, 1), 2, function(x) { sum(x > 0) >= 5 }, delta)
  d[d < 2] <- 0
  if (part == 1) return(sum(d == 2))

  land <- which(d == 2)
  sand <- unlist(lapply(land, function(x) any(d[x + delta] == 0)))
  d[land[sand]] <- 3
  d <- grow(d, 3, 2, 3, function(x) { sum(x == 3) >= 5 }, delta)
  if (part == 2) return(sum(d == 3))

  delta2 <- c((-nc-nc-1):(-nc-nc+1), (-nc-2):(-nc+2),
             -2, -1, 1, 2, (nc-2):(nc+2), (nc+nc-1):(nc+nc+1))

  tree <- unlist(lapply(land, function(x) all(d[x + delta2] == 2)))
  d[land[tree]] <- 4

  valid <- function(x) {(sum(x == 4) >= 4) && (sum(x == 3) == 0)}
  d <- grow(d, 2, 2, 4, valid, delta, TRUE)
  d <- grow(d, 2, 2, 4, valid, delta, TRUE)
  return(sum(d == 4))
}

part1 <- function() {
  solve("../data/everybody_codes_e2026_q05_p1.txt", part = 1)
}

part2 <- function() {
  solve("../data/everybody_codes_e2026_q05_p2.txt", part = 2)
}

part3 <- function() {
  solve("../data/everybody_codes_e2026_q05_p3.txt", part = 3)
}

part1()
part2()
part3()
