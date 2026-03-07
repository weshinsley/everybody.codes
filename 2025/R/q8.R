source("../../Util/utils.R")

part1 <- function(d = rl("../data/everybody_codes_e2025_q08_p1.txt"),
                  nails = 32) {
  d <- as.integer(unlist(strsplit(d, ",")[[1]]))
  sum(abs(diff(d)) == nails %/% 2)

}

part2 <- function(d = rl("../data/everybody_codes_e2025_q08_p2.txt"),
                  nails = 256, part = 2) {

  count_knots <- function(x, y, join) {
    kn <- 0
    set1 <- x:y
    set2 <- c(1:min(x, y), max(x,y):nails)
    set1 <- set1[!set1 %in% c(x, y)]
    set2 <- set2[!set2 %in% c(x, y)]
    for (j in set1) {
      for (k in set2) {
        kn <- kn + join[j, k]
      }
    }
    kn
  }

  join <- matrix(0, nrow = nails, ncol = nails)
  d <- as.integer(unlist(strsplit(d, ",")[[1]]))
  x <- d[1]
  y <- d[2]
  join[y, x] <- join[y, x] + 1
  join[x, y] <- join[x, y] + 1
  i <- 3
  knots <- 0
  while (i <= length(d)) {
    x <- y
    y <- d[i]
    join[y, x] <- join[y, x] + 1
    join[x, y] <- join[x, y] + 1
    if (part == 2) knots <- knots + count_knots(x, y, join)
    i <- i + 1
  }
  if (part == 2) return(knots)

  mx <- 0
  for (i in 1:nails) {
    for (j in 1:nails) {
      if (i == j) next
      kn <- 0
      if (join[i, j]) kn <- 1
      kn <- kn + count_knots(i, j, join)
      mx <- max(mx, kn)
    }
  }
  mx
}

part3 <- function(d = rl("../data/everybody_codes_e2025_q08_p3.txt"),
                  nails = 256) {
  part2(d, nails = nails, part = 3)
}

part1()
part2()
part3()
