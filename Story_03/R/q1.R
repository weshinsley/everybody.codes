source("../../Util/utils.R")

to_bin <- function(x) {
  x <- strsplit(x, "")[[1]]
  x <- x %in% LETTERS
  t <- 0
  p <- 1
  for (i in length(x):1) {
    t <- t + p * x[i]
    p <- p * 2
  }
  t
}

part1 <- function(d = rl("../data/everybody_codes_e3_q01_p1.txt")) {
  sum(unlist(lapply(d, function(x) {
    x <- strsplit(x, "[: ]")[[1]]
    for (i in 2:4) x[i] <- to_bin(x[i])
    x <- as.integer(x)
    return (if ((x[2] < x[3]) && (x[4] < x[3])) x[1] else 0)})))
}

get_p2_df <- function(d) {
  x <- as.data.frame(data.table::rbindlist(lapply(d, function(y) {
    x <- strsplit(y, "[: ]")[[1]]
    for (i in 2:5) x[i] <- to_bin(x[i])
    x <- as.integer(x)
    data.frame(id = x[1], r = x[2], g = x[3], b = x[4], s = x[5])})))
}

part2 <- function(d = rl("../data/everybody_codes_e3_q01_p2.txt")) {
  x <- get_p2_df(d)
  x$sum <- x$r + x$g + x$b
  x <- x[x$s == max(x$s), ]
  x <- x[x$sum == min(x$sum), ]
  x$id

}

part3 <- function(d = rl("../data/everybody_codes_e3_q01_p3.txt")) {
  x <- get_p2_df(d)
  x$matte <- x$s <= 30
  x$shiny <- x$s >= 33
  x <- x[x$matte | x$shiny, ]
  x$dr <- x$r > x$g & x$r > x$b
  x$dg <- x$g > x$r & x$g > x$b
  x$db <- x$b > x$g & x$b > x$r
  x <- x[x$dr | x$dg | x$dr, ]
  x$group <- x$matte + (2 * x$shiny) + (4 * x$dr) + (8 * x$dg) + (16 * x$db)
  tab <- table(x$group)
  sum(x$id[x$group == as.integer(names(tab)[which.max(tab)])])
}

part1()
part2()
part3()

