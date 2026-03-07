source("../../Util/utils.R")

add <- function(x, y) {
  c(x[1] + y[1], x[2] + y[2])
}

mul <- function(x, y) {
  c(x[1] * y[1] - x[2] * y[2], x[1] * y[2] + y[1] * x[2])
}

div <- function(x, y) {
  c(x[1] %/% y[1], x[2] %/% y[2])
}

part1 <- function(d = rl("../data/everybody_codes_e2025_q02_p1.txt")) {
  A <- as.integer(strsplit(gsub("A=\\[", "", gsub("\\]", "", d)), ",")[[1]])
  R <- c(0, 0)
  for (i in 1:3) {
    R <- add(div(mul(R, R), c(10, 10)), A)
  }
  sprintf("[%d,%d]", R[1], R[2])
}


part2 <- function(d = rl("../data/everybody_codes_e2025_q02_p2.txt"),
                  res = 10) {
  A <- as.integer(strsplit(gsub("A=\\[", "", gsub("\\]", "", d)), ",")[[1]])
  oppo <- add(A, c(1000, 1000))
  xs <- seq(from = A[1], to = oppo[1], by = res)
  ys <- seq(from = A[2], to = oppo[2], by = res)
  df <- data.frame(x = rep(xs, each = length(ys)), y = ys,
                   cx = 0, cy = 0)
  for (i in 1:100) {
    R1 <- trunc((df$cx * df$cx - df$cy * df$cy) / 100000) + df$x
    R2 <- trunc((df$cx * df$cy + df$cx * df$cy) / 100000) + df$y
    df$cx <- R1
    df$cy <- R2
    df <- df[abs(df$cx) <= 1000000, ]
    df <- df[abs(df$cy) <= 1000000, ]
  }
  nrow(df)
}

part3 <- function(d = rl("../data/everybody_codes_e2025_q02_p3.txt")) {
  part2(d, 1)
}

part1()
part2()
part3()
