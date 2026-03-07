source("../../Util/utils.R")

part1 <- function(d = ri("../data/everybody_codes_e2024_q08_p1.txt")) {
  base <- 1
  x <- 1
  t <- 1
  while (t < d) {
    x <- x + 2
    t <- t + x
    base <- base + 2
  }
  base * (t - d)
}

part2 <- function(d = ri("../data/everybody_codes_e2024_q08_p2.txt"),
                  priests = 10, blocks = 20240000) {
  thick <- 1
  base <- 1
  total <- 1
  while (total < blocks) {
    base <- base + 2
    thick <- (thick * d) %% acolytes
    total <- total + (base * thick)
  }
  base * (total - blocks)
}

part3 <- function(d = ri("../data/everybody_codes_e2024_q08_p3.txt"),
                  acolytes = 10, blocks =  202400000) {
  thick <- 1
  base <- 1
  total <- 1
  heights <- 1
  removal <- 0
  while (total - removal < blocks) {
    base <- base + 2
    thick <- ((thick * d) %% acolytes) + acolytes
    heights <- c(heights + thick, thick)
    total <- total + (base * thick)
    removal <- (d * base * heights[1]) %% acolytes
    if (length(heights) > 2) {
      for (col in 2:(length(heights) - 1)) {
        removal <- removal + 2 * ((d * base * heights[col]) %% acolytes)
      }
    }
  }
  (total - removal) - blocks
}

part1()
part2()
part3()
