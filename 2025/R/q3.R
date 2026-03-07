source("../../Util/utils.R")

part1 <- function(d = rl("../data/everybody_codes_e2025_q03_p1.txt")) {
  packs <- as.integer(strsplit(d, ",")[[1]])
  sum(unique(packs))
}

part2 <- function(d = rl("../data/everybody_codes_e2025_q03_p2.txt")) {
  packs <- as.integer(strsplit(d, ",")[[1]])
  sum(sort(unique(packs))[1:20])
}

part3 <- function(d = rl("../data/everybody_codes_e2025_q03_p3.txt")) {
  packs <- as.integer(strsplit(d, ",")[[1]])
  max(table(packs))
}

part1()
part2()
part3()
