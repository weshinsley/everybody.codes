source("../../Util/utils.R")

part1 <- function(d = rt("../data/everybody_codes_e2024_q11_p1.txt"),
                  start = "A", days = 4) {
  types <- sort(unique(c(names(d), as.character(unlist(d)))))
  pop <- list()
  for (type in types) {
    pop[[type]] <- 0
  }
  zeroes <- pop
  pop[[start]] <- 1
  for (day in 1:days) {
    pop2 <- zeroes
    for (from in names(d)) {
      outcomes <- d[[from]]
      for (outcome in outcomes) {
        pop2[[outcome]] <- pop2[[outcome]] + pop[[from]]
      }
    }
    pop <- pop2
  }
  sum(unlist(pop))
}

part2 <- function(d = rt("../data/everybody_codes_e2024_q11_p2.txt")) {
  part1(d, "Z", 10)
}

part3 <- function(d = rt("../data/everybody_codes_e2024_q11_p3.txt")) {
  types <- sort(unique(names(d)))
  res <- unlist(lapply(types, function(type) part1(d, type, 20)))
  max(res) - min(res)
}

part1()
part2()
part3()
