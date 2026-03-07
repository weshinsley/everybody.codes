source("../../Util/utils.R")
install.packages("clue")

parse <- function(d) {
  blank <- which(trimws(d) == "")
  list(
    grid = rcg(d[1:(blank - 1)]),
    commands = (d[(blank + 1):length(d)])
  )
}

slot <- function(d, i, start = i) {
  x <- (2 * start) - 1
  y <- 1
  cmds <- strsplit(d$commands[i], "")[[1]]
  inst <- 1
  while (y <= nrow(d$grid)) {
    if (d$grid[y, x] == ".") {
      y <- y + 1
      next
    }
    cmd <- cmds[inst]
    inst <- inst + 1
    if (x == 1) cmd <- "R"
    if (x == ncol(d$grid)) cmd <- "L"
    x <- x + (cmd == "R") - (cmd == "L")
  }
  slot <- ((x + 1) %/% 2)
  max(0, (slot * 2) - start)

}

part1 <- function(d = rl("../data/everybody_codes_e2_q01_p1.txt")) {
  d <- parse(d)
  sum(unlist(lapply(1:9, function(i) slot(d, i))))
}

part2 <- function(d = rl("../data/everybody_codes_e2_q01_p2.txt")) {
  d <- parse(d)
  sum(unlist(lapply(1:length(d$commands), function(i) {
    max(unlist(lapply(1:13, function(j) { slot(d, i, j) })))})))
}

part3 <- function(d = rl("../data/everybody_codes_e2_q01_p3.txt")) {
  d <- parse(d)
  slots <- (1 + ncol(d$grid)) %/% 2
  res <- matrix(Inf, nrow = 6, ncol = slots)
  for (coin in 1:6) {
    for (j in seq_len(slots)) {
      res[coin, j] <- slot(d, coin, j)
    }
  }
  paste(sum(res[cbind(1:6, clue::solve_LSAP(res))]),
        sum(res[cbind(1:6, clue::solve_LSAP(res, maximum = TRUE))]))
}

part1()
part2()
part3()
