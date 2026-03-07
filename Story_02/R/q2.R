source("../../Util/utils.R")

part1 <- function(d = rl("../data/everybody_codes_e2_q02_p1.txt")) {
  shots <- 0
  darts <- c("R", "G", "B")
  di <- 1
  d <- strsplit(d, "")[[1]]
  bi <- 1
  while (bi <= length(d)) {
    shots <- shots + 1
    dart <- darts[di]
    di <- if (di == length(darts)) 1 else di + 1
    while ((bi <= length(d)) && (d[bi] == dart)) {
      d[bi] <- "-"
      bi <- bi + 1
    }
    if (bi <= length(d)) {
      d[bi] <- "-"
      bi <- bi + 1
    }
  }
  shots
}

part2 <- function(d = rl("../data/everybody_codes_e2_q02_p2.txt"),
                  reps = 100) {
  d <- strsplit(paste0(rep(d, reps), collapse = ""), "")[[1]]
  shot <- rep(FALSE, length(d))
  darts <- c("R", "G", "B")
  di <- 1
  first <- 1
  shots <- 0
  second_hits <- 0
  size <- length(d)
  while(size > 0) {
    shots <- shots + 1
    while (shot[first]) {
      first <- first + 1
      second_hits <- second_hits - 1
    }
    shot[first] <- TRUE
    dart <- darts[di]
    di <- if (di == length(darts)) 1 else di + 1
    if ((size %% 2) == 0) {
      oppo <- first + (size %/% 2) + second_hits
      if (dart == d[first]) {
        size <- size - 1
        shot[oppo] <- TRUE
        second_hits <- second_hits + 1
      }
    }
    size <- size - 1
    first <- first + 1
  }
  shots
}

part3 <- function(d = rl("../data/everybody_codes_e2_q02_p3.txt")) {
  part2(d, reps = 100000)
}

part1()
part2()
part3()
