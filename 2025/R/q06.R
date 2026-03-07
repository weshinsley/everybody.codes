source("../../Util/utils.R")

part1 <- function(d = rl("../data/everybody_codes_e2025_q06_p1.txt"),
                  p2 = FALSE) {
  d <- strsplit(d, "")[[1]]
  if (!p2) d <- d[d %in% c("a", "A")]
  mentors <- sort(unique(toupper(d)))
  students <- tolower(mentors)
  score <- 0
  for (stud in students) {
    ment <- toupper(stud)
    for (i in which(d == stud)) {
      score <- score + sum(d[1:i] == ment)
    }
  }
  score
}

part2 <- function(d = rl("../data/everybody_codes_e2025_q06_p2.txt")) {
  part1(d, TRUE)
}

count_p3 <- function(d, dist, from = 1, to = NA) {
  d <- strsplit(d, "")[[1]]
  if (is.na(to)) to <- length(d)
  tot <- 0
  for (stud in c("a", "b", "c")) {
    ments <- which(d == toupper(stud))
    locs <- which(d == stud)
    locs <- locs[locs >= from & locs <= to]
    for (loc in locs) {
      near <- ments[ments >= max(1, loc - dist)]
      near <- near[near <= min(length(d), loc + dist)]
      tot <- tot + length(near)
    }
  }
  tot
}


part3 <- function(d = rl("../data/everybody_codes_e2025_q06_p3.txt")) {
  if (nchar(d) < 1000) { # Just for tests!
    d <- paste0(rep(d, 1000), collapse="")
    count_p3(d, 1000)
  }
  pair <- paste0(d, d, collapse = "")
  left <- count_p3(pair, 1000, 1, nchar(d))
  right <- count_p3(pair, 1000, nchar(d) + 1, nchar(pair))
  trip <- paste0(d, d, d, collapse = "")
  mid <- count_p3(trip, 1000, nchar(d) + 1, 2 * nchar(d))
  left + (998 * mid) + right
}

part1()
part2()
part3()
