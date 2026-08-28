source("../../Util/utils.R")

part1 <- function(d = rl("../data/everybody_codes_e4_q02_p1.txt"), p2 = FALSE) {
  p <- parse(d)
  sky <- new.env(parent = emptyenv())
  current <- p$START
  assign(paste(current, collapse="_"), 1, envir = sky)
  for (move in p$MOVES) {
    beacon <- p[[move]]
    nextpos <- c((current[1] + beacon[1]) %/% 2,
                 (current[2] + beacon[2]) %/% 2)
    current <- nextpos
    assign(paste(current, collapse="_"), 1, envir = sky)
  }
  if (p2) return(fireflies(sky))
  length(ls(sky))
}

fireflies <- function(sky) {
  sky2 <- new.env(parent = emptyenv())
  for (coord in ls(sky)) {
    bits <- as.integer(strsplit(coord, "_")[[1]])
    bits[1] <- bits[1] - 1
    assign(paste(bits, collapse="_"), 1, envir = sky2)
    bits[1] <- bits[1] + 2
    assign(paste(bits, collapse="_"), 1, envir = sky2)
    bits[1] <- bits[1] - 1
    bits[2] <- bits[2] - 1
    assign(paste(bits, collapse="_"), 1, envir = sky2)
    bits[2] <- bits[2] + 2
    assign(paste(bits, collapse="_"), 1, envir = sky2)
  }
  sky2 <- ls(sky2)
  sky2 <- sky2[!sky2 %in% ls(sky)]
  return(length(sky2))
}

part2 <- function(d = rl("../data/everybody_codes_e4_q02_p2.txt")) {
  part1(d, TRUE)
}

part3 <- function(d = rl("../data/everybody_codes_e4_q02_p3.txt")) {
  p <- parse(d)
  queue <- list(c(0,0))
  i <- 1
  n <- 2
  sky <- new.env(parent = emptyenv())
  assign(paste(p$START, collapse = "_"), 1, envir = sky)
  while (i <= length(queue)) {
    current <- queue[[i]]
    i <- i + 1
    assign(paste(current, collapse = "_"), 1, envir = sky)
    for (move in c("A", "B", "C")) {
      beacon <- p[[move]]
      nextpos <- c((current[1] + beacon[1]) %/% 2,
                   (current[2] + beacon[2]) %/% 2)
      key <- paste(nextpos, collapse="_")
      if (!exists(key, envir = sky)) {
        queue[[n]] <- nextpos
        n <- n + 1
        assign(key, 1, envir = sky)
      }
    }
  }
  fireflies(sky)
}

parse <- function(d) {
  coords <- function(s, x) {
    as.integer(strsplit(gsub(sprintf("%s=\\[", x), "", gsub("\\]", "", s)), ",")[[1]])
  }
  res <- list(
    START = coords(d[1], "START"),
    A = coords(d[2], "A"),
    B = coords(d[3], "B"),
    C = coords(d[4], "C"))
    if (length(d) == 5) res$MOVES = strsplit(substring(d[5], 7), "")[[1]]
  res
}

part1()
part2()
part3()
