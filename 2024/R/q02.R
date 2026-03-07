source("../../Util/utils.R")

part1 <- function(d = rl("../data/everybody_codes_e2024_q02_p1.txt")) {
  words <- strsplit(gsub("WORDS:", "", d[1]), ",")[[1]]
  text <- d[3]
  sum(unlist(lapply(words, function(x) {
    res <- as.integer(gregexpr(sprintf("(?=%s)", x), text, perl = TRUE)[[1]])
    length(res[res != -1])
  })))
}

part2 <- function(d = rl("../data/everybody_codes_e2024_q02_p2.txt")) {
  words <- strsplit(gsub("WORDS:", "", d[1]), ",")[[1]]
  words <- unique(c(words, unlist(lapply(words, stringi::stri_reverse))))
  tot <- 0
  for (i in 3:length(d)) {
    text <- d[i]
    used <- rep(FALSE, nchar(text))
    for (word in words) {
      res <- gregexpr(sprintf("(?=%s)", word), text, perl = TRUE)
      if (res[[1]][1] != -1) {
        for (start in as.integer(res[[1]])) {
          used[start:(start + nchar(word) - 1)] <- TRUE
        }
      }
    }
    tot <- tot + sum(used)
  }
  tot
}

part3 <- function(d = rl("../data/everybody_codes_e2024_q02_p3.txt")) {
  words <- strsplit(gsub("WORDS:", "", d[1]), ",")[[1]]
  wid <- nchar(d[3])
  hei <- length(d) - 2
  myx <- matrix(data = strsplit(paste0(d[3:length(d)], collapse = ""), "")[[1]],
                ncol = wid, byrow = TRUE)
  used <- matrix(data = 0, ncol = wid, nrow = hei)
  dx <- c(1, 0, -1, 0)
  dy <- c(0, 1, 0, -1)
  
  for (word in words) {
    w <- strsplit(word, "")[[1]]
    starts <- as.data.frame(which(myx == w[1], arr.ind = TRUE))
    for (r in seq_len(nrow(starts))) {
      for (dir in 1:4) {
        x <- starts$col[r]
        y <- starts$row[r]
        ok <- TRUE
        pix <- list(c(x, y))
        if (length(w) > 1) {
          for (k in 2:length(w)) {
            x <- x + dx[dir]
            y <- y + dy[dir]
            if (x > wid) x <- 1
            if (x == 0) x <- wid
            if ((y > hei) || (y == 0)) { 
              ok <- FALSE
              break
            }
            if (myx[y, x] != w[k]) {
              ok <- FALSE
              break
            }
            pix[[k]] <- c(x, y)
          }
        }
        if (ok) {
          for (m in 1:length(pix)) {
            used[pix[[m]][2], pix[[m]][1]] <- 1
          }
        }
      }
    }
  }
  sum(used)
}

part1()
part2()
part3()
