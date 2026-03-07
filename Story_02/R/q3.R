source("../../Util/utils.R")

parse <- function(s) {
  s <- as.integer(strsplit(gsub(": faces=\\[", ",",
                                gsub("\\] seed=", ",", s)), ",")[[1]])
  list(id = s[1], seed = s[length(s)], faces = s[2:(length(s)-1)],
       pos = 0, pulse = s[length(s)], track_pos = 1)
}


part1 <- function(d = rl("../data/everybody_codes_e2_q03_p1.txt")) {
  dice <- lapply(d, parse)
  tot <- 0
  roll_no <- 0
  while(tot < 10000) {
    roll_no <- roll_no + 1
    for (s in seq_along(d)) {
      spin <- roll_no * dice[[s]]$pulse
      dice[[s]]$pos <- (dice[[s]]$pos + spin) %% length(dice[[s]]$faces)
      result <- dice[[s]]$faces[dice[[s]]$pos + 1]
      dice[[s]]$pulse <- (dice[[s]]$pulse + spin) %% dice[[s]]$seed
      dice[[s]]$pulse <- dice[[s]]$pulse + 1 + roll_no + dice[[s]]$seed
      tot <- tot + result
    }
  }
  roll_no
}

part2 <- function(d = rl("../data/everybody_codes_e2_q03_p2.txt")) {
  dice <- lapply(d[1:(length(d) - 2)], parse)
  done <- rep(FALSE, length(dice))
  matches <- as.numeric(strsplit(d[length(d)], "")[[1]])
  roll_no <- 0
  res <- c()
  while(any(!done)) {
    roll_no <- roll_no + 1
    for (s in seq_along(dice)) {
      if (done[s]) next
      spin <- roll_no * dice[[s]]$pulse
      dice[[s]]$pos <- (dice[[s]]$pos + spin) %% length(dice[[s]]$faces)
      result <- dice[[s]]$faces[dice[[s]]$pos + 1]
      dice[[s]]$pulse <- (dice[[s]]$pulse + spin) %% dice[[s]]$seed
      dice[[s]]$pulse <- dice[[s]]$pulse + 1 + roll_no + dice[[s]]$seed
      if (result == matches[dice[[s]]$track_pos]) {
        dice[[s]]$track_pos <- dice[[s]]$track_pos + 1
        if (dice[[s]]$track_pos > length(matches)) {
          done[s] <- TRUE
          res <- c(res, s)
        }
      }
    }
  }
  paste0(res, collapse = ",")
}

part3 <- function(d = rl("../data/everybody_codes_e2_q03_p3.txt")) {
  x <- which(d == "")
  dice <- lapply(d[1:(x-1)], parse)
  grid <- rig(d[(x+1):length(d)], pad = 1)
  visit <- matrix(0, ncol = ncol(grid), nrow = nrow(grid))
  roll_no <- 0
  queues <- list()
  for (i in seq_along(dice)) queues[[i]] <- list()
  while(TRUE) {
    roll_no <- roll_no + 1
    for (s in seq_along(dice)) {
      spin <- roll_no * dice[[s]]$pulse
      dice[[s]]$pos <- (dice[[s]]$pos + spin) %% length(dice[[s]]$faces)
      result <- dice[[s]]$faces[dice[[s]]$pos + 1]
      dice[[s]]$pulse <- (dice[[s]]$pulse + spin) %% dice[[s]]$seed
      dice[[s]]$pulse <- dice[[s]]$pulse + 1 + roll_no + dice[[s]]$seed

      places <- which(grid == result)
      if (roll_no == 1) {
        queues[[s]] <- places
        visit[places] <- 1
      } else {
        prev <- queues[[s]]
        prev <- unique(c(prev, prev - 1, prev + 1,
                         prev - nrow(grid), prev + nrow(grid)))
        prev <- prev[grid[prev] == result]
        visit[prev] <- 1
        queues[[s]] <- prev
      }
    }
    spots <- sum(unlist(lapply(queues, length)))
    if (spots == 0) {
      for (r in seq_len(nrow(visit))) {
        ss <- visit[r, ]
        ss <- gsub("0", " ", gsub("1", "#", paste0(ss, collapse="")))
        message(ss)
      }
      return(sum(visit))
    }
  }
}

part1()
part2()
part3()
