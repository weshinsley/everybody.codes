source("../../Util/utils.R")

part1 <- function(d = rt("../data/everybody_codes_e2024_q06_p1.txt"),
                  part2 = FALSE) {
  res <- data.frame()
  state <- list(path = "RR", at = "RR", short = "R", steps = 0)
  work <- list(state)
  i <- 1
  j <- 2
  while (i <= length(work)) {
    state <- work[[i]]
    i <- i + 1
    if (state$at == "@") {
      res <- rbind(res, data.frame(steps = state$steps,
                                   path = state$path,
                                   short = state$short))
      next
    }
    for (dest in d[[state$at]]) {
      new_state <- state
      new_state$at <- dest
      new_state$path <- paste0(state$path, dest)
      new_state$short <- paste0(state$short, substr(dest, 1, 1))
      new_state$steps <- state$steps + 1
      work[[j]] <- new_state
      j <- j + 1
    }
  }
  dups <- res$steps[duplicated(res$steps)]
  res <- res[!res$steps %in% dups,]
  if (part2) res$short else res$path
}

part2 <- function(d = rt("../data/everybody_codes_e2024_q06_p2.txt")) {
  part1(d, TRUE)
}

part3 <- function(d = rt("../data/everybody_codes_e2024_q06_p3.txt")) {
  d[["BUG"]] <- NULL
  d[["ANT"]] <- NULL
  for (i in seq_along(d)) {
    d[[i]] <- d[[i]][d[[i]] != "ANT"]
    d[[i]] <- d[[i]][d[[i]] != "BUG"]
  }
  part1(d, TRUE)
}

part1()
part2()
part3()
