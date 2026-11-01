source("../../Util/utils.R")

library(dplyr)

part1 <- function(d = rkv("../data/everybody_codes_e2026_q04_p1.txt")) {
  d2 <- as.data.frame(d %>% group_by(id) %>% summarise(t = sum(time)))
  d2 <- d2[order(d2$t), ]
  sum(1:nrow(d2) * d2$t)
}

part2 <- function(d = rkv("../data/everybody_codes_e2026_q04_p2.txt")) {
  d2 <- as.data.frame(d %>% group_by(id) %>% summarise(p = n(), t = sum(time)))
  d2 <- d2[order(-d2$p, d2$t), ]
  sum(1:nrow(d2) * d2$t)
}

part3 <- function(d = rkv("../data/everybody_codes_e2026_q04_p3.txt")) {
  ids <- sort(unique(d$id))
  tots <- rep(0, length(ids))
  d <- split(d, d$quest)
  for (q in seq_along(d)) {
    dp <- split(d[[q]], d[[q]]$part)
    for (p in seq_along(dp)) {
      if (p > 1) {
        lookup <- match(dp[[p]]$id, dp[[p - 1]]$id)
        dp[[p]]$time <- dp[[p]]$time + dp[[p - 1]]$time[lookup]
      }

      dp[[p]] <- dp[[p]][order(dp[[p]]$time), ]
      n <- nrow(dp[[p]])
      dp[[p]]$score <- seq(from = (50 * p), by = -1, length.out = n)
      if (n > 1) {
        for (i in 2:n) {
          if (dp[[p]]$time[i] == dp[[p]]$time[i - 1]) {
            dp[[p]]$score[i] <- dp[[p]]$score[i - 1]
          }
        }
      }
      dp[[p]]$score[dp[[p]]$score < 0] <- 0
      tots[dp[[p]]$id] <- tots[dp[[p]]$id] + dp[[p]]$score
    }
  }
  tots <- sort(tots, decreasing = TRUE)
  muls <- 1:length(tots)
  for (i in 2:length(muls)) {
    if (tots[i] == tots[i - 1]) muls[i] <- muls[i - 1]
  }
  sum(tots * muls)

}

part1()
part2()
part3()
