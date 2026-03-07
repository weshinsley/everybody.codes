source("../../Util/utils.R")

quality <- function(sword) {
  sword <- sword[seq(2, length(sword), by = 3)]
  sword <- sword[!is.na(sword)]
  as.numeric(paste0(sword, collapse=""))
}

build <- function(nums) {
  sword <- rep(NA, length(nums) * 3)
  for (num in as.numeric(nums)) {
    i <- 2
    while(TRUE) {
      if (is.na(sword[i])) {
        sword[i] <- num
        break
      }
      if ((is.na(sword[i - 1])) && (num < sword[i])) {
        sword[i - 1] <- num
        break
      }
      if ((is.na(sword[i + 1])) && (num > sword[i])) {
        sword[i + 1] <- num
        break
      }
      i <- i + 3
    }
  }
  sword
}

part1 <- function(d = rl("../data/everybody_codes_e2025_q05_p1.txt")) {
  id <- as.integer(strsplit(d, ":")[[1]][1])
  nums <- as.integer(strsplit(strsplit(d, ":")[[1]][-1], ",")[[1]])
  quality(build(nums))
}

part2 <- function(d = rl("../data/everybody_codes_e2025_q05_p2.txt")) {
  res <- unlist(lapply(d, part1))
  max(res) - min(res)
}

part3 <- function(d = rl("../data/everybody_codes_e2025_q05_p3.txt")) {
  ids <- lapply(d, function(x) as.integer(strsplit(x, ":")[[1]][1]))
  ss <- lapply(d, function(x) build(strsplit(strsplit(x, ":")[[1]][2], ",")[[1]]))
  nums <- lapply(ss, function(x) {
    res <- unlist(lapply(seq(2, length(x), by = 3), function(i) {
      row <- c(x[i - 1], x[i], x[i + 1])
      row <- row[!is.na(row)]
      paste0(row, collapse = "")}))

    for (i in seq_along(res)) {
      while (nchar(res[i]) < 4) res[i] <- paste0("0", res[i])
    }

    paste0(res[res != ""], collapse=",")
  })

  qs <- lapply(ss, quality)
  df <- data.frame(id = unlist(ids), quality = unlist(qs),
                   num = unlist(nums))
  df <- df[order(df$quality, df$num, df$id, decreasing = TRUE),]
  sum(df$id * seq_len(nrow(df)))
}

part1()
part2()
part3()
