source("../../Util/utils.R")

thing <- function(s) {
  s <- strsplit(s, " ")[[1]]
  as.data.frame(data.table::rbindlist(lapply(s, function(x) {
    x <- strsplit(x, "")[[1]]
    if (x[2] == ">") data.frame(a = as.integer(x[1]), b = as.integer(x[3]))
    else data.frame(a = as.integer(x[3]), b = as.integer(x[1]))
  })))
}

solve <- function(df) {
  maxn <- max(df$a, df$b)
  res <- rep(0, maxn)
  i <- 1
  j <- maxn
  sel <- 1:maxn
  while (i < j) {
    res[i] <- sel[which(!sel %in% df$b)]
    res[j] <- sel[which(!sel %in% df$a)]
    df <- df[!df$b %in% res[j], ]
    df <- df[!df$a %in% res[i], ]
    sel <- sel[sel != res[j] & sel != res[i]]
    i <- i + 1
    j <- j - 1
  }
  if (length(sel) == 1) res[i] <- sel
  as.integer(paste(res, collapse = ""))
}

part1 <- function(d = rl("../data/everybody_codes_e2026_q02_p1.txt")) {
  sum(unlist(lapply(lapply(d, thing), solve)))
}

part2 <- function(d = rl("../data/everybody_codes_e2026_q02_p2.txt")) {
  part1(d)
}

part3 <- function(d = rl("../data/everybody_codes_e2026_q02_p3.txt")) {
  part1(d)
}

part1()
part2()
part3()
