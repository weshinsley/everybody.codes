source("../../Util/utils.R")

part1 <- function(d = rig("../data/everybody_codes_e2024_q03_p1.txt"),
                  diag = FALSE) {
  tot <- 0
  dep <- 1
  while(TRUE) {
    newtot <- sum(d == dep)
    if (newtot == 0) break
    tot <- tot + newtot
    dig <- as.data.frame(which(d == dep, arr.ind = TRUE))
    dig$tot <- 0
    for (r in seq_len(nrow(dig))) {
      dig$tot[r] <- (
        (d[dig$row[r] - 1, dig$col[r]] == dep) +
        (d[dig$row[r] + 1, dig$col[r]] == dep) +          
        (d[dig$row[r], dig$col[r] - 1] == dep) +
        (d[dig$row[r], dig$col[r] + 1] == dep))
      if (diag) {
        dig$tot[r] <- dig$tot[r] + (
          (d[dig$row[r] - 1, dig$col[r] - 1] == dep) +
          (d[dig$row[r] + 1, dig$col[r] - 1] == dep) +          
          (d[dig$row[r] - 1, dig$col[r] + 1] == dep) +
          (d[dig$row[r] + 1, dig$col[r] + 1] == dep))
      }
    }
    dig <- dig[dig$tot == 4 + (4 * diag), ]
    for (r in seq_len(nrow(dig))) {
      d[dig$row[r], dig$col[r]] <- (dep + 1)
    }
    dep <- dep + 1
  }
  tot
}

part2 <- function(d = rig("../data/everybody_codes_e2024_q03_p2.txt")) {
  part1(d)
}

part3 <- function(d = rig("../data/everybody_codes_e2024_q03_p3.txt")) {
  part1(d, diag = TRUE)
}

part1()
part2()
part3()
