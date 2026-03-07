source("../../Util/utils.R")

part1 <- function(d = rcg("../data/everybody_codes_e2024_q10_p1.txt")) {
  d2 <- d
  dots <- as.data.frame(which(d == ".", arr.ind = TRUE))
  for (i in seq_len(nrow(dots))) {
    row <- dots$row[i]
    col <- dots$col[i]
    cref <- c(d[row,], d[, col])
    cref <- cref[!cref %in% c(".", "*")]
    cref <- cref[duplicated(cref)]
    d2[row, col] <- cref
  }
  paste0(c(d2[3,3:6], d2[4, 3:6], d2[5, 3:6], d2[6, 3:6]), collapse="")
}

part2 <- function(d = rl("../data/everybody_codes_e2024_q10_p2.txt")) {
  tot <- 0
  for (j in seq(1, 55, by = 9)) {
    bits <- d[(j:(j + 7))]
    for (i in seq(1, 127, by = 9)) {
      res <- strsplit(part1(rcg(substring(bits, i, i + 7))), "")[[1]]
      tot <- tot + sum(seq_along(res) * match(res, LETTERS))
    }
  }
  tot
}

part3 <- function(d = rcg("../data/everybody_codes_e2024_q10_p3.txt")) {
  nc <- ncol(d)
  nr <- nrow(d)
  nx <- (nc - 2) / 6
  ny <- (nr - 2) / 6
  old_tot <- 0
  tot <- 1
  while (old_tot != tot) {
    for (i in seq(1, nc - 3, by = 6)) {
      for (j in seq(1, nr - 3, by = 6)) {
        g <- d[j:(j + 7), i:(i+7)]
        fix <- TRUE
        while (fix) {
          fix <- FALSE
          dots <- as.data.frame(which(g == ".", arr.ind = TRUE))
          for (k in seq_len(nrow(dots))) {
            row <- dots$row[k]
            col <- dots$col[k]

            # First cross-reference column and row

            cref <- intersect(g[c(1, 2, 7, 8), col],
                              g[row, c(1, 2, 7, 8)])
            cref <- cref[!cref %in% c(".", "*", "?")]
            if (length(cref) == 1) {
              g[row, col] <- cref
              fix <- TRUE
              next
            }

            # Now check if we know the four column vals, and have
            # just one dot missing.

            col_opts <- g[c(1, 2, 7, 8), col]
            col_inner <- g[3:6, col]
            if ((!"?" %in% col_opts) && (sum(col_inner == ".") == 1)) {
              bits <- c(col_opts, col_inner)
              bits <- bits[bits != "."]
              bits <- bits[!bits %in% bits[duplicated(bits)]]
              if (length(bits) == 1) {
                g[row, col] <- bits
                fix <- TRUE
                next
              }
            }

            # Same for the row

            row_opts <- g[row, c(1, 2, 7, 8)]
            row_inner <- g[row, 3:6]
            if ((!"?" %in% row_opts) && (sum(row_inner == ".") == 1)) {
              bits <- c(row_opts, row_inner)
              bits <- bits[bits != "."]
              bits <- bits[!bits %in% bits[duplicated(bits)]]
              if (length(bits) == 1) {
                g[row, col] <- bits
                fix <- TRUE
                next
              }
            }
          }

          # See if we can fill in any question marks

          qs <- as.data.frame(which(g == "?", arr.ind = TRUE))
          for (k in seq_len(nrow(qs))) {
            row <- qs$row[k]
            col <- qs$col[k]

            # Look in column - we want one ? and no dots

            col_opts <- g[c(1, 2, 7, 8), col]
            col_inner <- g[3:6, col]
            if ((!"." %in% col_inner) && (sum(col_opts == "?") == 1)) {
              bits <- c(col_opts, col_inner)
              bits <- bits[bits != "?"]
              bits <- bits[!bits %in% bits[duplicated(bits)]]
              if (length(bits) == 1) {
                g[row, col] <- bits
                fix <- TRUE
                next
              }
            }

            # Same for the row

            row_opts <- g[row, c(1, 2, 7, 8)]
            row_inner <- g[row, 3:6]
            if ((!"." %in% row_inner) && (sum(row_opts == "?") == 1)) {
              bits <- c(row_opts, row_inner)
              bits <- bits[bits != "?"]
              bits <- bits[!bits %in% bits[duplicated(bits)]]
              if (length(bits) == 1) {
                 g[row, col] <- bits
                fix <- TRUE
                next
              }
            }
          }
        }

        d[j:(j + 7), i:(i+7)] <- g
      }
    }
    old_tot <- tot
    tot <- 0
    for (i in seq(1, nc - 3, by = 6)) {
      for (j in seq(1, nr - 3, by = 6)) {
        g <- d[j:(j + 7), i:(i+7)]
        code <- c(g[3,3:6], g[4, 3:6], g[5, 3:6], g[6, 3:6])
        if (!"." %in% code) {
          tot <- tot + sum(seq_along(code) * match(code, LETTERS))
        }
      }
    }
  }
  tot
}

part1()
part2()
part3()
