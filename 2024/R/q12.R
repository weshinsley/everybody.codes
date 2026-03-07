source("../../Util/utils.R")

part1 <- function(d = rcg("../data/everybody_codes_e2024_q12_p1.txt")) {
  targetsT <- as.data.frame(which(d == "T", arr.ind = TRUE))
  targetsT$id <- "T"
  targetsH <- as.data.frame(which(d == "H", arr.ind = TRUE))
  if (nrow(targetsH) > 0) targetsH$id <- "H"
  targets <- rbind(targetsT, targetsH)
  targets$best <- 0
  canons <- rbind(as.data.frame(which(d == "C", arr.ind = TRUE)),
                  as.data.frame(which(d == "B", arr.ind = TRUE)),
                  as.data.frame(which(d == "A", arr.ind = TRUE)))
  canons$id <- c("C", "B", "A")
  canons$score <- c(3, 2, 1)
  for (ti in seq_len(nrow(targets))) {
    for (ci in seq_len(nrow(canons))) {
      tx <- targets$col[ti]
      ty <- targets$row[ti]
      cx <- canons$col[ci]
      cy <- canons$row[ci]
      if (ty < cy) {
        tx <- tx + (cy - ty)
        ty <- cy
      } else if (ty > cy) {
        tx <- tx - (ty - cy)
        ty <- cy
      }
      if ((tx - cx) %% 3 == 0) {
        p <- (tx - cx) %/% 3
        score <- p * canons$score[ci]
        targets$best[ti] <- max(targets$best[ti], score)
      }
    }
  }
  targets$best[targets$id == "H"] <- targets$best[targets$id == "H"] * 2
  sum(targets$best)
}

part2 <- function(d = rcg("../data/everybody_codes_e2024_q12_p2.txt")) {
  part1(d)
}

part3 <- function(d = read.csv("../data/everybody_codes_e2024_q12_p3.txt",
                               sep = " ", col.names=c("x", "y"), header = F)) {
  canons <- data.frame(x = 0, y = 0:2, id = c("A","B","C"))
  total <- 0
  for (i in seq_len(nrow(d))) {
    res <- data.frame(alt = rep(0, 3), score = rep(0, 3))
    met <- d[i, ]
    for (j in seq_len(nrow(canons))) {
      can <- canons[j, ]
      for (delay in 0:9) {
        tx <- met$x - delay
        ty <- met$y - delay
        dx <- tx - can$x
        dy <- ty - can$y

        # Straight in line - dx=dy,
        # Collide at x / 2 - if x is even.

        p <- dx %/% 2
        if ((dx == dy) && ((dx %% 2) == 0)) {
          res$alt[j] <- p + can$y
          res$score[j] <- p * (1 + can$y)
          break
        }

        # Hit it on the flat. So we go up and right p, then just right q.
        # Meteor x = mx - (p + q)    Meteor y = my - (p + q)
        # Canon  x = cx + (p + q)    Canon y = cy + p
        # mx = cx + 2p + 2q   =>   (mx - cx) = 2p + 2q   = dx
        # my = cy + 2p + q    =>   (my - cy) = 2p + q    = dy
        # Solve simultaneously: q = dx - dy
        #                   and p = dy - (dx / 2)
        # Again only for even dx, so we don't pass through.

        q <- dx - dy
        p <- dy - (dx %/% 2)
        if (((dx %% 2) == 0) && (q > 0) && (q <= p)) {
          res$alt[j] <- p + can$y
          res$score[j] <- p * (1 + can$y)
          break
        }

        # Otherwise we have to hit it on the way down. So we've
        # travelled up "p", horizontally "p", and then down "q".
        # Meteor x = mx - (2p + q) Meteor y = my - (2p + q)
        # Canon x = cx + (2p + q). Canon y = (cy + p) - q
        # (mx - cx) = 4p + 2q     = dx
        # (my - cy) = 3p          = dy
        # p = cy / 3   and   q = (dx / 2) - (2dy / 3)

        q <- (dx %/% 2) - ((2 * dy) %/% 3)
        p <- dy %/% 3

        if (((dy %% 3) == 0) && ((dx %% 2) == 0) && (q > 0)) {
          res$alt[j] <- (p + can$y) - q
          res$score[j] <- p * (1 + can$y)
          break
        }
      }
    }
    res <- res[res$alt == max(res$alt), ]
    total <- total + (min(res$score))
  }

  total
}

part1()
part2()
part3()
