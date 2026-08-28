source("../../Util/utils.R")

part1 <- function(d = rl("../data/everybody_codes_e4_q03_p1.txt")) {
  p <- d
  if (class(p) != "list") p <- parse(d)
  p$horiz <- 1 + rep(p$horiz, 1 + (p$height %/% length(p$horiz)))[1:(p$height + 1)]
  p$vert <- 1 + rep(p$vert, 1 + (p$width %/% length(p$vert)))[1:(p$width + 1)]
  mT <- mB <- mL <- mR <- matrix(0, ncol = p$width, nrow = p$height)
  pulse <- list(rep(c(1, 0), 1 + max(p$height %/% 2, p$width %/% 2)),
                rep(c(0, 1), 1 + max(p$height %/% 2, p$width %/% 2)))
  for (j in 1:(p$height + 1)) {
    if (j > 1) mB[j - 1, ] <- pulse[[p$horiz[j]]][1:p$width]
    if (j <= p$height) mT[j, ] <- pulse[[p$horiz[j]]][1:p$width]
    if (j > p$height) mB[j - 1, ] <- pulse[[p$horiz[j]]][1:p$width]
  }

  for (i in 1:(p$width + 1)) {
    if (i > 1) mR[, i - 1] <- pulse[[p$vert[i]]][1:p$height]
    if (i <= p$width) mL[, i] <- pulse[[p$vert[i]]][1:p$height]
    if (i > p$width) mR[, i - 1] <- pulse[[p$vert[i]]][1:p$height]
  }
  list(sum(mT * mB * mL * mR == 1), mT, mB, mL, mR)
}

part2 <- function(d = rl("../data/everybody_codes_e4_q03_p2.txt")) {
  res <- part1(d)
  mT <- res[[2]]
  mB <- res[[3]]
  mL <- res[[4]]
  mR <- res[[5]]
  iso <- which(mT * mB * mL * mR == 1)
  mC <- mT * 0
  wid <- ncol(mT)
  hei <- nrow(mT)
  i <- 1
  j <- 1
  col <- 2
  dx <- c(1, 0, -1, 0)
  dy <- c(0, 1, 0, -1)
  db <- list(mR, mB, mL, mT)
  while (TRUE) {
    while (mC[j, i] != 0) {
      col <- mC[j, i]
      i <- i + 1
      if (i > wid) {
        i <- 1
        j <- j + 1
        col <- mC[j - 1, i]
        if (j > hei) {
          return(list(max(sum(mC[iso] == 1), sum(mC[iso] == 2)), mC, iso))
        }
      }
    }
    col <- (3 - col)
    mC[j, i] <- col
    queue <- list(c(i, j))
    n <- 1
    x <- 1
    while (x <= n) {
      cell <- queue[[x]]
      x <- x + 1
      ci <- cell[1]
      cj <- cell[2]
      for (dir in 1:4) {
        if (db[[dir]][cj, ci] == 0) {
          nx <- ci + dx[dir]
          ny <- cj + dy[dir]
          if ((nx >= 1) && (nx <= wid) && (ny >= 1) && (ny <= hei)) {
            if (mC[ny, nx] == 0) {
              mC[ny, nx] <- col
              n <- n + 1
              queue[[n]] <- c(nx, ny)
            }
          }
        }
      }
    }
  }
}

part3 <- function(d = rl("../data/everybody_codes_e4_q03_p3.txt")) {
  # Do some inspection here to find the period...
  p <- parse(d)
  ptry <- p
  ptry$width <- 500
  ptry$height <- 500
  tt <- part2(ptry)
  mcg <- tt[[2]]
  iso <- tt[[3]]

  for (iper in length(p$vert):250) {
    if (all(mcg[1, 1:iper] == mcg[1, (iper+1):(2 * iper)])) break
  }

  for (jper in length(p$horiz):250) {
    if (all(mcg[1:jper, 1] == mcg[(jper+1):(2 * jper), 1])) break
  }

  isos <- mcg * 0
  isos[iso] <- mcg[iso]

  xrepeats <- p$width %/% iper
  yrepeats <- p$height %/% jper
  xextra <- p$width %% iper
  yextra <- p$height %% jper

  main1 <- as.double(sum(isos[1:jper, 1:iper] == 1))
  right1 <- as.double(sum(isos[1:jper, 1:xextra] == 1))
  bottom1 <- as.double(sum(isos[1:yextra, 1:iper] == 1))
  bottomright1 <- as.double(sum(isos[1:yextra, 1:xextra] == 1))

  main2 <- as.double(sum(isos[1:jper, 1:iper] == 2))
  right2 <- as.double(sum(isos[1:jper, 1:xextra] == 2))
  bottom2 <- as.double(sum(isos[1:yextra, 1:iper] == 2))
  bottomright2 <- as.double(sum(isos[1:yextra, 1:xextra] == 2))

  options(digits = 22)
  total1 <- (main1 * xrepeats * yrepeats) + (right1 * yrepeats) +
            (bottom1 * xrepeats) + bottomright1
  total2 <- (main2 * xrepeats * yrepeats) + (right2 * yrepeats) +
    (bottom2 * xrepeats) + bottomright2

  max(total1, total2)
}

parse <- function(d) {
  res <- strsplit(d, "=")
  r <- list()
  for (x in res) {
    r[[x[1]]] <- x[2]
  }
  r$width <- as.integer(r$width)
  r$height <- as.integer(r$height)
  r[["horiz"]] <- as.integer(strsplit(r$`horizontal-offsets`, "")[[1]])
  r[["vert"]] <- as.integer(strsplit(r$`vertical-offsets`, "")[[1]])
  r$`horizontal-offsets` <- NULL
  r$`vertical-offsets` <- NULL
  r
}

part1()[[1]]
part2()[[1]]
part3()
