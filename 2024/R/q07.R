source("../../Util/utils.R")

part1 <- function(d = rt("../data/everybody_codes_e2024_q07_p1.txt")) {
  score <- rep(0, length(d))
  current <- rep(10, length(d))
  index <- 1
  for (i in 1:10) {
    for (j in seq_along(d)) {
      if (d[[j]][index] == "+") current[j] <- current[j] + 1
      else if (d[[j]][index] == "-") current[j] <- current[j] - 1
      score[j] <- score[j] + current[j]
    }
    index <- index + 1
    if (index > length(d[[1]])) index <- 1

  }
  lets <- names(d)
  paste0(lets[order(score, decreasing = TRUE)], collapse = "")
}

conv_track <- function(t, loops = 10) {
  t <- lapply(t, function(x) strsplit(x, "")[[1]])
  res <- t[[1]][2]
  t[[1]][2] <- " "
  x <- 3
  y <- 1

  while (t[[y]][x] != "S") {
    res <- c(res, t[[y]][x])
    t[[y]][x] <- " "
    if ((y > 1) && (t[[y - 1]][x] != " ")) {
      y <- y - 1
    } else if ((y < length(t)) && (t[[y + 1]][x]!= " ")) {
      y <- y + 1
    } else if ((x > 1) && (t[[y]][x - 1] != " ")) {
      x <- x - 1
    } else if ((x < length(t[[1]])) && (t[[y]][x + 1] != " ")) {
      x <- x + 1
    }
  }
  res <- c(res, "S")
  res <- rep(res, loops)
}


part2 <- function(d = rt("../data/everybody_codes_e2024_q07_p2.txt"),
                  track = c(
  "S-=++=-==++=++=-=+=-=+=+=--=-=++=-==++=-+=-=+=-=+=+=++=-+==++=++=-=-=--",
  "-                                                                     -",
  "=                                                                     =",
  "+                                                                     +",
  "=                                                                     +",
  "+                                                                     =",
  "=                                                                     =",
  "-                                                                     -",
  "--==++++==+=+++-=+=-=+=-+-=+-=+-=+=-=+=--=+++=++=+++==++==--=+=++==+++-")) {

  track <- conv_track(track)
  score <- rep(0, length(d))
  current <- rep(10, length(d))
  index <- 1
  for (ch in track) {
    if (ch == "+") current <- current + 1
    else if (ch == "-") current <- current - 1
    else {
      for (j in seq_along(d)) {
        current[j] <- current[j] + (d[[j]][index] == "+") - (d[[j]][index] == "-")
      }
    }
    index <- index + 1
    if (index > length(d[[1]])) index <- 1
    score <- score + current
  }
  lets <- names(d)
  paste0(lets[order(score, decreasing = TRUE)], collapse = "")
}

part3 <- function(d = rt("../data/everybody_codes_e2024_q07_p3.txt")) {
  res <- list()
  opts <- function(p = 5, m = 3, e = 3, str = c()) {
    if ((p == 0) && (m == 0) && (e == 0)) {
      res[[length(res) + 1]] <<- str
      return()
    } else {
      if (p > 0) opts(p - 1, m, e, c(str, "+"))
      if (m > 0) opts(p, m - 1, e, c(str, "-"))
      if (e > 0) opts(p, m, e - 1, c(str, "="))
    }
  }
  opts()

  track <- conv_track(c(
"S+= +=-== +=++=     =+=+=--=    =-= ++=     +=-  =+=++=-+==+ =++=-=-=--",
"- + +   + =   =     =      =   == = - -     - =  =         =-=        -",
"= + + +-- =-= ==-==-= --++ +  == == = +     - =  =    ==++=    =++=-=++",
"+ + + =     +         =  + + == == ++ =     = =  ==   =   = =++=       ",
"= = + + +== +==     =++ == =+=  =  +  +==-=++ =   =++ --= + =          ",
"+ ==- = + =   = =+= =   =       ++--          +     =   = = =--= ==++==",
"=     ==- ==+-- = = = ++= +=--      ==+ ==--= +--+=-= ==- ==   =+=    =",
"-               = = = =   +  +  ==+ = = +   =        ++    =          -",
"-               = + + =   +  -  = + = = +   =        +     =          -",
"--==++++==+=+++-= =-= =-+-=  =+-= =-= =--   +=++=+++==     -=+=++==+++-"),
2024)

  reps <- length(track) / 11
  cmds <- rep(d$A, reps)
  df <- data.frame(track = track, cmds = cmds)
  TP <- df$track == "+"
  TM <- df$track == "-"
  TS <- df$track %in% c("=", "S")
  df$delta[TP] <- 1
  df$delta[TM] <- -1
  df$delta[TS & df$cmds == "+"] <- 1
  df$delta[TS & df$cmds == "-"] <- -1
  df$delta[TS & df$cmds == "="] <- 0
  score <- sum(cumsum(df$delta + 10))
  win <- 0
  for (r in seq_along(res)) {
    df$cmds <- rep(res[[r]], reps)
    df$delta[TS & df$cmds == "+"] <- 1
    df$delta[TS & df$cmds == "-"] <- -1
    df$delta[TS & df$cmds == "="] <- 0
    score2 <- sum(cumsum(df$delta + 10))
    if (score2 > score) {
      win <- win + 1
    }
  }
  win
}

part1()
part2()
part3()
