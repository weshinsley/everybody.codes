source("../../Util/utils.R")

round <- function(n, cols) {
  col <- ((n - 1) %% 4) + 1
  clap <- cols[[col]][1]
  cols[[col]] <- cols[[col]][-1]
  newcol <- col + 1
  if (newcol == 5) newcol = 1
  last <- length(cols[[newcol]])
  steps <- clap
  while (steps > (2 * last)) {
    steps <- steps - (2 * last)
  }
  if (steps <= last) {
    cols[[newcol]] <- append(cols[[newcol]], clap, steps - 1)
  } else {
    cols[[newcol]] <- append(cols[[newcol]], clap, last - (steps - (last + 1)))
  }
  cols
}

parse <- function(d) {
  d <- lapply(strsplit(d, " "), as.integer)
  lapply(1:4, function(y) 
    unlist(lapply(seq_along(d), function(x) d[[x]][y])))
}

part1 <- function(d = rl("../data/everybody_codes_e2024_q05_p1.txt")) {
  cols <- parse(d)
  for (i in 1:10) cols <- round(i, cols)
  as.integer(paste0(c(cols[[1]][1], cols[[2]][1], cols[[3]][1], cols[[4]][1]),
          collapse = ""))
}

part2 <- function(d = rl("../data/everybody_codes_e2024_q05_p2.txt"), 
                  part3 = FALSE) {
  cols <- parse(d)
  count <- new.env()
  i <- 1
  while (TRUE) {
    cols <- round(i, cols)
    
    shout <- paste0(c(cols[[1]][1], cols[[2]][1], 
                      cols[[3]][1], cols[[4]][1]), collapse = "")
    if (!exists(shout, envir = count)) {
      assign(shout, 1, envir = count)
    } else {
      x <- get(shout, envir = count)
      x <- x + 1
      assign(shout, x, envir = count)
      if (!part3) {
        if (x == 2024) {
          return(format(as.double(shout) * i, digits = 16))
        }
      } else {
        if (x == 10) {
          return(sort(ls(count), decreasing = TRUE)[1])
        }
      }
    }
    i <- i + 1
  }
}

part3 <- function(d = rl("../data/everybody_codes_e2024_q05_p3.txt")) {
  part2(d, TRUE)
}

part1()
part2()
part3()
