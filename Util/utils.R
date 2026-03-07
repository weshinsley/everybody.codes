# Readlines without warning

rl <- function(x) {
  readLines(x, warn = FALSE)
}

# Read a list of integers

ri <- function(x) {
  as.integer(rl(x))
}

# Read a matrix of characters
rcg <- function(d) {
  if (length(d) == 1) d <- rl(d)
  wid <- nchar(d[1])
  hei <- length(d)
  m <- matrix(unlist(lapply(d, function(x) {
    strsplit(x, "")[[1]]
  })), byrow = TRUE, ncol = wid, nrow = hei)
}


# Read a grid of characters, converting to integers

rig <- function(d, chars = c(".", "#"), pad = 1) {
  if (length(d) == 1) d <- rl(d)
  wid <- nchar(d[1])
  hei <- length(d)
  m <- matrix(as.integer(unlist(lapply(d, function(x) {
    x <- strsplit(x, "")[[1]]
    for (j in seq_along(chars)) {
      x[x == chars[j]] <- (j - 1)
    }
    x
  }))), byrow = TRUE, ncol = wid, nrow = hei)

  if (pad >= 1) {
    m2 <- matrix(0, ncol = wid + (2 * pad), nrow = hei + (2 * pad))
    m2[(pad + 1):(pad + hei),(pad + 1):(pad + wid)] <- m
    m <- m2
  }
  m
}

# Read a tree in the form A:B,C into a nested list

rt <- function(d) {
  if (length(d) == 1) d <- rl(d)
  tree <- list()
  for (x in d) {
    xy <- strsplit(x, ":")[[1]]
    yy <- strsplit(xy[2], ",")[[1]]
    tree[[xy[1]]] <- yy
  }
  tree
}

# Read x=1 y=2 into a data frame.

rkv <- function(d) {
  if (length(d) == 1) d <- rl(d)
  as.data.frame(data.table::rbindlist(lapply(d, function(x) {
    x <- strsplit(x, "\\s+")[[1]]
    kvs <- strsplit(x, "=")
    names <- unlist(lapply(kvs, `[`, 1))
    values <- as.integer(unlist(lapply(kvs, `[`, 2)))
    df <- as.data.frame(as.list(values))
    names(df) <- names
    df
  })))
}

powmod <- function(a, b, m) {
  result <- 1
  a <- a %% m
  while (b > 0) {
    if (b %% 2 == 1) {
      result <- (result * a) %% m
    }
    b <- b %/% 2
    a <- (a * a) %% m
  }
  result
}
