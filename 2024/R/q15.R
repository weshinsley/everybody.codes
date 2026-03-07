source("../../Util/utils.R")

bfs <- function(d, startx, starty) {
  dist <- matrix(Inf, nrow = nrow(d), ncol = ncol(d))
  queue <- list(list(x = startx, y = starty, dist = 0))
  i <- 1
  dx <- c(0, 1, 0, -1)
  dy <- c(-1, 0, 1, 0)
  while (i <= length(queue)) {
    state <- queue[[i]]
    i <- i + 1
    if (dist[state$y, state$x] <= state$dist) next
    dist[state$y, state$x] <- state$dist
    for (dir in 1:4) {
      nx <- state$x + dx[dir]
      ny <- state$y + dy[dir]
      if ((nx == 0) || (ny == 0) || (nx > ncol(d)) || (ny > nrow(d))) next
      if (d[ny, nx] %in% c("#", "~")) next
      if (dist[ny, nx] > state$dist + 1) {
        newstate <- list(x = nx, y = ny, dist = state$dist + 1)
        queue[[length(queue) + 1]] <- newstate
      }
    }
  }
  dist
}

make_matrix <- function(d, startx, starty, lets = NULL) {
  if (is.null(lets)) {
    lets <- unique(as.character(d))
    lets <- sort(lets[!lets %in% c("#", ".", "~")])
  }
  objs <- data.frame(id = "@", col = startx, row = starty)
  for (let in lets) {
    places <- as.data.frame(which(d == let, arr.ind = TRUE))
    places$id <- let
    objs <- rbind(objs, places)
  }

  m <- matrix(Inf, ncol = nrow(objs), nrow = nrow(objs))
  for (i in seq_len(nrow(objs))) {
    cat("\r ", i, "/", nrow(objs))
    dist <- bfs(d, objs$col[i], objs$row[i])
    for (j in seq_len(nrow(objs))) {
      m[j, i] <- dist[objs$row[j], objs$col[j]]
    }
  }
  cat("\r               ")
  list(m = m, objs = objs)
}

dfs <- function(m, objs, avail, at = 1, dist = 0) {
  if (dist >= best) return()
  if (length(avail) == 0) {
    dist <- dist + m[at, 1]
    if (dist < best) cat("\r ", dist)
    best <<- min(best, dist)
    return()
  }
  targets <- which(objs$id %in% avail)
  for (target in targets) {
    newdist <- dist + m[at, target]
    newavail <- avail[avail != objs$id[target]]
    dfs(m, objs, newavail, target, newdist)
  }
}

best <- Inf

part1 <- function(d = rcg("../data/everybody_codes_e2024_q15_p1.txt"),
                  startx = which(d[1, ] == "."), starty = 1) {
  dist <- bfs(d, startx, starty)
  2 * min(dist[which(d == "H")])
}

part2 <- function(d = rcg("../data/everybody_codes_e2024_q15_p2.txt")) {
  m <- make_matrix(d, which(d[1, ] == "."), 1)
  best <<- Inf
  dfs(m$m, m$objs, unique(m$objs$id)[-1])
  cat("\r            ")
  best
}

part3 <- function(d = rcg("../data/everybody_codes_e2024_q15_p3.txt")) {

  # This will be specific to my data. By inspection - three colums,
  # with E..K and K..R being the joiners.
  # Remove the bottom left E and bottom right R to avoid confusion, so
  # the E and R in the joiners are the only instances.

  d[76, 2] <- "#"
  d[76, 254] <- "#"

  # Do the left side. Set a block to the right of the E

  theE <- as.data.frame(which(d == "E", arr.ind = TRUE))
  theR <- as.data.frame(which(d == "R", arr.ind = TRUE))
  d[theE$row, theE$col + 1] <- "#"
  m <- make_matrix(d, theE$col, theE$row, c("A", "B", "C", "D"))
  best <<- Inf
  dfs(m$m, m$objs, unique(m$objs$id[-1]))
  left_dist <- best
  d[theE$row, theE$col + 1] <- "."

  # Do the middle. Block above the E and R

  d[theE$row - 1, theE$col] <- "#"
  d[theR$row - 1, theR$col] <- "#"
  m <- make_matrix(d, which(d[1, ] == "."), 1,
                   c("G", "H", "I", "J", "K", "E", "R"))
  best <<- Inf
  dfs(m$m, m$objs, unique(m$objs$id[-1]))
  mid_dist <- best
  d[theE$row - 1, theE$col] <- "."
  d[theR$row - 1, theR$col] <- "."

  # And do the right - block to the left of the R

  d[theR$row, theR$col - 1] <- "#"
  m <- make_matrix(d, theR$col, theR$row, c("N", "O", "P", "Q"))
  best <<- Inf
  dfs(m$m, m$objs, unique(m$objs$id[-1]))
  right_dist <- best
  left_dist + mid_dist + right_dist
}

part1()
part2()
part3()
