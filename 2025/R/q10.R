source("../../Util/utils.R")

dx <- c(2, 2, 1, -1, -2, -2, -1, 1)
dy <- c(-1, 1, 2, 2, 1, -1, -2, -2)
part1 <- function(d = rcg("../data/everybody_codes_e2025_q10_p1.txt"),
                  moves = 4) {
  drag <- as.integer(which(d == "D", arr.ind = TRUE))
  visit <- matrix(0, nrow = nrow(d), ncol = ncol(d))
  visit[drag[2], drag[1]] <- 1
  for (i in 1:moves) {
    v <- as.data.frame(which(visit == 1, arr.ind = TRUE))
    for (j in seq_len(nrow(v))) {
      for (k in 1:8) {
        nx <- v$col[j] + dx[k]
        ny <- v$row[j] + dy[k]
        if ((nx < 1) | (ny < 1) | (nx > ncol(d)) | ny > nrow(d)) next
        visit[ny, nx] <- 1
      }
    }
  }
  sum(d[visit == 1] == "S")
}

part2 <- function(d = rcg("../data/everybody_codes_e2025_q10_p2.txt"),
                  moves = 20) {
  sheep <- matrix(as.integer(d == "S"), ncol = ncol(d), nrow = nrow(d))
  hide <- matrix(as.integer(d == "#"), ncol = ncol(d), nrow = nrow(d))
  visit1 <- matrix(as.integer(d == "D"), ncol = ncol(d), nrow = nrow(d))
  eat <- 0
  for (i in 1:moves) {
    visit2 <- visit1 * 0
    v <- as.data.frame(which(visit1 == 1, arr.ind = TRUE))
    for (j in seq_len(nrow(v))) {
      for (k in 1:8) {
        nx <- v$col[j] + dx[k]
        ny <- v$row[j] + dy[k]
        if ((nx < 1) | (ny < 1) | (nx > ncol(d)) | ny > nrow(d)) next
        visit2[ny, nx] <- 1
      }
    }
    visit1 <- visit2
    eat <- eat + sum(visit2 * sheep * (1 - hide))
    sheep[(visit2 == 1) & (hide == 0)] <- 0
    for (j in nrow(d):2) {
      sheep[j, ] <- sheep[j-1, ]
    }
    sheep[1, ] <- 0
    eat <- eat + sum(visit2 * sheep * (1 - hide))
    sheep[(visit2 == 1) & (hide == 0)] <- 0
  }
  eat
}

part3 <- function(d = rcg("../data/everybody_codes_e2025_q10_p3.txt")) {
  cache <- new.env(parent = emptyenv())
  sheep <- as.data.frame(which(d == "S", arr.ind = TRUE))
  drag <- as.data.frame(which(d == "D", arr.ind = TRUE))
  sheep <- sheep[order(sheep$col, sheep$row),]
  xs <- sheep$col
  sheep <- sheep$row

  hash <- function(dragx, dragy, sheep, turn) {
    hash <- ((dragy * ncol(d)) + dragx)
    for (i in seq_along(sheep)) {
      hash <- (hash * 8) + sheep[i]
    }
    hash <- (hash * 2) + turn
    paste0("_", hash)
  }

  explore_drag <- function(dragx, dragy, sheep) {
    # Check previous results of function

    code <- hash(dragx, dragy, sheep, 0)
    if (exists(code, envir = cache)) {
      return(get(code, envir = cache))
    }
    # For each of the potential 8 moves, go there,
    # eat a sheep if possible, and sum the results from explore_sheep.

    score <- 0
    for (k in 1:8) {
      nx <- dragx + dx[k]
      ny <- dragy + dy[k]
      if ((nx < 1) || (ny < 1) || (nx > ncol(d)) || (ny > nrow(d))) next
      sheep2 <- sheep
      sx <- which(xs == nx)
      if ((length(sx) == 1) && (ny == sheep[sx]) && (d[ny, nx] != '#')) {
        sheep2[sx] <- 0
      }
      score <- score + explore_sheep(nx, ny, sheep2)
    }
    assign(code, score, envir = cache)
    return(score)
  }

  explore_sheep <- function(dragx, dragy, sheep) {
    # Check previous results of function
    code <- hash(dragx, dragy, sheep, 1)
    if (exists(code, envir = cache)) {
      return(get(code, envir = cache))
    }
    # If any shape has escaped, then prune and return 0.

    if (any(sheep > nrow(d))) {
      assign(code, 0, envir = cache)
      return(0)
    }

    # Extra prune - if it's hashes from a sheep to the bottom, then
    # it can't be caught.
    for (i in seq_along(sheep)) {
      if (sheep[i] == 0) next
      if (all(d[sheep[i]:nrow(d), xs[i]] == "#")) {
        assign(code, 0, envir = cache)
        return(0)
      }
    }

    # If there are no sheep, prune and return 1.
    if (sum(sheep) == 0) {
      assign(code, 1, envir = cache)
      return(1)
    }

    # Otherwise, sum the number of successes from branches
    # starting with each possible sheep move.

    # First tho, need to know whether any moves are possible. If not,
    # then the dragon gets second. But we can't play the "don't walk into the
    # dragon" card if other moves are available.

    any_moves <- FALSE
    for (i in seq_along(sheep)) {
      if ((sheep[i] == 0) || (sheep[i] > nrow(d))) next
      if ((sheep[i] == nrow(d)) || (d[sheep[i] + 1, xs[i]] == '#') ||
          (dragx != xs[i]) || (dragy != sheep[i] + 1)) {
        any_moves <- TRUE
        break
      }
    }

    score <- 0
    for (i in seq_along(sheep)) {
      if (sheep[i] == 0) next # Eaten sheep.
      sheep2 <- sheep
      sheep2[i] <- sheep2[i] + 1
      if (sheep2[i] > nrow(d)) next

      if ((dragx == xs[i]) && (dragy == sheep2[i]) && # Don't walk.
          (d[sheep2[i], xs[i]] == "#")) {
        score <- score + explore_drag(dragx, dragy, sheep2)
        next
      }
      if ((dragx != xs[i]) || (dragy != sheep2[i])) {
        score <- score + explore_drag(dragx, dragy, sheep2) # Explore D move
        next
      }
      if (!any_moves) {
        score <- score + explore_drag(dragx, dragy, sheep)
      }
    }
    assign(code, score, envir = cache) # Cache function result and return it.
    return(score)
  }

  explore_sheep(drag$col, drag$row, sheep)
}

options(digits=14)
part1()
part2()
part3()
