source("../../Util/utils.R")

parse <- function(d) {
  nums <- as.integer(strsplit(d[1], ",")[[1]])
  cats <- rep(list(NULL), length(nums))
  for (i in 3:length(d)) {
    for (j in 0:(length(nums))) {
      str <- substring(d[i], 1 + (j * 4), 3 + (j * 4))
      if (trimws(str) != "") cats[[j + 1]] <- c(cats[[j + 1]], str)
    }
  }
  list(nums = nums, cats = cats, counts = unlist(lapply(cats, length)))
}

part1 <- function(d = parse(rl("../data/everybody_codes_e2024_q16_p1.txt")),
                            n = 100) {
  indexes <- 1 + ((d$nums * n) %% d$counts)
  cats <- paste(unlist(lapply(seq_along(d$nums),
    function(i) d$cats[[i]][indexes[i]])), collapse = " ")

  freq <- as.integer(table(strsplit(gsub(" ", "", cats), "")[[1]]))
  list(out = cats, score = sum(freq[freq >= 3] - 2))
}

part2 <- function(d = parse(rl("../data/everybody_codes_e2024_q16_p2.txt")),
                  target = 202420242024) {
  n <- 0
  sel <- NULL
  cumul <- 0
  set <- new.env(parent = emptyenv())
  while (n < target) {
    n <- n + 1
    indexes <- 1 + ((d$nums * n) %% d$counts)
    hash <- paste0(indexes, collapse="_")
    if (exists(hash, envir = set)) {
      loop_size <- n - 1
      loops <- (target %/% loop_size) - 1
      #cat("Repeat spotted at n=",n,"\n")
      n <- n + (loops * loop_size)
      #cat("n=",n," Add loops * cumul = ", loops, "*", cumul," = ", loops * cumul, "\n")
      cumul <- cumul + (loops * cumul)
      set <- new.env(parent = emptyenv())
    }
    assign(hash, 0, envir = set)
    cats <- unlist(lapply(seq_along(d$nums),
      function(i) strsplit(d$cats[[i]][indexes[i]], "")[[1]]))
    if (is.null(sel)) {
      sel <- seq(1, length(cats), by = 3)
      sel <- sort(c(sel, sel + 2))
    }
    freq <- as.integer(table(cats[sel]))
    val <- sum(freq[freq >= 3] - 2)
    cumul <- cumul + val
  }
  cumul
}

part3 <- function(d = parse(rl("../data/everybody_codes_e2024_q16_p3.txt"))) {
  set <- new.env(parent = emptyenv())
  work <- c(new.env(parent = emptyenv()), new.env(parent = emptyenv()))
  buf <- 1
  start <- paste0(rep(1, length(d$nums)), collapse = "_")
  assign(start, c(0, 0), envir = work[[buf]])
  sel <- NULL

  for (round in 1:256) {
    work[[3 - buf]] <- new.env(parent = emptyenv())
    states <- ls(work[[buf]])
    for (state in states) {
      minmax <- get(state, envir = work[[buf]])
      before <- as.integer(strsplit(state, "_")[[1]])
      after <- before
      mid <- before
      before <- (before - 1) + d$nums
      after <- after + 1 + d$nums
      mid <- mid + d$nums

      for (s in 1:3) {
        spin <- if (s == 1) before else if (s == 2) mid else after
        for (i in seq_along(mid)) {
          while (spin[i] > d$counts[i]) spin[i] <- spin[i] - d$counts[i]
        }
        if (any(spin > d$counts)) {
          browser()
        }
        hash <- paste0(spin, collapse = "_")
        cats <- unlist(lapply(seq_along(d$nums),
                              function(i) strsplit(d$cats[[i]][spin[i]], "")[[1]]))
        if (exists(hash, envir = set)) {
          score <- get(hash, envir = set)
        } else {
          cats <- unlist(lapply(seq_along(d$nums),
                                function(i) strsplit(d$cats[[i]][spin[i]], "")[[1]]))
          if (is.null(sel)) {
            sel <- seq(1, length(cats), by = 3)
            sel <- sort(c(sel, sel + 2))
          }

          freq <- as.integer(table(cats[sel]))
          score <- sum(freq[freq >= 3] - 2)
          assign(hash, score, envir = set)
        }

        new_minmax <- minmax + score
        if (exists(hash, envir = work[[3 - buf]])) {
          mm <- get(hash, envir = work[[3 - buf]])
          new_minmax[1] <- min(new_minmax[1], mm[1])
          new_minmax[2] <- max(new_minmax[2], mm[2])
        }
        assign(hash, new_minmax, envir = work[[3 - buf]])
      }
    }

    min_round <- Inf
    max_round <- -Inf
    states <- ls(work[[3 - buf]])
    for (state in states) {
      minmax <- get(state, envir = work[[3 - buf]])
      min_round <- min(min_round, minmax[1])
      max_round <- max(max_round, minmax[2])
    }
    buf <- 3 - buf
  }
  sprintf("%s %s", max_round, min_round)
}

part1()$out
part2()
part3()
