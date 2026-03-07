source("../../Util/utils.R")

parse_line <- function(s) {
  s <- strsplit(s, ", ")[[1]]
  r <- data.frame(
    id = as.integer(strsplit(s[1], "=")[[1]][2]),
    plug = strsplit(s[2], "=")[[1]][2],
    left = strsplit(s[3], "=")[[1]][2],
    right = strsplit(s[4], "=")[[1]][2],
    lid = 0, rid = 0, pid = 0,
    data = strsplit(s[5], "=")[[1]][2])
  r$left_col <- strsplit(r$left, " ")[[1]][1]
  r$left_shape <- strsplit(r$left, " ")[[1]][2]
  r$right_col <- strsplit(r$right, " ")[[1]][1]
  r$right_shape <- strsplit(r$right, " ")[[1]][2]
  r$plug_col <- strsplit(r$plug, " ")[[1]][1]
  r$plug_shape <- strsplit(r$plug, " ")[[1]][2]
  r

}

circle_tree <- function(d) {
  at <- 1
  visit <- c()
  bvisit <- c()
  while(TRUE) {
    node <- d[d$id == at, ]
    bvisit <- c(bvisit, paste(at, "L"))
    if (node$lid != 0) { ## Explore left if we can.
      at <- node$lid
      next
    }
    visit <- c(visit, at) ## Can't explore left - visit "*"
    bvisit <- c(bvisit, paste(at, "R"))
    if (node$rid != 0) { ## Explore right if we can.
      at <- node$rid
      #bvisit <- c(bvisit, paste(at, "R"))
      next
    }
    while (TRUE) {
      if (node$pid == 0) return(list(visit = visit, bvisit = bvisit))
      next_node <- d[d$id == node$pid, ] # Otherwise, here's the next node.
      if (next_node$lid == node$id) {
        visit <- c(visit, next_node$id)
        bvisit <- c(bvisit, paste(next_node$id, "R"))
        if (next_node$rid != 0) {
          at <- next_node$rid
          break
        }
      }
      node <- next_node
    }
  }
}

build_tree1 <- function(d) {
  i <- 1
  for (j in 2:nrow(d)) {
    x <- d[j, ]
    in_tree <- d[1:(j-1), ]
    sockets <- in_tree[(in_tree$left == x$plug & in_tree$lid == 0) |
                       (in_tree$right == x$plug & in_tree$rid == 0), ]
    if (nrow(sockets) > 1) { # Multiple nodes - choose the earliest in path}
      path <- circle_tree(d)$visit
      path <- path[path %in% sockets$id]
      sockets <- sockets[sockets$id == path[1], ]
    }
    d$pid[j] <- sockets$id
    if (sockets$left == x$plug) d$lid[d$id == sockets$id] <- x$id
    else d$rid[d$id == sockets$id] <- x$id
  }
  d
}

build_tree2 <- function(d) {
  i <- 1
  for (j in 2:nrow(d)) {
    x <- d[j, ]
    in_tree <- d[1:(j-1), ]
    sockets <- in_tree[
      ((in_tree$lid == 0) &
         ((in_tree$left_col == x$plug_col |
             in_tree$left_shape == x$plug_shape))) |
        ((in_tree$rid == 0) &
           ((in_tree$right_col == x$plug_col |
               in_tree$right_shape == x$plug_shape))), ]

    if (nrow(sockets) > 1) { # Multiple nodes - choose the earliest in path}
      path <- circle_tree(d)$visit
      path <- path[path %in% sockets$id]
      sockets <- sockets[sockets$id == path[1], ]
    }
    d$pid[j] <- sockets$id
    if ((sockets$lid == 0) &
        ((sockets$left_col == x$plug_col) |
        (sockets$left_shape == x$plug_shape))) {
      d$lid[d$id == sockets$id] <- x$id
    } else {
      d$rid[d$id == sockets$id] <- x$id
    }
  }
  d
}

build_tree3 <- function(d) {
  for (j in 2:nrow(d)) {
    addme <- d[j, ]
    places <- circle_tree(d)$bvisit
    junc <- 1
    while(TRUE) {
      place <- strsplit(places[junc], " ")[[1]]
      id <- as.integer(place[1])
      node <- d[d$id == id, ]

      if (place[2] == "L") {
        strong <- node$left == addme$plug
        weak <- node$left_col == addme$plug_col ||
                node$left_shape == addme$plug_shape
        if ((node$lid == 0) && (strong || weak)) {
          d$lid[d$id == id] <- addme$id
          d$pid[d$id == addme$id] <- id
          break
        }
        if ((node$lid != 0) && (strong)) {
          if (node$left != d$plug[d$id == node$lid]) {
            shunt <- d[d$id == node$lid, ]
            d$lid[d$id == id] <- 0
            d$pid[d$id == shunt$id] <- 0
            places <- circle_tree(d)$bvisit
            junc <- which(places == paste(place, collapse=" ")) + 1
            if (junc > length(places)) junc <- 1
            restart_search_at <- places[junc]

            d$lid[d$id == id] <- addme$id
            d$pid[d$id == addme$id] <- id
            addme <- d[d$id == shunt$id, ]
            places <- circle_tree(d)$bvisit
            junc <- which(places == restart_search_at)
            next
          }
        }
      } else { # "R"
        strong <- node$right == addme$plug
        weak <- node$right_col == addme$plug_col ||
                node$right_shape == addme$plug_shape
        if ((node$rid == 0) && (strong || weak)) {
          d$rid[d$id == id] <- addme$id
          d$pid[d$id == addme$id] <- id
          break
        }
        if ((node$rid != 0) && (strong)) {
          if (node$right != d$plug[d$id == node$rid]) {
            shunt <- d[d$id == node$rid, ]
            d$rid[d$id == id] <- 0
            d$pid[d$id == shunt$id] <- 0
            places <- circle_tree(d)$bvisit
            junc <- which(places == paste(node$id, "R", collapse=" ")) + 1
            if (junc > length(places)) junc <- 1
            restart_search_at <- places[junc]

            d$rid[d$id == id] <- addme$id
            d$pid[d$id == addme$id] <- id
            addme <- d[d$id == shunt$id, ]
            places <- circle_tree(d)$bvisit
            junc <- which(places == restart_search_at)
            next
          }
        }
      }
      junc <- if (junc == length(places)) 1 else junc + 1
    }
  }
  d
}

build_tree <- function(d, part) {
  if (part == 1) build_tree1(d)
  else if (part == 2) build_tree2(d)
  else if (part == 3) build_tree3(d)
}


part1 <- function(d = rl("../data/everybody_codes_e3_q03_p1.txt"),
                  part = 1) {
  d <- as.data.frame(data.table::rbindlist(lapply(d, parse_line)))
  d <- build_tree(d, part)
  x <- circle_tree(d)$visit
  sum(seq_along(x) * x)
}

part2 <- function(d = rl("../data/everybody_codes_e3_q03_p2.txt")) {
  part1(d, 2)
}

part3 <- function(d = rl("../data/everybody_codes_e3_q03_p3.txt")) {
  part1(d, 3)
}

part1()
part2()
part3()
d = rl("../data/everybody_codes_e3_q03_p3.txt")
