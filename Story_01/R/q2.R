source("../../Util/utils.R")

parseline <- function(s) {
  s <- strsplit(s, "\\s+")[[1]]
  if (s[1] == "ADD") {
    id <- as.integer(strsplit(s[2], "=")[[1]][2])
    left <- gsub("[[\\]]", "", strsplit(s[3], "=")[[1]][2], perl = TRUE)
    right <- gsub("[[\\]]", "", strsplit(s[4], "=")[[1]][2], perl = TRUE)
    left_val <- as.integer(strsplit(left, ",")[[1]][1])
    left_id <- strsplit(left, ",")[[1]][2]
    right_val <- as.integer(strsplit(right, ",")[[1]][1])
    right_id <- strsplit(right, ",")[[1]][2]
    list(command = s[1], id = id, left_val = left_val, left_id = left_id,
         right_val = right_val, right_id = right_id)
  } else {
    list(command = "SWAP", id = as.integer(s[2]))
  }
}

part1 <- function(d = rl("../data/everybody_codes_e1_q02_p1.txt"),
                  p3 = FALSE) {
  insert <- function(tree, id, val, label, tn) {
    if (nrow(tree[tree$tn == tn, ]) == 0) {
      return(rbind(tree, data.frame(
        x = 0, y = 0, id = id, val = val, label = label, lid = -1, rid = -1,
        tn = tn)))
    }
    loc <- which(tree$x == 0 & tree$y == 0 & tree$tn == tn)
    row <- tree[loc, ]
    while(TRUE) {
      if (val < row$val) {
        if (row$lid == -1) {
          tree$lid[loc] <- id
          xd <- 2^(23 - row$y)
          return(rbind(tree, data.frame(
            x = row$x - xd, y = row$y + 1, id = id, val = val, label = label,
            lid = -1, rid = -1, tn = tn)))
        }
        loc <- which(tree$id == row$lid)
        row <- tree[loc, ]
      } else {
        if (row$rid == -1) {
          tree$rid[loc] <- id
          xd <- 2^(23 - row$y)
          return(rbind(tree, data.frame(
            x = row$x + xd, y = row$y + 1, id = id, val = val, label = label,
            lid = -1, rid = -1, tn = tn)))
        }
        loc <- which(tree$id == row$rid)
        row <- tree[loc, ]
      }
    }
  }

  swap <- function(tree, id, p3 = FALSE) {
    lid <- which(tree$id == sprintf("L_%d", id))
    rid <- which(tree$id == sprintf("R_%d", id))
    rowl <- tree[lid, ]
    rowr <- tree[rid, ]
    tree$label[lid] <- rowr$label
    tree$label[rid] <- rowl$label
    tree$val[lid] <- rowr$val
    tree$val[rid] <- rowl$val
    tree
  }

  swap3 <- function(tree, id) {
    propogate <- function(tree, row_no) {
      q <- list(tree$id[row_no])
      i <- 1
      while (i <= length(q)) {
        node <- q[[i]]
        i <- i + 1
        row <- which(tree$id == node)
        node <- tree[row, ]
        for (child in c(node$lid, node$rid)) {
          if (child != -1) {
            row <- which(tree$id == child)
            tree$tn[row] <- node$tn
            tree$y[row] <- node$y + 1
            xd <- 2^(23 - node$y)
            if (child == node$lid) xd <- (-xd)
            tree$x[row] <- node$x + xd
            q[[length(q) + 1]] <- child
          }
        }
      }
      tree
    }

    ####

    lid_name <- sprintf("L_%d", id)
    rid_name <- sprintf("R_%d", id)
    lid <- which(tree$id == lid_name)
    rid <- which(tree$id == rid_name)
    parentL <- which((tree$lid == lid_name) | (tree$rid == lid_name))
    parentR <- which((tree$lid == rid_name) | (tree$rid == rid_name))

    if ((length(parentL) == 0) && (length(parentR) == 0)) { # whole tree.
      tree$tn <- 3 - tree$tn
      return(tree)
    }
    if (length(parentL) == 0)  {  # Left is root, right isn't.
      lroot <- tree[lid, ]
      rnode <- tree[rid, ]
      if (tree$lid[parentR] == rid_name) { # Update whichever parent pointer
        tree$lid[parentR] <- lroot$id
      } else {
        tree$rid[parentR] <- lroot$id
      }
      tree$tn[lid] <- rnode$tn        # Left node is now in right tree
      tree$y[lid] <- rnode$y          # y-cord is what right node had
      tree$x[lid] <- rnode$x          # x-cord too
      tree$tn[rid] <- lroot$tn        # Right node move to left tree
      tree$y[rid] <- lroot$y          # It's now a root but let's copy
      tree$x[rid] <- lroot$x          # from node anyway - see if this code
      tree <- propogate(tree, lid)    # solves all. Recalculate tn,x,y
      tree <- propogate(tree, rid)    # starting at the new node positions.
      return(tree)
    }
    if (length(parentR) == 0) { # Right is root, left isn't.}
      rroot <- tree[rid, ]
      lnode <- tree[lid, ]
      if (tree$lid[parentL] == lid_name) { # Update whichever parent pointer
        tree$lid[parentL] <- rroot$id
      } else {
        tree$rid[parentL] <- rroot$id
      }
      tree$tn[rid] <- lnode$tn        # Right node is now in left's tree
      tree$y[rid] <- lnode$y          # y-cord is what left node had
      tree$x[rid] <- lnode$x          # x-cord too
      tree$tn[lid] <- rroot$tn        # Left node move to right tree
      tree$y[lid] <- rroot$y          # It's now a root but let's copy
      tree$x[rid] <- rroot$x          # from node anyway - see if this code
      tree <- propogate(tree, lid)    # solves all. Recalculate tn,x,y
      tree <- propogate(tree, rid)    # starting at the new node positions.
      return(tree)
    }

    # Neither is a root.

    lnode <- tree[lid, ]
    rnode <- tree[rid, ]

    if (tree$lid[parentL] == lid_name) { # Update whichever parent pointer
      tree$lid[parentL] <- rnode$id
    } else {
      tree$rid[parentL] <- rnode$id
    }

    if (tree$lid[parentR] == rid_name) { # Update whichever parent pointer
      tree$lid[parentR] <- lnode$id
    } else {
      tree$rid[parentR] <- lnode$id
    }
    tree$tn[rid] <- lnode$tn        # Right node is now in left's tree
    tree$y[rid] <- lnode$y          # y-cord is what left node had
    tree$x[rid] <- lnode$x          # x-cord too
    tree$tn[lid] <- rnode$tn        # Left node move to right tree
    tree$y[lid] <- rnode$y          # It's now a root but let's copy
    tree$x[rid] <- rnode$x          # from node anyway - see if this code
    tree <- propogate(tree, parentL)    # solves all. Recalculate tn,x,y
    tree <- propogate(tree, parentR)    # starting at the new node positions.
    return(tree)
  }

  tree <- data.frame(x = c(), y = c(), id = c(), val = c(), label = c(),
                     lid = c(), rid = c(), tn = c())
  for (s in d) {
    s <- parseline(s)
    if (s$command == "ADD") {
      tree <- insert(tree, sprintf("L_%d", s$id), s$left_val, s$left_id, 1)
      tree <- insert(tree, sprintf("R_%d", s$id), s$right_val, s$right_id, 2)
    } else if (s$command == "SWAP") {
      tree <- if (p3) swap3(tree, s$id) else swap(tree, s$id)
    }
  }
  tree <- tree[order(tree$y, tree$x), ]
  level_l <- as.numeric(names(which.max(table(tree$y[tree$tn == 1]))))
  level_r <- as.numeric(names(which.max(table(tree$y[tree$tn == 2]))))
  paste0(c(tree$label[tree$y == level_l & tree$tn == 1],
           tree$label[tree$y == level_r & tree$tn == 2]), collapse = "")
}

part2 <- function(d = rl("../data/everybody_codes_e1_q02_p2.txt")) {
  part1(d)
}

part3 <- function(d = rl("../data/everybody_codes_e1_q02_p3.txt")) {
  part1(d, p3 = TRUE)
}

part1()
part2()
part3()
