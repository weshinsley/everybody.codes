source("../../Util/utils.R")

parse <- function(d) {
  lapply(d, function(x) {
    x <- strsplit(x, ":")[[1]]
    list(id = as.integer(x[1]),
         dna = strsplit(x[2], "")[[1]])
  })
}

is_child_of <- function(child, parent1, parent2) {
  list(compat = sum((child != parent1) & (child != parent2)),
       score = sum(child == parent1) * sum(child == parent2))
}


part1 <- function(d = rl("../data/everybody_codes_e2025_q09_p1.txt")) {
  d <- parse(d)
  res <- list(is_child_of(d[[1]]$dna, d[[2]]$dna, d[[3]]$dna),
              is_child_of(d[[2]]$dna, d[[1]]$dna, d[[3]]$dna),
              is_child_of(d[[3]]$dna, d[[1]]$dna, d[[2]]$dna))
  child <- which(unlist(lapply(res, `[[`, "compat") == 0))
  res[[3]]$score
}

part2 <- function(d = rl("../data/everybody_codes_e2025_q09_p2.txt"),
                  part = 2) {
  d <- parse(d)
  tree <- data.frame()
  tot <- 0
  for (p1 in 1:(length(d) - 1)) {
    for (p2 in (p1 + 1):length(d)) {
      for (ch in seq_along(d)) {
        if ((ch == p1) || (ch == p2)) next
        res <- is_child_of(d[[ch]]$dna, d[[p1]]$dna, d[[p2]]$dna)
        if (res$compat == 0) {
          tot <- tot + res$score
          if (part == 3) tree <- rbind(tree,
            data.frame(p1 = d[[p1]]$id, p2 = d[[p2]]$id, ch = d[[ch]]$id))
        }
      }
    }
  }
  if (part == 2) return(tot)
  tree$fam <- 0
  fno <- 0
  while (sum(tree$fam == 0) > 0) {
    first <- which(tree$fam == 0)[1]
    fno <- fno + 1
    tree$fam[first] <- fno
    join <- c(tree$p1[first], tree$p2[first], tree$ch[first])
    while (length(join) > 0) {
      kids <- c()
      for (k in join) {
        more <- which((tree$p1 == k | tree$p2 == k | tree$ch == k) & tree$fam == 0)
        if (length(more) > 0) {
          tree$fam[more] <- fno
          kids <- unique(c(kids, join, tree$ch[more], tree$p1[more], tree$p2[more]))
        }
      }
      join <- kids
    }
  }
  maxfam <- 0
  score <- 0
  for (i in 1:fno) {
    f <- which(tree$fam == i)
    fam <- unique(c(tree$p1[f], tree$p2[f], tree$ch[f]))
    if (length(fam) > maxfam) {
      maxfam <- length(fam)
      score <- sum(fam)
    }
  }
  score
}

part3 <- function(d = rl("../data/everybody_codes_e2025_q09_p3.txt")) {
  part2(d, part = 3)
}

part1()
part2()
part3()
