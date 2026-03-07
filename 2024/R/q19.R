source("../../Util/utils.R")

parse <- function(d) {
  list(code = strsplit(trimws(d[1]), "")[[1]], grid = rcg(d[3:length(d)]))
}

rotate <- function(grid, code) {
  p <- 1
  for (j in 2:(nrow(grid) - 1)) {
    for (i in 2:(ncol(grid) - 1)) {
      neigh <- c(grid[j - 1, (i - 1):(i + 1)],
                 grid[j, i + 1],
                 grid[j + 1, (i + 1):(i - 1)],
                 grid[j, i - 1])
      if (code[p] == "L") {
        neigh <- c(neigh[2:length(neigh)], neigh[1])
      } else {
        neigh <- c(neigh[length(neigh)], neigh[1:(length(neigh)-1)])
      }
      p <- p + 1
      if (p > length(code)) p <- 1
      grid[j - 1, (i - 1): (i + 1)] <- neigh[1:3]
      grid[j, i + 1] <- neigh[4]
      grid[j + 1, (i + 1):(i - 1)] <- neigh[5:7]
      grid[j, i - 1] <- neigh[8]
    }
  }
  grid
}

part1 <- function(d = parse(rl("../data/everybody_codes_e2024_q19_p1.txt")),
                            n = 1) {
  for (nn in 1:n) {
    d$grid <- rotate(d$grid, d$code)
  }
  str <- d$grid[as.integer(which(d$grid == ">", arr.ind = TRUE))[1], ]
  paste0(d$grid[2, 2:(ncol(d$grid) - 1)], collapse="")
  str <- str[(which(str == ">") + 1):(which(str == "<") - 1)]
  paste0(str, collapse = "")
}

get_transform <- function(grid, code) {
  nc <- ncol(grid)
  nr <- nrow(grid)
  lgth <- nc * nr
  nums1 <- matrix(1:lgth, nrow = nr, ncol = nc, byrow = TRUE)
  nums2 <- rotate(nums1, code)
  as.integer(t(nums2))
}

part2 <- function(d = parse(rl("../data/everybody_codes_e2024_q19_p2.txt")),
                  n = 100) {
  up <- get_transform(d$grid, d$code)
  d$grid <- as.character(t(d$grid))
  for (i in 1:n) {
    d$grid <- d$grid[up]
  }
  paste0(d$grid[(which(d$grid == ">") + 1):(which(d$grid == "<") - 1)],
         collapse = "")
}

part3 <- function(d = parse(rl("../data/everybody_codes_e2024_q19_p3.txt")),
                  tot = 1048576000) {
  d = parse(rl("../data/everybody_codes_e2024_q19_p3.txt"))
  tot = 1048576000
  nc <- ncol(d$grid)
  nr <- nrow(d$grid)
  maxlog <- 1 + floor(log(tot)/log(2))
  up <- get_transform(d$grid, d$code)
  d$grid <- as.character(t(d$grid))
  transforms <- list()
  transforms[[1]] <- up
  i <- 2
  while (i <= maxlog) {
    nums <- seq_along(d$grid)
    nums <- nums[up]
    nums <- nums[up]
    transforms[[i]] <- nums
    up <- nums
    i <- i + 1
  }
  pows <- 2^(0:(maxlog - 1))

  for (i in maxlog:1) {
    if (tot >= pows[i]) {
      tot <- tot - pows[i]
      d$grid <- d$grid[transforms[[i]]]
    }
  }
  d$grid <- matrix(d$grid, byrow = TRUE, ncol = nc, nrow = nr)
  for (j in 1:nrow(d$grid)) {
    message(paste0(d$grid[j, ], collapse = ""))
  }
  d$grid <- as.character(t(d$grid))
  paste0(d$grid[(which(d$grid == ">") + 1):(which(d$grid == "<") - 1)],
         collapse = "")
}

part1()
part2()
part3()
