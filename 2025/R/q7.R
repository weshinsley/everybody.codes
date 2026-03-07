source("../../Util/utils.R")

parse <- function(d) {
  words <- strsplit(d[1], ",")[[1]]
  rules <- list()
  for (i in 3:length(d)) {
    rule <- strsplit(d[i], " > ")[[1]]
    rules[[rule[1]]] <- strsplit(rule[2], ",")[[1]]
  }
  list(words = words, rules = rules, lhs = names(rules))
}

check_word <- function(word, d) {
  pos <- 1
  current <- substr(word, pos, pos)
  if (!current %in% d$lhs) next
  while (TRUE) {
    pos <- pos + 1
    if (pos > nchar(word)) return(word)
    nextch <- substr(word, pos, pos)
    if (!nextch %in% d$rules[[current]]) return(NULL)
    current <- nextch
  }
}

part1 <- function(d = rl("../data/everybody_codes_e2025_q07_p1.txt"),
                  part = 1) {
  d <- parse(d)
  tot <- 0
  for (w in seq_along(d$words)) {
    word <- d$words[w]
    res <- check_word(word, d)
    if ((part == 1) && (!is.null(res))) return(res)
    if ((part == 2) && (!is.null(res))) tot <- tot + w
  }
  tot
}

part2 <- function(d = rl("../data/everybody_codes_e2025_q07_p2.txt")) {
  part1(d, 2)
}


part3 <- function(d = rl("../data/everybody_codes_e2025_q07_p3.txt")) {
  d <- parse(d)
  d$words <- sort(unlist(lapply(d$words, function(x) check_word(x, d))))
  rhs_lets <- sort(unique(unlist(d$rules)))
  for (i in LETTERS) d$rules[[i]] <- NULL
  lhs_lets <- sort(names(d$rules))
  all_lets <- sort(unique(c(rhs_lets, lhs_lets)))

  for (w in rev(seq_along(d$words))) {
    subs <- lapply(d$words[-w], function(x) grepl(paste0("^", x), d$words[w]))
    if (any(unlist(subs))) d$words <- d$words[-w]
  }

  maxl <- 12 - min(unlist(lapply(d$words, nchar)))
  freq <- c(1, rep(0, 10))
  lookup <- list()
  for (i in all_lets) lookup[[i]] <- freq
  for (lgth in 2:maxl) {
    for (start in all_lets) {
      tot <- 0
      if (start %in% lhs_lets) {
        for (after in d$rules[[start]]) tot <- tot + lookup[[after]][lgth - 1]
      }
      lookup[[start]][lgth] <- tot
    }
  }
  tot <- 0
  for (w in d$words) {
    len <- nchar(w)
    lastch <- substring(w, len, len)
    for (opts in (8-len):(12-len)) {
      tot <- tot + lookup[[lastch]][opts]
    }
  }
  tot
}

part1()
part2()
part3()
