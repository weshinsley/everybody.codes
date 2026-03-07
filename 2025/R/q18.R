source("../../Util/utils.R")

parse <- function(d) {
  plants <- data.frame()
  i <- 1
  while(i <= length(d)) {
    s <- d[i]
    s <- as.integer(strsplit(gsub("Plant ", "", gsub(" with thickness ", ",",
                  gsub(":", "", s))), ",")[[1]])
    plant <- data.frame(id = s[1], thickness = s[2])
    i <- i + 1
    while ((i <= length(d)) && (nchar(d[i]) > 0) && (substring(d[i], 1, 1) == "-")) {
      s <- d[i]
      if (grepl("free branch", s)) {
        s <- as.integer(gsub("- free branch with thickness ", "", s))
        plant$br <- NA
        plant$brt <- s
        plants <- rbind(plants, plant)
      } else {
        s <- as.integer(strsplit(gsub("- branch to Plant ", "",
                      gsub(" with thickness ", ",", s)), ",")[[1]])
        plant$br <- s[1]
        plant$brt <- s[2]
        plants <- rbind(plants, plant)
      }
      i <- i + 1
    }
  }
  plants
}

get_energy <- function(d, id) {
  ds <- d[d$id == id, ]
  if ((nrow(ds) == 1) && (is.na(ds$br))) {
    if ((ds$brt >= ds$thickness)) return(ds$brt)
    else return(0)
  }
  e <- 0
  for (j in seq_len(nrow(ds))) {
    link <- ds[j, ]
    elink <- get_energy(d, link$br)
    e <- e + (elink * link$brt)
  }
  return(if (e >= ds$thickness[1]) e else 0)
}

part1 <- function(d = rl("../data/everybody_codes_e2025_q18_p1.txt")) {
  d <- parse(d)
  plants <- unique(d$id)
  terminal <- plants[!plants %in% d$br]
  get_energy(d,terminal)
}

part2 <- function(d = rl("../data/everybody_codes_e2025_q18_p2.txt")) {
  spaces <- which(d == "")
  codes <- d[(spaces[length(spaces)] + 1):length(d)]
  d <- parse(d[1:(spaces[length(spaces)]-2)])
  tot <-0
  plants <- unique(d$id)
  terminal <- plants[!plants %in% d$br]
  for (code in codes) {
    nums <- as.numeric(strsplit(code," ")[[1]])
    d$brt[is.na(d$br)] <- nums
    tot <- tot + get_energy(d, terminal)
  }
  tot
}

part3 <- function(d = rl("../data/everybody_codes_e2025_q18_p3.txt")) {
  spaces <- which(d == "")
  codes <- d[(spaces[length(spaces)] + 1):length(d)]
  d <- parse(d[1:(spaces[length(spaces)]-2)])
  tot <-0
  plants <- unique(d$id)
  terminal <- plants[!plants %in% d$br]
  negs <- d$br[d$brt < 0]
  free <- d$id[is.na(d$br)]
  code <- as.integer(!free %in% negs)
  d$brt[is.na(d$br)] <- code
  e <- get_energy(d, terminal)
  tot <- 0
  for (code in codes) {
    nums <- as.numeric(strsplit(code," ")[[1]])
    d$brt[is.na(d$br)] <- nums
    e2 <- get_energy(d, terminal)
    if (e2 > 0) tot <- tot + (e - e2)
  }
  tot
}

part1()
part2()
part3()
