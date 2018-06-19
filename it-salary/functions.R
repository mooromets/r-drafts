# filter experience
filterExp <- function(.data, expVal, prec = .2) {
  filter(.data, between(exp, 
                        round(expVal * (1 - prec), 1), 
                        round(expVal * (1 + prec), 1)))
}

filterLang <- function(.data, langs = c("C++", "C")) {
  filter(.data, lang %in% langs)
}

filterCity <- function(.data, locs = "Киев") {
  filter(.data, loc %in% locs)
}

filterRoleclass <- function(.data, class = "DEV") {
  filter(.data, cls == class)
}
