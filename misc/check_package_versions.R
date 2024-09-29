library(bibtex)
library(dplyr)

bib <- read.bib("../wollschlaeger_gddmr_000.bib")

get_cran <- function(x, what=c("version", "year")) {
    what <- match.arg(what)
    if(what == "version") {
        ll <- grep("Version:", x) + 1
        gsub("^<td>([[:digit:].-]+)</td>$", "\\1", x[ll])
    } else if(what == "year") {
        ll <- grep("Published:", x) + 1
        gsub("^<td>([[:digit:]]{4})-.+</td>$", "\\1", x[ll])
    }
}

get_crantv <- function(x, what="version") {
    what <- match.arg(what)
    ll   <- grep("DC\\.issued", x)
    gsub("^.+([[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}).+$", "\\1", x[ll])
}

get_cran_url <- function(x) {
    url     <- x$url
    is_cran <- grepl("CRAN\\.R-project\\.org", url)
    if(is_cran) {
        url
    } else {
        pack_name <- gsub("^\\{?([[:alnum:]]+)\\}?:.+$", "\\1", x$title)
        paste0("https://CRAN.R-project.org/package=", pack_name)
    }
}

check_version <- function(x) {
    is_package <- !is.null(x$note) &&
                  (x$bibtype == "Misc") &&
                  grepl("^R package", x$note, ignore.case=TRUE)
    is_crantv  <- !is.null(x$note) &&
                  (x$bibtype == "Misc") &&
                  grepl("CRAN.+task.+view", x$title, ignore.case=TRUE)

    if(is_package) {
        ver_bib   <- gsub("^R package version ([[:digit:].-]+)$", "\\1", x$note)
        year_bib  <- x$year
        url_cran  <- get_cran_url(x)
        cran      <- readLines(url_cran)
        ver_cran  <- get_cran(cran, "version")
        year_cran <- get_cran(cran, "year")
        d <- try(data.frame(key=x$key,
                        title=x$title,
                        ver_bib=ver_bib,
                        year_bib=year_bib,
                        ver_cran=ver_cran,
                        year_cran=year_cran,
                        stringsAsFactors=FALSE))
        if(inherits(d, "try-error")) { browser(); return(NULL) }
        return(d)
    } else if(is_crantv) {
        ver_bib   <- gsub("^Version (.+)$", "\\1", x$note, ignore.case=TRUE)
        year_bib  <- gsub("^([[:digit:]]{4})-.+$", "\\1", ver_bib)
        cran      <- readLines(x$url)
        ver_cran  <- get_crantv(cran, "version")
        year_cran <- gsub("^([[:digit:]]{4})-.+$", "\\1", ver_cran)
        d <- try(data.frame(key=x$key,
                          title=x$title,
                          ver_bib=ver_bib,
                          ver_cran=ver_cran,
                          year_bib=year_bib,
                          year_cran=year_cran,
                          stringsAsFactors=FALSE))
        if(inherits(d, "try-error")) { browser(); return(NULL) }
        return(d)
    }

    return(NULL)
}

# cranL <- Filter(Negate(is.null), Map(check_version, bib))
# cran  <- do.call("rbind.data.frame", cranL)
# save(cran, file=sprintf("checked_packages_%s.Rdata", Sys.Date()))

load("checked_packages_2020-03-17.Rdata")
cran %>%
    filter(ver_bib != ver_cran) %>%
    View()
