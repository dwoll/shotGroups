library(stringr)

path <- ".."
f <- list.files(path=path,
                pattern="wollschlaeger_gddmr_[[:digit:]]+.+\\.tex$")
f <- f[!grepl("99|900|settings|rLong", f)]

fL <- lapply(paste(path, f, sep="/"), function(f) {
    con <- file(f, encoding="UTF8")
    on.exit(close(con))
    txt <- readLines(con)
    txt_mod <- str_replace_all(txt,     "<<", "<")
    txt_mod <- str_replace_all(txt_mod, ">>", ">")
    txt_mod <- str_replace_all(txt_mod, "\"", "'")
    txt_mod
})
fAll <- unlist(fL)

lst_start <- grep("^\\\\begin\\{lstlisting\\}", fAll)
lst_end   <- grep("^\\\\end\\{lstlisting\\}",   fAll)

lstL <- Map(function(x) {
    idx_start <- lst_start[x]+1
    idx_end   <- lst_end[x]-1
    fAll[idx_start:idx_end] }, seq_along(lst_start))

lstL <- setNames(lstL, seq_along(lst_start))

lst_ncharL <- lapply(lstL, nchar)
lst_gt7XL  <- which(sapply(lst_ncharL, function(x) any(x > 71)))
# lstL[lst_gt7XL]
lstL[lst_gt7XL[1]]
lstL[lst_gt7XL[2]]
lstL[lst_gt7XL[3]]
lstL[lst_gt7XL[4]]
lstL[lst_gt7XL[5]]

lst_eq71L  <- which(sapply(lst_ncharL, function(x) any(x == 71L)))
lst_eq71   <- unlist(lstL[lst_eq71L])
writeLines(lst_eq71[nchar(lst_eq71) == 71L], "lines_eq71.txt")

lst_eq72L  <- which(sapply(lst_ncharL, function(x) any(x == 72L)))
lst_eq72   <- unlist(lstL[lst_eq72L])
writeLines(lst_eq72[nchar(lst_eq72) == 72L], "lines_eq72.txt")
