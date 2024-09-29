f <- list.files(".", "*.tex")
f <- f[!grepl("99|settings|rLong", f)]

fL <- lapply(f, readLines)
fAll <- unlist(fL)

# filter single-line LaTeX elements

fF <- gsub("\\\\chapter\\*?", "", fAll)
fF <- gsub("\\\\section\\*?", "", fF)
fF <- gsub("\\\\subsection\\*?", "", fF)
fF <- gsub("\\\\subsubsection\\*?", "", fF)

fF <- gsub("\\\\emph", "", fF)
fF <- gsub("\\\\tiny", "", fF)
fF <- gsub("\\\\item", "", fF)
fF <- gsub("\\\\footnote", "", fF)
fF <- gsub("\\\\textbf", "", fF)
fF <- gsub("\\\\textsf", "", fF)
fF <- gsub("\\\\texttt", "", fF)
fF <- gsub("\\\\textit", "", fF)
fF <- gsub("\\\\sffamily", "", fF)
fF <- gsub("\\\\caption", "", fF)
fF <- gsub("\\\\texorpdfstring", "", fF)
fF <- gsub("\\\\hline", "", fF)
fF <- gsub("\\\\\\\\", "", fF)
fF <- gsub("&", "", fF)
fF <- gsub("\\{\\\\\quotedblbase\\}", "", fF) # \quotedblbase
fF <- gsub("\\{\\\\\textquotedblleft\\}", "", fF) #\textquotedblleft
fF <- gsub("\\\\cite\\{.+?\\}", "", fF)
fF <- gsub("\\\\citeA\\{.+?\\}", "", fF)
fF <- gsub("\\\\citeA\\[.+\\]\\{.+?\\}", "", fF)
fF <- gsub("\\\\citeNP\\{.+?\\}", "", fF)
fF <- gsub("\\\\citeNP\\[.+\\]\\{.+?\\}", "", fF)
fF <- gsub("\\\\label\\{.+?\\}", "", fF)
fF <- gsub("\\\\ref\\{.+?\\}", "", fF)
fF <- gsub("\\\\url\\{.+?\\}", "", fF)
fF <- gsub("\\\\index\\[.+\\]\\{.+?\\}", "", fF)
fF <- gsub("\\\\index\\{.+?\\}", "", fF)
fF <- gsub("\\\\lstinline!.+?!", "", fF)
fF <- gsub("\\\\lstinline\\{.+?\\}", "", fF)
fF <- gsub("\\\\lstinline\\[.+\\]!.+?!", "", fF)
fF <- gsub("\\$.+?\\$", "", fF)
fF <- gsub("\\|see\\{.+\\}", "", fF)
fF <- gsub("|textbf}", "", fF)
fF <- gsub("\\\\hfill", "", fF)
fF <- gsub("\\\\&", "", fF)
fF <- gsub("\\\\noindent", "", fF)
fF <- gsub("\\\\vspace\\{.+?\\}", "", fF)
#fF <- gsub("\\\\vspace\\*\\{.+?\\}", "", fF)

fF <- gsub("z\\.\\\\,B\\.\\\\", "", fF)
fF <- gsub("i\\.\\\\,S\\.\\\\", "", fF)
fF <- gsub("i\\.\\\\,a\\.\\\\", "", fF)
fF <- gsub("s\\.\\\\,o\\.", "", fF)
fF <- gsub("s\\.\\\\,u\\.", "", fF)
fF <- gsub("o\\.\\\\,g\\.\\\\", "", fF)
fF <- gsub("d\\.\\\\,h\\.\\\\", "", fF)
fF <- gsub("u\\.\\\\,U\\.\\\\", "", fF)
fF <- gsub("u\\.\\\\,a\\.\\\\", "", fF)
fF <- gsub("i\\.\\\\,d\\.\\\\,R\\.\\\\", "", fF)
fF <- gsub("(ggf|vgl|bzw|Abschn|inkl|bzgl|etc|Tab|Abb|sog|Kap|ca|s)\\.\\\\", "", fF)
fF <- gsub("\\\\my.+?\\{", "", fF)

writeLines(text=fF, con="spellcheck.txt")

## in SublimeText
## (?s)\\begin\{lstlisting\}.+?\\end\{lstlisting\}

fF <- readLines("spellcheck.txt")

fF <- gsub("\\\\begin\\{.+?\\}", "", fF)
fF <- gsub("\\\\end\\{.+?\\}", "", fF)

fF <- gsub("\\{", " ", fF)
fF <- gsub("\\}", " ", fF)
fF <- gsub("\\(", " ", fF)
fF <- gsub("\\)", " ", fF)
fF <- gsub("\\[", " ", fF)
fF <- gsub("\\]", " ", fF)
fF <- gsub("@", " ", fF)
fF <- gsub("\\|", " ", fF)
fF <- gsub("%", " ", fF)
fF <- gsub("--", " ", fF)

writeLines(text=fF, con="spellcheck.txt")
