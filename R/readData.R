## read files that are comma or whitespace-delimited
## variable names must not have spaces
readDataMisc <-
function(fPath=".", fNames, fPat, combine=TRUE, dstTarget, conversion) {
    files <- getFileNames(fPath, fNames, fPat)
    if(length(files) < 1L) { stop("No files were selected") }

    missing_dstTarget  <- missing(dstTarget)
    missing_conversion <- missing(conversion)

    ## determine whether file is csv or whitespace delimited
    isCSV <- function(ext, nFields) {
        csv <- if(tolower(ext) == "csv") {        # explicit file extension
            TRUE
        } else if((length(unique(nFields)) == 1L) && (nFields[1] >= 2L)) {
            ## same number of comma-separated fields in all rows
            ## and at least two comma-separated fields
            TRUE
        } else {
            FALSE
        }
    }

    ## read a single file, possibly csv, possibly whitespace delimited
    readMe <- function(f) {
        ## pieces  <- strsplit(f, "\\.")[[1]]
        ## ext     <- tolower(pieces[length(pieces)]) # file extensions
        ext     <- tools::file_ext(f)
        nFields <- count.fields(f, sep=",")

        ## choose appropriate function to read in file
        readFun <- if(isCSV(ext, nFields)) {
            read.csv
        } else {
            read.table
        }

        DF <- readFun(f, header=TRUE, stringsAsFactors=FALSE)
        DF <- setNames(DF, tolower(names(DF))) # convert var names to lower case
        DF[["file"]] <- basename(tools::file_path_sans_ext(f))  # add filename

        ## add distance if provided
        if(!missing_dstTarget) {
            DF[["distance"]] <- dstTarget
        }

        ## infer (x,y)-coord units from conversion
        if(!missing_conversion) {
            DF[["distance.unit"]] <- getUnits(conversion, first=TRUE)  # unit for distance
            DF[["point.unit"]]    <- getUnits(conversion, first=FALSE) # unit for xy-coords
        }

        DF
    }

    ## read multiple files, some possibly csv, some possibly whitespace delimited
    DFs <- lapply(files, readMe)
    names(DFs) <- paste0("file", seq_along(DFs))  # name them

    ## build shared set of variable names
    varNameL <- lapply(DFs, names)           # list of variable names
    varNames <- Reduce(intersect, varNameL)  # intersection of all var names

    ## make sure that the data frames all have the correct variables
    ## remove dots from variable names
    wants   <- c("distance", "group", "aimx", "aimy")  # useful
    vnNoDot <- vapply(strsplit(varNames, "\\."),
                      function(y) { paste0(y, collapse="") },
                      character(1))
    has     <- wants %in% vnNoDot

    if(!all(has)) {
        warning(c("At least one file is missing variable(s)\n",
            paste(wants[!has], collapse=" "),
            "\nthat may be required later by analysis functions"))
    }

    ## make sure each data frame has either x, y or point.x, point.y
    replaceXY <- function(x) {
        dfNames  <- names(x)
        needsXY1 <- c("point.x", "point.y")  # coordinates must have this name
        needsXY2 <- c("x", "y")              # or this
        needsXY3 <- c("shotx", "shoty")      # or this (Taran)
        hasXY1   <- needsXY1 %in% dfNames
        hasXY2   <- needsXY2 %in% dfNames
        hasXY3   <- needsXY3 %in% dfNames

        ## exactly one of x, y - point.x, point.y - ShotX, ShotY
        if(sum(c(all(hasXY1), all(hasXY2), all(hasXY3))) != 1) {
            stop("Coordinates must be named x, y - point.x, point.y - or ShotX, ShotY")
        }

        if(sum(c("z" %in% dfNames, "point.z" %in% dfNames, "shotz" %in% dfNames)) > 1L) {
            stop("Coordinates must be named z, point.z, or ShotZ")
        }

        ## if X, Y, Z or ShotX, ShotY, ShotZ -> rename to Point.X, Point.Y, Point.Z
        if(all(hasXY2) | all(hasXY3)) {
            dfNames[dfNames %in% c("x", "shotx")] <- "point.x"
            dfNames[dfNames %in% c("y", "shoty")] <- "point.y"
            dfNames[dfNames %in% c("z", "shotz")] <- "point.z"
            warning("Variables (Shot)X, (Shot)Y were renamed to point.x, point.y")
            names(x) <- dfNames
        }

        ## if AimX, AimY, AimZ -> rename to Aim.X, Aim.Y, Aim.Z
        if(all(c("aimx", "aimy") %in% dfNames)) {
            dfNames[dfNames %in% "aimx"] <- "aim.x"
            dfNames[dfNames %in% "aimy"] <- "aim.y"
            dfNames[dfNames %in% "aimz"] <- "aim.z"
            warning("Variables AimX, AimY were renamed to aim.x, aim.y")
            names(x) <- dfNames
        }

        x
    }

    DFs <- lapply(DFs, replaceXY)

    if(combine) {
        return(combineData(DFs))
    } else {
        return(DFs)
    }
}

## read files from OnTarget-output (version 1.*)
readDataOT1 <-
function(fPath=".", fNames, fPat, combine=TRUE, dstTarget, conversion) {
    files <- getFileNames(fPath, fNames, fPat)
    if(length(files) < 1L) { stop("No files were selected") }

    missing_dstTarget  <- missing(dstTarget)
    missing_conversion <- missing(conversion)
    
    ## assumed variables: Project Title, Group, Ammunition, Distance,
    ## Aim X, Aim Y, Center X, Center Y, Point X, Point Y
    ## 10 fields + trailing tab = 11
    nFields <- unlist(lapply(files, function(x) { count.fields(x, sep="\t") } ))
    if(!all(nFields == 11L)) {
        stop(c("It appears at least one file does not contain exactly\n",
               "the required set of 10 variables - see help(readDataOT1)\n",
               "maybe you should use readDataMisc() instead"))
    }

    ## read in files into a list of data frames
    readMe <- function(f) {
        DF <- read.delim(f,
                         colClasses=c("character", "factor", "character",
                                      "numeric", "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "numeric", "NULL"),
                         strip.white=TRUE,
                         stringsAsFactors=FALSE)
        DF <- setNames(DF, tolower(names(DF)))  # convert var names to lower case
        DF[["file"]] <- basename(tools::file_path_sans_ext(f))  # add filename
        
        ## add distance if provided
        if(!missing_dstTarget) {
            DF[["distance"]] <- dstTarget
        }
        
        ## infer (x,y)-coord units from conversion
        if(!missing_conversion) {
            DF[["distance.unit"]] <- getUnits(conversion, first=TRUE)  # unit for distance
            DF[["point.unit"]]    <- getUnits(conversion, first=FALSE) # unit for xy-coords
        }
        
        DF
    }

    DFs <- lapply(files, readMe)

    names(DFs) <- paste("file", seq_along(DFs), sep="")  # name them

    ##  build shared set of variable names
    varNames <- Reduce(intersect, lapply(DFs, names))

    ## make sure that the data frames all have the correct variables
    wants <- c("group", "distance", "aim.x", "aim.y", "point.x", "point.y")
    has   <- wants %in% varNames
    if(!all(has)) {
        warning(c("At least one file is missing variable(s)\n",
                  paste(wants[!has], collapse= " "),
                  "\nthat may be required later by analysis functions"))
    }

    if(combine) {
        return(combineData(DFs))
    } else {
        return(DFs)
    }
}

## read files from OnTarget-output version 2.*, 3.7*, 3.8*
readDataOT2 <-
function(fPath=".", fNames, fPat, combine=TRUE, dstTarget, conversion) {
    files <- getFileNames(fPath, fNames, fPat)
    if(length(files) < 1L) { stop("No files were selected") }

    missing_dstTarget  <- missing(dstTarget)
    missing_conversion <- missing(conversion)
    
    ## assumed variables: Project Title, Group, Ammunition, Distance,
    ## Aim X, Aim Y, Center X, Center Y, Point X, Point Y, Velocity (optional)
    ## 10 or 11 fields
    nFields <- unlist(lapply(files, function(x) count.fields(x, sep=",")))
    colClasses <- if(all(nFields == 10L)) {
        c("character", "factor", "character", "numeric", "numeric",
          "numeric", "numeric", "numeric", "numeric", "numeric")
    } else if(all(nFields == 11L)) {
        c("character", "factor", "character", "numeric", "numeric",
          "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")
    } else {
        stop(c("It appears at least one file does not contain exactly\n",
               "the required set of variables - see help(readDataOT2)\n",
               "maybe you should use readDataMisc() instead"))
    }

    ## read in files into a list of data frames
    readMe <- function(f) {
        DF <- read.csv(f, colClasses=colClasses, strip.white=TRUE,
                       stringsAsFactors=FALSE)
        DF <- setNames(DF, tolower(names(DF))) # convert var names to lower case
        
        DF[["file"]] <- basename(tools::file_path_sans_ext(f))  # add filename
        
        ## add distance if provided
        if(!missing_dstTarget) {
            DF[["distance"]] <- dstTarget
        }
        
        ## infer (x,y)-coord units from conversion
        if(!missing_conversion) {
            DF[["distance.unit"]] <- getUnits(conversion, first=TRUE)  # unit for distance
            DF[["point.unit"]]    <- getUnits(conversion, first=FALSE) # unit for xy-coords
        }
        
        DF
    }
    
    DFs <- lapply(files, readMe)

    names(DFs) <- paste("file", seq_along(DFs), sep="")  # name them

    ##  build shared set of variable names
    varNames <- Reduce(intersect, lapply(DFs, names))

    ## make sure that the data frames all have the correct variables
    wants <- c("group", "distance", "aim.x", "aim.y", "point.x", "point.y")
    has   <- wants %in% varNames
    if(!all(has)) {
        warning(c("At least one file is missing variable(s)\n",
                  paste(wants[!has], collapse= " "),
                  "\nthat may be required later by analysis functions"))
    }

    if(combine) {
        return(combineData(DFs))
    } else {
        return(DFs)
    }
}

## read Silver Mountain e-target files that are comma delimited
readDataSMT <-
function(fPath=".", fNames, fPat, combine=TRUE) {
    files <- getFileNames(fPath, fNames, fPat)
    if(length(files) < 1L) { stop("No files were selected") }

    ## set locale to US for date parsing, and re-set on exit
    lc_current <- Sys.getlocale("LC_TIME")
    Sys.setlocale(category="LC_TIME", locale="C")
    on.exit(Sys.setlocale(category="LC_TIME", locale=lc_current))

    ## assumed variables: string, shooter, frame, distance, date, score,
    ## moa_x, moa_y, scope_x, scope_y, adj_x, adj_y, v, adj_y_avg, 
    ## adj_y_sd, v_avg, v_sd
    colClasses <- c("character", "character", "character", "numeric", "character",
                    "character", "numeric", "numeric", "numeric", "numeric", "numeric",
                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")
    
    varNames <- c("project.title", "shooter", "frame", "distance",
                  "date", "score", "point.x", "point.y", "aim.x", "aim.y",
                  "adj.x", "adj.y", "velocity", "adj.y.avg", "adj.y.sd",
                  "velocity.avg", "velocity.sd")

    ## read in files into a list of data frames
    readMe <- function(f) {
        ## there are different versions of SMT files
        n_fields <- unique(count.fields(f, sep=","))

        if(n_fields != length(colClasses)) {
            if(n_fields == (length(colClasses) + 2L)) {
                ## probably two additional variables
                colClasses <- c("numeric", colClasses, "character")
                varNames   <- c("number", varNames, "blank")
            } else {
                ## don't know how to deal with this
                return(NULL)
            }
        }

        DF        <- read.csv(f, colClasses=colClasses, strip.white=TRUE)
        names(DF) <- varNames
        DF$file   <- basename(tools::file_path_sans_ext(f))  # add filename
        DF$group  <- 1
        
        ## fill missing rows - distance is in m
        DF[["project.title"]] <- DF[["project.title"]][1]
        DF[["shooter"]]       <- DF[["shooter"]][1]
        DF[["frame"]]         <- NULL
        DF[["distance"]]      <- DF[["distance"]][1]*getConvFac("m2yd")
        DF[["distance.unit"]] <- "yd"
        DF[["point.unit"]]    <- "in"
        DF[["adj.x"]]         <- NULL
        DF[["adj.y"]]         <- NULL
        DF[["adj.y.avg"]]     <- NULL
        DF[["adj.y.sd"]]      <- NULL
        DF[["velocity.avg"]]  <- NULL
        DF[["velocity.sd"]]   <- NULL
        if(hasName(DF, "blank"))  { DF[["blank"]]  <- NULL }
        if(hasName(DF, "number")) { DF[["number"]] <- NULL }
        
        ## remove last line with sd/averages
        DF <- DF[-nrow(DF), , drop=FALSE]
        
        ## convert date to date object
        DF[["date.time"]] <- strptime(DF[["date"]], "%a %b %d %Y %H:%M:%S")
        DF[["date"]]      <- as.Date(strptime(DF[["date"]], "%a %b %d %Y %H:%M:%S"))
        
        ## convert coords from MOA to inches - may be negative
        DF[["point.x"]] <- fromMOA(abs(DF[["point.x"]]), dst=DF[["distance"]],
                                   conversion="yd2in", type="MOA") *
            sign(DF[["point.x"]])
        DF[["point.y"]] <- fromMOA(abs(DF[["point.y"]]), dst=DF[["distance"]],
                                   conversion="yd2in", type="MOA") *
            sign(DF[["point.y"]])
        DF[["aim.x"]]   <- fromMOA(abs(DF[["aim.x"]]),   dst=DF[["distance"]],
                                   conversion="yd2in", type="MOA") *
            sign(DF[["aim.x"]])
        DF[["aim.y"]]   <- fromMOA(abs(DF[["aim.y"]]),   dst=DF[["distance"]],
                                   conversion="yd2in", type="MOA") *
            sign(DF[["aim.y"]])
        DF
    }
    
    DFs <- lapply(files, readMe)

    names(DFs) <- paste("file", seq_along(DFs), sep="")  # name them
    
    ##  build shared set of variable names
    varNames <- Reduce(intersect, lapply(DFs, names))
    
    ## make sure that the data frames all have the correct variables
    wants <- c("group", "distance", "aim.x", "aim.y", "point.x", "point.y")
    has   <- wants %in% varNames
    if(!all(has)) {
        warning(c("At least one file is missing variable(s)\n",
                  paste(wants[!has], collapse= " "),
                  "\nthat may be required later by analysis functions"))
    }
    
    if(combine) {
        return(combineData(DFs))
    } else {
        return(DFs)
    }
}

## read ShotMarker e-target files
readDataShotMarker <-
function(fPath=".", fNames, fPat, combine=TRUE) {
    files <- getFileNames(fPath, fNames, fPat)
    if(length(files) < 1L) { stop("No files were selected") }
        
    ## set locale to US for date parsing, and re-set on exit
    lc_current <- Sys.getlocale("LC_TIME")
    Sys.setlocale(category="LC_TIME", locale="C")
    on.exit(Sys.setlocale(category="LC_TIME", locale=lc_current))

    ## read in files into a list of data frames
    readMe <- function(f) {
        ext <- tools::file_ext(f)  # file extension
        DF <- if(tolower(ext) == "csv") {
            parse_ShotMarkerCSV(readLines(f))
        } else if((tolower(ext) == "tar") || (tolower(ext) == "gz")) {
            if(requireNamespace("jsonlite", quietly=TRUE)) {
                parse_ShotMarkerBackup(f)
            } else {
                stop("Could not find package 'jsonlite' - please install first")
            }
        } else {
            stop("Unknown file type")
        }
        
        DF$file <- basename(tools::file_path_sans_ext(f))  # add filename
        
        ## convert distance to yard
        DF[["distance"]] <- DF[["distance"]]*getConvFac(paste0(DF[["distance.unit"]], "2yd"))
        DF[["distance.unit"]] <- "yd"
        
        ## convert velocity to fps
        DF[["velocity"]] <- DF[["velocity"]]*getConvFac(paste0(DF[["velocity.unit"]], "2ft"))
        DF[["velocity.unit"]] <- "ft"
        
        ## convert coords to inch
        DF[["point.x"]] <- DF[["point.x"]]*getConvFac(paste0(DF[["point.unit"]], "2in"))
        DF[["point.y"]] <- DF[["point.y"]]*getConvFac(paste0(DF[["point.unit"]], "2in"))
        DF[["point.unit"]] <- "in"
        
        ## coords may be missing
        DF[!is.na(DF[["point.x"]]) & !is.na(DF[["point.y"]]), , drop=FALSE]
    }

    DFs <- lapply(files, readMe)
        
    names(DFs) <- paste("file", seq_along(DFs), sep="")  # name them
        
    ##  build shared set of variable names
    varNames <- Reduce(intersect, lapply(DFs, names))
        
    ## make sure that the data frames all have the correct variables
    wants <- c("group", "distance", "aim.x", "aim.y", "point.x", "point.y")
    has   <- wants %in% varNames
    if(!all(has)) {
        warning(c("At least one file is missing variable(s)\n",
                  paste(wants[!has], collapse= " "),
                  "\nthat may be required later by analysis functions"))
    }
        
    if(combine) {
        return(combineData(DFs))
    } else {
        return(DFs)
    }
}
