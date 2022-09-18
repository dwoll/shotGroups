#####---------------------------------------------------------------------------
## check for byte-order-bark
remove_bom <- function(x, encoding=c("UTF-8", "UTF-16LE", "UTF-16BE")) {
    encoding <- match.arg(encoding)
    line1    <- strsplit(x, "")[[1]]
    
    if(encoding == "UTF-8") {
        bom <- rawToChar(as.raw(strtoi(c("EF", "BB", "BF"), base=16L)))
        if(grepl(bom, paste0(line1[1:3], collapse=""))) {
            line1[1:3] <- ""
            x          <- paste0(line1, collapse="")
        }
    } else if(encoding == "UTF-16LE") {
        bom <- rawToChar(as.raw(strtoi(c("FF", "FE"), base=16L)))
        if(grepl(bom, paste0(line1[1:2], collapse=""))) {
            line1[1:2] <- ""
            x          <- paste0(line1, collapse="")
        }
    } else if(encoding == "UTF-16BE") {
        bom <- rawToChar(as.raw(strtoi(c("FE", "FF"), base=16L)))
        if(grepl(bom, paste0(line1[1:2], collapse=""))) {
            line1[1:2] <- ""
            x          <- paste0(line1, collapse="")
        }
    }
    
    x
}

## parse character vector from ShotMarker CSV file
parse_ShotMarkerCSV <- function(x) {
    ## remove byte-order-mark from first line if present
    x[1] <- remove_bom(x[1])
    
    ## session defined by newline + line with non-empty first cell
    ## in some versions, lines with only commas may have replaced empty lines
    idx_empty1     <- !nzchar(x)
    idx_empty2     <- vapply(strsplit(x, ","), function(y) { all(!nzchar(y)) }, logical(1))
    idx_empty      <- idx_empty1 | idx_empty2
    idx_have_first <- nzchar(x) & (substr(x, 1, 1) != ",")
    sStart <- which(idx_empty)[(which(idx_empty) + 1) %in% which(idx_have_first)] + 1
    sLen   <- diff(c(sStart, length(x)+1))                # length of sections
    if((length(sLen) < 1L) || all(sLen < 1L)) {
        stop("No structures found")
    }

    sessionL <- Map(function(start, len) x[start:(start+len-1)], sStart, sLen)

    ## extract file header and header info
    header <- x[seq_len(sStart[1]-1)]                        # header

    ## extract session data
    get_coords <- function(session, group) {
        # browser()
        ## extract session info
        session_info <- read.csv(text=session[1], header=FALSE,
                                 stringsAsFactors=FALSE)
        session_date <- as.Date(strptime(session_info[["V1"]], format="%b %d %Y"))
        session_name <- session_info[["V2"]]
        dist_start   <- regexpr("[[:digit:]]+(mm|m|y|yd|f|ft)", session[1])
        dist_len     <- attr(dist_start, "match.length")
        dist_end     <- dist_start + dist_len - 1
        session_dist_str  <- substring(session[1],
                                       first=dist_start,
                                       last=dist_end)
        distnum_start     <- regexpr("[[:digit:]]+", session_dist_str)
        distnum_len       <- attr(distnum_start, "match.length")
        distnum_end       <- distnum_start + distnum_len - 1
        session_dist      <- as.numeric(substring(session_dist_str,
                                                  first=distnum_start,
                                                  last=distnum_end))
        distunit_start    <- regexpr("(mm|m|y|yd|ft)", session_dist_str)
        distunit_len      <- attr(distunit_start, "match.length")
        distunit_end      <- distunit_start + distunit_len - 1
        dunits            <- c(m="m", mm="mm", y="yd", yd="yd", f="ft", ft="ft")
        session_dist_unit <- unname(dunits[substring(session_dist_str,
                                                     first=distunit_start,
                                                     last=distunit_end)])

        ## extract individual coordinates / velocity
        ## do we have avg/sd lines?
        have_avg_sd <- any(grepl("avg:,", session)) &&
                       any(grepl("SD:,",  session))
        offset_top <- 3 ## session header, newline, coord data
        offset_bot <- if(have_avg_sd) { 4 } else { 2 }
        coord_txt0 <- session[offset_top:(length(session) - offset_bot)]
        ## remove initial ,
        coord_txt1 <- substring(coord_txt0, 2)
        coord_txt2 <- coord_txt1
        ## all rows except header have trailing ','
        coord_txt2[-1] <- substr(coord_txt2[-1], start=1,
                                 stop=nchar(coord_txt2[-1])-1)
        shots <- read.csv(text=coord_txt2,
                          header=TRUE,
                          stringsAsFactors=FALSE)
        
        if(nrow(shots) < 1L) {
            NULL
        } else {
            shots_date_time <- paste0(session_date, " ", shots[["time"]])
            data.frame(group=session_name,
                       date=session_date,
                       date.time=as.POSIXct(strptime(shots_date_time,
                                                     format="%Y-%m-%d %I:%M:%S %p")),
                       distance=session_dist,
                       distance.unit=session_dist_unit,
                       aim.x=0,
                       aim.y=0,
                       point.x=shots[["x..inch."]],
                       point.y=shots[["y..inch."]],
                       point.unit="in",
                       score=shots[["score"]],
                       velocity=shots[["v..fps."]],
                       velocity.unit="ft",
                       stringsAsFactors=FALSE)
        }
    }

    coordL <- Map(get_coords, sessionL, seq_along(sessionL))
    coordL <- Filter(Negate(is.null), coordL)
    coords <- do.call("rbind", coordL)
    return(coords)
}

#####---------------------------------------------------------------------------
## read in ShotMarker backup file
parse_string <- function(s, n) {
    dunits <- c(m="m", mm="mm", y="yd", yd="yd", f="ft", ft="ft")
    dshots <- as.data.frame(s[["shots"]])

    if(nrow(dshots) < 1L) {
        NULL
    } else {
        d <- data.frame(group=s[["name"]],
                        caliber=s[["bullet"]],
                        shot=as.character(dshots[["display_text"]]),
                        date=as.Date(as.POSIXct(dshots[["ts"]] / 1000, origin="1970-01-01")),
                        date.time=   as.POSIXct(dshots[["ts"]] / 1000, origin="1970-01-01"),
                        distance=s[["dist"]],
                        distance.unit=unname(dunits[s[["dist_unit"]]]),
                        aim.x=0,
                        aim.y=0,
                        point.x=dshots[["x"]] + s[["cal_x"]],
                        point.y=dshots[["y"]] + s[["cal_y"]],
                        point.unit="mm",
                        velocity=dshots[["v"]],
                        target=paste0("x_", s[["face_id"]]),
                        stringsAsFactors=FALSE)
        
        d[["score"]] <- if(hasName(dshots, "score")) {
            dshots[["score"]]
        } else { NA_integer_ }
        
        ## some shots are invalid -> attribute hide = TRUE, otherwise NA
        dsub <- if(hasName(dshots, "hide")) {
            d[is.na(dshots[["hide"]]) | !dshots[["hide"]], , drop=FALSE]
        } else {
            d
        }
        
        dsub
    }
}

parse_ShotMarkerBackup <- function(f) {
    ## untar backup file into random temp directory
    tmpf <- paste0("sm_", gsub("^[^0-9](.+)$", "\\1", tempfile(pattern="", tmpdir="")))
    tmpd <- normalizePath(file.path(tempdir(), tmpf), mustWork=FALSE)
    untar(f, exdir=tmpd)
    Sys.chmod(tmpd, mode="0755")
    
    ## read data and archive json structures
    ## data not always included
    ## jdata    <- jsonlite::read_json(file.path(tmpd, "data.txt"))
    jarchive <- jsonlite::read_json(file.path(tmpd, "archive.txt"))
    strings  <- names(jarchive)

    ## assume millisecond timestamp
    string_dates <- unlist(lapply(strings, function(s) {
        as.Date(jarchive[[s]][["ts"]] / (1000*60*60*24) , origin="1970-01-01")
    }))

    string_times <- unlist(lapply(strings, function(s) {
        jarchive[[s]][["time"]]  
    }))
    
    ## zlib files with coordinate data from 1 string/session each
    jstringL <- lapply(strings, function(s) {
        fzlib      <- paste0(tmpd, "/string-", s, ".z")
        charstream <- try(readZlib(fzlib))
        if(inherits(charstream, "try-error")) {
            return(NULL)
        } else {
            return(jsonlite::fromJSON(charstream))
        }
    })

    DFL <- Map(parse_string, jstringL, seq_along(jstringL))
    DFL <- Filter(Negate(is.null), DFL)
    DF  <- do.call("rbind", DFL)
    vunits <- c('fps'="ft", 'ft/s'="ft", 'mps'="m", 'm/s'="m")
    ## velocity unit -> implied: per second
    ## even though jdata::velocity_unit is "fps", it's really m/s
    DF$velocity.unit <- "m" # unname(vunits[jdata$velocity_unit])
    DF
}

## read raw zlib byte stream, decompress and convert to character
readZlib <- function(f) {
    con <- file(f, "rb")
    on.exit(close(con))
    
    fsize <- file.info(f)$size
    frawz <- readBin(con, what="raw", n=fsize)
    fraw  <- memDecompress(frawz, type="gzip")
    rawToChar(fraw)
}
