getXYmat <-
function(DF,
         xyTopLeft=TRUE,
         relPOA=TRUE,
         center=FALSE) {
    if(!is.data.frame(DF)) { stop("DF must be a data frame") }

    ## convert DF names to lower case
    DF <- setNames(DF, tolower(names(DF)))

    ## make sure DF has the required variable names
    ## coordinates need to be called either X, Y order Point.X, Point.Y
    dfNames  <- names(DF)                # what variables are present
    needsXY1 <- c("point.x", "point.y")  # coordinates must have this name
    needsXY2 <- c("x", "y")              # or this
    wantsAIM <- c("aim.x", "aim.y")      # useful
    hasXY1   <- needsXY1 %in% dfNames
    hasXY2   <- needsXY2 %in% dfNames
    hasAIM   <- wantsAIM %in% dfNames    # useful ones we have

    if(!xor(all(hasXY1), all(hasXY2))) {
       stop("Coordinates must be named either x, y or point.x, point.y")
    }

    if(("z" %in% dfNames) && ("point.z" %in% dfNames)) {
        stop("Coordinates must be named either z or point.z")
    }

    ## analysis should be relative to POA, but POA is missing
    if(!all(hasAIM) && relPOA) {
        warning(c("The data frame is missing variable(s)\n",
                  paste(wantsAIM[!hasAIM], collapse=" "), "\n",
                  "Point of Aim is assumed to be in (0,0)"))
        relPOA <- FALSE
    }
    
    ## if names are X, Y, Z rename to Point.X, Point.Y, Point.Z
    if(all(hasXY2)) {
        dfNames <- names(DF)
        dfNames[dfNames %in% "x"] <- "point.x"
        dfNames[dfNames %in% "y"] <- "point.y"
        dfNames[dfNames %in% "z"] <- "point.z"
        warning("Variables x, y were renamed to point.x, point.y")
        names(DF) <- dfNames
    }
    
    ## center shots?
    ## set point of aim to mean -> later used to center to (0,0)
    centerOne <- function(x) {
        x$aim.x <- mean(x$point.x)
        x$aim.y <- mean(x$point.y)
        if(hasName(x, "point.z")) {
            x$aim.z <- mean(x$point.z)
        }
        x
    }
    
    DF <- if(center) {
        if(hasName(DF, "series")) {        # center per group
            DFspl <- split(DF, DF$series, drop=TRUE)
            DFL   <- lapply(DFspl, centerOne)
            do.call("rbind", DFL)
        } else {                         # center overall
            centerOne(DF)
        }
    } else {
        DF
    }
    
    ## coords not relative to POA
    if(!relPOA && !center) {
        DF$aim.x <- 0                    # -> set POA to (0,0)
        DF$aim.y <- 0

        if("point.z" %in% dfNames) {
            DF$aim.z <- 0
        }
    }
    
    ## POA may still be missing (relPOA=TRUE, center=FALSE) -> set to (0,0)
    if(!hasName(DF, "aim.x")  || !hasName(DF, "aim.y")) {
        DF$aim.x <- 0
        DF$aim.y <- 0
    }
    
    ## coords relative to point of aim
    ## y-coords exported from OnTarget: (0,0) is top-left
    x <- DF$point.x - DF$aim.x           # x-coords
    y <- if(xyTopLeft) {
        -(DF$point.y - DF$aim.y)         # y-coords
    } else {
          DF$point.y - DF$aim.y
    }

    z <- if("point.z" %in% dfNames) {
        DF$point.z - DF$aim.z            # z-coords
    } else {
        NULL
    }

    cbind(x, y, z)               # new (x,y)-coords as matrix
}
