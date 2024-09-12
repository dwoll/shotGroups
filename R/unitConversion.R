## conversion of absolute size units
getConvFac <-
function(conversion) {
    ## check how the conversion factor is indicated
    convFac <- if(is.character(conversion)) {
        ## conversion factors for length units
        facMM2MM <-       1
        facMM2CM <-       1/10
        facMM2M  <-       1/1000
        facMM2KM <-       1/1000000
        facMM2IN <-       1/25.4
        facMM2FT <-       1/304.8
        facMM2YD <-       1/914.4
        facCM2MM <-      10
        facCM2CM <-       1
        facCM2M  <-       1/100
        facCM2KM <-       1/100000
        facCM2IN <-       1/2.54
        facCM2FT <-       1/30.48
        facCM2YD <-       1/91.44
        facM2MM  <-    1000
        facM2CM  <-     100
        facM2M   <-       1
        facM2KM  <-       1/1000
        facM2IN  <-       1/0.0254
        facM2FT  <-       1/0.3048
        facM2YD  <-       1/0.9144
        facKM2MM <- 1000000
        facKM2CM <-  100000
        facKM2M  <-    1000
        facKM2KM <-       1/1
        facKM2IN <-       1/0.0000254
        facKM2FT <-       1/0.0003048
        facKM2YD <-       1/0.0009144
        facIN2MM <-      25.4
        facIN2CM <-       2.54
        facIN2M  <-       0.0254
        facIN2KM <-       0.0000254
        facIN2IN <-       1
        facIN2FT <-       1/12
        facIN2YD <-       1/36
        facFT2MM <-     304.8
        facFT2CM <-      30.48
        facFT2M  <-       0.3048
        facFT2KM <-       0.0003048
        facFT2IN <-      12
        facFT2FT <-       1
        facFT2YD <-       1/3
        facYD2MM <-     914.4
        facYD2CM <-      91.44
        facYD2M  <-       0.9144
        facYD2KM <-       0.0009144
        facYD2IN <-      36
        facYD2FT <-       3
        facYD2YD <-       1
        
        v <- c("mm2mm"  =facMM2MM, 
               "mm2cm"  =facMM2CM,
               "mm2m"   =facMM2M,
               "mm2km"  =facMM2KM,
               "mm2in"  =facMM2IN,
               "mm2inch"=facMM2IN,
               "mm2ft"  =facMM2FT,
               "mm2feet"=facMM2FT,
               "mm2foot"=facMM2FT,
               "mm2yd"  =facMM2YD,
               "mm2yard"=facMM2YD,
               
               "cm2mm"  =facCM2MM,
               "cm2cm"  =facCM2CM,
               "cm2m"   =facCM2M,
               "cm2km"  =facCM2KM,
               "cm2in"  =facCM2IN,
               "cm2inch"=facCM2IN,
               "cm2ft"  =facCM2FT,
               "cm2feet"=facCM2FT,
               "cm2foot"=facCM2FT,
               "cm2yd"  =facCM2YD,
               "cm2yard"=facCM2YD,
               
               "m2mm"   =facM2MM,
               "m2cm"   =facM2CM,
               "m2m"    =facM2M,
               "m2km"   =facM2KM,
               "m2in"   =facM2IN,
               "m2inch" =facM2IN,
               "m2ft"   =facM2FT,
               "m2feet" =facM2FT,
               "m2foot" =facM2FT,
               "m2yd"   =facM2YD,
               "m2yard" =facM2YD,
               
               "km2mm"  =facKM2MM,
               "km2cm"  =facKM2CM,
               "km2m"   =facKM2M,
               "km2km"  =facKM2KM,
               "km2in"  =facKM2IN,
               "km2inch"=facKM2IN,
               "km2ft"  =facKM2FT,
               "km2feet"=facKM2FT,
               "km2foot"=facKM2FT,
               "km2yd"  =facKM2YD,
               "km2yard"=facKM2YD,
               
               "in2mm"    =facIN2MM,
               "inch2mm"  =facIN2MM,
               "in2cm"    =facIN2CM,
               "inch2cm"  =facIN2CM,
               "in2m"     =facIN2M,
               "inch2m"   =facIN2M,
               "in2km"    =facIN2KM,
               "inch2km"  =facIN2KM,
               "in2in"    =facIN2IN,
               "in2inch"  =facIN2IN,
               "inch2in"  =facIN2IN,
               "inch2inch"=facIN2IN,
               "in2ft"    =facIN2FT,
               "in2feet"  =facIN2FT,
               "in2foot"  =facIN2FT,
               "inch2ft"  =facIN2FT,
               "inch2feet"=facIN2FT,
               "inch2foot"=facIN2FT,
               "in2yd"    =facIN2YD,
               "inch2yard"=facIN2YD,
               
               "yd2mm"    =facYD2MM,
               "yard2mm"  =facYD2MM,
               "yd2cm"    =facYD2CM,
               "yard2cm"  =facYD2CM,
               "yd2m"     =facYD2M,
               "yard2m"   =facYD2M,
               "yd2km"    =facYD2KM,
               "yard2km"  =facYD2KM,
               "yd2in"    =facYD2IN,
               "yard2inch"=facYD2IN,
               "yd2ft"    =facYD2FT,
               "yd2feet"  =facYD2FT,
               "yd2foot"  =facYD2FT,
               "yard2ft"  =facYD2FT,
               "yard2feet"=facYD2FT,
               "yard2foot"=facYD2FT,
               "yd2yd"    =facYD2YD,
               "yd2yard"  =facYD2YD,
               "yard2yd"  =facYD2YD,
               "yard2yard"=facYD2YD,
               
               "ft2mm"    =facFT2MM,
               "feet2mm"  =facFT2MM,
               "ft2cm"    =facFT2CM,
               "feet2cm"  =facFT2CM,
               "ft2m"     =facFT2M,
               "feet2m"   =facFT2M,
               "ft2km"    =facFT2KM,
               "feet2km"  =facFT2KM,
               "ft2in"    =facFT2IN,
               "ft2inch"  =facFT2IN,
               "feet2in"  =facFT2IN,
               "feet2inch"=facFT2IN,
               "foot2in"  =facFT2IN,
               "foot2inch"=facFT2IN,
               "ft2ft"    =facFT2FT,
               "ft2feet"  =facFT2FT,
               "ft2foot"  =facFT2FT,
               "feet2ft"  =facFT2FT,
               "feet2feet"=facFT2FT,
               "feet2foot"=facFT2FT,
               "foot2ft"  =facFT2FT,
               "foot2feet"=facFT2FT,
               "foot2foot"=facFT2FT,
               "ft2yd"    =facFT2YD,
               "feet2yd"  =facFT2YD,
               "foot2yd"  =facFT2YD,
               "ft2yard"  =facFT2YD,
               "feet2yard"=facFT2YD,
               "foot2yard"=facFT2YD)

        ## did we catch all requested conversion types?
        # if(any(is.na(converted))) {
        #     converted <- v[conversion]
        #     warning(c('Conversion type(s) "', paste(conversion[is.na(converted)], collapse=", "),
        #               '" not found'))
        # }

        unname(v[conversion])
    } else if(is.numeric(conversion)) {     # factor is given directly
        if(any(na.omit(conversion) <= 0)) { # already present NAs are kept
            is.na(conversion) <- conversion <= 0
            warning("Conversion factor must be > 0")
        }

        conversion
    } else {
        stop("conversion must be a character constant or numeric factor")
    }                                # if(is.character(conversion))

    return(convFac)
}

## determine unit of (x,y)-coords from conversion string
getUnits <-
function(x="m2cm", first=TRUE) {
    if(!is.character(x)) {
        warning("Unit not recognized - input must have form like m2cm")
        return(" ")
    }

    units <- strsplit(x, "2")           # first and second part of string
    if(!all(lengths(units) == 2L)) {    # check that there are two parts
        idx <- lengths(units) != 2L
        units[idx] <- NA_character_
        warning(paste0("Unit(s) ", paste(x[idx], collapse=", "),
                       " not recognized - input must have form like m2cm"))
    }

    # knownUnits <- c("km", "m", "cm", "mm", "yd", "yard",
    #                 "ft", "foot", "feet", "in", "inch")
    # isKnown <- vapply(units, function(x) { all(x %in% knownUnits) }, logical(1))
    # if(!all(isKnown)) {
    #     warning(c("Unit not recognized - needs to be one of\n",
    #               paste(knownUnits, collapse=" ")))
    #     return("")
    # }

    ## replace feet with ft, yard with yd, inch with in
    replaceUnit <- function(x) {
        x <- gsub("yard", "yd", x)
        x <- gsub("foot", "ft", x)
        x <- gsub("feet", "ft", x)
        gsub("inch", "in", x)
    }

    units <- lapply(units, replaceUnit)
    if(first) {
        vapply(units, head, FUN.VALUE=character(1), n=1)
    } else {
        vapply(units, tail, FUN.VALUE=character(1), n=1)
    }
}

## determine conversion between distance to target and point coords
determineConversion <- function(x) {
    stopifnot(is.data.frame(x))
    unitDst <- if(hasName(x, "distance.unit")) {
        x[["distance.unit"]]
    } else {
        NA_character_
    }

    unitXY <- if(hasName(x, "point.unit")) {
        x[["point.unit"]]
    } else {
        NA_character_
    }

    conversion <- paste0(unitDst, "2", unitXY)
    is.na(conversion) <- is.na(unitDst) | is.na(unitXY)
    conversion
}
