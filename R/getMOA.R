## input range checking for angular diameter <-> absolute size conversion
checkSAngDst <-
function(s, ang, dst, type=c("deg", "rad", "MOA", "SMOA", "mrad", "mil")) {
    type <- match.arg(tolower(type),
                      choices=c("deg", "rad", "moa", "smoa", "mrad", "mil"))
    if(!missing(s)) {
        stopifnot(is.numeric(s))
        is.na(s) <- s < 0
    } else {
        s <- NA_real_
    }

    if(!missing(ang)) {
        ## pi/21600
        cnstPi21600    <- 0.00014544410433286079807697423070738439278690599071181
        ## 1 / atan(1/7200)
        cnst1atan17200 <- 7200.0000462962960581466263965930916898334026996030
        stopifnot(is.numeric(ang))
        is.na(ang) <- ang < 0
        is.na(ang) <- switch(type,
             deg=(ang >= 180),
             moa=(ang >= (60*180)),
            smoa=(ang >= (cnstPi21600*cnst1atan17200*60*180)),
             rad=(ang >= pi),
            mrad=(ang >= (1000*pi)),
             mil=(ang >= 3200))
    } else {
        ang <- NA_real_
    }

    if(!missing(dst)) {
        stopifnot(is.numeric(dst))
        is.na(dst) <- dst <= 0
    } else {
        dst <- NA_real_
    }

    list(s=s, ang=ang, dst=dst, type=type)
}

## convert size to angular measure
## angle in degree
## angle in radian
## MOA (= arcmin)
## Shooter's MOA (SMOA = IPHY, inches per hundred yards)
## milliradian mrad (1/1000 of a rad)
## mil -> NATO convention
getMOA <-
function(x, dst, conversion, type=c("deg", "rad", "MOA", "SMOA", "mrad", "mil")) {
    arg  <- checkSAngDst(s=x, dst=dst, type=type)
    x    <- arg$s
    dst  <- arg$dst
    type <- arg$type

    if(missing(dst)) {
        dst <- NA_real_
    }
    
    if(missing(conversion)) {
        conversion <- NA_character_
    }

    ## convert distance measure to the unit of the (x,y)-coordinates
    dstCommon <- getConvFac(conversion) * dst

    ## MOA
    ## calculate angle in rad, convert to deg, convert to MOA
    ## one degree = 1/360 of a circle's arc
    ## one degree = 60 MOA -> 1 MOA = 1/60 degree
    ## rad <- 2*atan(x/(2*dstCommon))  # size as angle in rad
    ## deg <- (360/(2*pi)) * rad       # size as angle in degree
    ## MOA <- 60*deg                   # degree in MOA

    ## mrad
    ## calculate angle in rad, convert to milliradian
    ## 1 rad = 1000 mils -> 1 mrad = 1/1000 rad
    ## rad  <- atan(x/dstCommon)      # size as angle in rad
    ## mrad <- 1000*rad

    ## constants 360/pi, 21600/pi, 1/atan(1/7200), 6400/pi
    cnst360pi      <-  114.59155902616464175359630962821034066481094493313
    cnst21600pi    <- 6875.4935415698785052157785776926204398886566959877
    cnst1atan17200 <- 7200.0000462962960581466263965930916898334026996030
    cnst6400pi     <- 2037.1832715762602978417121711681838340410834654778

    atanArg <- x/(2*dstCommon)
    angle   <- switch(type,
            deg=     cnst360pi*atan(atanArg), # size in degree
            rad=             2*atan(atanArg), # size in radian
            moa=   cnst21600pi*atan(atanArg), # size in arcmin
           smoa=cnst1atan17200*atan(atanArg), # size in SMOA
           mrad=          2000*atan(atanArg), # size in milliradian
            mil=    cnst6400pi*atan(atanArg)) # size in NATO mil

    return(angle)
}

## convert angular measure to size
## MOA (= arcmin)
## Shooter's MOA (SMOA = IPHY, inches per hundred yards)
## milliradian mrad (1/1000 of a rad)
fromMOA <-
function(x, dst, conversion, type=c("deg", "rad", "MOA", "SMOA", "mrad", "mil")) {
    arg  <- checkSAngDst(ang=x, dst=dst, type=type)
    x    <- arg$ang
    dst  <- arg$dst
    type <- arg$type

    if(missing(dst)) {
        dst <- NA_real_
    }
    
    if(missing(conversion)) {
        conversion <- NA_character_
    }
    
    ## convert distance measure to the unit of the (x,y)-coordinates
    dstCommon <- getConvFac(conversion) * dst

    ## MOA
    ## convert MOA to degree, convert to rad, calculate size
    ## one degree = 1/360 of a circle's arc
    ## one degree = 60 MOA -> 1 MOA = 1/60 degree
    ## deg <- MOA / 60                   # MOA as angle in degree
    ## rad <- (2*pi/360) * deg           # deg as angle in rad
    ## x   <- 2*dstCommon * tan(rad/2)   # angle in rad as size

    ## SMOA
    ## get SMOA for 1 MOA = conversion factor for MOA2SMOA
    ## MOA2SMOA <- (21600/pi)*atan(1/7200)
    ## cnstmilI <- pi/6400
    MOA2SMOA    <- 0.95492965241113508367872462693595042591621952511092
    cnstPi360   <- 0.0087266462599716478846184538424430635672143594427086
    cnstPi21600 <- 0.00014544410433286079807697423070738439278690599071181
    cnstPi6400  <- 0.00049087385212340519350978802863742232565580771865236
    
    ## mrad
    ## convert milliradian to rad, calculate size
    ## rad <- (1/1000)*mrad              # milliradian as rad
    ## x   <- 2*dstCommon * tan(rad/2)   # angle in rad as size

#     size <- switch(type,
#            moa =           2*dstCommon*tan(x*pi/21600), # size from arcmin
#            smoa=MOA2SMOA * 2*dstCommon*tan(x*pi/21600), # size from SMOA
#            mrad=           2*dstCommon*tan(0.0005*x),   # size from milliradian
#             mil=           2*dstCommon*tan(x*pi/6400)) # size from NATO mil
    size <- switch(type,
             deg=         2*dstCommon*tan(x*cnstPi360),    # size from degree
             rad=         2*dstCommon*tan(x/2),            # size from radian
             moa=         2*dstCommon*tan(x*cnstPi21600),  # size from arcmin
            smoa=MOA2SMOA*2*dstCommon*tan(x*cnstPi21600),  # size from SMOA
            mrad=         2*dstCommon*tan(x/2000),         # size from milliradian
             mil=         2*dstCommon*tan(x*cnstPi6400))   # size from NATO mil

    return(size)
}

#####---------------------------------------------------------------------------
## get distance from absolute and angular size
getDistance <-
function(x, angular, conversion,
         type=c("deg", "rad", "MOA", "SMOA", "mrad", "mil")) {
    arg     <- checkSAngDst(s=x, ang=angular, type=type)
    x       <- arg$s
    angular <- arg$ang
    type    <- arg$type

    if(missing(conversion)) {
        conversion <- NA_character_
    }
    
    ## SMOA2MOA <- (21600/pi)*atan(1/7200)
    SMOA2MOA      <- 0.95492965241113508367872462693595042591621952511092
    ## pi/360
    cnstPi360     <- 0.0087266462599716478846184538424430635672143594427086
    ## pi/(2*60*180)
    cnstPi21600   <- 0.00014544410433286079807697423070738439278690599071181
    ## atan(1/7200)
    cnstAtan7200  <- 0.00013888888799582762807755515064054772884717306648502
    ## pi/6400
    cnstPi6400    <- 0.00049087385212340519350978802863742232565580771865236

    ## distance in unit of (x,y)-coordinates
    dstCommon <- switch(type,
           deg=(x/2)*(1/tan(angular*cnstPi360)),
           rad=(x/2)*(1/tan(angular/2)),
           moa=(x/2)*(1/tan(angular*cnstPi21600)),
          smoa=(x/2)*(1/tan(angular*cnstAtan7200)),
          mrad=(x/2)*(1/tan(angular/2000)),
           mil=(x/2)*(1/tan(angular*cnstPi6400)))
    
    ## convert distance measure from the unit of the (x,y)-coordinates
    dst <- (1/getConvFac(conversion)) * dstCommon
    return(dst)
}

#####---------------------------------------------------------------------------
## get all angular measures for sizes x
makeMOA <-
function(x, dst=NA_real_, conversion=NA_character_) {
    if(length(x) < 2L) {
        ## only 1 value -> return vector
        v0 <- c(x,
                getMOA(x, dst=dst, conversion=conversion, type="MOA"),
                getMOA(x, dst=dst, conversion=conversion, type="SMOA"),
                getMOA(x, dst=dst, conversion=conversion, type="mrad"))
        
        ## remove NA entries if angular size cannot be determined
        v1 <- na.omit(setNames(v0, c("unit", "MOA", "SMOA", "mrad")))
        attr(v1, "na.action") <- NULL
        v1
    } else {
        ## multiple values -> return matrix
        m0 <- rbind(unit=x,
                     MOA=getMOA(x, dst=dst, conversion=conversion, type="MOA"),
                    SMOA=getMOA(x, dst=dst, conversion=conversion, type="SMOA"),
                    mrad=getMOA(x, dst=dst, conversion=conversion, type="mrad"))
        
        ## remove rows that are completely NA
        idx <- apply(m0, 1, function(r) { all(is.na(r)) })
        m0[!idx, , drop=FALSE]
    }
}
