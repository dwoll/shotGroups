#####-----------------------------------------------------------------------
## draw a target
drawTarget <-
function(x, unit, dstTarget, conversion, add=FALSE, cex=par("cex")) {
    if(missing(unit)) {
        unit <- NA_real_
    }

    if(missing(dstTarget)) {
        dstTarget <- NA_real_
    }
    
    if(missing(conversion)) {
        conversion <- NA_character_
    }

    ## get chosen target definition in desired unit
    trgt <- getTarget(x, unit=unit, dstTarget=dstTarget, conversion=conversion)
    if(hasName(trgt, "failed")) {
        warning("Could not calculate ring radii in desired unit")
        return(invisible(NULL))
    }

    ## open plot or add to existing plot?
    if(!add) {
        plot(0, 0, type="n", xlim=trgt[["xLims"]], ylim=trgt[["yLims"]],
             xlab=NA, ylab=NA, asp=1)
    }                                    # if(add)

    ## do we need a special drawing function?
    fun <- if(hasName(trgt, "draw_fun")) {
        eval(trgt[["draw_fun"]])
    } else {
        drawTarget_default
    }

    fun(trgt, cex=cex)
    return(invisible(trgt))
}

## draw a target
drawTarget_default <-
function(x, cex=par("cex")) {
    ## draw circles first, start from the outside
    with(x$inUnit, symbols(rep(0, length(ringR)), rep(0, length(ringR)), add=TRUE,
                           circles=rev(ringR), bg=rev(x$cols), fg=rev(x$colsTxt),
                           inches=FALSE))
    # abline(v=0, h=0, col="lightgray")    # add point of aim

    ## add ring numbers except for bullseye (ring number maxVal)
    ## bullseye has inner sub-division -> start numbers on ring 3
    rings1 <- with(x, seq(from=maxVal-nRings+1, to=maxVal-1, length.out=nRings-1)) # left side of center
    rings2 <- c(rings1, rev(rings1))                         # both sides
    pos1   <- with(x$inUnit, ringR[3:length(ringR)] - (ringW/2)) # right side of center
    pos2   <- c(-rev(pos1), pos1)                            # both sides
    cols1  <- with(x$inUnit, x$colsTxt[3:length(ringR)])     # right side of center
    cols2  <- c(rev(cols1), cols1)                           # both sides

    text(pos2, 0, cex=cex, label=rings2, col=cols2)
    text(0, pos2, cex=cex, label=rings2, col=cols2)
}

#####-----------------------------------------------------------------------
## draw extra targets
drawTarget_x <- function(x, cex=par("cex")) {
    ## color definitions
    ## background (fill)
    colors_poly_BG     <- c(b ="#999999", g ="#cccccc", w ="#ffffff",
                            bl="#ffffff", gl="#cccccc", wl="#ffffff")
    colors_poly_top_BG <- c(b ="#999999", g ="#cccccc", w ="#ffffff",
                            bl="#ffffff", gl="#cccccc", wl="#ffffff00")
    colors_rings_BG    <- c(b ="#999999",               w ="#ffffff",
                            bl="#ffffff00", gl="#ffffff00")
    colors_txt         <- c(bl="#999999", gl="#cccccc", wl="#ffffff")
    ## foreground (border)
    colors_poly_FG     <- c(b ="#999999", g ="#999999", w ="#999999",
                            bl="#999999", gl="#cccccc", wl="#ffffff")
    colors_poly_top_FG <- c(b ="#999999", g ="#999999", w ="#999999",
                            bl="#999999", gl="#cccccc", wl="#ffffff")
    colors_rings_FG    <- c(b ="#cccccc",               w ="#999999",
                            bl="#999999", gl="#cccccc")
    ## all R colors
    colorsAll <- colors()

    ## do the actual drawing
    ## start with empty plot, add board
    # plot(0, 0, type="n", xlim=x[["xLims"]], ylim=x[["yLims"]], xlab=NA, ylab=NA, asp=1)
    rect(x[["xLims"]][1], x[["yLims"]][1],
         x[["xLims"]][2], x[["yLims"]][2], col=colorsAll[396], border=NA)

    # ## rings without fill
    # if(hasName(face, "rings")) {
    #     symbols(rev(rings[["x"]]), rev(rings[["y"]]), add=TRUE,
    #             circles=rev(rings[["diam"]]/2),
    #             bg=NULL,
    #             fg=colors_rings_FG[rev(rings[["color"]])],
    #             inches=FALSE)
    # }

    ## polygon
    if(hasName(x, "polyL")) {
        drawPoly <- function(p) {
            polygon(p[["points"]][["x"]], p[["points"]][["y"]],
                    col   =colors_poly_BG[p[["color"]]],
                    border=colors_poly_FG[p[["color"]]])
        }
        lapply(rev(x[["polyL"]]), drawPoly)
    }

    ## rings
    if(hasName(x, "rings")) {
        rings <- x[["rings"]]
        symbols(rev(rings[["x"]]), rev(rings[["y"]]), add=TRUE,
                circles=rev(rings[["diam"]]/2),
                bg=colors_rings_BG[rev(rings[["color"]])],
                fg=colors_rings_FG[rev(rings[["color"]])],
                inches=FALSE)
    }
    
    ## polygon without fill
    if(hasName(x, "polyL")) {
        drawPoly <- function(p) {
            polygon(p[["points"]][["x"]], p[["points"]][["y"]],
                    col   =NULL,
                    border=colors_poly_FG[p[["color"]]])
        }
        lapply(rev(x[["polyL"]]), drawPoly)
    }
    
    ## polygon_top
    if(hasName(x, "poly_topL")) {
        drawPoly <- function(p) {
            polygon(p[["points"]][["x"]], p[["points"]][["y"]],
                    col   =colors_poly_top_BG[p[["color"]]],
                    border=colors_poly_top_FG[p[["color"]]])
        }
        lapply(rev(x[["poly_topL"]]), drawPoly)
    }

    ## rings_top
    if(hasName(x, "rings_top")) {
        rings_top <- x[["rings_top"]]
        symbols(rev(rings_top[["x"]]), rev(rings_top[["y"]]), add=TRUE,
                circles=rev(rings_top[["diam"]]/2),
                bg=colors_rings_BG[rev(rings_top[["color"]])],
                fg=colors_rings_FG[rev(rings_top[["color"]])],
                inches=FALSE)
    }

    # ## finally rings_top without fill
    # if(hasName(x, "rings_top")) {
    #     symbols(rev(rings_top[["x"]]), rev(rings_top[["y"]]), add=TRUE,
    #             circles=rev(rings_top[["diam"]]/2),
    #             bg=NULL,
    #             fg=colors_rings_FG[rev(rings_top[["color"]])],
    #             inches=FALSE)
    # }

    ## text last
    if(hasName(x, "txt")) {
        txt <- x[["txt"]]
        if(hasName(txt, "angle")) {
            txtSpl <- split(txt, txt[["angle"]])
            lapply(txtSpl, function(y) {
                text(y[["x"]], y[["y"]],
                     labels=y[["text"]],
                     srt=-y[["angle"]][1],
                     adj=c(0.5, 0.5),
                     cex=cex,
                     col=colors_txt[y[["color"]]])
            })
        } else {
            text(txt[["x"]], txt[["y"]],
                 labels=txt[["text"]],
                 adj=c(0.5, 0.5),
                 cex=cex,
                 col=colors_txt[txt[["color"]]])
        }
    }
    
    return(invisible(NULL))
}

## f <- readRDS("../target_defs/targets_add.rda")
## idx_poly      <- which(vapply(f, function(x) { hasName(x, "polyL")     }, logical(1)))
## idx_poly_top  <- which(vapply(f, function(x) { hasName(x, "poly_topL") }, logical(1)))
## idx_rings_top <- which(vapply(f, function(x) { hasName(x, "rings_top") }, logical(1)))

## incorrect
## LoadDev
## NOR500UNL

## shotGroups:::drawTarget("x_LoadDev", unit="cm")
## shotGroups:::drawTarget("x_NOR500UNL", unit="cm")
## shotGroups:::drawTarget("x_IBS100BR", unit="cm")

#####-----------------------------------------------------------------------
## ISSF special targets
drawTarget_ISSF25mRF <-
function(x, cex=par("cex")) {
    ## draw circles first, start from the outside
    with(x$inUnit, symbols(rep(0, length(ringR)), rep(0, length(ringR)), add=TRUE,
                           circles=rev(ringR), bg=rev(x$cols), fg=rev(x$colsTxt),
                           inches=FALSE))
    with(x$inUnit, rect(-ringR[length(ringR)],  -extra/2,
                        -ringR[4]+(0.15*ringW),  extra/2, col="white", border=NA))
    with(x$inUnit, rect( ringR[4]-(0.15*ringW), -extra/2,
                         ringR[length(ringR)],   extra/2, col="white", border=NA))
    # abline(v=0, h=0, col="lightgray")    # add point of aim

    ## add ring numbers except for bullseye (ring number maxVal)
    ## bullseye has inner sub-division -> start numbers on ring 3
    rings1 <- with(x, seq(from=maxVal-nRings+1, to=maxVal-1, length.out=nRings-1)) # left side of center
    rings2 <- c(rings1, rev(rings1))                         # both sides
    pos1   <- with(x$inUnit, ringR[3:length(ringR)] - (ringW/2)) # right side of center
    pos2   <- c(-rev(pos1), pos1)                            # both sides
    cols1  <- with(x$inUnit, x$colsTxt[3:length(ringR)])     # right side of center
    cols2  <- c(rev(cols1), cols1)                           # both sides

    text(0, pos2, cex=cex, label=rings2, col=cols2)
}

drawTarget_ISSF50m <-
function(x, cex=par("cex")) {
    ## draw circles first, start from the outside
    with(x$inUnit, symbols(rep(0, length(ringR)), rep(0, length(ringR)), add=TRUE,
                           circles=rev(ringR), bg=rev(x$cols), fg=rev(x$colsTxt),
                           inches=FALSE))
    with(x$inUnit, symbols(0, 0, add=TRUE, circles=extra, bg=x$cols[1], fg=NA,
                           inches=FALSE))
    with(x$inUnit, symbols(rep(0, length(ringR)-3), rep(0, length(ringR)-3), add=TRUE,
                           circles=rev(ringR)[-(1:3)], bg=rev(x$cols)[-(1:3)],
                           fg=rev(x$colsTxt)[-(1:3)], inches=FALSE))

    # abline(v=0, h=0, col="lightgray")    # add point of aim

    ## add ring numbers 1-8
    ## bullseye has inner sub-division -> start numbers on ring 4
    rings1 <- with(x, seq(from=maxVal-nRings+1, to=maxVal-2, length.out=nRings-2)) # left side of center
    rings2 <- c(rings1, rev(rings1))                         # both sides
    pos1   <- with(x$inUnit, ringR[4:length(ringR)] - (ringW/2)) # right side of center
    pos2   <- c(-rev(pos1), pos1)                            # both sides
    cols1  <- with(x$inUnit, x$colsTxt[4:length(ringR)])     # right side of center
    cols2  <- c(rev(cols1), cols1)                           # both sides

    text(pos2, 0, cex=cex, label=rings2, col=cols2)
    text(0, pos2, cex=cex, label=rings2, col=cols2)
}

## draw ISSF 300m target
drawTarget_ISSF300m <-
function(x, cex=par("cex")) {
    ## draw circles first, start from the outside
    with(x$inUnit, symbols(rep(0, length(ringR)), rep(0, length(ringR)), add=TRUE,
                           circles=rev(ringR), bg=rev(x$cols), fg=rev(x$colsTxt),
                           inches=FALSE))
    # abline(v=0, h=0, col="lightgray")    # add point of aim

    ## add ring numbers except for bullseye (ring number maxVal)
    ## bullseye has inner sub-division -> start numbers on ring 3
    rings <- with(x, seq(from=maxVal-nRings+1, to=maxVal-1, length.out=nRings-1)) # left side of center
    cols  <- with(x$inUnit, x$colsTxt[length(ringR):3])          # right side of center
    pos   <- with(x$inUnit, -ringR[length(ringR):3] + (ringW/2)) # right side of center
    ang   <- with(x, txtRot)
    angX  <- with(x, txtRot*pi/180)
    posLU <- cbind(pos, 0) %*% with(x, cbind(c(cos(  angX), sin(  angX)), c(-sin(  angX), cos(  angX))))
    posRU <- cbind(pos, 0) %*% with(x, cbind(c(cos(3*angX), sin(3*angX)), c(-sin(3*angX), cos(3*angX))))
    posRL <- cbind(pos, 0) %*% with(x, cbind(c(cos(5*angX), sin(5*angX)), c(-sin(5*angX), cos(5*angX))))
    posLL <- cbind(pos, 0) %*% with(x, cbind(c(cos(7*angX), sin(7*angX)), c(-sin(7*angX), cos(7*angX))))

    text(posLU[ , 1], posLU[ , 2], cex=cex, label=rings, col=cols, srt=  ang)
    text(posRU[ , 1], posRU[ , 2], cex=cex, label=rings, col=cols, srt=7*ang)
    text(posRL[ , 1], posRL[ , 2], cex=cex, label=rings, col=cols, srt=5*ang)
    text(posLL[ , 1], posLL[ , 2], cex=cex, label=rings, col=cols, srt=3*ang)
}

#####-----------------------------------------------------------------------
## NRA HPR targets
drawTarget_NRA_HPR <-
function(x, cex=par("cex")) {
    ## draw circles first, start from the outside
    with(x$inUnit, symbols(rep(0, length(ringR)), rep(0, length(ringR)), add=TRUE,
                           circles=rev(ringR), bg=rev(x$cols), fg=rev(x$colsTxt),
                           inches=FALSE))
    # abline(v=0, h=0, col="lightgray")    # add point of aim

    ## add ring numbers
    ## bullseye has inner sub-division -> start numbers on ring 2
    rings1 <- with(x, seq(from=maxVal-nRings+1, to=maxVal, length.out=nRings)) # left side of center
    outRng <- 1:(length(rings1)-2)
    outPos <- 3:length(rings1)
    rings2 <- c(rings1[outRng], rev(rings1[outRng]))         # both sides
    pos1   <- with(x$inUnit, ringR[2:length(ringR)] - (ringW/2)) # right side of center
    pos2   <- c(-rev(pos1[outPos]), pos1[outPos])            # both sides
    cols1  <- with(x$inUnit, x$colsTxt[2:length(ringR)])     # right side of center
    cols2  <- c(rev(cols1[outPos]), cols1[outPos])           # both sides

    text(pos2, 0,      cex=cex, label=rings2,           col=cols2)
    text(0, pos1[1:2], cex=cex, label=rev(rings1)[1:2], col=cols1[1:2])
    text(0, 0, cex=cex, label="X", col=cols1[1])
}

drawTarget_NRA_MR63 <-
function(x, cex=par("cex")) {
    ## outer frame
    with(x$inUnit, rect(-frame, -frame, frame, frame, col=x$colsTxt[1], border=NA))
    ## draw circles first, start from the outside
    with(x$inUnit, symbols(rep(0, length(ringR)), rep(0, length(ringR)), add=TRUE,
                           circles=rev(ringR), bg=rev(x$cols), fg=rev(x$colsTxt),
                           inches=FALSE))
    # abline(v=0, h=0, col="lightgray")    # add point of aim
        
    ## add ring numbers except for bullseye (ring number maxVal)
    ## bullseye has inner sub-division -> start numbers on ring 2
    rings1 <- with(x, seq(from=maxVal, to=maxVal-nRings+1, length.out=nRings)) # top of center
    pos1   <- with(x$inUnit,
                   c(ringR[2           :(2+x$nSmall-1)] - (ringW/2),
                     ringR[(2+x$nSmall):length(ringR)]  - (ringWL/2))) # top of center
    cols1  <- with(x$inUnit, x$colsTxt[2:length(ringR)])         # top of center
    
    text(pos1, 0, cex=cex, label=rings1, col=cols1)
    text(0,    0, cex=cex, label="X",    col=cols1[1])
}

drawTarget_NRA_MR1 <-
function(x, cex=par("cex")) {
    ## outer frame
    with(x$inUnit, rect(-frame, -frame, frame, frame, col=x$colsTxt[1], border=NA))
    ## draw circles first, start from the outside
    with(x$inUnit, symbols(rep(0, length(ringR)), rep(0, length(ringR)), add=TRUE,
                           circles=rev(ringR), bg=rev(x$cols), fg=rev(x$colsTxt),
                           inches=FALSE))
    # abline(v=0, h=0, col="lightgray")    # add point of aim
    
    ## add ring numbers except for bullseye (ring number maxVal)
    ## bullseye has inner sub-division -> start numbers on ring 2
    rings1 <- with(x, seq(from=maxVal, to=maxVal-nRings+1, length.out=nRings)) # top of center
    pos1   <- with(x$inUnit,
                   c(ringR[2           :(2+x$nSmall-1)] - (ringW/2),
                     ringR[(2+x$nSmall):length(ringR)]  - (ringWL/2))) # top of center
    cols1  <- with(x$inUnit, x$colsTxt[2:length(ringR)])         # top of center

    text(0, pos1, cex=cex, label=rings1, col=cols1)
    text(0, 0,    cex=cex, label="X",    col=cols1[1])
}

drawTarget_NRA_MR1FC <-
function(x, cex=par("cex")) {
    ## outer frame
    with(x$inUnit, rect(-frame, -frame, frame, frame, col=x$colsTxt[1], border=NA))
    ## draw circles first, start from the outside
    with(x$inUnit, symbols(rep(0, length(ringR)), rep(0, length(ringR)), add=TRUE,
                           circles=rev(ringR), bg=rev(x$cols), fg=rev(x$colsTxt),
                           inches=FALSE))
    # abline(v=0, h=0, col="lightgray")    # add point of aim
        
    ## add ring numbers except for bullseye (ring number maxVal)
    ## bullseye has inner sub-division -> start numbers on ring 2
    rings1 <- with(x, seq(from=maxVal-nRings+1, to=maxVal, length.out=nRings)) # left side of center
    rings2 <- c(rings1, rev(rings1))                                           # both sides
    pos1   <- with(x$inUnit,
                   c(ringR[2]                           - (ringW/4),
                     ringR[3           :(2+x$nSmall-1)] - (ringW/2),
                     ringR[(2+x$nSmall):length(ringR)]  - (ringWL/2))) # top of center
    pos2   <- c(-rev(pos1), pos1)                                              # both sides
    cols1  <- with(x$inUnit, x$colsTxt[2:length(ringR)])                       # left side of center
    cols2  <- c(rev(cols1), cols1)                                             # both sides
    
    angX  <- 45*pi/180
    posLU <- cbind(pos1[5], 0) %*% with(x, cbind(c(cos(5*angX), sin(5*angX)), c(-sin(5*angX), cos(5*angX))))
    posRU <- cbind(pos1[5], 0) %*% with(x, cbind(c(cos(7*angX), sin(7*angX)), c(-sin(7*angX), cos(7*angX))))
    text(posLU[ , 1], posLU[ , 2], cex=cex, label="F", col=cols1[1])
    text(posRU[ , 1], posRU[ , 2], cex=cex, label="F", col=cols1[1])

    text(pos2, 0, cex=x$cexMagFac*cex, label=rings2, col=cols2)
    text(0,    0, cex=x$cexMagFac*cex, label="X",    col=cols1[1])
}

drawTarget_NRA_LRFC <-
function(x, cex=par("cex")) {
    ## outer frame
    with(x$inUnit, rect(-frame, -frame, frame, frame, col=x$colsTxt[1], border=NA))
    ## draw circles first, start from the outside
    with(x$inUnit, symbols(rep(0, length(ringR)), rep(0, length(ringR)), add=TRUE,
                           circles=rev(ringR), bg=rev(x$cols), fg=rev(x$colsTxt),
                           inches=FALSE))
    # abline(v=0, h=0, col="lightgray")    # add point of aim
        
    ## add ring numbers except for bullseye (ring number maxVal)
    ## bullseye has inner sub-division -> start numbers on ring 3
    rings1 <- with(x, seq(from=maxVal-nRings+1, to=maxVal, length.out=nRings)) # left side of center
    rings2 <- c(rings1, rev(rings1))                                           # both sides
    pos1   <- with(x$inUnit,
                   c(ringR[2]                                    - (ringW/4),
                     ringR[3           :(2+x$nSmall-1)]          - (ringW/2),
                     ringR[(2+x$nSmall):(2+x$nSmall+x$nLarge-1)] - (ringWL/2), # top of center
                     ringR[(2+x$nSmall+x$nLarge):length(ringR)]  - (ringWLL/2))) # top of center
    pos2   <- c(-rev(pos1), pos1)                                              # both sides
    cols1  <- with(x$inUnit, x$colsTxt[2:length(ringR)])                       # left side of center
    cols2  <- c(rev(cols1), cols1)                                             # both sides
    
    angX  <- 45*pi/180
    posLU <- cbind(pos1[4], 0) %*% with(x, cbind(c(cos(5*angX), sin(5*angX)), c(-sin(5*angX), cos(5*angX))))
    posRU <- cbind(pos1[4], 0) %*% with(x, cbind(c(cos(7*angX), sin(7*angX)), c(-sin(7*angX), cos(7*angX))))
    text(posLU[ , 1], posLU[ , 2], cex=cex, label="F", col=cols1[1])
    text(posRU[ , 1], posRU[ , 2], cex=cex, label="F", col=cols1[1])
    
    text(pos2, 0, cex=cex, label=rings2, col=cols2)
    text(0,    0, cex=cex, label="X",    col=cols1[1])
}

drawTarget_NRA_MR65FC <-
function(x, cex=par("cex")) {
    ## outer frame
    with(x$inUnit, rect(-frame, -frame, frame, frame, col=x$colsTxt[1], border=NA))
    ## draw circles first, start from the outside
    with(x$inUnit, symbols(rep(0, length(ringR)), rep(0, length(ringR)), add=TRUE,
                           circles=rev(ringR), bg=rev(x$cols), fg=rev(x$colsTxt),
                           inches=FALSE))
    # abline(v=0, h=0, col="lightgray")    # add point of aim
    
    ## add ring numbers except for bullseye (ring number maxVal)
    ## bullseye has inner sub-division -> start numbers on ring 3
    rings1 <- with(x, seq(from=maxVal-nRings+1, to=maxVal, length.out=nRings)) # left side of center
    pos1   <- with(x$inUnit, c(ringR[2]               - (ringW/4),
                               ringR[3:length(ringR)] - (ringW/2))) # right side of center
    cols1  <- with(x$inUnit, x$colsTxt[2:length(ringR)])                       # left side of center

    angX  <- 45*pi/180
    posLU <- cbind(pos1[4], 0) %*% with(x, cbind(c(cos(5*angX), sin(5*angX)), c(-sin(5*angX), cos(5*angX))))
    posRU <- cbind(pos1[4], 0) %*% with(x, cbind(c(cos(7*angX), sin(7*angX)), c(-sin(7*angX), cos(7*angX))))
    text(posLU[ , 1], posLU[ , 2], cex=cex, label="F", col=cols1[1])
    text(posRU[ , 1], posRU[ , 2], cex=cex, label="F", col=cols1[1])
    
    text(0, pos1, cex=cex, label=rev(rings1), col=cols1)
    text(0,    0, cex=cex, label="X",         col=cols1[1])
}

#####-----------------------------------------------------------------------
## BDMP special targets
drawTarget_BDMP3 <-
function(x, cex=par("cex")) {
    ## draw circles first, start from the outside
    with(x$inUnit, symbols(rep(0, length(ringR)), rep(0, length(ringR)), add=TRUE,
                           circles=rev(ringR), bg=rev(x$cols), fg=rev(x$colsTxt),
                           inches=FALSE))
    with(x$inUnit, rect(-extra/2, -extra/2, extra/2, extra/2, col="black", border=NA))
    with(x$inUnit, symbols(rep(0, length(ringR)), rep(0, length(ringR)), add=TRUE,
                           circles=rev(ringR), bg=NA, fg=rev(x$colsTxt), inches=FALSE))
    with(x$inUnit, symbols(0, 0, add=TRUE, circles=ringR[1], bg=x$cols[1], fg=NA, inches=FALSE))
    # abline(v=0, h=0, col="lightgray")    # add point of aim
}

drawTarget_BDMP4 <-
function(x, cex=par("cex")) {
    ## background black square
    with(x$inUnit, rect(-extra/2, -extra/2, extra/2, extra/2, col="black", border=NA))
    ## draw circles first, start from the outside
    with(x$inUnit, symbols(rep(0, length(ringR)), rep(0, length(ringR)), add=TRUE,
                           circles=rev(ringR), bg=rev(x$cols), fg=rev(x$colsTxt),
                           inches=FALSE))
    # abline(v=0, h=0, col="lightgray")    # add point of aim
}

#####-----------------------------------------------------------------------
## DSU special targets
drawTarget_DSUa <-
function(x, cex=par("cex")) {
    ## central rectangles and circle
    with(x$inUnit, rect(-ringD8/2,  -ringD8V/2,  ringD8/2,  ringD8V/2,
                        col=x$cols[2], border=x$cols[2]))
    with(x$inUnit, rect(-ringD8/2+ringD8thick, -ringD8V/2+ringD8thick,
                         ringD8/2-ringD8thick,  ringD8V/2-ringD8thick,
                        col=x$cols[1], border=x$cols[1]))
    with(x$inUnit, rect(-ringD9/2,  -ringD9V/2,  ringD9/2,  ringD9V/2,
                        col=x$cols[2], border=x$cols[2]))
    with(x$inUnit, rect(-ringD9/2+ringD9thick, -ringD9V/2+ringD9thick,
                         ringD9/2-ringD9thick,  ringD9V/2-ringD9thick,
                         col=x$cols[1], border=x$cols[1]))
    with(x$inUnit, rect(-ringD10/2, -ringD10V/2, ringD10/2, ringD10V/2,
                        col=x$cols[2], border=x$cols[1]))
    with(x$inUnit, drawCircle(c(0, 0), ringD10i/2, bg=x$cols[1], fg=x$cols[2]))

    ## offset circles
    with(x$inUnit, drawCircle(c(0, ringD9V/2 + ringOff + ringOffD/2),
                              radius=ringOffD/2, bg=x$cols[2], fg=x$cols[2]))
    with(x$inUnit, drawCircle(c(0, ringD9V/2 + ringOff + ringOffD/2),
                              radius=ringOffDi/2, bg=x$cols[1], fg=x$cols[1]))
    with(x$inUnit, rect(-crossW, ringD9V/2 + ringOff + ringOffD/2 - crossThick/2,
                         crossW, ringD9V/2 + ringOff + ringOffD/2 + crossThick/2,
                         col=x$cols[2], border=x$cols[2]))
    with(x$inUnit, rect(-crossThick/2, ringD9V/2 + ringOff + ringOffD/2 - crossW,
                         crossThick/2, ringD9V/2 + ringOff + ringOffD/2 + crossW,
                         col=x$cols[2], border=x$cols[2]))

    ## angles
    ## LU
    with(x$inUnit, rect(-ringD9/2 - angleOff,          ringD9V/2 + angleOff,
                        -ringD9/2 - angleOff + angleL, ringD9V/2 + angleOff - angleThick,
                         col=x$cols[2], border=x$cols[2]))
    with(x$inUnit, rect(-ringD9/2 - angleOff,              ringD9V/2 + angleOff,
                        -ringD9/2 - angleOff + angleThick, ringD9V/2 + angleOff - angleL,
                         col=x$cols[2], border=x$cols[2]))
    ## LD
    with(x$inUnit, rect(-ringD9/2 - angleOff,          -ringD9V/2 - angleOff,
                        -ringD9/2 - angleOff + angleL, -ringD9V/2 - angleOff + angleThick,
                         col=x$cols[2], border=x$cols[2]))
    with(x$inUnit, rect(-ringD9/2 - angleOff,              -ringD9V/2 - angleOff,
                        -ringD9/2 - angleOff + angleThick, -ringD9V/2 - angleOff + angleL,
                         col=x$cols[2], border=x$cols[2]))
    ## RD
    with(x$inUnit, rect(ringD9/2 + angleOff,          -ringD9V/2 - angleOff,
                        ringD9/2 + angleOff - angleL, -ringD9V/2 - angleOff + angleThick,
                        col=x$cols[2], border=x$cols[2]))
    with(x$inUnit, rect(ringD9/2 + angleOff,              -ringD9V/2 - angleOff,
                        ringD9/2 + angleOff - angleThick, -ringD9V/2 - angleOff + angleL,
                        col=x$cols[2], border=x$cols[2]))
    ## RU
    with(x$inUnit, rect(ringD9/2 + angleOff,          ringD9V/2 + angleOff,
                        ringD9/2 + angleOff - angleL, ringD9V/2 + angleOff - angleThick,
                        col=x$cols[2], border=x$cols[2]))
    with(x$inUnit, rect(ringD9/2 + angleOff,              ringD9V/2 + angleOff,
                        ringD9/2 + angleOff - angleThick, ringD9V/2 + angleOff - angleL,
                        col=x$cols[2], border=x$cols[2]))
}

drawTarget_DSUb <-
function(x, cex=par("cex")) {
    ## draw circle sectors first, start from the outside
    drawOval <- function(i) {
        ang <- (180/pi)*with(x$inUnit, atan((ringRV-ringR) / ringR))
        with(x$inUnit, drawDSUOval(c(0, 0),
                                   h=ringRV[i]-ringR[i],
                                   radius=ringR[i],
                                   angle=ang[i],
                                   fg=x$colsTxt[i],
                                   bg=x$cols[i]))
    }

    lapply(rev(seq_along(x$inUnit$ringR)), drawOval)

    rings1 <- with(x, seq(from=maxVal-nRings+1, to=maxVal-1, length.out=nRings-1)) # left side of center
    rings2 <- c(rings1, rev(rings1))                         # both sides
    posX1  <- with(x$inUnit, ringR[3:length(ringR)] - (ringW/2)) # x: right side of center
    posX2  <- c(-rev(posX1), posX1)                          # both sides
    posY1  <- with(x$inUnit, ringRV[3:length(ringRV)] - (ringWV/2)) # y: right side of center
    posY2  <- c(-rev(posY1), posY1)                          # y: both sides
    cols1  <- with(x$inUnit, x$colsTxt[3:length(ringR)])     # right side of center
    cols2  <- c(rev(cols1), cols1)                           # both sides

    text(posX2, 0, cex=cex, label=rings2, col=cols2)
    text(0, posY2, cex=cex, label=rings2, col=cols2)
}
