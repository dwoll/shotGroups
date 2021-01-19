source("helper.R")

shinyServer(function(input, output, session) {
    session$onSessionEnded(stopApp)
    
    #####-----------------------------------------------------------------------
    ## provide the data - reactive conductor
    #####-----------------------------------------------------------------------

    coords <- reactive({
        ## only change when explicitly applied
        input$applyData

        ## isolate against non-applied changes in data input UI elements
        isolate({
            if(input$datIn == '1') {
                ## built in data
                get(dataBuiltIn[input$builtInData])
            } else if(input$datIn == '2') {
                ## upload files
                if(!is.null(input$fileUpload)) {
                    fPath <- input$fileUpload$datapath

                    if(input$fileType == '1') {          ## OnTarget 1.*
                        readDataOT1(fPath=dirname(fPath),  fNames=basename(fPath))
                    } else if(input$fileType == '2') {   ## OnTarget 2.*, 3.*
                        readDataOT2(fPath=dirname(fPath),  fNames=basename(fPath))
                    } else if(input$fileType == '3') {   ## Silver Mountain e-target
                        readDataSMT(fPath=dirname(fPath),  fNames=basename(fPath))
                    } else if(input$fileType == '4') {   ## ShotMarker e-target
                        readDataShotMarker(fPath=dirname(fPath),  fNames=basename(fPath))
                    } else if(input$fileType == '5') {   ## other
                        readDataMisc(fPath=dirname(fPath), fNames=basename(fPath))
                    }
                } else {
                    NULL
                }
            } else if(input$datIn == '3') {
                ## paste data
                fPath <- tempfile()
                writeLines(input$datPaste, fPath)
                if(input$fileType == '1') {          ## OnTarget 1.*
                    readDataOT1(dirname(fPath),  fNames=basename(fPath))
                } else if(input$fileType == '2') {   ## OnTarget 2.*, 3.*
                    readDataOT2(dirname(fPath),  fNames=basename(fPath))
                } else if(input$fileType == '3') {   ## Silver Mountain e-target
                    readDataSMT(dirname(fPath),  fNames=basename(fPath))
                } else if(input$fileType == '4') {   ## ShotMarker e-target
                    readDataShotMarker(dirname(fPath),  fNames=basename(fPath))
                } else if(input$fileType == '5') {   ## other
                    readDataMisc(dirname(fPath), fNames=basename(fPath))
                }
            } else {
                NULL
            }
        })
    })

    getXYsub <- reactive({
        xy <- coords()
        if(!is.null(coords)) {
            ## limit xy to currently selected groups such that distance
            ## adapts automatically
            groupSel <- if(input$task == '<div>\n  Hit probability\n  <i class=\"glyphicon glyphicon-arrow-right\"></i>\n  region\n</div>') {
                getGroups(xy)[input$hitpGroupSel1]
            } else if(input$task == '<div>\n  Region\n  <i class=\"glyphicon glyphicon-arrow-right\"></i>\n  hit probability\n</div>') {
                getGroups(xy)[input$hitpGroupSel2]
            } else {
                getGroups(xy)
            }
            
            xySub <- xy[xy$series %in% groupSel , , drop=FALSE]
        } else {
            NULL
        }
    })

    #####---------------------------------------------------------------------------
    ## provide file information - UI element
    #####---------------------------------------------------------------------------

    output$fileInfo <- renderUI({
        xy <- coords()
        dstTrgt <- if(!is.null(xy$distance) && !all(is.na(xy$distance))) {
            paste(round(sort(unique(xy$distance))), collapse=", ")
        } else {
            "not available"
        }
        
        unit_distance <- if(!is.null(xy$distance.unit)) {
            paste(unique(xy$distance.unit), collapse=", ")
        } else {
            NULL
        }
        
        unit_xy <- if(!is.null(xy$point.unit)) {
            paste(unique(xy$point.unit), collapse=", ")
        } else {
            NULL
        }
        
        nGroups <- if(!is.null(xy$series) && !all(is.na(xy$series))) {
            nlevels(xy$series)
        } else {
            nlevels(xy$group)
        }

        comment <- attributes(xy)$comment
        ammo <- if(!is.null(xy$ammunition) && !all(is.na(xy$ammunition)) && !all(xy$ammunition == "")) {
            paste(unique(xy$ammunition), collapse=", ")
        } else {
            NULL
        }

        ## isolate against non-applied changes in data input UI elements
        isolate({
            x <- if(input$datIn == "1") {
                paste0("<p>For details on this data set (measurement units etc.), see ",
                       "<a href='https://www.rdocumentation.org/packages/shotGroups/functions/",
                       dataBuiltIn[input$builtInData], "'>", dataBuiltIn[input$builtInData],
                       "</a></p>Name: ", dataBuiltIn[input$builtInData], "<br />")
            } else if(input$datIn == "2") {
                if(!is.null(input$fileUpload)) {
                    paste0("<p>Name: ", paste(basename(input$fileUpload$name), collapse=", "), "<br />")
                } else {
                    "<p>"
                }
            } else {
                paste0("<p>Pasted data<br />")
            }

            y <- paste0(x, "Number of shots: ", nrow(xy),
                   "<br />Distance to target ", dstTrgt,
                   ifelse(!is.null(unit_distance), paste0(" [", unit_distance, "]"), ""),
                   ifelse(!is.null(unit_xy), "<br />Measurement unit coordinates: ", ""), unit_xy,
                   "<br />Number of groups: ", nGroups,
                   ifelse(!is.null(comment), "<br />Additional information: ", ""), comment,
                   ifelse(!is.null(ammo),    "<br />Ammunition: ", ""), ammo, "</p>")
            HTML(y)
        })
    })

    #####---------------------------------------------------------------------------
    ## provide short file information - UI element
    #####---------------------------------------------------------------------------

    output$fileInfoShort <- renderUI({
        xy <- coords()

        ## isolate against non-applied changes in data input UI elements
        isolate({
            nGroups <- if(!is.null(xy$series) && !all(is.na(xy$series))) {
                nlevels(xy$series)
            } else {
                nlevels(xy$group)
            }

            x <- if(input$datIn == "1") {
                paste0("<p>Name: ", dataBuiltIn[input$builtInData], "<br />")
            } else if((input$datIn == "2") && !is.null(input$fileUpload)) {
                paste0("<p>Name: ", paste(basename(input$fileUpload$name), collapse=", "), "<br />")
            } else {
                paste0("<p>Pasted data<br />")
            }

            y <- paste0(x, "Number of shots: ", nrow(xy),
                        "<br />Number of groups: ", nGroups, "</p>")
            HTML(y)
        })
    })

    #####---------------------------------------------------------------------------
    ## provide file name + ammo information - reactive conductor
    #####---------------------------------------------------------------------------

    fileName <- reactive({
        xy <- coords()
        ammo <- if(!is.null(xy$ammunition)    &&
                   !all(is.na(xy$ammunition)) &&
                   !all(xy$ammunition == "")) {
            paste(unique(xy$ammunition), collapse=", ")
        } else {
            NULL
        }

        ## isolate against non-applied changes in data input UI elements
        isolate({
            fName <- if(input$datIn == "1") {
                dataBuiltIn[input$builtInData]
            } else if(input$datIn == "2") {
                if(!is.null(input$fileUpload)) {
                    paste(basename(input$fileUpload$name), collapse=", ")
                } else {
                    NULL
                }
            } else {
                paste0("Pasted data")
            }
            c(fName, ammo, "")
        })
    })

    #####---------------------------------------------------------------------------
    ## distance to target, unit distance, unit xy-coords - UI element
    #####---------------------------------------------------------------------------

    dst_current <- reactiveValues(d=-1)
    setCurrentDst <- reactive({
        xySub <- getXYsub()
        if(!is.null(xySub) && hasName(xySub, "distance")) {
            if(length(unique(xySub$distance)) == 1L) {
                ## distance to target is given in input data and unique
                dst_old <- dst_current$d
                dst_new <- round(unique(xySub$distance))
                if(is.na(dst_old) || (dst_old != dst_new)) {
                    dst_current$d <- dst_new
                }
            } else {
                # browser()
                dst_current$d <- -1
            }
        }
    })
    
    observeEvent(input$task, { setCurrentDst() })
    observeEvent(input$hitpGroupSel1, {
        if(input$task == '<div>\n  Hit probability\n  <i class=\"glyphicon glyphicon-arrow-right\"></i>\n  region\n</div>') {
            setCurrentDst() } })
    
    observeEvent(input$hitpGroupSel2, {
        if(input$task == '<div>\n  Region\n  <i class=\"glyphicon glyphicon-arrow-right\"></i>\n  hit probability\n</div>') {
            setCurrentDst() } })
    
    getUnitDstXY <- reactive({
        dst_current$d
        input$applyData
        
        ## isolate against changes in input$task
        ## only needs to change when new data is applied
        ## or when distance to target changes
        isolate({
            xySub <- getXYsub()
            if(!is.null(xySub)) {
                dstTarget <- if(hasName(xySub, "distance") &&
                                (length(unique(xySub$distance)) == 1L)) {
                    ## distance to target is given in input data and unique
                    round(unique(xySub$distance))
                } else {
                    ## default distance to target
                    NA_real_
                }
                
                unitDst <- if(hasName(xySub, "distance.unit") &&
                              (length(unique(xySub$distance.unit)) == 1L)) {
                    unit <- unique(xySub$distance.unit)
                    if(unit == "(unknown)") {
                        NA_character_
                    } else if(unit == "m") {
                        2
                    } else if(unit %in% c("y", "yd", "yard")) {
                        3
                    } else if(unit %in% c("f", "ft", "feet", "foot")) {
                        4
                    } else {
                        NA_character_
                    }
                } else {
                    NA_character_
                }
                
                unitXY <- if(hasName(xySub, "point.unit") &&
                             (length(unique(xySub$point.unit)) == 1L)) {
                    unit <- unique(xySub$point.unit)
                    if(unit == "(unknown)") {
                        NA_character_
                    } else if(unit == "cm") {
                        2
                    } else if(unit == "mm") {
                        3
                    } else if(unit %in% c("in", "inch")) {
                        4
                    }
                } else {
                    NA_character_
                }
                
                list(dstTarget=dstTarget, unitDst=unitDst, unitXY=unitXY)
            } else {
                NULL
            }
        })
    })
    
    output$unitDstXY <- renderUI({
        unitDstXY <- getUnitDstXY()
        if(!is.null(unitDstXY)) {
            dst_current$d <- unitDstXY$dstTarget
            ## dst to target, unit dst, unit xy
            inputPanel(numericInput("dstTrgt", h5("Distance to target"),
                                    min=0, step=1, value=unitDstXY$dstTarget),
                       selectInput("unitDst", h5("Measurement unit distance"),
                                   choices=unitsDst, selected=unitDstXY$unitDst),
                       selectInput("unitXY", h5("Measurement unit coordinates"),
                                   choices=unitsXY, selected=unitDstXY$unitXY))
        }
    })
    
    #####---------------------------------------------------------------------------
    ## string for conversion argument - unit distance to unit xy-coords
    ## reactive conductor
    #####---------------------------------------------------------------------------

    conversionStr <- reactive({
        paste0(unitsDstInv[input$unitDst], "2",
               unitsXYInv[input$unitXY], collapse="")
    })

    #####---------------------------------------------------------------------------
    ## hit probability -> radius
    #####---------------------------------------------------------------------------

    ## output list - reactive conductor
    output$hitpGroups1 <- renderUI({
        xy <- coords()
        if(!is.null(xy)) {
            choices <- getGroups(xy, choices=TRUE)
            checkboxGroupInput("hitpGroupSel1",
                               label=h5("Select groups"),
                               choices=choices,
                               selected=seq_len(min(length(choices), 2L)))
        } else {
            NULL
        }
    })

    ## CEP output list - reactive conductor
    CEPListRadius <- reactive({
        xySub <- getXYsub()
        if(!is.null(xySub)) {
            ## if no CEP type is selected -> fall back to default CorrNormal
            CEPtype <- if(!is.null(input$hitpCEPtype1)) {
                CEPtypesInv[input$hitpCEPtype1]
            } else {
                "CorrNormal"
            }

            ## hit probability -> radius
            x <- getCEP(xySub,
                        center=input$hitpCenter1,
                        CEPlevel=input$hitpLevel,
                        dstTarget=input$dstTrgt,
                        conversion=conversionStr(),
                        accuracy=input$hitpAcc,
                        type=CEPtype,
                        doRob=input$hitpDoRob1)$CEP[[1]]
            setNames(list(x), paste0("CEP_", 100*input$hitpLevel, "%"))
        } else {
            NULL
        }
    })

    ## confidence ellipse output list - reactive conductor
    confEllList <- reactive({
        xySub <- getXYsub()
        if(!is.null(xySub)) {
            x <- getConfEll(xySub,
                            center=input$hitpCenter1,
                            level=input$hitpLevel,
                            dstTarget=input$dstTrgt,
                            conversion=conversionStr(),
                            doRob=input$hitpDoRob1)

            x <- if(input$hitpDoRob1) {
                list(confEllSizeRobust=x$sizeRob, confEllShapeRobust=x$shapeRob)
            } else {
                list(confEllSize=x$size, confEllShape=x$shape)
            }

            outNames <- paste0(names(x), "_", 100*input$hitpLevel, "%")
            setNames(x, outNames)
        } else {
            NULL
        }
    })

    ## extrapolation to different distance output list
    extraListRadius <- reactive({
        xySub <- getXYsub()
        if(!is.null(xySub)) {
            ## if no CEP type is selected -> fall back to default CorrNormal
            CEPtype <- if(!is.null(input$hitpCEPtype1)) {
                CEPtypesInv[input$hitpCEPtype1]
            } else {
                "CorrNormal"
            }

            # browser()
            ## hit probability -> radius
            res1a <- getCEP(xySub,
                            center=input$hitpCenter1,
                            CEPlevel=input$hitpLevel,
                            dstTarget=as.numeric(input$dstTrgt),
                            conversion=conversionStr(),
                            accuracy=input$hitpAcc,
                            type=CEPtype,
                            doRob=input$hitpDoRob1)

            res1b <- if("MOA" %in% rownames(res1a$CEP[[1]])) {
                res1a$CEP[[1]]["MOA", , drop=FALSE]
            } else {
                NA_real_
            }

            res2a <- getConfEll(xySub,
                                center=input$hitpCenter1,
                                level=input$hitpLevel,
                                dstTarget=input$dstTrgt,
                                conversion=conversionStr(),
                                doRob=input$hitpDoRob1)

            res2b <- if(input$hitpDoRob1) {
                if("MOA" %in% rownames(res2a$sizeRob)) {
                    res2a$sizeRob["MOA", , drop=FALSE]
                }
                NA_real_
            } else if("MOA" %in% rownames(res2a$size)) {
                res2a$size["MOA", , drop=FALSE]
            } else {
                NA_real_
            }

            CEP <- fromMOA(res1b,
                           dst=input$hitpExtraDst1,
                           conversion=paste0(unitsDstInv[input$hitpUnitExtraDst1], "2",
                                             unitsXYInv[input$unitXY], collapse=""))
            ConfEll <- fromMOA(res2b,
                               dst=input$hitpExtraDst1,
                               conversion=paste0(unitsDstInv[input$hitpUnitExtraDst1], "2",
                                                 unitsXYInv[input$unitXY], collapse=""))

            if(is.matrix(CEP) && is.matrix(ConfEll)) {
                rownames(CEP)     <- "unit"
                rownames(ConfEll) <- "unit"
            }

            x <- list(CEP=CEP, ConfEll=ConfEll)
            setNames(x, paste0(names(x), "_", 100*input$hitpLevel, "%", "_@",
                               input$hitpExtraDst1, na.omit(unitsDstInv[input$hitpUnitExtraDst1])))
        } else {
            NULL
        }
    })

    ## CEP output
    output$CEPRadius <- renderPrint({
        CEPListRadius()
    })

    ## confidence ellipse output
    output$confEll <- renderPrint({
        confEllList()
    })

    ## extrapolation to different distance output
    output$extraRadius <- renderPrint({
        extraListRadius()
    })

    ## save output to text file
    output$saveRadius <- downloadHandler(
        filename=function() { "groupRadius.txt" },
        content=function(file) {
            writeLines(fileName(), con=file)
            out1 <- CEPListRadius()
            out2 <- confEllList()
            out3 <- extraListRadius()
            out  <- c(out1, out2, out3)
            Map(textOut, out, names(out), file)
        },
        contentType='text/plain' # MIME type
    )

    #####---------------------------------------------------------------------------
    ## radius -> hit probability
    #####---------------------------------------------------------------------------

    ## CEP output list - reactive conductor    ## output list - reactive conductor
    output$hitpGroups2 <- renderUI({
        xy <- coords()
        if(!is.null(xy)) {
            choices <- getGroups(xy, choices=TRUE)
            checkboxGroupInput("hitpGroupSel2",
                               label=h5("Select groups"),
                               choices=choices,
                               selected=seq_len(min(length(choices), 2L)))
        } else {
            NULL
        }
    })

    CEPListHitProb <- reactive({
        xySub <- getXYsub()
        if(!is.null(xySub)) {
            ## if no CEP type is selected -> fall back to default CorrNormal
            CEPtype <- if(!is.null(input$hitpCEPtype2)) {
                CEPtypesInv[input$hitpCEPtype2]
            } else {
                "CorrNormal"
            }

            x <- getHitProb(xySub,
                            r=input$hitpR,
                            center=input$hitpCenter2,
                            unit=hitpRUnitInv[[input$hitpUnitR]],
                            dstTarget=input$dstTrgt,
                            conversion=conversionStr(),
                            accuracy=input$hitpAcc,
                            type=CEPtype,
                            doRob=input$hitpDoRob2)
            setNames(list(x), paste0("hitProbRadius_", input$hitpR))
        } else {
            NULL
        }
    })

    ## extrapolation to different distance output list
    extraListHitProb <- reactive({
        xySub <- getXYsub()
        if(!is.null(xySub)) {
            ## if no CEP type is selected -> fall back to default CorrNormal
            CEPtype <- if(!is.null(input$hitpCEPtype2)) {
                CEPtypesInv[input$hitpCEPtype2]
            } else {
                "CorrNormal"
            }

            ## radius -> hit probability
            MOA <- getMOA(input$hitpR,
                          dst=input$hitpExtraDst2,
                          conversion=paste0(unitsDstInv[input$hitpUnitExtraDst2], "2",
                                            unitsXYInv[input$unitXY], collapse=""))
            x <- getHitProb(xySub,
                            r=MOA,
                            center=input$hitpCenter2,
                            unit="MOA",
                            dstTarget=as.numeric(input$dstTrgt),
                            conversion=conversionStr(),
                            accuracy=input$hitpAcc,
                            type=CEPtype,
                            doRob=input$hitpDoRob2)
            setNames(list(x), paste0("hitProbRadius_", input$hitpR, "_@",
                                     input$hitpExtraDst2, unitsDstInv[input$hitpUnitExtraDst2]))
        } else {
            NULL
        }
    })

    ## CEP output
    output$CEPHitProb <- renderPrint({
        CEPListHitProb()
    })

    ## extrapolation to different distance output
    output$extraHitProb <- renderPrint({
        extraListHitProb()
    })

    ## save output to text file
    output$saveHitProb <- downloadHandler(
        filename=function() { "groupHitProbability.txt" },
        content=function(file) {
            writeLines(fileName(), con=file)
            out1 <- CEPListHitProb()
            out2 <- extraListHitProb()
            out  <- c(out1, out3)
            Map(textOut, out, names(out), file)
        },
        contentType='text/plain' # MIME type
    )
})
