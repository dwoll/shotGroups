## TODO
## save diagrams as zip file with jpegs
## compareGroups() -> overflow with horizontal scrollbar
## full info for compare groups
## or compare groups separately for shape, spread, location
## -> checkbox "combine all groups"
## -> checkbox for each group
## upload image -> integrate over arbitrary convex polygon

library(shiny)
library(shotGroups)

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
        if(!is.null(xy)) {
            groupSel <- if(input$task == 'Shape') {
                getGroups(xy)[input$shapeGroupSel]
            } else if(input$task == 'Precision') {
                getGroups(xy)[input$spreadGroupSel]
            } else if(input$task == 'Accuracy') {
                getGroups(xy)[input$locGroupSel]
            } else if(input$task == 'Compare groups') {
                getGroups(xy)[input$compGroupSel]
            } else if(input$task == 'Target plot') {
                getGroups(xy)[input$trgtGroupSel]
            } else {
                getGroups(xy)
            }
            
            xySub <- xy[xy$series %in% groupSel, , drop=FALSE]
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
        ammo    <- if(!is.null(xy$ammunition)    &&
                      !all(is.na(xy$ammunition)) &&
                      !all(xy$ammunition == "")) {
            paste(unique(xy$ammunition), collapse=", ")
        } else {
            NULL
        }

        ## isolate against non-applied changes in data input UI elements
        isolate({
            x <- if(input$datIn == "1") {
                paste0("<p>For details on this data set (measurement units etc.), see ",
                       "<a href='http://www.rdocumentation.org/packages/shotGroups/functions/",
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

    observeEvent(input$task,               { setCurrentDst() })
    observeEvent(input$shapeGroupSel, {
        if(input$task == 'Shape')          { setCurrentDst() } })

    observeEvent(input$spreadGroupSel, {
        if(input$task == 'Precision')      { setCurrentDst() } })

    observeEvent(input$locGroupSel, {
        if(input$task == 'Accuracy')       { setCurrentDst() } })

    observeEvent(input$compGroupSel, {
        if(input$task == 'Compare groups') { setCurrentDst() } })

    observeEvent(input$trgtGroupSel, {
        if(input$task == 'Target plot')    { setCurrentDst() } })

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
    ## group shape
    #####---------------------------------------------------------------------------

    ## output list - reactive conductor
    output$shapeGroups <- renderUI({
        xy <- coords()
        if(!is.null(xy)) {
            choices <- getGroups(xy, choices=TRUE)
            #if(length(choices) <= 5L) {
                checkboxGroupInput("shapeGroupSel",
                                   label=h5("Select groups"),
                                   choices=choices,
                                   selected=seq_len(min(length(choices), 2L)))
            #} else {
            #    selectizeInput("shapeGroupSel",
            #                   label=h5("Select groups"),
            #                   choices=choices, multiple=TRUE,
            #                   selected=seq_along(choices),
            #                   width="100%")
            #}
        } else {
            NULL
        }
    })

    ## output list - reactive conductor
    shapeList <- reactive({
        xySub <- getXYsub()
        if(!is.null(xySub)) {
            groupShape(xySub,
                       center=input$shapeCenter,
                       plots=FALSE,
                       dstTarget=input$dstTrgt,
                       conversion=conversionStr(),
                       bandW=input$shapeBW,
                       outlier=c("1"="mcd", "2"="pca")[input$shapeOutlier])
        } else {
            NULL
        }
    })

    ## output - only selected list components
    output$shape <- renderPrint({
        out <- shapeList()
        ## some requested stats may be missing because package is not installed
        req <- shapeOutInv[input$shapeOut]
        out[req[hasName(out, req)]]
    })

    ## save output to file
    output$saveShape <- downloadHandler(
        filename=function() { "groupShape.txt" },
        content=function(file) {
            writeLines(fileName(), con=file)
            out    <- shapeList()
            outSel <- out[shapeOutInv[input$shapeOut]]
            outSelNames <- names(outSel)
            Map(textOut, outSel, outSelNames, file)
        },
        contentType='text/plain' # MIME type
    )

    ## create diagrams - output UI slots
    output$shapePlot <- renderUI({
        shapePlotOutL <- lapply(seq_len(nShapePlots), function(i) {
            plotOutput(paste0("shapePlot", i))
        })

        ## convert the list to a tagList and return
        do.call(tagList, shapePlotOutL)
    })

    ## the actual plots - adapted from Winston Chang https://gist.github.com/wch/5436415
    for(i in seq_len(nShapePlots)) {
        local({
            localI <- i
            output[[paste0("shapePlot", localI)]] <- renderPlot({
                xySub <- getXYsub()
                if(!is.null(xySub)) {
                    shotGroups:::groupShapePlot(xySub,
                        which=localI,
                        center=input$shapeCenter,
                        dstTarget=input$dstTrgt,
                        conversion=conversionStr(),
                        bandW=input$shapeBW,
                        outlier=c("1"="mcd", "2"="pca")[input$shapeOutlier])
                } else {
                    NULL
                }
            })
        })
    }

    ## save diagrams to file
    output$saveShapePDF <- downloadHandler(
        filename=function() { "groupShape.pdf" },
        content=function(file) {
            xySub <- getXYsub()
            if(!is.null(xySub)) {
                pdf(file)
                for(i in seq_len(nShapePlots)) {
                    shotGroups:::groupShapePlot(xySub,
                        which=i,
                        center=input$shapeCenter,
                        dstTarget=input$dstTrgt,
                        conversion=conversionStr(),
                        bandW=input$shapeBW,
                        outlier=c("1"="mcd", "2"="pca")[input$shapeOutlier])
                }
                dev.off()
            } else {
                NULL
            }
        },
        contentType='application/pdf' # MIME type
    )

    #####---------------------------------------------------------------------------
    ## group spread / precision
    #####---------------------------------------------------------------------------

    ## output list - reactive conductor
    output$spreadGroups <- renderUI({
        xy <- coords()
        if(!is.null(xy)) {
            choices <- getGroups(xy, choices=TRUE)
            checkboxGroupInput("spreadGroupSel",
                               label=h5("Select groups"),
                               choices=choices,
                               selected=seq_len(min(length(choices), 2L)))
        } else {
            NULL
        }
    })

    ## output list - reactive conductor
    spreadList <- reactive({
        ## if no CEP type is selected -> fall back to default CorrNormal
        CEPtype <- if(!is.null(input$spreadCEPtype)) {
            CEPtypesInv[input$spreadCEPtype]
        } else {
            "CorrNormal"
        }

        bootCI <- if(!is.null(input$spreadCItype)) {
            CItypesInv[input$spreadCItype]
        } else {
            "none"
        }

        xySub <- getXYsub()
        if(!is.null(xySub)) {
            groupSpread(xySub,
                        center=input$spreadCenter,
                        plots=FALSE,
                        CEPtype=CEPtype,
                        CEPlevel=input$spreadCEPlevel,
                        CIlevel=input$spreadCIlevel,
                        bootCI=bootCI,
                        dstTarget=input$dstTrgt,
                        conversion=conversionStr())
        }
    })

    ## output - only selected list components
    output$spread <- renderPrint({
        out <- spreadList()
        outSel <- out[spreadOutInv[input$precisionOut]]
        ## paste CI/CEP level
        outSelNames <- names(outSel)
        idxCEP      <- outSelNames %in% CEPOutComps
        idxCI       <- outSelNames %in% CIOutComps
        outSelNames[idxCEP] <- paste0(outSelNames[idxCEP], "_", 100*input$spreadCEPlevel, "%")
        outSelNames[idxCI]  <- paste0(outSelNames[idxCI],  "_", 100*input$spreadCIlevel,  "%")
        setNames(outSel, outSelNames)
    })

    ## save output to file
    output$saveSpread <- downloadHandler(
        filename=function() { "groupPrecision.txt" },
        content=function(file) {
            writeLines(fileName(), con=file)
            out    <- spreadList()
            outSel <- out[spreadOutInv[input$precisionOut]]
            ## paste CI/CEP level
            outSelNames <- names(outSel)
            idxCEP      <- outSelNames %in% CEPOutComps
            idxCI       <- outSelNames %in% CIOutComps
            outSelNames[idxCEP] <- paste0(outSelNames[idxCEP], "_", 100*input$spreadCEPlevel, "%")
            outSelNames[idxCI]  <- paste0(outSelNames[idxCI],  "_", 100*input$spreadCIlevel,  "%")
            outSel <- setNames(outSel, outSelNames)
            Map(textOut, outSel, outSelNames, file)
        },
        contentType='text/plain' # MIME type
    )

    ## create diagrams - output UI slots
    output$spreadPlot <- renderUI({
        spreadPlotOutL <- lapply(1:nSpreadPlots, function(i) {
            plotOutput(paste0("spreadPlot", i))
        })

        ## convert the list to a tagList and return
        do.call(tagList, spreadPlotOutL)
    })

    ## the actual plots - adapted from Winston Chang https://gist.github.com/wch/5436415
    for(i in seq_len(nSpreadPlots)) {
        local({
            localI <- i
            output[[paste0("spreadPlot", localI)]] <- renderPlot({
                xySub <- getXYsub()
                if(!is.null(xySub)) {
                    shotGroups:::groupSpreadPlot(xySub,
                        which=localI,
                        center=input$spreadCenter,
                        CEPlevel=input$spreadCEPlevel,
                        CIlevel=input$spreadCIlevel,
                        dstTarget=input$dstTrgt,
                        conversion=conversionStr())
                } else {
                    NULL
                }
            })
        })
    }

    ## save diagrams to file
    output$saveSpreadPDF <- downloadHandler(
        filename=function() { "groupPrecision.pdf" },
        content=function(file) {
            xySub <- getXYsub()
            if(!is.null(xySub)) {
                pdf(file)
                for(i in seq_len(nSpreadPlots)) {
                    shotGroups:::groupSpreadPlot(xySub,
                        which=i,
                        center=input$spreadCenter,
                        CEPlevel=input$spreadCEPlevel,
                        CIlevel=input$spreadCIlevel,
                        dstTarget=input$dstTrgt,
                        conversion=conversionStr())
                }
                dev.off()
            } else {
                NULL
            }
        },
        contentType='application/pdf' # MIME type
    )

    #####---------------------------------------------------------------------------
    ## group location / accuracy
    #####---------------------------------------------------------------------------

    ## output list - reactive conductor
    output$locGroups <- renderUI({
        xy <- coords()
        if(!is.null(xy)) {
            choices <- getGroups(xy, choices=TRUE)
            checkboxGroupInput("locGroupSel",
                               label=h5("Select groups"),
                               choices=choices,
                               selected=seq_len(min(length(choices), 2L)))
        } else {
            NULL
        }
    })

    ## output list - reactive conductor
    locationList <- reactive({
        bootCI <- if(!is.null(input$locCItype)) {
            CItypesInv[input$locCItype]
        } else {
            "none"
        }

        xySub <- getXYsub()
        if(!is.null(xySub)) {
            groupLocation(xySub,
                          plots=FALSE,
                          level=input$locLevel,
                          bootCI=bootCI,
                          dstTarget=input$dstTrgt,
                          conversion=conversionStr())
        } else {
            NULL
        }
    })

    ## output - only selected list components
    output$location <- renderPrint({
        out    <- locationList()
        outSel <- out[locationOutInv[input$locationOut]]
        ## paste CI/CEP level
        outSelNames <- names(outSel)
        idx         <- outSelNames %in% CIOutComps
        outSelNames[idx] <- paste0(outSelNames[idx], "_", 100*input$locLevel, "%")
        setNames(outSel, outSelNames)
    })

    ## save output to file
    output$saveLocation <- downloadHandler(
        filename=function() { "groupAccuracy.txt" },
        content=function(file) {
            writeLines(fileName(), con=file)
            out    <- locationList()
            outSel <- out[locationOutInv[input$locationOut]]
            ## paste CI/CEP level
            outSelNames <- names(outSel)
            idx         <- outSelNames %in% CIOutComps
            outSelNames[idx] <- paste0(outSelNames[idx], "_", 100*input$locLevel, "%")
            outSel <- setNames(outSel, outSelNames)
            Map(textOut, outSel, outSelNames, file)
        },
        contentType='text/plain' # MIME type
    )

    ## show diagram
    output$locationPlot <- renderPlot({
        xySub <- getXYsub()
        if(!is.null(xySub)) {
            groupLocation(xySub,
                          plots=TRUE,
                          level=input$locLevel,
                          bootCI="none",
                          dstTarget=input$dstTrgt,
                          conversion=conversionStr())
        } else {
            NULL
        }
    })

    ## save diagram to file
    output$saveLocationPDF <- downloadHandler(
        filename=function() { "groupAccuracy.pdf" },
        content=function(file) {
            xySub <- getXYsub()
            if(!is.null(xySub)) {
                pdf(file)
                groupLocation(xySub,
                              plots=TRUE,
                              level=input$locLevel,
                              bootCI="none",
                              dstTarget=input$dstTrgt,
                              conversion=conversionStr())
                dev.off()
            } else {
                NULL
            }
        },
        contentType='application/pdf' # MIME type
    )

    #####-----------------------------------------------------------------------
    ## compare groups
    #####-----------------------------------------------------------------------

    ## output list - reactive conductor
    output$compGroups <- renderUI({
        xy <- coords()
        if(!is.null(xy)) {
            choices <- getGroups(xy, choices=TRUE)
            checkboxGroupInput("compGroupSel",
                               label=h5("Select groups"),
                               choices=choices,
                               selected=c(1, 2))
        } else {
            NULL
        }
    })

    ## list of output comonents to choose from - UI element
    output$compOut <- renderUI({
#         xy <- coords()
#         if(!is.null(xy)) {
#             groupSel <- getGroups(xy)[input$compGroupSel]
#             compChoices <- if(length(groupSel) <= 2) {
#                 compOut2
#             } else {
#                 compOut3plus
#             }
            compChoices <- compOut2
            selectizeInput("compareOut", label=h5("Select the output elements you want to see"),
                           choices=compChoices, multiple=TRUE,
                           selected=c("1", "2", "5", "8", "9", "10",
                                      "12", "14", "15"), width="100%")
#         } else {
#             NULL
#         }
    })

    ## output - only selected list components
    compareList <- reactive({
        CEPtype  <- if(!is.null(input$cmpCEPtype)) {
            CEPtypesInv[input$cmpCEPtype]
        } else {
            "CorrNormal"
        }
        xySub <- getXYsub()
        if(!is.null(xySub)) {
            groupSel <- getGroups(xySub)
            res <- compareGroups(xySub,
                          plots=FALSE,
                          xyTopLeft=input$cmpXYTL,
                          center=input$cmpCenter,
#                          ABalt=c('two.sided', 'less', 'greater'),
#                          Walt=c('two.sided', 'less', 'greater'),
                          CEPtype=CEPtype,
                          CEPlevel=input$compareCEPlevel,
                          CIlevel=input$compareCIlevel,
                          conversion=conversionStr())
            list(len=length(groupSel), res=res)
        } else {
            NULL
        }
    })

    ## output - only selected list components
    output$compare <- renderPrint({
        out <- compareList()
        outSel <- if(out$len <= 2) {
            out$res[compOut2Inv[input$compareOut]]
        } else {
            out$res[compOut3plusInv[input$compareOut]]
        }
        ## paste CI/CEP level
        outSelNames <- names(outSel)
        idxCEP <- outSelNames %in% CEPOutComps
        idxCI  <- outSelNames %in% CIOutComps
        outSelNames[idxCEP] <- paste0(outSelNames[idxCEP], "_", 100*input$compareCEPlevel, "%")
        outSelNames[idxCI]  <- paste0(outSelNames[idxCI],  "_", 100*input$compareCIlevel,  "%")
        setNames(outSel, outSelNames)
    })


    ## save output to text file
    output$saveCompare <- downloadHandler(
        filename=function() { "groupCompare.txt" },
        content=function(file) {
            writeLines(fileName(), con=file)
            out    <- compareList()
            outSel <- if(out$len <= 2) {
                out$res[compOut2Inv[input$compareOut]]
            } else {
                out$res[compOut3plusInv[input$compareOut]]
            }
            ## paste CI/CEP level
            outSelNames <- names(outSel)
            idxCEP <- outSelNames %in% CEPOutComps
            idxCI  <- outSelNames %in% CIOutComps
            outSelNames[idxCEP] <- paste0(outSelNames[idxCEP], "_", 100*input$compareCEPlevel, "%")
            outSelNames[idxCI]  <- paste0(outSelNames[idxCI],  "_", 100*input$compareCIlevel,  "%")
            outSel <- setNames(outSel, outSelNames)
            Map(textOut, outSel, outSelNames, file)
        },
        contentType='text/plain' # MIME type
    )

    ## create diagrams - output UI slots
    output$comparePlot <- renderUI({
        comparePlotOutL <- lapply(seq_len(nComparePlots), function(i) {
            plotOutput(paste0("comparePlot", i))
        })

        ## convert the list to a tagList and return
        do.call(tagList, comparePlotOutL)
    })

    ## the actual plots - adapted from Winston Chang https://gist.github.com/wch/5436415
    for(i in seq_len(nComparePlots)) {
        local({
            localI <- i
            output[[paste0("comparePlot", localI)]] <- renderPlot({
                xySub <- getXYsub()
                if(!is.null(xySub)) {
                    shotGroups:::compareGroupsPlot(xySub,
                        which=localI,
                        xyTopLeft=input$cmpXYTL,
                        center=input$cmpCenter,
                        CEPlevel=input$compareCEPlevel,
                        CIlevel=input$compareCIlevel,
                        conversion=conversionStr())
                } else {
                    NULL
                }
            })
        })
    }

    ## save diagrams to file
    output$saveComparePDF <- downloadHandler(
        filename=function() { "groupCompare.pdf" },
        content=function(file) {
            xySub <- getXYsub()
            if(!is.null(xySub)) {
                pdf(file)
                for(i in seq_len(nComparePlots)) {
                    shotGroups:::compareGroupsPlot(xySub,
                        which=i,
                        xyTopLeft=input$cmpXYTL,
                        center=input$cmpCenter,
                        CEPlevel=input$compareCEPlevel,
                        CIlevel=input$compareCIlevel,
                        conversion=conversionStr())
                }
                dev.off()
            } else {
                NULL
            }
        },
        contentType='application/pdf' # MIME type
    )

    #####---------------------------------------------------------------------------
    ## target plot
    #####---------------------------------------------------------------------------

    ## output list - reactive conductor
    output$trgtGroups <- renderUI({
        xy <- coords()
        if(!is.null(xy)) {
            choices <- getGroups(xy, choices=TRUE)
            checkboxGroupInput("trgtGroupSel",
                               label=h5("Select groups"),
                               choices=choices,
                               selected=seq_len(min(length(choices), 2L)))
        } else {
            NULL
        }
    })

    output$trgtTargetSel <- renderUI({
        selected <- 16
        xySub    <- getXYsub()
        if(!is.null(xySub)) {
            if(hasName(xySub, "target") &&
               length(unique(xySub[["target"]])) == 1L) {
                trgt <- unique(xySub[["target"]])
                if(hasName(targetL, trgt)) {
                    selected <- targetL[trgt]
                }
            }
        }
        selectInput("trgtTarget", "Target type", choices=targetL,
                    selected=selected)
    })
    
    output$trgtCaliberSel <- renderUI({
        caliber <- 9
        xySub   <- getXYsub()
        if(!is.null(xySub)) {
            if(hasName(xySub, "caliber") &&
               length(unique(xySub[["caliber"]])) == 1L) {
                caliber <- unique(xySub[["caliber"]])
            }
        }
        numericInput("trgtCaliber", "Caliber [mm]",
                     min=0, step=1, value=caliber)
    })
    
    output$targetPlot <- renderPlot({
        xySub <- getXYsub()
        if(!is.null(xySub)) {
            drawGroup(xySub,
                      center=input$trgtCenter,
                      xyTopLeft=input$trgtXYTL,
                      bb=input$trgtBB,
                      bbMin=input$trgtBBmin,
                      bbDiag=input$trgtBBdiag,
                      minCirc=input$trgtMinCirc,
                      maxSpread=input$trgtMaxSpread,
                      meanDist=input$trgtMeanDist,
                      confEll=input$trgtConfEll,
                      CEP=input$trgtCEP,
                      ringID=input$trgtRingID,
                      valueID=input$trgtValueID,
                      scaled=input$trgtScaled,
                      caliber=input$trgtCaliber,
                      level=input$trgtCEPlevel,
                      dstTarget=input$dstTrgt,
                      conversion=conversionStr(),
                      unit=unitsPlotInv[input$trgtUnitPlot],
                      alpha=input$trgtAlpha,
                      target=targetLinv[[input$trgtTarget]])
        } else {
            NULL
        }
    })

    ## simulated ring count output
    output$simRingCount <- renderPrint({
        xySub <- getXYsub()
        if(!is.null(xySub)) {
            if(input$trgtTarget != "1") {
                simRingCount(xySub,
                             center=input$trgtCenter,
                             caliber=input$trgtCaliber,
                             unit=unitsXYInv[input$unitXY],
                             target=targetLinv[[input$trgtTarget]])
            } else {
                NULL
            }
        } else {
            NULL
        }
    })

    ## save diagram to file
    output$saveTargetPDF <- downloadHandler(
        filename=function() { "groupTarget.pdf" },
        content=function(file) {
            xySub <- getXYsub()
            if(!is.null(xySub)) {
                pdf(file)
                drawGroup(xySub,
                          center=input$trgtCenter,
                          xyTopLeft=input$trgtXYTL,
                          bb=input$trgtBB,
                          bbMin=input$trgtBBmin,
                          bbDiag=input$trgtBBdiag,
                          minCirc=input$trgtMinCirc,
                          maxSpread=input$trgtMaxSpread,
                          meanDist=input$trgtMeanDist,
                          confEll=input$trgtConfEll,
                          CEP=input$trgtCEP,
                          ringID=input$trgtRingID,
                          valueID=input$trgtValueID,
                          scaled=input$trgtScaled,
                          caliber=input$trgtCaliber,
                          level=input$trgtCEPlevel,
                          dstTarget=input$dstTrgt,
                          conversion=conversionStr(),
                          unit=unitsPlotInv[input$trgtUnitPlot],
                          alpha=input$trgtAlpha,
                          target=targetLinv[[input$trgtTarget]])
                dev.off()
            } else {
                NULL
            }
        },
        contentType='application/pdf' # MIME type
    )
})
