#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## shotGroups shiny App
## Daniel Wollschlaeger <dwoll@kuci.org>
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

library(shiny)
library(shotGroups)
library(bs4Dash)

source("global.R", encoding="UTF8")

#####---------------------------------------------------------------------------
## app code
#####---------------------------------------------------------------------------

shinyApp(
    #####-----------------------------------------------------------------------
    ## UI
    #####-----------------------------------------------------------------------
    ui=dashboardPage(
        # theme = "custom.css",
        title="Analyze location, accuracy, precision using shotGroups",
        sidebar=source("app_ui_sidebar.R", encoding="UTF8")$value,
        header=dashboardHeader(
            tags$code(tags$h3("Analyze location, accuracy, precision using 'shotGroups'"))
        ),
        body=dashboardBody(
            tabItems(
                tabItem(
                    tabName="tab_data",
                    source("app_ui_tab_data.R", local=TRUE, encoding="UTF8")$value
                ),
                tabItem(
                    tabName="tab_group_shape",
                    source("app_ui_tab_group_shape.R", local=TRUE, encoding="UTF8")$value
                ),
                tabItem(
                    tabName="tab_group_precision",
                    source("app_ui_tab_group_precision.R", local=TRUE, encoding="UTF8")$value
                ),
                tabItem(
                    tabName="tab_group_accuracy",
                    source("app_ui_tab_group_accuracy.R", local=TRUE, encoding="UTF8")$value
                ),
                tabItem(
                    tabName="tab_group_compare",
                    source("app_ui_tab_group_compare.R", local=TRUE, encoding="UTF8")$value
                ),
                tabItem(
                    tabName="tab_target_plot",
                    source("app_ui_tab_target_plot.R", local=TRUE, encoding="UTF8")$value
                ),
                tabItem(
                    tabName="tab_about",
                    source("app_ui_tab_about.R", local=TRUE, encoding="UTF8")$value
                )
            )
        )
    ),
    #####-----------------------------------------------------------------------
    ## server
    #####-----------------------------------------------------------------------
    server=function(input, output, session) {
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

        getUnitDstXY <- reactive({
            input$applyData
            
            ## isolate against changes in input$task
            ## only needs to change when new data is applied
            ## or when distance to target changes
            isolate({
                xy <- coords()
                if(!is.null(xy)) {
                    dstTarget <- if(hasName(xy, "distance") &&
                                    (length(unique(na.omit(xy$distance))) == 1L)) {
                        ## distance to target is given in input data and unique
                        round(unique(na.omit(xy$distance)))
                    } else {
                        ## default distance to target
                        NA_real_
                    }

                    if(!is.na(dstTarget)) {
                        dst_old <- dst_current$d
                        dst_new <- dstTarget
                        if(is.na(dst_old) || (dst_old != dst_new)) {
                            dst_current$d <- dst_new
                        }
                    } else {
                        # browser()
                        dst_current$d <- -1
                    }
                    
                    unitDst <- if(hasName(xy, "distance.unit") &&
                                  (length(unique(na.omit(xy$distance.unit))) == 1L)) {
                        unit <- unique(na.omit(xy$distance.unit))
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

                    unitXY <- if(hasName(xy, "point.unit") &&
                                 (length(unique(na.omit(xy$point.unit))) == 1L)) {
                        unit <- unique(na.omit(xy$point.unit))
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
                tagList(numericInput("dstTrgt", h5("Distance to target"),
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
            if(!is.null(xy) && (nrow(xy) >= 1L)) {
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
            xy <- coords()
            xySub <- if(!is.null(xy) && hasName(input, "shapeGroupSel")) {
                groupSel <- getGroups(xy)[input$shapeGroupSel]
                xySub <- xy[xy$series %in% groupSel, , drop=FALSE]
            } else {
                NULL
            }
            
            if(!is.null(xySub) && (nrow(xySub) >= 1L)) {
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
            if(!is.null(out)) {
                ## some requested stats may be missing because package is not installed
                req <- shapeOutInv[input$shapeOut]
                out[req[hasName(out, req)]]
            } else {
                invisible(NULL)
            }
        })
        
        ## save output to file
        output$saveShape <- downloadHandler(
            filename=function() { "groupShape.txt" },
            content=function(file) {
                writeLines(fileName(), con=file)
                out <- shapeList()
                if(!is.null(out)) {
                    outSel <- out[shapeOutInv[input$shapeOut]]
                    outSelNames <- names(outSel)
                    Map(textOut, outSel, outSelNames, file)
                } else {
                    invisible(NULL)
                }
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
                    xy <- coords()
                    xySub <- if(!is.null(xy) && hasName(input, "shapeGroupSel")) {
                        groupSel <- getGroups(xy)[input$shapeGroupSel]
                        xySub <- xy[xy$series %in% groupSel, , drop=FALSE]
                    } else {
                        NULL
                    }
                    if(!is.null(xySub) && (nrow(xySub) >= 1L)) {
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
                xy <- coords()
                xySub <- if(!is.null(xy) && hasName(input, "shapeGroupSel")) {
                    groupSel <- getGroups(xy)[input$shapeGroupSel]
                    xySub <- xy[xy$series %in% groupSel, , drop=FALSE]
                } else {
                    NULL
                }
                
                if(!is.null(xySub) && (nrow(xySub) >= 1L)) {
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
            
            xy <- coords()
            xySub <- if(!is.null(xy) && hasName(input, "spreadGroupSel")) {
                groupSel <- getGroups(xy)[input$spreadGroupSel]
                xySub <- xy[xy$series %in% groupSel, , drop=FALSE]
            } else {
                NULL
            }

            if(!is.null(xySub) && (nrow(xySub) >= 1L)) {
                groupSpread(xySub,
                            center=input$spreadCenter,
                            plots=FALSE,
                            CEPtype=CEPtype,
                            CEPlevel=input$spreadCEPlevel,
                            CIlevel=input$spreadCIlevel,
                            bootCI=bootCI,
                            dstTarget=input$dstTrgt,
                            conversion=conversionStr())
            } else {
                NULL
            }
        })
        
        ## output - only selected list components
        output$spread <- renderPrint({
            out <- spreadList()
            if(!is.null(out)) {
                outSel <- out[spreadOutInv[input$precisionOut]]
                ## paste CI/CEP level
                outSelNames <- names(outSel)
                idxCEP      <- outSelNames %in% CEPOutComps
                idxCI       <- outSelNames %in% CIOutComps
                outSelNames[idxCEP] <- paste0(outSelNames[idxCEP], "_", 100*input$spreadCEPlevel, "%")
                outSelNames[idxCI]  <- paste0(outSelNames[idxCI],  "_", 100*input$spreadCIlevel,  "%")
                setNames(outSel, outSelNames)
            } else {
                invisible(NULL)
            }
        })
        
        ## save output to file
        output$saveSpread <- downloadHandler(
            filename=function() { "groupPrecision.txt" },
            content=function(file) {
                writeLines(fileName(), con=file)
                out <- spreadList()
                if(!is.null(out)) {
                    outSel <- out[spreadOutInv[input$precisionOut]]
                    ## paste CI/CEP level
                    outSelNames <- names(outSel)
                    idxCEP      <- outSelNames %in% CEPOutComps
                    idxCI       <- outSelNames %in% CIOutComps
                    outSelNames[idxCEP] <- paste0(outSelNames[idxCEP], "_", 100*input$spreadCEPlevel, "%")
                    outSelNames[idxCI]  <- paste0(outSelNames[idxCI],  "_", 100*input$spreadCIlevel,  "%")
                    outSel <- setNames(outSel, outSelNames)
                    Map(textOut, outSel, outSelNames, file)
                } else {
                    invisible(NULL)
                }
            },
            contentType='text/plain' # MIME type
        )
        
        ## create diagrams - output UI slots
        output$spreadPlot <- renderUI({
            spreadPlotOutL <- lapply(seq_len(nSpreadPlots), function(i) {
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
                    xy <- coords()
                    xySub <- if(!is.null(xy) && hasName(input, "spreadGroupSel")) {
                        groupSel <- getGroups(xy)[input$spreadGroupSel]
                        xySub <- xy[xy$series %in% groupSel, , drop=FALSE]
                    } else {
                        NULL
                    }
                    
                    if(!is.null(xySub) && (nrow(xySub) >= 1L)) {
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
                xy <- coords()
                xySub <- if(!is.null(xy) && hasName(input, "spreadGroupSel")) {
                    groupSel <- getGroups(xy)[input$spreadGroupSel]
                    xySub <- xy[xy$series %in% groupSel, , drop=FALSE]
                } else {
                    NULL
                }
                
                if(!is.null(xySub) && (nrow(xySub) >= 1L)) {
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
            
            xy <- coords()
            xySub <- if(!is.null(xy) && hasName(input, "locGroupSel")) {
                groupSel <- getGroups(xy)[input$locGroupSel]
                xySub <- xy[xy$series %in% groupSel, , drop=FALSE]
            } else {
                NULL
            }
            
            if(!is.null(xySub) && (nrow(xySub) >= 1L)) {
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
            out <- locationList()
            if(!is.null(out)) {
                outSel <- out[locationOutInv[input$locationOut]]
                ## paste CI/CEP level
                outSelNames <- names(outSel)
                idx         <- outSelNames %in% CIOutComps
                outSelNames[idx] <- paste0(outSelNames[idx], "_", 100*input$locLevel, "%")
                setNames(outSel, outSelNames)
            } else {
                invisible(NULL)
            }
        })
        
        ## save output to file
        output$saveLocation <- downloadHandler(
            filename=function() { "groupAccuracy.txt" },
            content=function(file) {
                writeLines(fileName(), con=file)
                out <- locationList()
                if(!is.null(out)) {
                    outSel <- out[locationOutInv[input$locationOut]]
                    ## paste CI/CEP level
                    outSelNames <- names(outSel)
                    idx         <- outSelNames %in% CIOutComps
                    outSelNames[idx] <- paste0(outSelNames[idx], "_", 100*input$locLevel, "%")
                    outSel <- setNames(outSel, outSelNames)
                    Map(textOut, outSel, outSelNames, file)
                } else {
                    invisible(NULL)
                }
            },
            contentType='text/plain' # MIME type
        )
        
        ## show diagram
        output$locationPlot <- renderPlot({
            xy <- coords()
            xySub <- if(!is.null(xy) && hasName(input, "locGroupSel")) {
                groupSel <- getGroups(xy)[input$locGroupSel]
                xySub <- xy[xy$series %in% groupSel, , drop=FALSE]
            } else {
                NULL
            }
            
            if(!is.null(xySub) && (nrow(xySub) >= 1L)) {
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
                xy <- coords()
                xySub <- if(!is.null(xy) && hasName(input, "locGroupSel")) {
                    groupSel <- getGroups(xy)[input$locGroupSel]
                    xySub <- xy[xy$series %in% groupSel, , drop=FALSE]
                } else {
                    NULL
                }
                
                if(!is.null(xySub) && (nrow(xySub) >= 1L)) {
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
            
            xy <- coords()
            xySub <- if(!is.null(xy) && hasName(input, "compGroupSel")) {
                groupSel <- getGroups(xy)[input$compGroupSel]
                xySub <- xy[xy$series %in% groupSel, , drop=FALSE]
            } else {
                NULL
            }
            
            if(!is.null(xySub) && (nrow(xySub) >= 1L)) {
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
            if(!is.null(out)) {
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
            } else {
                invisible(NULL)
            }
        })
        
        
        ## save output to text file
        output$saveCompare <- downloadHandler(
            filename=function() { "groupCompare.txt" },
            content=function(file) {
                writeLines(fileName(), con=file)
                out <- compareList()
                if(!is.null(out)) {
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
                } else {
                    invisible(NULL)
                }
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
                    xy <- coords()
                    xySub <- if(!is.null(xy) && hasName(input, "compGroupSel")) {
                        groupSel <- getGroups(xy)[input$compGroupSel]
                        xySub <- xy[xy$series %in% groupSel, , drop=FALSE]
                    } else {
                        NULL
                    }
                    
                    if(!is.null(xySub) && (nrow(xySub) >= 1L)) {
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
                xy <- coords()
                xySub <- if(!is.null(xy) && hasName(input, "compGroupSel")) {
                    groupSel <- getGroups(xy)[input$compGroupSel]
                    xySub <- xy[xy$series %in% groupSel, , drop=FALSE]
                } else {
                    NULL
                }
                
                if(!is.null(xySub) && (nrow(xySub) >= 1L)) {
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
            xy <- coords()
            xySub <- if(!is.null(xy) && hasName(input, "trgtGroupSel")) {
                groupSel <- getGroups(xy)[input$trgtGroupSel]
                xySub <- xy[xy$series %in% groupSel, , drop=FALSE]
            } else {
                NULL
            }
            
            if(!is.null(xySub) && (nrow(xySub) >= 1L)) {
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
            xy <- coords()
            xySub <- if(!is.null(xy) && hasName(input, "trgtGroupSel")) {
                groupSel <- getGroups(xy)[input$trgtGroupSel]
                xySub <- xy[xy$series %in% groupSel, , drop=FALSE]
            } else {
                NULL
            }
            
            if(!is.null(xySub) && (nrow(xySub) >= 1L)) {
                if(hasName(xySub, "caliber") &&
                   length(unique(xySub[["caliber"]])) == 1L) {
                    caliber <- unique(xySub[["caliber"]])
                }
            }
            numericInput("trgtCaliber", "Caliber [mm]",
                         min=0, step=1, value=caliber)
        })
        
        output$targetPlot <- renderPlot({
            xy <- coords()
            xySub <- if(!is.null(xy) && hasName(input, "trgtGroupSel")) {
                groupSel <- getGroups(xy)[input$trgtGroupSel]
                xySub <- xy[xy$series %in% groupSel, , drop=FALSE]
            } else {
                NULL
            }
            
            if(!is.null(xySub) && (nrow(xySub) >= 1L)) {
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
            xy <- coords()
            xySub <- if(!is.null(xy) && hasName(input, "trgtGroupSel")) {
                groupSel <- getGroups(xy)[input$trgtGroupSel]
                xySub <- xy[xy$series %in% groupSel, , drop=FALSE]
            } else {
                NULL
            }
            
            if(!is.null(xySub) && (nrow(xySub) >= 1L)) {
                if(input$trgtTarget != "1") {
                    simRingCount(xySub,
                                 center=input$trgtCenter,
                                 caliber=input$trgtCaliber,
                                 unit=unitsXYInv[input$unitXY],
                                 target=targetLinv[[input$trgtTarget]])
                } else {
                    invisible(NULL)
                }
            } else {
                invisible(NULL)
            }
        })
        
        ## save diagram to file
        output$saveTargetPDF <- downloadHandler(
            filename=function() { "groupTarget.pdf" },
            content=function(file) {
                xy <- coords()
                xySub <- if(!is.null(xy) && hasName(input, "trgtGroupSel")) {
                    groupSel <- getGroups(xy)[input$trgtGroupSel]
                    xySub <- xy[xy$series %in% groupSel, , drop=FALSE]
                } else {
                    NULL
                }
                
                if(!is.null(xySub) && (nrow(xySub) >= 1L)) {
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
    }
)
