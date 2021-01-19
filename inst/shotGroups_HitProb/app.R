#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## shotGroups shiny App
## Daniel Wollschlaeger <dwoll@kuci.org>
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

library(shiny)
library(bs4Dash)
library(shotGroups)

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
        title="Determine hit probability from region using shotGroups",
        sidebar=source("app_ui_sidebar.R", encoding="UTF8")$value,
        header=dashboardHeader(
            tags$code(tags$h3("Determine hit probability from region using 'shotGroups'"))
        ),
        body=dashboardBody(
            tabItems(
                tabItem(
                    tabName="tab_data",
                    source("app_ui_tab_data.R", local=TRUE, encoding="UTF8")$value
                ),
                tabItem(
                    tabName="tab_radius",
                    source("app_ui_tab_radius.R", local=TRUE, encoding="UTF8")$value
                ),
                tabItem(
                    tabName="tab_hit_probability",
                    source("app_ui_tab_hit_probability.R", local=TRUE, encoding="UTF8")$value
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

        getUnitDstXY <- reactive({
            input$applyData
            
            ## isolate against changes in input$task
            ## only needs to change when new data is applied
            ## or when distance to target changes
            isolate({
                xy <- coords()
                if(!is.null(xy) && (nrow(xy) >= 1L)) {
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
        ## hit probability -> radius
        #####---------------------------------------------------------------------------
        
        ## output list - reactive conductor
        output$hitpGroups1 <- renderUI({
            xy <- coords()
            if(!is.null(xy) && (nrow(xy) >= 1L)) {
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
            xy <- coords()
            xySub <- if(!is.null(xy) && hasName(input, "hitpGroupSel1")) {
                groupSel <- getGroups(xy)[input$hitpGroupSel1]
                xySub <- xy[xy$series %in% groupSel, , drop=FALSE]
            } else {
                NULL
            }
            
            if(!is.null(xySub) && (nrow(xySub) >= 1L)) {
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
                invisible(NULL)
            }
        })
        
        ## confidence ellipse output list - reactive conductor
        confEllList <- reactive({
            xy <- coords()
            xySub <- if(!is.null(xy) && hasName(input, "hitpGroupSel1")) {
                groupSel <- getGroups(xy)[input$hitpGroupSel1]
                xySub <- xy[xy$series %in% groupSel, , drop=FALSE]
            } else {
                NULL
            }
            
            if(!is.null(xySub) && (nrow(xySub) >= 1L)) {
                x <- getConfEll(xySub,
                                center=input$hitpCenter1,
                                level=input$hitpLevel,
                                dstTarget=input$dstTrgt,
                                conversion=conversionStr(),
                                doRob=input$hitpDoRob1)
                
                y <- if(input$hitpDoRob1) {
                    list(confEllSizeRobust=x$sizeRob, confEllShapeRobust=x$shapeRob)
                } else {
                    list(confEllSize=x$size, confEllShape=x$shape)
                }
                
                outNames <- paste0(names(y), "_", 100*input$hitpLevel, "%")
                setNames(y, outNames)
            } else {
                invisible(NULL)
            }
        })
        
        ## extrapolation to different distance output list
        extraListRadius <- reactive({
            xy <- coords()
            xySub <- if(!is.null(xy) && hasName(input, "hitpGroupSel1")) {
                groupSel <- getGroups(xy)[input$hitpGroupSel1]
                xySub <- xy[xy$series %in% groupSel, , drop=FALSE]
            } else {
                NULL
            }
            
            if(!is.null(xySub) && (nrow(xySub) >= 1L)) {
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
                    } else {
                        NA_real_
                    }
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
                invisible(NULL)
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
                if(!is.null(out1) && !is.null(out2) && !is.null(out3)) {
                    out  <- c(out1, out2, out3)
                    Map(textOut, out, names(out), file)
                } else {
                    invisible(NULL)
                }
            },
            contentType='text/plain' # MIME type
        )
        
        #####---------------------------------------------------------------------------
        ## radius -> hit probability
        #####---------------------------------------------------------------------------
        
        ## CEP output list - reactive conductor    ## output list - reactive conductor
        output$hitpGroups2 <- renderUI({
            xy <- coords()
            if(!is.null(xy) && (nrow(xy) >= 1L)) {
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
            xy <- coords()
            xySub <- if(!is.null(xy) && hasName(input, "hitpGroupSel2")) {
                groupSel <- getGroups(xy)[input$hitpGroupSel2]
                xySub <- xy[xy$series %in% groupSel, , drop=FALSE]
            } else {
                NULL
            }
            
            if(!is.null(xySub) && (nrow(xySub) >= 1L)) {
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
                invisible(NULL)
            }
        })
        
        ## extrapolation to different distance output list
        extraListHitProb <- reactive({
            xy <- coords()
            xySub <- if(!is.null(xy) && hasName(input, "hitpGroupSel2")) {
                groupSel <- getGroups(xy)[input$hitpGroupSel2]
                xySub <- xy[xy$series %in% groupSel, , drop=FALSE]
            } else {
                NULL
            }
            
            if(!is.null(xySub) && (nrow(xySub) >= 1L)) {
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
                invisible(NULL)
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
                if(!is.null(out1) && !is.null(out2)) {
                    out <- c(out1, out2)
                    Map(textOut, out, names(out), file)
                } else {
                    invisible(NULL)
                }
            },
            contentType='text/plain' # MIME type
        )
    }
)
