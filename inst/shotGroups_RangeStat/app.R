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
        title="Estimate Rayleigh sigma from range statistics using shotGroups",
        sidebar=source("app_ui_sidebar.R", encoding="UTF8")$value,
        header=dashboardHeader(
            tags$code(tags$h3("Estimate Rayleigh \U03C3 from range statistics using 'shotGroups'"))
        ),
        body=dashboardBody(
            bs4TabItems(
                tabItem(
                    tabName="tab_sigma",
                    source("app_ui_tab_sigma.R", local=TRUE, encoding="UTF8")$value
                ),
                tabItem(
                    tabName="tab_n_groups",
                    source("app_ui_tab_n_groups.R", local=TRUE, encoding="UTF8")$value
                ),
                tabItem(
                    tabName="tab_ci_width",
                    source("app_ui_tab_ci_width.R", local=TRUE, encoding="UTF8")$value
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
        
        #####---------------------------------------------------------------------------
        ## Range statistics -> Rayleigh sigma
        #####---------------------------------------------------------------------------
        output$range2sigma <- renderPrint({
            rangeStatStats <- as.numeric(strsplit(input$rangeStatStats, "[[:blank:]]")[[1]])
            stat           <- rangeStatInv[input$rangeStatType1]
            n              <- input$rangeStatN
            nGroups        <- input$rangeStatNGroups
            CEPlevel       <- input$rangeStatCEPLevel
            CIlevel        <- input$rangeStatCILevel
            dstTrgt1       <- input$dstTrgt1
            unitDstTrgt1   <- unitsDstInv[input$unitDstTrgt1]
            unitXY1        <- unitsXYInv[input$unitXY1]
            conversion     <- paste0(unitDstTrgt1, "2", unitXY1, collapse="")
            
            if(input$rangeStatSigmaCEP == 1) {
                sigma <- range2sigma(rangeStatStats, stat=stat, n=n, nGroups=nGroups,
                                     CIlevel=CIlevel, collapse=TRUE, dstTarget=dstTrgt1,
                                     conversion=conversion)
                Filter(Negate(is.null), sigma)
            } else {
                CEP <- range2CEP(rangeStatStats, stat=stat, n=n, nGroups=nGroups,
                                 CEPlevel=CEPlevel, CIlevel=CIlevel,
                                 collapse=TRUE, dstTarget=dstTrgt1,
                                 conversion=conversion)
                Filter(Negate(is.null), CEP)
            }
        })
        
        ## save output to file
        # output$saveSigma <- downloadHandler(
        #     filename=function() { "range2sigma.txt" },
        #     content=function(file) {
        #         writeLines(fileName(), con=file)
        #         "ABC"
        #     },
        #     contentType='text/plain' # MIME type
        # )
        
        #####---------------------------------------------------------------------------
        ## Efficiency - required number of groups
        #####---------------------------------------------------------------------------
        output$effNGroups <- renderPrint({
            stat    <- rangeStatSigInv[input$effStatType1]
            n       <- as.numeric(strsplit(input$effN1, "[[:blank:]]")[[1]])
            CIlevel <- input$effCILevel1
            CIwidth <- input$effCIWidth1
            efficiency(n=n, CIlevel=CIlevel, CIwidth=CIwidth, stat=stat)
        })
        # var tips = ['shots per group',
        #             'required number of groups with n shots each (including fractional groups)',
        #             'required number of groups with n shots each (full groups only)',
        #             'required total number of shots, assuming we're shooting groups of size n each (including fractional groups)',
        #             'required total number of shots, assuming we're shooting groups of size n each (full groups only)',
        #             'desired CI level (coverage probability)',
        #             'desired CI width (as fraction of the mean)'],
        #     header = table.columns().header();
        #     for (var i = 0; i < tips.length; i++) {
        #         $(header[i]).attr('title', tips[i]);
        #     }
        # "))
        
        #####---------------------------------------------------------------------------
        ## Efficiency - achievable CI width
        #####---------------------------------------------------------------------------
        output$effCIWidth <- renderPrint({
            stat    <- rangeStatSigInv[input$effStatType2]
            n       <- as.numeric(strsplit(input$effN2, "[[:blank:]]")[[1]])
            nGroups <- input$effNGroups2
            CIlevel <- input$effCILevel2
            out <- efficiency(n=n, nGroups=nGroups, CIlevel=CIlevel, stat=stat)
            out <- cbind(out, E=out[ , "CIwidth"]/2)
            rownames(out) <- NULL
            out
        })
    }
)
