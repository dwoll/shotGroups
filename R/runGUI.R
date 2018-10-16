runGUI <-
function(app=c("analyze", "hitprob", "range", "angular"), ...) {
    app <- match.arg(toupper(app),
                     choices=c("ANALYZE", "HITPROB", "RANGE", "ANGULAR"),
                     several.ok=FALSE)

    appDir <- if(app == "ANALYZE") {
        system.file("shotGroups_AnalyzeGroups", package="shotGroups")
    } else if(app == "HITPROB") {
        system.file("shotGroups_HitProb", package="shotGroups")
    } else if(app == "RANGE") {
        system.file("shotGroups_RangeStat", package="shotGroups")
    } else if(app == "ANGULAR") {
        system.file("shotGroups_AngularSize", package="shotGroups")
    } else {
    	""
    }

    if(!nzchar(appDir)) {
        stop("Could not find Shiny directory. Try re-installing 'shotGroups'.", call.=FALSE)
    }

    if(requireNamespace("shiny", quietly=TRUE)) {
        shiny::runApp(appDir, ...)
    } else {
        stop("Could not find package shiny - please install first")
    }
}
