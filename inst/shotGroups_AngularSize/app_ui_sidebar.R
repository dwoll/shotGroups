dashboardSidebar(
    id="sidebar",
    skin="light",
    fixed=TRUE,
    minified=TRUE,
    collapsed=FALSE,
    status="primary",
    # elevation=1,
    sidebarMenu(
        # id="current_tab",
        flat = FALSE,
        compact = FALSE,
        childIndent = TRUE,
        menuItem(
            "Background math",
            tabName="tab_math",
            icon=icon("square-root-alt", lib="font-awesome")
        ),
        menuItem(
            tagList("Absolute", icon("arrow-right", lib="font-awesome"), "angular"),
            tabName="tab_angular",
            icon = tags$i(class = "far fa-circle")
            # icon = icon("circle-thin", lib="font-awesome")
        ),
        menuItem(
            tagList("Angular", icon("arrow-right", lib="font-awesome"), "absolute"),
            tabName="tab_absolute",
            icon = tags$i(class = "far fa-circle")
            # icon = icon("circle-thin", lib="font-awesome")
        ),
        menuItem(
            tagList("Abs+ang", icon("arrow-right", lib="font-awesome"), "distance"),
            tabName="tab_distance",
            icon = tags$i(class = "far fa-circle")
            # icon = icon("circle-thin", lib="font-awesome")
        ),
        menuItem(
            "About",
            tabName="tab_about",
            icon=icon("lightbulb", lib="font-awesome")
        )
    )
)
