dashboardSidebar(
    id="sidebar",
    skin="light",
    fixed=TRUE,
    minified=TRUE,
    collapsed=FALSE,
    status="primary",
    # brandColor="primary",
    # elevation=1,
    sidebarMenu(
        menuItem(
            "Data",
            tabName="tab_data",
            icon=icon("table", lib="font-awesome")
        ),
        menuItem(
            tagList("Hit probability", icon("arrow-right", lib="font-awesome"), "radius"),
            tabName="tab_radius",
            icon=icon("circle-thin", lib="font-awesome")
        ),
        menuItem(
            tagList("Radius", icon("arrow-right", lib="font-awesome"), "hit probability"),
            tabName="tab_hit_probability",
            icon=icon("circle-thin", lib="font-awesome")
        ),
        menuItem(
            "About",
            tabName="tab_about",
            icon=icon("lightbulb", lib="font-awesome")
        ),
        tagList(br(), p("Current data:")),
        uiOutput("fileInfoShort")
    )
)
