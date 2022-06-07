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
            icon = tags$i(class = "far fa-circle")
            # icon = icon("circle-thin", lib="font-awesome")
        ),
        menuItem(
            tagList("Hit probability", icon("arrow-right", lib="font-awesome"), "radius"),
            tabName="tab_radius",
            icon = tags$i(class = "far fa-circle")
            # icon = icon("circle-thin", lib="font-awesome")
        ),
        menuItem(
            tagList("Radius", icon("arrow-right", lib="font-awesome"), "hit probability"),
            tabName="tab_hit_probability",
            icon = tags$i(class = "far fa-circle")
            # icon = icon("circle-thin", lib="font-awesome")
        ),
        menuItem(
            "About",
            tabName="tab_about",
            icon = tags$i(class = "far fa-circle")
            # icon = icon("circle-thin", lib="font-awesome")
        ),
        tagList(br(), p("Current data:")),
        uiOutput("fileInfoShort")
    )
)
