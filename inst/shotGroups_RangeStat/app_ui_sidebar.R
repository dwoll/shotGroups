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
            tagList("Range stats", icon("arrow-right", lib="font-awesome"), "Rayleigh \\(\\sigma\\)"),
            tabName="tab_sigma",
            icon=icon("circle-thin", lib="font-awesome")
        ),
        menuItem(
            "Efficiency: # of groups",
            tabName="tab_n_groups",
            icon=icon("circle-thin", lib="font-awesome")
        ),
        menuItem(
            "Efficiency: CI width",
            tabName="tab_ci_width",
            icon=icon("circle-thin", lib="font-awesome")
        ),
        menuItem(
            "About",
            tabName="tab_about",
            icon=icon("lightbulb", lib="font-awesome")
        )
    )
)
