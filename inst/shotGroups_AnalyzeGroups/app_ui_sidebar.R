dashboardSidebar(
    skin="light",
    status="primary",
    # elevation=1,
    sidebarMenu(
        menuItem(
            "Data",
            tabName="tab_data",
            icon=icon("table", lib="font-awesome")
        ),
        menuItem(
            "Group characteristics",
            icon=icon("chart-bar", lib="font-awesome"),
            startExpanded = TRUE,
            menuSubItem(
                text = "Shape",
                tabName="tab_group_shape",
                icon = icon("circle-thin", lib="font-awesome")
            ),
            menuSubItem(
                text = "Precision",
                tabName="tab_group_precision",
                icon = icon("circle-thin", lib="font-awesome")
            ),
            menuSubItem(
                text = "Accuracy",
                tabName="tab_group_accuracy",
                icon = icon("circle-thin", lib="font-awesome")
            ),
            menuSubItem(
                text = "Compare groups",
                tabName="tab_group_compare",
                icon = icon("circle-thin", lib="font-awesome")
            )
        ),
        menuItem(
            "Target plot",
            tabName="tab_target_plot",
            icon=icon("bullseye", lib="font-awesome")
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
