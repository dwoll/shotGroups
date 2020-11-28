bs4DashSidebar(
    skin="light",
    status="primary",
    brandColor="primary",
    # elevation=1,
    bs4SidebarMenu(
        id="sidebar",
        bs4SidebarMenuItem(
            "Data",
            tabName="tab_data",
            icon="table"
        ),
        bs4SidebarMenuItem(
            tagList("Hit probability", icon("arrow-right", lib="font-awesome"), "radius"),
            tabName="tab_radius",
            icon="circle-thin"
        ),
        bs4SidebarMenuItem(
            tagList("Radius", icon("arrow-right", lib="font-awesome"), "hit probability"),
            tabName="tab_hit_probability",
            icon="circle-thin"
        ),
        bs4SidebarMenuItem(
            "About",
            tabName="tab_about",
            icon="lightbulb"
        ),
        tagList(br(), p("Current data:")),
        uiOutput("fileInfoShort")
    )
)
