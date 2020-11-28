bs4DashSidebar(
    skin="light",
    status="primary",
    brandColor="primary",
    # elevation=1,
    bs4SidebarMenu(
        id="sidebar",
        bs4SidebarMenuItem(
            "Background math",
            tabName="tab_math",
            icon="square-root-alt"
        ),
        bs4SidebarMenuItem(
            tagList("Absolute", icon("arrow-right", lib="font-awesome"), "angular"),
            tabName="tab_angular",
            icon="circle-thin"
        ),
        bs4SidebarMenuItem(
            tagList("Angular", icon("arrow-right", lib="font-awesome"), "absolute"),
            tabName="tab_absolute",
            icon="circle-thin"
        ),
        bs4SidebarMenuItem(
            tagList("Abs+ang", icon("arrow-right", lib="font-awesome"), "distance"),
            tabName="tab_distance",
            icon="circle-thin"
        ),
        bs4SidebarMenuItem(
            "About",
            tabName="tab_about",
            icon="lightbulb"
        )
    )
)
