bs4DashSidebar(
    skin="light",
    status="primary",
    brandColor="primary",
    # elevation=1,
    bs4SidebarMenu(
        id="sidebar",
        bs4SidebarMenuItem(
            tagList("Range stats", icon("arrow-right", lib="font-awesome"), "Rayleigh \U03C3"),
            tabName="tab_sigma",
            icon="circle-thin"
        ),
        bs4SidebarMenuItem(
            "Efficiency: # of groups",
            tabName="tab_n_groups",
            icon="circle-thin"
        ),
        bs4SidebarMenuItem(
            "Efficiency: CI width",
            tabName="tab_ci_width",
            icon="circle-thin"
        ),
        bs4SidebarMenuItem(
            "About",
            tabName="tab_about",
            icon="lightbulb"
        )
    )
)
