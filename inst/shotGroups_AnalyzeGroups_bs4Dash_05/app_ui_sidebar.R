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
            "Group characteristics",
            icon="chart-bar",
            startExpanded = TRUE,
            bs4SidebarMenuSubItem(
                text = "Shape",
                tabName="tab_group_shape",
                icon = "circle-thin"
            ),
            bs4SidebarMenuSubItem(
                text = "Precision",
                tabName="tab_group_precision",
                icon = "circle-thin"
            ),
            bs4SidebarMenuSubItem(
                text = "Accuracy",
                tabName="tab_group_accuracy",
                icon = "circle-thin"
            ),
            bs4SidebarMenuSubItem(
                text = "Compare groups",
                tabName="tab_group_compare",
                icon = "circle-thin"
            )
        ),
        bs4SidebarMenuItem(
            "Target plot",
            tabName="tab_target_plot",
            icon="bullseye"
        ),
        bs4SidebarMenuItem(
            "About",
            tabName="tab_about",
            icon="lightbulb"
        ),
        uiOutput("fileInfoShort")
    )
)
