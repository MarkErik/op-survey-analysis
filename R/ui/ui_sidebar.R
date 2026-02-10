# UI Sidebar Component

# Create the sidebar navigation panel
# Note: Free-text question navigation has been moved to Question Responses tab
create_sidebar_panel <- function(free_text_questions) {
  div(class = "sidebar-panel",
    div(class = "sidebar-header",
      h3("Navigation"),
      p("Use the tabs above to navigate the application", class = "sidebar-description")
    )
  )
}