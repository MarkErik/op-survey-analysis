# UI Header Component

# Create the application header
create_app_header <- function() {
  div(class = "app-header",
    div(class = "header-content",
      h1("Survey Explorer", class = "app-title"),
      p("Explore survey responses with interactive analysis", class = "app-subtitle")
    )
  )
}