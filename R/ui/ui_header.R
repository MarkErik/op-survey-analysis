# UI Header Component

# Create the application header
create_app_header <- function() {
  div(class = "app-header",
    div(class = "header-content",
      h1("Online Pathways - Survey Analysis", class = "app-title"),
      p("Responses collected anonymously between November 24 - December 24, 2025", class = "app-subtitle")
    )
  )
}