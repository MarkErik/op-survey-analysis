# UI Participant Profile Tab Component

# Create the participant profile tab content
create_participant_profile_tab <- function() {
  tabPanel("Participant Profile",
    fluidRow(
      column(12,
        h3("Participant Profile"),
        p("Click on any response in the table to view the participant's complete profile."),
        
        # UI for participant profile
        uiOutput("profile_ui")
      )
    )
  )
}