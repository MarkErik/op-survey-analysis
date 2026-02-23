# R/module_filters.R
# Filter module for the CPSC Experience Survey Explorer
# Provides UI and server functions for section filter management

# =============================================================================
# UI Module - Filter Display
# =============================================================================

#' Filter UI module function
#'
#' Creates the section filter UI component for the application.
#' Displays a dropdown selector for filtering data by section.
#'
#' @param id Character module ID for namespacing
#' @return UI element for section filter
#' @export
filterUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(12,
        div(
          class = "filter-container",
          h4("Filter by Section", class = "filter-title"),
          selectInput(
            ns("selected_section"),
            label = NULL,
            choices = c("All Sections" = ""),
            selected = "",
            width = "100%",
            class = "filter-select"
          ),
          helpText("Select a specific section to filter the data display.", class = "filter-help")
        )
      )
    )
  )
}

# =============================================================================
# Server Module - Filter State Management
# =============================================================================

#' Filter module server function
#'
#' Manages filter state and provides reactive values for section selection.
#' Handles filter persistence and reset functionality.
#'
#' @param id Character module ID for namespacing
#' @param data_server Reactive data server module (optional)
#' @return List of reactive values and functions for filter management
#' @export
filterServer <- function(id, data_server = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    # =============================================================================
    # Reactive Values for Filter State
    # =============================================================================

    #' Reactive value for selected section
    #' 
    #' Stores the currently selected section filter value.
    #' Updated when user changes the dropdown selection.
    selected_section <- reactiveVal("")

    #' Reactive value for available sections
    #' 
    #' Stores the list of available sections from the data.
    #' Updated when data is loaded.
    available_sections <- reactiveVal(character(0))

    # =============================================================================
    # Reactive Filter Logic
    # =============================================================================

    #' Reactive expression for filtered data
    #' 
    #' Returns data filtered by the currently selected section.
    #' 
    #' @return Tibble with filtered survey data
    filtered_data <- reactive({
      tryCatch({
        # Get data from data server if provided
        if (!is.null(data_server)) {
          data <- data_server$getDataReactive()
        } else {
          # Fallback: use reactive data from module
          data <- reactive({
            # Placeholder - should be connected to data module
            tibble::tibble()
          })()
        }

        # Apply section filter
        if (!is.null(input$selected_section) && input$selected_section != "") {
          filtered <- dplyr::filter(data, dplyr::all_of(COL_SECTION) == input$selected_section)
          return(filtered)
        }

        return(data)

      }, error = function(e) {
        return(tibble::tibble())
      })
    })

    # =============================================================================
    # Filter Update Logic
    # =============================================================================

    #' Update available sections when data changes
    #' 
    #' Called when data is loaded to populate the filter dropdown.
    #' 
    #' @param sections Character vector of available sections
    updateAvailableSections <- function(sections) {
      available_sections(sections)
    }

    #' Update selected section from input
    #' 
    #' Called when user changes the dropdown selection.
    #' 
    #' @param section_value Character section value
    updateSelectedSection <- function(section_value) {
      selected_section(section_value)
    }

    # =============================================================================
    # Filter Reset Functionality
    # =============================================================================

    #' Reset filter to default state
    #' 
    #' Clears the section filter and resets to "All Sections".
    #' 
    #' @return NULL
    resetFilter <- function() {
      selected_section("")
      return(NULL)
    }

    # =============================================================================
    # Filter Persistence
    # =============================================================================

    #' Save filter state to session storage
    #' 
    #' Persists the current filter selection across page reloads.
    #' 
    #' @return NULL
    saveFilterState <- function() {
      session$setCustomMessage("save_filter", selected_section())
      return(NULL)
    }

    #' Load filter state from session storage
    #' 
    #' Restores the previously saved filter selection on app startup.
    #' 
    #' @return NULL
    loadFilterState <- function() {
      # Listen for saved filter state
      observeEvent(session$recvCustomMessage("load_filter"), {
        saved_filter <- session$recvCustomMessage("load_filter")
        if (!is.null(saved_filter) && saved_filter != "") {
          selected_section(saved_filter)
        }
      })
      return(NULL)
    }

    # =============================================================================
    # Filter Change Observers
    # =============================================================================

    #' Observe changes to selected section
    #' 
    #' Updates the reactive filtered data when section selection changes.
    #' 
    observeEvent(input$selected_section, {
      updateSelectedSection(input$selected_section)
    })

    # =============================================================================
    # Filter UI Output
    # =============================================================================

    #' Render available sections in filter dropdown
    #' 
    #' Updates the choices in the selectInput when sections become available.
    observe({
      secs <- available_sections()
      if (length(secs) > 0) {
        updateSelectInput(
          session,
          "selected_section",
          choices = c("All Sections" = "") %>% append(secs, after = 0),
          selected = selected_section()
        )
      }
    })

    # Return reactive values and functions for use by other modules
    return(list(
      selected_section = selected_section,
      available_sections = available_sections,
      filtered_data = filtered_data,
      updateAvailableSections = updateAvailableSections,
      updateSelectedSection = updateSelectedSection,
      resetFilter = resetFilter,
      saveFilterState = saveFilterState,
      loadFilterState = loadFilterState
    ))
  })
}
