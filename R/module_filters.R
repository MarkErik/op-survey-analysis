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

filterServer <- function(id, data_server = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    selected_section <- reactiveVal("")
    available_sections <- reactiveVal(character(0))

    filtered_data <- reactive({
      tryCatch({
        if (!is.null(data_server)) {
          data <- data_server$getDataReactive()
        } else {
          data <- reactive({
            tibble::tibble()
          })()
        }

        if (!is.null(input$selected_section) && input$selected_section != "") {
          filtered <- dplyr::filter(data, dplyr::all_of(COL_SECTION) == input$selected_section)
          return(filtered)
        }

        return(data)

      }, error = function(e) {
        return(tibble::tibble())
      })
    })

    updateAvailableSections <- function(sections) {
      available_sections(sections)
    }

    updateSelectedSection <- function(section_value) {
      selected_section(section_value)
    }

    resetFilter <- function() {
      selected_section("")
      return(NULL)
    }

    saveFilterState <- function() {
      session$setCustomMessage("save_filter", selected_section())
      return(NULL)
    }

    loadFilterState <- function() {
      observeEvent(session$recvCustomMessage("load_filter"), {
        saved_filter <- session$recvCustomMessage("load_filter")
        if (!is.null(saved_filter) && saved_filter != "") {
          selected_section(saved_filter)
        }
      })
      return(NULL)
    }

    observeEvent(input$selected_section, {
      updateSelectedSection(input$selected_section)
    })

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
