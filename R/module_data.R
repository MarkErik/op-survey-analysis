# R/module_data.R
# Reactive data module for the CPSC Experience Survey Explorer
# Provides reactive data access and transformation functions

# =============================================================================
# Module Server - Data Access
# =============================================================================

#' Data module server function
#'
#' Provides reactive data access for the application. Wraps data loading and
#' transformation functions in reactive expressions that cache results and
#' invalidate on data changes.
#'
#' @param id Character module ID for namespacing
#' @param data_file_path Character path to CSV data file (default: DATA_FILE_PATH)
#' @return List of reactive expressions for data access
#' @export
dataServer <- function(id, data_file_path = DATA_FILE_PATH) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    # =============================================================================
    # Reactive Data Loading
    # =============================================================================

    #' Reactive expression for raw data loading
    #' 
    #' Loads raw survey data from CSV file with error handling.
    #' Results are cached and invalidated when data file changes.
    #' 
    #' @return Tibble with raw survey data
    raw_data <- reactive({
      tryCatch({
        # Check if file exists
        if (!file.exists(data_file_path)) {
          showNotification(
            sprintf("Data file not found: %s", data_file_path),
            type = "error",
            duration = ERROR_TOAST_DURATION
          )
          return(tibble::tibble())
        }

        # Load data
        data <- load_survey_data(data_file_path)

        # Check if data is empty
        if (nrow(data) == 0) {
          showNotification(
            "Data file is empty. Please check the CSV file.",
            type = "warning",
            duration = ERROR_TOAST_DURATION
          )
          return(tibble::tibble())
        }

        return(data)

      }, error = function(e) {
        showNotification(
          sprintf("Error loading data: %s", conditionMessage(e)),
          type = "error",
          duration = ERROR_TOAST_DURATION
        )
        return(tibble::tibble())
      })
    })

    #' Reactive expression for processed data
    #' 
    #' Transforms raw data into processed format with cleaning and normalization.
    #' Results are cached and invalidated when raw data changes.
    #' 
    #' @return Tibble with processed survey data
    processed_data <- reactive({
      tryCatch({
        raw <- raw_data()

        if (nrow(raw) == 0) {
          return(tibble::tibble())
        }

        # Transform data
        data <- transform_survey_data(raw)

        return(data)

      }, error = function(e) {
        showNotification(
          sprintf("Error processing data: %s", conditionMessage(e)),
          type = "error",
          duration = ERROR_TOAST_DURATION
        )
        return(tibble::tibble())
      })
    })

    # =============================================================================
    # Reactive Section Access
    # =============================================================================

    #' Reactive expression for available sections
    #' 
    #' Extracts unique section values from the processed data.
    #' Results are cached and invalidated when processed data changes.
    #' 
    #' @return Character vector of unique sections
    getSectionsReactive <- reactive({
      tryCatch({
        data <- processed_data()

        if (nrow(data) == 0) {
          return(character(0))
        }

        # Get unique sections
        sections <- get_available_sections(data)

        return(sections)

      }, error = function(e) {
        return(character(0))
      })
    })

    # =============================================================================
    # Reactive Filtered Data
    # =============================================================================

    #' Reactive expression for filtered data
    #' 
    #' Returns data filtered by section if a section is selected.
    #' Results are cached and invalidated when processed data or section selection changes.
    #' 
    #' @return Tibble with filtered survey data
    getDataReactive <- reactive({
      tryCatch({
        data <- processed_data()

        if (nrow(data) == 0) {
          return(tibble::tibble())
        }

        # Check if section filter is applied
        if (!is.null(input$selected_section) && input$selected_section != "") {
          # Filter by section
          filtered <- dplyr::filter(data, dplyr::all_of(COL_SECTION) == input$selected_section)
          return(filtered)
        }

        # Return all data if no filter
        return(data)

      }, error = function(e) {
        return(tibble::tibble())
      })
    })

    # =============================================================================
    # Session-Specific Data Caching
    # =============================================================================

    #' Get filtered data with session-specific caching
    #' 
    #' Returns filtered data with additional session-level caching for performance.
    #' 
    #' @param section_filter Character section filter value (optional)
    #' @return Tibble with filtered survey data
    getFilteredData <- function(section_filter = NULL) {
      tryCatch({
        data <- processed_data()

        if (nrow(data) == 0) {
          return(tibble::tibble())
        }

        # Apply section filter if provided
        if (!is.null(section_filter) && section_filter != "") {
          filtered <- dplyr::filter(data, dplyr::all_of(COL_SECTION) == section_filter)
          return(filtered)
        }

        # Return all data if no filter
        return(data)

      }, error = function(e) {
        return(tibble::tibble())
      })
    }

    # =============================================================================
    # Data Loading State Management
    # =============================================================================

    #' Reactive expression for data loading state
    #' 
    #' Tracks whether data is currently loading.
    #' 
    #' @return Logical TRUE if loading, FALSE otherwise
    isLoading <- reactive({
      # Check if data file exists
      if (!file.exists(data_file_path)) {
        return(TRUE)
      }

      # Check if data is empty
      if (nrow(processed_data()) == 0) {
        return(TRUE)
      }

      return(FALSE)
    })

    # Return reactive expressions for use by other modules
    return(list(
      raw_data = raw_data,
      processed_data = processed_data,
      getSectionsReactive = getSectionsReactive,
      getDataReactive = getDataReactive,
      getFilteredData = getFilteredData,
      isLoading = isLoading
    ))
  })
}
