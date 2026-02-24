# R/module_data.R
# Reactive data module for the CPSC Experience Survey Explorer
# Provides reactive data access and transformation functions with loading states

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
    # Data Loading State Management
    # =============================================================================

    #' Reactive expression for data loading state
    #'
    #' Tracks whether data is currently loading and provides feedback to users.
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

    #' Reactive expression for data load status
    #'
    #' Provides detailed status information about data loading.
    #'
    #' @return List with status, message, and data info
    dataLoadStatus <- reactive({
      # Check if file exists
      if (!file.exists(data_file_path)) {
        return(list(
          status = "error",
          message = "Data file not found",
          error_type = "data_not_found",
          data_loaded = FALSE,
          data_rows = 0
        ))
      }

      # Check if file is readable
      if (!file.access(data_file_path, 4)) {
        return(list(
          status = "error",
          message = "Data file is not readable",
          error_type = "data_loading",
          data_loaded = FALSE,
          data_rows = 0
        ))
      }

      # Load data
      data <- tryCatch({
        load_survey_data(data_file_path)
      }, error = function(e) {
        log_error("data_load_failed", conditionMessage(e))
        return(NULL)
      })

      # Check if data is empty
      if (is.null(data) || nrow(data) == 0) {
        return(list(
          status = "warning",
          message = "Data file is empty",
          error_type = "data_empty",
          data_loaded = FALSE,
          data_rows = 0
        ))
      }

      return(list(
        status = "ok",
        message = "Data loaded successfully",
        error_type = "none",
        data_loaded = TRUE,
        data_rows = nrow(data)
      ))
    })

    # =============================================================================
    # Reactive Data Loading
    # =============================================================================

    #' Reactive expression for raw data loading
    #'
    #' Loads raw survey data from CSV file with error handling and loading states.
    #' Results are cached and invalidated when data file changes.
    #'
    #' @return Tibble with raw survey data
    raw_data <- reactive({
      # Check data status first
      status <- dataLoadStatus()
      
      if (status$status != "ok") {
        show_error_notification(status$error_type)
        return(tibble::tibble())
      }
      
      return(status$data)
    })

    #' Reactive expression for processed data
    #'
    #' Transforms raw data into processed format with cleaning and normalization.
    #' Results are cached and invalidated when raw data changes.
    #'
    #' @return Tibble with processed survey data
    processed_data <- reactive({
      # Check data status first
      status <- dataLoadStatus()
      
      if (status$status != "ok") {
        return(tibble::tibble())
      }
      
      # Transform data
      data <- tryCatch({
        transform_survey_data(status$data)
      }, error = function(e) {
        log_error("data_transform_failed", conditionMessage(e))
        show_error_notification("data_processing")
        return(tibble::tibble())
      })
      
      return(data)
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
      # Check data status first
      status <- dataLoadStatus()
      
      if (status$status != "ok") {
        return(character(0))
      }
      
      # Get unique sections
      sections <- tryCatch({
        get_available_sections(status$data)
      }, error = function(e) {
        log_error("sections_extraction_failed", conditionMessage(e))
        return(character(0))
      })
      
      return(sections)
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
      # Check data status first
      status <- dataLoadStatus()
      
      if (status$status != "ok") {
        return(tibble::tibble())
      }
      
      # Check if section filter is applied
      if (!is.null(input$selected_section) && input$selected_section != "") {
        # Validate section filter
        if (!validate_inputs(input$selected_section, "options", getSectionsReactive())) {
          return(tibble::tibble())
        }
        
        # Filter by section
        filtered <- tryCatch({
          dplyr::filter(status$data, dplyr::all_of(COL_SECTION) == input$selected_section)
        }, error = function(e) {
          log_error("filtering_failed", conditionMessage(e))
          return(tibble::tibble())
        })
        
        return(filtered)
      }

      # Return all data if no filter
      return(status$data)
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
      # Check data status first
      status <- dataLoadStatus()
      
      if (status$status != "ok") {
        return(tibble::tibble())
      }
      
      # Apply section filter if provided
      if (!is.null(section_filter) && section_filter != "") {
        # Validate section filter
        if (!validate_inputs(section_filter, "options", getSectionsReactive())) {
          return(tibble::tibble())
        }
        
        filtered <- tryCatch({
          dplyr::filter(status$data, dplyr::all_of(COL_SECTION) == section_filter)
        }, error = function(e) {
          log_error("filtering_failed", conditionMessage(e))
          return(tibble::tibble())
        })
        
        return(filtered)
      }

      # Return all data if no filter
      return(status$data)
    })

    # Return reactive expressions for use by other modules
    return(list(
      raw_data = raw_data,
      processed_data = processed_data,
      getSectionsReactive = getSectionsReactive,
      getDataReactive = getDataReactive,
      getFilteredData = getFilteredData,
      isLoading = isLoading,
      dataLoadStatus = dataLoadStatus
    ))
  })
}
