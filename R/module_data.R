dataServer <- function(id, data_file_path = DATA_FILE_PATH) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    isLoading <- reactive({
      if (!file.exists(data_file_path)) {
        return(TRUE)
      }

      if (nrow(processed_data()) == 0) {
        return(TRUE)
      }

      return(FALSE)
    })

    dataLoadStatus <- reactive({
      if (!file.exists(data_file_path)) {
        return(list(
          status = "error",
          message = "Data file not found",
          error_type = "data_not_found",
          data_loaded = FALSE,
          data_rows = 0
        ))
      }

      if (!file.access(data_file_path, 4)) {
        return(list(
          status = "error",
          message = "Data file is not readable",
          error_type = "data_loading",
          data_loaded = FALSE,
          data_rows = 0
        ))
      }

      data <- tryCatch({
        load_survey_data(data_file_path)
      }, error = function(e) {
        log_error("data_load_failed", conditionMessage(e))
        return(NULL)
      })

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

    raw_data <- reactive({
      status <- dataLoadStatus()
      
      if (status$status != "ok") {
        show_error_notification(status$error_type)
        return(tibble::tibble())
      }
      
      return(status$data)
    })

    processed_data <- reactive({
      status <- dataLoadStatus()
      
      if (status$status != "ok") {
        return(tibble::tibble())
      }
      
      data <- tryCatch({
        transform_survey_data(status$data)
      }, error = function(e) {
        log_error("data_transform_failed", conditionMessage(e))
        show_error_notification("data_processing")
        return(tibble::tibble())
      })
      
      return(data)
    })

    getSectionsReactive <- reactive({
      status <- dataLoadStatus()
      
      if (status$status != "ok") {
        return(character(0))
      }
      
      sections <- tryCatch({
        get_available_sections(status$data)
      }, error = function(e) {
        log_error("sections_extraction_failed", conditionMessage(e))
        return(character(0))
      })
      
      return(sections)
    })

    getDataReactive <- reactive({
      status <- dataLoadStatus()
      
      if (status$status != "ok") {
        return(tibble::tibble())
      }
      
      if (!is.null(input$selected_section) && input$selected_section != "") {
        if (!validate_inputs(input$selected_section, "options", getSectionsReactive())) {
          return(tibble::tibble())
        }
        
        filtered <- tryCatch({
          dplyr::filter(status$data, dplyr::all_of(COL_SECTION) == input$selected_section)
        }, error = function(e) {
          log_error("filtering_failed", conditionMessage(e))
          return(tibble::tibble())
        })
        
        return(filtered)
      }

      return(status$data)
    })

    getFilteredData <- function(section_filter = NULL) {
      status <- dataLoadStatus()
      
      if (status$status != "ok") {
        return(tibble::tibble())
      }
      
      if (!is.null(section_filter) && section_filter != "") {
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

      return(status$data)
    }

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
