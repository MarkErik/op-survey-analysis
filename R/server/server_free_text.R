# Free Text Tab Server Logic
# Server-side logic for the Free Text tab including reactive filtering,
# plot rendering, participant profile modal, and insights generation

#' Setup Free Text Tab Server
#'
#' Configures all reactive values, observers, and render functions for the Free Text tab.
#' This function should be called within the main server function.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param df Reactive expression returning the survey data
#' @param free_text_questions A character vector of free text question column names
#' @export
setup_free_text_tab <- function(input, output, session, df, free_text_questions) {
  ns <- session$ns
  
  # Reactive values for filters and participant selection
  rv <- reactiveValues(
    section_filter = "all",
    experience_filter = "all",
    search_term = "",
    selected_participant = NULL,
    participant_list = NULL
  )
  
  # Observe section filter changes
  observeEvent(input$free_text_section_filter, {
    req(input$free_text_section_filter)
    rv$section_filter <- input$free_text_section_filter
  })
  
  # Observe experience filter changes
  observeEvent(input$free_text_experience_filter, {
    req(input$free_text_experience_filter)
    rv$experience_filter <- input$free_text_experience_filter
  })
  
  # Observe search term changes with debounce
  observeEvent(input$free_text_search, {
    rv$search_term <- input$free_text_search
  }, ignoreInit = TRUE)
  
  # Reactive filtered data based on filters
  filtered_data <- reactive({
    data <- df()
    
    if (is.null(data)) {
      return(NULL)
    }
    
    # Apply section filter
    if (!is.null(rv$section_filter) && rv$section_filter != "all") {
      data <- data[data$section == rv$section_filter, ]
    }
    
    # Apply experience filter
    if (!is.null(rv$experience_filter) && rv$experience_filter != "all") {
      exp_mapping <- list(
        "none" = "No experience at all",
        "some" = "Took programming course before (either in school, or online tutorials)",
        "high" = "Highly experienced (comfortable writing own programs)"
      )
      data <- data[data$prior_programming_experience == exp_mapping[[rv$experience_filter]], ]
    }
    
    return(data)
  })
  
  # Reactive responses for selected question
  question_responses <- reactive({
    data <- filtered_data()
    selected_question <- input$free_text_question
    
    if (is.null(data) || is.null(selected_question)) {
      return(NULL)
    }
    
    if (!selected_question %in% names(data)) {
      return(NULL)
    }
    
    responses <- data %>%
      filter(!is.na(!!sym(selected_question)) & !!sym(selected_question) != "") %>%
      select(
        participant_id,
        section,
        prior_programming_experience,
        learning_preference,
        all_of(selected_question)
      )
    
    # Apply search filter
    if (rv$search_term != "") {
      responses <- responses %>%
        filter(grepl(rv$search_term, !!sym(selected_question), ignore.case = TRUE))
    }
    
    return(responses)
  })
  
  # Update participant list for selector
  observe({
    data <- df()
    if (!is.null(data)) {
      participant_choices <- data %>%
        arrange(participant_id) %>%
        mutate(
          label = paste0(participant_id, " - ", section, " (",
                        case_when(
                          prior_programming_experience == "No experience at all" ~ "No Exp",
                          prior_programming_experience == "Took programming course before (either in school, or online tutorials)" ~ "Some Exp",
                          prior_programming_experience == "Highly experienced (comfortable writing own programs)" ~ "High Exp",
                          TRUE ~ "Unknown"
                        ), ")")
        ) %>%
        pull(label, participant_id)
      
      updateSelectInput(session, "participant_selector", 
                       choices = c("Select a participant..." = "", participant_choices))
      rv$participant_list <- data$participant_id
    }
  })
  
  # Render response statistics
  output$free_text_total_responses <- renderText({
    responses <- question_responses()
    if (is.null(responses)) {
      return("0")
    }
    format(nrow(responses), big.mark = ",")
  })
  
  output$free_text_response_rate <- renderText({
    data <- filtered_data()
    responses <- question_responses()
    if (is.null(data) || is.null(responses)) {
      return("0%")
    }
    rate <- round(nrow(responses) / nrow(data) * 100, 1)
    paste0(rate, "%")
  })
  
  output$free_text_avg_words <- renderText({
    responses <- question_responses()
    if (is.null(responses) || nrow(responses) == 0) {
      return("0")
    }
    
    selected_question <- input$free_text_question
    word_counts <- sapply(responses[[selected_question]], function(text) {
      length(strsplit(text, "\\s+")[[1]])
    })
    
    round(mean(word_counts), 1)
  })
  
  # Render response list
  output$free_text_response_list <- renderUI({
    responses <- question_responses()
    selected_question <- input$free_text_question
    
    if (is.null(responses) || nrow(responses) == 0) {
      return(div(
        class = "no-responses",
        p("No responses found for the selected filters.")
      ))
    }
    
    # Create response cards
    response_cards <- lapply(seq_len(nrow(responses)), function(i) {
      resp <- responses[i, ]
      response_text <- resp[[selected_question]]
      word_count <- length(strsplit(response_text, "\\s+")[[1]])
      
      div(
        class = "response-card",
        div(
          class = "response-header",
          div(
            class = "response-meta",
            span(class = "participant-id", resp$participant_id),
            span(class = "section", resp$section),
            span(class = "word-count", paste0(word_count, " words"))
          ),
          actionButton(
            ns(paste0("view_profile_", resp$participant_id)),
            icon("user"),
            class = "btn-icon btn-view-profile",
            title = "View Participant Profile"
          )
        ),
        div(
          class = "response-text",
          p(response_text)
        )
      )
    })
    
    do.call(tagList, response_cards)
  })
  
  # Handle view profile button clicks
  observeEvent(input$view_participant_profile, {
    selected <- input$participant_selector
    if (selected != "") {
      rv$selected_participant <- selected
    }
  })
  
  # Handle individual view profile buttons
  observe({
    participant_list <- rv$participant_list
    if (!is.null(participant_list)) {
      for (pid in participant_list) {
        local({
          participant_id <- pid
          observeEvent(input[[paste0("view_profile_", participant_id)]], {
            rv$selected_participant <- participant_id
          })
        })
      }
    }
  })
  
  # Participant profile modal
  observeEvent(rv$selected_participant, {
    req(rv$selected_participant)
    
    showModal(modalDialog(
      title = paste("Participant Profile:", rv$selected_participant),
      size = "l",
      footer = tagList(
        actionButton(ns("prev_participant"), icon("arrow-left"), "Previous"),
        actionButton(ns("next_participant"), "Next", icon("arrow-right")),
        modalButton("Close")
      ),
      uiOutput(ns("participant_profile_content"))
    ))
  })
  
  # Navigate to previous participant
  observeEvent(input$prev_participant, {
    req(rv$participant_list)
    current_idx <- which(rv$participant_list == rv$selected_participant)
    if (current_idx > 1) {
      rv$selected_participant <- rv$participant_list[current_idx - 1]
    }
  })
  
  # Navigate to next participant
  observeEvent(input$next_participant, {
    req(rv$participant_list)
    current_idx <- which(rv$participant_list == rv$selected_participant)
    if (current_idx < length(rv$participant_list)) {
      rv$selected_participant <- rv$participant_list[current_idx + 1]
    }
  })
  
  # Render participant profile content
  output$participant_profile_content <- renderUI({
    participant_id <- rv$selected_participant
    data <- df()
    
    req(participant_id)
    req(data)
    
    participant_data <- data[data$participant_id == participant_id, ]
    if (nrow(participant_data) == 0) {
      return(p("Participant not found."))
    }
    
    p <- participant_data[1, ]
    
    # Define all question columns by category
    context_cols <- c("section", "prior_programming_experience", "learning_preference")
    course_agreement_cols <- c(
      "how_much_do_you_agree_with_the_statement_1",
      "how_much_do_you_agree_with_the_statement_2",
      "how_much_do_you_agree_with_the_statement_3",
      "how_much_do_you_agree_with_the_statement_4",
      "how_much_do_you_agree_with_the_statement_5",
      "how_much_do_you_agree_with_the_statement_6"
    )
    learning_cols <- c(
      "how_much_do_the_following_elements_contribute_to_your_learning_1",
      "how_much_do_the_following_elements_contribute_to_your_learning_2",
      "how_much_do_the_following_elements_contribute_to_your_learning_3",
      "how_much_do_the_following_elements_contribute_to_your_learning_4",
      "how_much_do_the_following_elements_contribute_to_your_learning_5",
      "how_much_do_the_following_elements_contribute_to_your_learning_6",
      "how_much_do_the_following_elements_contribute_to_your_learning_7",
      "how_much_do_the_following_elements_contribute_to_your_learning_8",
      "how_much_do_the_following_elements_contribute_to_your_learning_9",
      "how_much_do_the_following_elements_contribute_to_your_learning_10",
      "how_much_do_the_following_elements_contribute_to_your_learning_11"
    )
    community_cols <- c(
      "how_much_do_you_agree_with_the_following_statements_1",
      "how_much_do_you_agree_with_the_following_statements_2",
      "how_much_do_you_agree_with_the_following_statements_3",
      "how_much_do_you_agree_with_the_following_statements_4",
      "how_much_do_you_agree_with_the_following_statements_5"
    )
    
    # Get free text columns (those not in the above lists and not metadata)
    all_cols <- names(data)
    metadata_cols <- c("participant_id", "course_number", "section_time", "timestamp")
    likert_cols <- c(course_agreement_cols, learning_cols, community_cols)
    free_text_cols <- setdiff(all_cols, c(metadata_cols, context_cols, likert_cols))
    
    # Helper function to create Likert response display
    create_likert_display <- function(value) {
      if (is.na(value)) return(span(class = "no-response", "Not answered"))
      
      labels <- c("1" = "Strongly Disagree", "2" = "Disagree", 
                  "3" = "Neutral", "4" = "Agree", "5" = "Strongly Agree")
      label <- labels[as.character(value)]
      color_class <- case_when(
        value == 1 ~ "strongly-disagree",
        value == 2 ~ "disagree",
        value == 3 ~ "neutral",
        value == 4 ~ "agree",
        value == 5 ~ "strongly-agree",
        TRUE ~ "neutral"
      )
      span(class = paste("likert-response", color_class), label)
    }
    
    # Build profile sections
    profile_sections <- list()
    
    # Context section
    profile_sections$context <- div(
      class = "profile-section",
      h4("Context"),
      div(
        class = "profile-row",
        strong("Section: "), p(p$section)
      ),
      div(
        class = "profile-row",
        strong("Experience: "), p(p$prior_programming_experience)
      ),
      div(
        class = "profile-row",
        strong("Learning Preference: "), p(p$learning_preference)
      )
    )
    
    # Course agreement section
    profile_sections$course <- div(
      class = "profile-section",
      h4("Course Agreement Statements"),
      lapply(course_agreement_cols, function(col) {
        if (col %in% names(p) && !is.na(p[[col]])) {
          div(
            class = "profile-row",
            div(class = "question-text", get_column_display_name(col)),
            create_likert_display(p[[col]])
          )
        }
      })
    )
    
    # Learning elements section
    profile_sections$learning <- div(
      class = "profile-section",
      h4("Learning Elements Contribution"),
      lapply(learning_cols, function(col) {
        if (col %in% names(p) && !is.na(p[[col]])) {
          div(
            class = "profile-row",
            div(class = "question-text", get_column_display_name(col)),
            create_likert_display(p[[col]])
          )
        }
      })
    )
    
    # Community section
    profile_sections$community <- div(
      class = "profile-section",
      h4("Community & Belonging"),
      lapply(community_cols, function(col) {
        if (col %in% names(p) && !is.na(p[[col]])) {
          div(
            class = "profile-row",
            div(class = "question-text", get_column_display_name(col)),
            create_likert_display(p[[col]])
          )
        }
      })
    )
    
    # Free text section (only show answered questions)
    answered_free_text <- free_text_cols[sapply(free_text_cols, function(col) {
      col %in% names(p) && !is.na(p[[col]]) && p[[col]] != ""
    })]
    
    if (length(answered_free_text) > 0) {
      profile_sections$free_text <- div(
        class = "profile-section",
        h4("Free Text Responses"),
        lapply(answered_free_text, function(col) {
          div(
            class = "profile-row free-text-row",
            div(class = "question-text", get_column_display_name(col)),
            div(class = "free-text-response", p(p[[col]]))
          )
        })
      )
    }
    
    do.call(tagList, profile_sections)
  })
  
  # Render question response counts plot
  output$free_text_question_counts <- renderPlot({
    data <- df()
    if (is.null(data)) {
      return(NULL)
    }
    result <- generate_question_response_counts_plot(data, free_text_questions)
    result$plot
  })
  
  # Render word cloud for selected question
  output$free_text_wordcloud <- renderPlot({
    responses <- question_responses()
    selected_question <- input$free_text_question
    
    if (is.null(responses) || is.null(selected_question)) {
      return(NULL)
    }
    
    result <- generate_response_wordcloud(responses, selected_question)
    result$plot
  })
  
  # Render themes plot for selected question
  output$free_text_themes <- renderPlot({
    responses <- question_responses()
    selected_question <- input$free_text_question
    
    if (is.null(responses) || is.null(selected_question)) {
      return(NULL)
    }
    
    result <- generate_response_themes_plot(responses, selected_question)
    result$plot
  })
  
  # Render sentiment plot for selected question
  output$free_text_sentiment <- renderPlot({
    responses <- question_responses()
    selected_question <- input$free_text_question
    
    if (is.null(responses) || is.null(selected_question)) {
      return(NULL)
    }
    
    result <- generate_response_sentiment_plot(responses, selected_question)
    result$plot
  })
  
  # Generate and render insights
  output$free_text_insights <- renderUI({
    data <- filtered_data()
    if (is.null(data)) {
      return(div(class = "insights-empty", p("No data available for insights.")))
    }
    
    insights <- generate_free_text_insights(data, free_text_questions)
    
    insights_list <- lapply(insights, function(insight) {
      div(
        class = "insight-item",
        div(class = "insight-type", insight$type),
        div(class = "insight-content", insight$content)
      )
    })
    
    do.call(tagList, insights_list)
  })
}
