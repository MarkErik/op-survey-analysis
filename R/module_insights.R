insightsUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(12,
        div(
          class = "insights-navigation",
          h3("Advanced Analytics", class = "section-title"),
          div(
            class = "analysis-selector",
            selectInput(
              ns("analysis_type"),
              label = "Select Analysis Type:",
              choices = c(
                "Correlation Analysis" = "correlation",
                "Regression Analysis" = "regression",
                "Student Segmentation" = "segmentation",
                "Section Comparison" = "section_comparison",
                "Effect Size Analysis" = "effect_size",
                "Reliability Analysis" = "reliability",
                "Interaction Analysis" = "interaction",
                "Satisfaction Predictors" = "satisfaction_predictors"
              ),
              selected = "correlation"
            )
          )
        )
      )
    ),

    fluidRow(
      column(12,
        div(
          class = "loading-indicator",
          div(
            id = "insights_loading_spinner",
            style = "display: none;",
            class = "spinner",
            icon("spinner", spin = TRUE, lib = "font-awesome"),
            span("Analyzing...")
          )
        )
      )
    ),

    fluidRow(
      column(12,
        div(
          class = "analysis-content",
          div(
            id = ns("panel_correlation"),
            class = "analysis-panel",
            h4("Correlation Matrix", class = "panel-title"),
            plotOutput(ns("correlation_heatmap"),
              height = "500px"
            ),
            DT::dataTableOutput(ns("correlation_insights"))
          )
        )
      )
    ),

    fluidRow(
      column(12,
        div(
          id = ns("panel_regression"),
          class = "analysis-panel",
          h4("Regression Analysis", class = "panel-title"),
          DT::dataTableOutput(ns("regression_predictors"))
        )
      )
    ),

    fluidRow(
      column(12,
        div(
          id = ns("panel_segmentation"),
          class = "analysis-panel",
          h4("Student Segmentation", class = "panel-title"),
          DT::dataTableOutput(ns("cluster_summary")),
          DT::dataTableOutput(ns("cluster_profiles"))
        )
      )
    ),

    fluidRow(
      column(12,
        div(
          id = ns("panel_section_comparison"),
          class = "analysis-panel",
          h4("Section Comparison", class = "panel-title"),
          DT::dataTableOutput(ns("section_comparison_table")),
          DT::dataTableOutput(ns("effect_size_table"))
        )
      )
    ),

    fluidRow(
      column(12,
        div(
          id = ns("panel_effect_size"),
          class = "analysis-panel",
          h4("Effect Size Analysis", class = "panel-title"),
          DT::dataTableOutput(ns("effect_size_table"))
        )
      )
    ),

    fluidRow(
      column(12,
        div(
          id = ns("panel_reliability"),
          class = "analysis-panel",
          h4("Reliability Analysis", class = "panel-title"),
          DT::dataTableOutput(ns("cronbach_alpha_table"))
        )
      )
    ),

    fluidRow(
      column(12,
        div(
          id = ns("panel_interaction"),
          class = "analysis-panel",
          h4("Interaction Analysis", class = "panel-title"),
          DT::dataTableOutput(ns("interaction_effects"))
        )
      )
    ),

    fluidRow(
      column(12,
        div(
          id = ns("panel_satisfaction_predictors"),
          class = "analysis-panel",
          h4("Satisfaction Predictors", class = "panel-title"),
          DT::dataTableOutput(ns("satisfaction_drivers"))
        )
      )
    )
  )
}

insightsServer <- function(id, data_server = NULL, filter_server = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    filtered_data <- reactive({
      tryCatch({
        if (!is.null(data_server)) {
          data_server$getDataReactive()
        } else {
          reactive({
            tibble::tibble()
          })()
        }
      }, error = function(e) {
        return(tibble::tibble())
      })
    })

    selected_analysis <- reactiveVal("correlation")

    observeEvent(input$analysis_type, {
      selected_analysis(input$analysis_type)
      shinyjs::hide(c(
        "panel_correlation", "panel_regression", "panel_segmentation",
        "panel_section_comparison", "panel_effect_size", "panel_reliability",
        "panel_interaction", "panel_satisfaction_predictors"
      ))
      shinyjs::show(paste0("panel_", input$analysis_type))
    })

    correlation_matrix <- reactive({
      tryCatch({
        data <- filtered_data()
        
        if (nrow(data) == 0) {
          return(NULL)
        }

        likert_data <- data %>%
          dplyr::select(
            dplyr::all_of(COL_CONTENT_RELEVANT),
            dplyr::all_of(COL_EXCITED_CONTENT),
            dplyr::all_of(COL_SATISFIED_FEEDBACK),
            dplyr::all_of(COL_APPLY_LEARNING),
            dplyr::all_of(COL_EASY_ASK_HELP),
            dplyr::all_of(COL_MEETING_GOALS),
            dplyr::all_of(COL_PRE_WRITTEN_CODE),
            dplyr::all_of(COL_STUDYING_MIDTERMS),
            dplyr::all_of(COL_TOPHAT_QUIZZES),
            dplyr::all_of(COL_PRESENTATION_SLIDES),
            dplyr::all_of(COL_HANDOUTS_NOTES),
            dplyr::all_of(COL_CODING_OWN),
            dplyr::all_of(COL_LIVE_CODING),
            dplyr::all_of(COL_LABS),
            dplyr::all_of(COL_ASK_QUESTIONS),
            dplyr::all_of(COL_ASSIGNMENTS),
            dplyr::all_of(COL_COMFORTABLE_SPEAKING),
            dplyr::all_of(COL_PART_OF_CLASS),
            dplyr::all_of(COL_FRIENDS_IMPORTANT),
            dplyr::all_of(COL_UNIVERSITY_COMMUNITY),
            dplyr::all_of(COL_EASY_MEET_PEOPLE)
          ) %>%
          dplyr::mutate(
            content_relevant = extract_likert_value(dplyr::all_of(COL_CONTENT_RELEVANT)),
            excited_content = extract_likert_value(dplyr::all_of(COL_EXCITED_CONTENT)),
            satisfied_feedback = extract_likert_value(dplyr::all_of(COL_SATISFIED_FEEDBACK)),
            apply_learning = extract_likert_value(dplyr::all_of(COL_APPLY_LEARNING)),
            easy_ask_help = extract_likert_value(dplyr::all_of(COL_EASY_ASK_HELP)),
            meeting_goals = extract_likert_value(dplyr::all_of(COL_MEETING_GOALS)),
            pre_written_code = extract_likert_value(dplyr::all_of(COL_PRE_WRITTEN_CODE)),
            studying_midterms = extract_likert_value(dplyr::all_of(COL_STUDYING_MIDTERMS)),
            tophat_quizzes = extract_likert_value(dplyr::all_of(COL_TOPHAT_QUIZZES)),
            presentation_slides = extract_likert_value(dplyr::all_of(COL_PRESENTATION_SLIDES)),
            handouts_notes = extract_likert_value(dplyr::all_of(COL_HANDOUTS_NOTES)),
            coding_own = extract_likert_value(dplyr::all_of(COL_CODING_OWN)),
            live_coding = extract_likert_value(dplyr::all_of(COL_LIVE_CODING)),
            labs = extract_likert_value(dplyr::all_of(COL_LABS)),
            ask_questions = extract_likert_value(dplyr::all_of(COL_ASK_QUESTIONS)),
            assignments = extract_likert_value(dplyr::all_of(COL_ASSIGNMENTS)),
            comfortable_speaking = extract_likert_value(dplyr::all_of(COL_COMFORTABLE_SPEAKING)),
            part_of_class = extract_likert_value(dplyr::all_of(COL_PART_OF_CLASS)),
            friends_important = extract_likert_value(dplyr::all_of(COL_FRIENDS_IMPORTANT)),
            university_community = extract_likert_value(dplyr::all_of(COL_UNIVERSITY_COMMUNITY)),
            easy_meet_people = extract_likert_value(dplyr::all_of(COL_EASY_MEET_PEOPLE))
          ) %>%
          dplyr::filter(!dplyr::any_of(dplyr::c(
            content_relevant, excited_content, satisfied_feedback,
            apply_learning, easy_ask_help, meeting_goals,
            pre_written_code, studying_midterms, tophat_quizzes,
            presentation_slides, handouts_notes, coding_own,
            live_coding, labs, ask_questions, assignments,
            comfortable_speaking, part_of_class, friends_important,
            university_community, easy_meet_people
          )))

        if (nrow(likert_data) == 0) {
          return(NULL)
        }

        cor_matrix <- cor(likert_data, use = "pairwise.complete.obs")

        return(cor_matrix)

      }, error = function(e) {
        return(NULL)
      })
    })

    output$correlation_heatmap <- renderGirafe({
      cor_mat <- correlation_matrix()
      
      if (is.null(cor_mat) || nrow(cor_mat) == 0) {
        girafe(
          ggplot2::ggplot() +
            ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No data available for correlation analysis", size = 5) +
            ggplot2::theme_void(),
          width = 100,
          height = 100
        )
        return()
      }

      question_names <- c(
        "Content Relevant", "Excited Content", "Satisfied Feedback",
        "Apply Learning", "Easy Ask Help", "Meeting Goals",
        "Pre-written Code", "Studying Midterms", "TopHat Quizzes",
        "Presentation Slides", "Handouts/Notes", "Coding Own",
        "Live Coding", "Labs", "Ask Questions", "Assignments",
        "Comfortable Speaking", "Part of Class", "Friends Important",
        "University Community", "Easy Meet People"
      )

      color_palette <- colorRampPalette(c("#DC3545", "#FFFFFF", "#28A745"))(100)

      p <- ggplot2::ggplot() +
        ggplot2::geom_tile(
          data = as.data.frame(cor_mat),
          ggplot2::aes(x = Var1, y = Var2, fill = value)
        ) +
        ggplot2::scale_fill_gradient2(
          low = "#DC3545",
          mid = "#FFFFFF",
          high = "#28A745",
          midpoint = 0,
          na.value = "#CCCCCC"
        ) +
        ggplot2::scale_x_discrete(labels = question_names) +
        ggplot2::scale_y_discrete(labels = question_names) +
        ggplot2::labs(
          title = "Correlation Matrix",
          x = "",
          y = ""
        ) +
        ggplot2::theme_void() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
          axis.text.y = ggplot2::element_text(size = 8),
          panel.grid = ggplot2::element_blank()
        )

      girafe(
        p,
        width = 100,
        height = 100
      )
    })

    output$correlation_insights <- DT::renderDataTable({
      cor_mat <- correlation_matrix()
      
      if (is.null(cor_mat) || nrow(cor_mat) == 0) {
        return(DT::datatable(tibble::tibble()))
      }

      question_names <- c(
        "Content Relevant", "Excited Content", "Satisfied Feedback",
        "Apply Learning", "Easy Ask Help", "Meeting Goals",
        "Pre-written Code", "Studying Midterms", "TopHat Quizzes",
        "Presentation Slides", "Handouts/Notes", "Coding Own",
        "Live Coding", "Labs", "Ask Questions", "Assignments",
        "Comfortable Speaking", "Part of Class", "Friends Important",
        "University Community", "Easy Meet People"
      )

      insights <- data.frame(
        Question1 = character(),
        Question2 = character(),
        Correlation = numeric(),
        Strength = character(),
        Direction = character(),
        stringsAsFactors = FALSE
      )

      for (i in 1:(nrow(cor_mat) - 1)) {
        for (j in (i + 1):nrow(cor_mat)) {
          corr <- cor_mat[i, j]
          if (!is.na(corr)) {
            strength <- ifelse(abs(corr) < 0.3, "Weak",
                               ifelse(abs(corr) < 0.5, "Moderate",
                                      ifelse(abs(corr) < 0.7, "Strong", "Very Strong")))
            direction <- ifelse(corr > 0, "Positive", "Negative")
            insights <- rbind(insights, data.frame(
              Question1 = question_names[i],
              Question2 = question_names[j],
              Correlation = round(corr, 3),
              Strength = strength,
              Direction = direction,
              stringsAsFactors = FALSE
            ))
          }
        }
      }

      insights <- insights %>%
        dplyr::arrange(dplyr::desc(abs(Correlation)))

      DT::datatable(
        insights,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          searching = TRUE,
          ordering = TRUE,
          paging = TRUE,
          info = TRUE,
          lengthChange = FALSE
        ),
        rownames = FALSE,
        selection = "none",
        colnames = c("Question 1", "Question 2", "Correlation", "Strength", "Direction")
      )
    })

    regression_predictors <- reactive({
      tryCatch({
        data <- filtered_data()
        
        if (nrow(data) == 0) {
          return(tibble::tibble())
        }

        predictors <- data %>%
          dplyr::select(
            dplyr::all_of(COL_CONTENT_RELEVANT),
            dplyr::all_of(COL_EXCITED_CONTENT),
            dplyr::all_of(COL_SATISFIED_FEEDBACK),
            dplyr::all_of(COL_APPLY_LEARNING),
            dplyr::all_of(COL_EASY_ASK_HELP),
            dplyr::all_of(COL_MEETING_GOALS),
            dplyr::all_of(COL_PRE_WRITTEN_CODE),
            dplyr::all_of(COL_STUDYING_MIDTERMS),
            dplyr::all_of(COL_TOPHAT_QUIZZES),
            dplyr::all_of(COL_PRESENTATION_SLIDES),
            dplyr::all_of(COL_HANDOUTS_NOTES),
            dplyr::all_of(COL_CODING_OWN),
            dplyr::all_of(COL_LIVE_CODING),
            dplyr::all_of(COL_LABS),
            dplyr::all_of(COL_ASK_QUESTIONS),
            dplyr::all_of(COL_ASSIGNMENTS),
            dplyr::all_of(COL_COMFORTABLE_SPEAKING),
            dplyr::all_of(COL_PART_OF_CLASS),
            dplyr::all_of(COL_FRIENDS_IMPORTANT),
            dplyr::all_of(COL_UNIVERSITY_COMMUNITY),
            dplyr::all_of(COL_EASY_MEET_PEOPLE)
          ) %>%
          dplyr::mutate(
            content_relevant = extract_likert_value(dplyr::all_of(COL_CONTENT_RELEVANT)),
            excited_content = extract_likert_value(dplyr::all_of(COL_EXCITED_CONTENT)),
            satisfied_feedback = extract_likert_value(dplyr::all_of(COL_SATISFIED_FEEDBACK)),
            apply_learning = extract_likert_value(dplyr::all_of(COL_APPLY_LEARNING)),
            easy_ask_help = extract_likert_value(dplyr::all_of(COL_EASY_ASK_HELP)),
            meeting_goals = extract_likert_value(dplyr::all_of(COL_MEETING_GOALS)),
            pre_written_code = extract_likert_value(dplyr::all_of(COL_PRE_WRITTEN_CODE)),
            studying_midterms = extract_likert_value(dplyr::all_of(COL_STUDYING_MIDTERMS)),
            tophat_quizzes = extract_likert_value(dplyr::all_of(COL_TOPHAT_QUIZZES)),
            presentation_slides = extract_likert_value(dplyr::all_of(COL_PRESENTATION_SLIDES)),
            handouts_notes = extract_likert_value(dplyr::all_of(COL_HANDOUTS_NOTES)),
            coding_own = extract_likert_value(dplyr::all_of(COL_CODING_OWN)),
            live_coding = extract_likert_value(dplyr::all_of(COL_LIVE_CODING)),
            labs = extract_likert_value(dplyr::all_of(COL_LABS)),
            ask_questions = extract_likert_value(dplyr::all_of(COL_ASK_QUESTIONS)),
            assignments = extract_likert_value(dplyr::all_of(COL_ASSIGNMENTS)),
            comfortable_speaking = extract_likert_value(dplyr::all_of(COL_COMFORTABLE_SPEAKING)),
            part_of_class = extract_likert_value(dplyr::all_of(COL_PART_OF_CLASS)),
            friends_important = extract_likert_value(dplyr::all_of(COL_FRIENDS_IMPORTANT)),
            university_community = extract_likert_value(dplyr::all_of(COL_UNIVERSITY_COMMUNITY)),
            easy_meet_people = extract_likert_value(dplyr::all_of(COL_EASY_MEET_PEOPLE))
          ) %>%
          dplyr::filter(!dplyr::any_of(dplyr::c(
            content_relevant, excited_content, satisfied_feedback,
            apply_learning, easy_ask_help, meeting_goals,
            pre_written_code, studying_midterms, tophat_quizzes,
            presentation_slides, handouts_notes, coding_own,
            live_coding, labs, ask_questions, assignments,
            comfortable_speaking, part_of_class, friends_important,
            university_community, easy_meet_people
          )))

        if (nrow(predictors) == 0) {
          return(tibble::tibble())
        }

        outcome <- predictors$meeting_goals
        
        predictor_cols <- setdiff(names(predictors), "meeting_goals")
        
        model <- lm(meeting_goals ~ ., data = predictors[, predictor_cols])
        
        coef_summary <- summary(model)$coefficients
        
        predictors_df <- data.frame(
          Predictor = predictor_cols,
          Coefficient = round(coef_summary[, 1], 4),
          StdError = round(coef_summary[, 2], 4),
          tValue = round(coef_summary[, 3], 4),
          PValue = round(coef_summary[, 4], 4),
          stringsAsFactors = FALSE
        )
        
        predictors_df$Interpretation <- sapply(predictors_df$Coefficient, function(c) {
          if (is.na(c)) return("N/A")
          if (c > 0) return("Positive effect")
          if (c < 0) return("Negative effect")
          return("No effect")
        })
        
        predictors_df <- predictors_df %>%
          dplyr::arrange(dplyr::desc(abs(Coefficient)))
        
        return(predictors_df)

      }, error = function(e) {
        return(tibble::tibble())
      })
    })

    output$regression_predictors <- DT::renderDataTable({
      pred_df <- regression_predictors()
      
      if (nrow(pred_df) == 0) {
        return(DT::datatable(tibble::tibble()))
      }

      DT::datatable(
        pred_df,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          searching = TRUE,
          ordering = TRUE,
          paging = TRUE,
          info = TRUE,
          lengthChange = FALSE
        ),
        rownames = FALSE,
        selection = "none",
        colnames = c("Predictor", "Coefficient", "Std Error", "t-value", "p-value", "Interpretation")
      )
    })

    cluster_analysis <- reactive({
      tryCatch({
        data <- filtered_data()
        
        if (nrow(data) == 0) {
          return(list(clusters = tibble::tibble(), profiles = tibble::tibble()))
        }

        likert_data <- data %>%
          dplyr::select(
            dplyr::all_of(COL_CONTENT_RELEVANT),
            dplyr::all_of(COL_EXCITED_CONTENT),
            dplyr::all_of(COL_SATISFIED_FEEDBACK),
            dplyr::all_of(COL_APPLY_LEARNING),
            dplyr::all_of(COL_EASY_ASK_HELP),
            dplyr::all_of(COL_MEETING_GOALS),
            dplyr::all_of(COL_PRE_WRITTEN_CODE),
            dplyr::all_of(COL_STUDYING_MIDTERMS),
            dplyr::all_of(COL_TOPHAT_QUIZZES),
            dplyr::all_of(COL_PRESENTATION_SLIDES),
            dplyr::all_of(COL_HANDOUTS_NOTES),
            dplyr::all_of(COL_CODING_OWN),
            dplyr::all_of(COL_LIVE_CODING),
            dplyr::all_of(COL_LABS),
            dplyr::all_of(COL_ASK_QUESTIONS),
            dplyr::all_of(COL_ASSIGNMENTS),
            dplyr::all_of(COL_COMFORTABLE_SPEAKING),
            dplyr::all_of(COL_PART_OF_CLASS),
            dplyr::all_of(COL_FRIENDS_IMPORTANT),
            dplyr::all_of(COL_UNIVERSITY_COMMUNITY),
            dplyr::all_of(COL_EASY_MEET_PEOPLE)
          ) %>%
          dplyr::mutate(
            content_relevant = extract_likert_value(dplyr::all_of(COL_CONTENT_RELEVANT)),
            excited_content = extract_likert_value(dplyr::all_of(COL_EXCITED_CONTENT)),
            satisfied_feedback = extract_likert_value(dplyr::all_of(COL_SATISFIED_FEEDBACK)),
            apply_learning = extract_likert_value(dplyr::all_of(COL_APPLY_LEARNING)),
            easy_ask_help = extract_likert_value(dplyr::all_of(COL_EASY_ASK_HELP)),
            meeting_goals = extract_likert_value(dplyr::all_of(COL_MEETING_GOALS)),
            pre_written_code = extract_likert_value(dplyr::all_of(COL_PRE_WRITTEN_CODE)),
            studying_midterms = extract_likert_value(dplyr::all_of(COL_STUDYING_MIDTERMS)),
            tophat_quizzes = extract_likert_value(dplyr::all_of(COL_TOPHAT_QUIZZES)),
            presentation_slides = extract_likert_value(dplyr::all_of(COL_PRESENTATION_SLIDES)),
            handouts_notes = extract_likert_value(dplyr::all_of(COL_HANDOUTS_NOTES)),
            coding_own = extract_likert_value(dplyr::all_of(COL_CODING_OWN)),
            live_coding = extract_likert_value(dplyr::all_of(COL_LIVE_CODING)),
            labs = extract_likert_value(dplyr::all_of(COL_LABS)),
            ask_questions = extract_likert_value(dplyr::all_of(COL_ASK_QUESTIONS)),
            assignments = extract_likert_value(dplyr::all_of(COL_ASSIGNMENTS)),
            comfortable_speaking = extract_likert_value(dplyr::all_of(COL_COMFORTABLE_SPEAKING)),
            part_of_class = extract_likert_value(dplyr::all_of(COL_PART_OF_CLASS)),
            friends_important = extract_likert_value(dplyr::all_of(COL_FRIENDS_IMPORTANT)),
            university_community = extract_likert_value(dplyr::all_of(COL_UNIVERSITY_COMMUNITY)),
            easy_meet_people = extract_likert_value(dplyr::all_of(COL_EASY_MEET_PEOPLE))
          ) %>%
          dplyr::filter(!dplyr::any_of(dplyr::c(
            content_relevant, excited_content, satisfied_feedback,
            apply_learning, easy_ask_help, meeting_goals,
            pre_written_code, studying_midterms, tophat_quizzes,
            presentation_slides, handouts_notes, coding_own,
            live_coding, labs, ask_questions, assignments,
            comfortable_speaking, part_of_class, friends_important,
            university_community, easy_meet_people
          )))

        if (nrow(likert_data) == 0) {
          return(list(clusters = tibble::tibble(), profiles = tibble::tibble()))
        }

        likert_scaled <- scale(likert_data)

        max_clusters <- min(5, nrow(likert_scaled))
        silhouette_scores <- sapply(1:max_clusters, function(k) {
          set.seed(42)
          clusters <- kmeans(likert_scaled, centers = k, nstart = 10)
          sil <- silhouette(clusters$cluster, dist(likert_scaled))
          mean(sil[, 3])
        })
        optimal_k <- which.max(silhouette_scores)

        set.seed(42)
        kmeans_result <- kmeans(likert_scaled, centers = optimal_k, nstart = 10)

        likert_data$cluster <- kmeans_result$cluster

        cluster_profiles <- likert_data %>%
          dplyr::group_by(cluster) %>%
          dplyr::summarise(
            n = dplyr::n(),
            content_relevant = mean(content_relevant, na.rm = TRUE),
            excited_content = mean(excited_content, na.rm = TRUE),
            satisfied_feedback = mean(satisfied_feedback, na.rm = TRUE),
            apply_learning = mean(apply_learning, na.rm = TRUE),
            easy_ask_help = mean(easy_ask_help, na.rm = TRUE),
            meeting_goals = mean(meeting_goals, na.rm = TRUE),
            pre_written_code = mean(pre_written_code, na.rm = TRUE),
            studying_midterms = mean(studying_midterms, na.rm = TRUE),
            tophat_quizzes = mean(tophat_quizzes, na.rm = TRUE),
            presentation_slides = mean(presentation_slides, na.rm = TRUE),
            handouts_notes = mean(handouts_notes, na.rm = TRUE),
            coding_own = mean(coding_own, na.rm = TRUE),
            live_coding = mean(live_coding, na.rm = TRUE),
            labs = mean(labs, na.rm = TRUE),
            ask_questions = mean(ask_questions, na.rm = TRUE),
            assignments = mean(assignments, na.rm = TRUE),
            comfortable_speaking = mean(comfortable_speaking, na.rm = TRUE),
            part_of_class = mean(part_of_class, na.rm = TRUE),
            friends_important = mean(friends_important, na.rm = TRUE),
            university_community = mean(university_community, na.rm = TRUE),
            easy_meet_people = mean(easy_meet_people, na.rm = TRUE)
          ) %>%
          dplyr::arrange(dplyr::desc(meeting_goals))

        cluster_profiles$ClusterName <- sapply(cluster_profiles$cluster, function(c) {
          if (c == 1) return("Highly Engaged")
          if (c == 2) return("Struggling")
          if (c == 3) return("Satisfied but Quiet")
          if (c == 4) return("Moderately Engaged")
          return("Mixed")
        })

        return(list(clusters = cluster_profiles, profiles = likert_data))

      }, error = function(e) {
        return(list(clusters = tibble::tibble(), profiles = tibble::tibble()))
      })
    })

    output$cluster_summary <- DT::renderDataTable({
      cluster_df <- cluster_analysis()$clusters
      
      if (nrow(cluster_df) == 0) {
        return(DT::datatable(tibble::tibble()))
      }

      DT::datatable(
        cluster_df,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          searching = TRUE,
          ordering = TRUE,
          paging = TRUE,
          info = TRUE,
          lengthChange = FALSE
        ),
        rownames = FALSE,
        selection = "none",
        colnames = c("Cluster", "Size", "Content Relevant", "Excited Content",
                     "Satisfied Feedback", "Apply Learning", "Meeting Goals")
      )
    })

    output$cluster_profiles <- DT::renderDataTable({
      profiles_df <- cluster_analysis()$profiles
      
      if (nrow(profiles_df) == 0) {
        return(DT::datatable(tibble::tibble()))
      }

      DT::datatable(
        profiles_df,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          searching = TRUE,
          ordering = TRUE,
          paging = TRUE,
          info = TRUE,
          lengthChange = FALSE
        ),
        rownames = FALSE,
        selection = "none",
        colnames = c("Cluster", "Content Relevant", "Excited Content",
                     "Satisfied Feedback", "Apply Learning", "Meeting Goals")
      )
    })

    section_comparison <- reactive({
      tryCatch({
        data <- filtered_data()
        
        if (nrow(data) == 0) {
          return(tibble::tibble())
        }

        comp_data <- data %>%
          dplyr::select(
            dplyr::all_of(COL_SECTION),
            dplyr::all_of(COL_CONTENT_RELEVANT),
            dplyr::all_of(COL_EXCITED_CONTENT),
            dplyr::all_of(COL_SATISFIED_FEEDBACK),
            dplyr::all_of(COL_APPLY_LEARNING),
            dplyr::all_of(COL_EASY_ASK_HELP),
            dplyr::all_of(COL_MEETING_GOALS)
          ) %>%
          dplyr::mutate(
            content_relevant = extract_likert_value(dplyr::all_of(COL_CONTENT_RELEVANT)),
            excited_content = extract_likert_value(dplyr::all_of(COL_EXCITED_CONTENT)),
            satisfied_feedback = extract_likert_value(dplyr::all_of(COL_SATISFIED_FEEDBACK)),
            apply_learning = extract_likert_value(dplyr::all_of(COL_APPLY_LEARNING)),
            easy_ask_help = extract_likert_value(dplyr::all_of(COL_EASY_ASK_HELP)),
            meeting_goals = extract_likert_value(dplyr::all_of(COL_MEETING_GOALS))
          ) %>%
          dplyr::filter(!dplyr::any_of(dplyr::c(
            content_relevant, excited_content, satisfied_feedback,
            apply_learning, easy_ask_help, meeting_goals
          )))

        if (nrow(comp_data) == 0) {
          return(tibble::tibble())
        }

        section_stats <- comp_data %>%
          dplyr::group_by(dplyr::all_of(COL_SECTION)) %>%
          dplyr::summarise(
            n = dplyr::n(),
            content_relevant = mean(content_relevant, na.rm = TRUE),
            excited_content = mean(excited_content, na.rm = TRUE),
            satisfied_feedback = mean(satisfied_feedback, na.rm = TRUE),
            apply_learning = mean(apply_learning, na.rm = TRUE),
            easy_ask_help = mean(easy_ask_help, na.rm = TRUE),
            meeting_goals = mean(meeting_goals, na.rm = TRUE)
          ) %>%
          dplyr::arrange(dplyr::desc(meeting_goals))

        return(section_stats)

      }, error = function(e) {
        return(tibble::tibble())
      })
    })

    output$section_comparison_table <- DT::renderDataTable({
      section_df <- section_comparison()
      
      if (nrow(section_df) == 0) {
        return(DT::datatable(tibble::tibble()))
      }

      DT::datatable(
        section_df,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          searching = TRUE,
          ordering = TRUE,
          paging = TRUE,
          info = TRUE,
          lengthChange = FALSE
        ),
        rownames = FALSE,
        selection = "none",
        colnames = c("Section", "N", "Content Relevant", "Excited Content",
                     "Satisfied Feedback", "Apply Learning", "Meeting Goals")
      )
    })

    effect_size <- reactive({
      tryCatch({
        data <- filtered_data()
        
        if (nrow(data) == 0) {
          return(tibble::tibble())
        }

        comp_data <- data %>%
          dplyr::select(
            dplyr::all_of(COL_SECTION),
            dplyr::all_of(COL_CONTENT_RELEVANT),
            dplyr::all_of(COL_EXCITED_CONTENT),
            dplyr::all_of(COL_SATISFIED_FEEDBACK),
            dplyr::all_of(COL_APPLY_LEARNING),
            dplyr::all_of(COL_EASY_ASK_HELP),
            dplyr::all_of(COL_MEETING_GOALS)
          ) %>%
          dplyr::mutate(
            content_relevant = extract_likert_value(dplyr::all_of(COL_CONTENT_RELEVANT)),
            excited_content = extract_likert_value(dplyr::all_of(COL_EXCITED_CONTENT)),
            satisfied_feedback = extract_likert_value(dplyr::all_of(COL_SATISFIED_FEEDBACK)),
            apply_learning = extract_likert_value(dplyr::all_of(COL_APPLY_LEARNING)),
            easy_ask_help = extract_likert_value(dplyr::all_of(COL_EASY_ASK_HELP)),
            meeting_goals = extract_likert_value(dplyr::all_of(COL_MEETING_GOALS))
          ) %>%
          dplyr::filter(!dplyr::any_of(dplyr::c(
            content_relevant, excited_content, satisfied_feedback,
            apply_learning, easy_ask_help, meeting_goals
          )))

        if (nrow(comp_data) == 0) {
          return(tibble::tibble())
        }

        effect_df <- data.frame(
          Question = character(),
          EffectSize = numeric(),
          Interpretation = character(),
          stringsAsFactors = FALSE
        )

        questions <- c("content_relevant", "excited_content", "satisfied_feedback",
                       "apply_learning", "easy_ask_help", "meeting_goals")

        for (q in questions) {
          section_values <- comp_data %>%
            dplyr::group_by(dplyr::all_of(COL_SECTION)) %>%
            dplyr::summarise(value = mean(!!sym(q), na.rm = TRUE)) %>%
            dplyr::arrange(dplyr::desc(value))

          if (nrow(section_values) >= 2) {
            highest <- section_values$value[1]
            lowest <- section_values$value[nrow(section_values)]
            eff <- calculate_cohens_d(highest, lowest)
            
            effect_df <- rbind(effect_df, data.frame(
              Question = q,
              EffectSize = eff$d,
              Interpretation = eff$interpretation,
              stringsAsFactors = FALSE
            ))
          }
        }

        return(effect_df)

      }, error = function(e) {
        return(tibble::tibble())
      })
    })

    output$effect_size_table <- DT::renderDataTable({
      eff_df <- effect_size()
      
      if (nrow(eff_df) == 0) {
        return(DT::datatable(tibble::tibble()))
      }

      DT::datatable(
        eff_df,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          searching = TRUE,
          ordering = TRUE,
          paging = TRUE,
          info = TRUE,
          lengthChange = FALSE
        ),
        rownames = FALSE,
        selection = "none",
        colnames = c("Question", "Effect Size", "Interpretation")
      )
    })

    cronbach_alpha <- reactive({
      tryCatch({
        data <- filtered_data()
        
        if (nrow(data) == 0) {
          return(tibble::tibble())
        }

        alpha_results <- data.frame(
          Group = character(),
          Alpha = numeric(),
          Interpretation = character(),
          N = integer(),
          stringsAsFactors = FALSE
        )

        q_group <- data %>%
          dplyr::select(
            dplyr::all_of(COL_CONTENT_RELEVANT),
            dplyr::all_of(COL_EXCITED_CONTENT),
            dplyr::all_of(COL_SATISFIED_FEEDBACK),
            dplyr::all_of(COL_APPLY_LEARNING),
            dplyr::all_of(COL_EASY_ASK_HELP),
            dplyr::all_of(COL_MEETING_GOALS)
          ) %>%
          dplyr::mutate(
            content_relevant = extract_likert_value(dplyr::all_of(COL_CONTENT_RELEVANT)),
            excited_content = extract_likert_value(dplyr::all_of(COL_EXCITED_CONTENT)),
            satisfied_feedback = extract_likert_value(dplyr::all_of(COL_SATISFIED_FEEDBACK)),
            apply_learning = extract_likert_value(dplyr::all_of(COL_APPLY_LEARNING)),
            easy_ask_help = extract_likert_value(dplyr::all_of(COL_EASY_ASK_HELP)),
            meeting_goals = extract_likert_value(dplyr::all_of(COL_MEETING_GOALS))
          ) %>%
          dplyr::filter(!dplyr::any_of(dplyr::c(
            content_relevant, excited_content, satisfied_feedback,
            apply_learning, easy_ask_help, meeting_goals
          )))

        if (nrow(q_group) > 2) {
          alpha <- psych::alpha(q_group[, c("content_relevant", "excited_content",
                                             "satisfied_feedback", "apply_learning",
                                             "easy_ask_help", "meeting_goals")])$total$alpha
          alpha_results <- rbind(alpha_results, data.frame(
            Group = "Course Satisfaction",
            Alpha = round(alpha, 3),
            Interpretation = ifelse(alpha >= 0.8, "Excellent",
                                     ifelse(alpha >= 0.7, "Good",
                                            ifelse(alpha >= 0.6, "Acceptable", "Poor"))),
            N = nrow(q_group),
            stringsAsFactors = FALSE
          ))
        }

        q_group <- data %>%
          dplyr::select(
            dplyr::all_of(COL_PRE_WRITTEN_CODE),
            dplyr::all_of(COL_STUDYING_MIDTERMS),
            dplyr::all_of(COL_TOPHAT_QUIZZES),
            dplyr::all_of(COL_PRESENTATION_SLIDES),
            dplyr::all_of(COL_HANDOUTS_NOTES),
            dplyr::all_of(COL_CODING_OWN),
            dplyr::all_of(COL_LIVE_CODING),
            dplyr::all_of(COL_LABS),
            dplyr::all_of(COL_ASK_QUESTIONS),
            dplyr::all_of(COL_ASSIGNMENTS)
          ) %>%
          dplyr::mutate(
            pre_written_code = extract_likert_value(dplyr::all_of(COL_PRE_WRITTEN_CODE)),
            studying_midterms = extract_likert_value(dplyr::all_of(COL_STUDYING_MIDTERMS)),
            tophat_quizzes = extract_likert_value(dplyr::all_of(COL_TOPHAT_QUIZZES)),
            presentation_slides = extract_likert_value(dplyr::all_of(COL_PRESENTATION_SLIDES)),
            handouts_notes = extract_likert_value(dplyr::all_of(COL_HANDOUTS_NOTES)),
            coding_own = extract_likert_value(dplyr::all_of(COL_CODING_OWN)),
            live_coding = extract_likert_value(dplyr::all_of(COL_LIVE_CODING)),
            labs = extract_likert_value(dplyr::all_of(COL_LABS)),
            ask_questions = extract_likert_value(dplyr::all_of(COL_ASK_QUESTIONS)),
            assignments = extract_likert_value(dplyr::all_of(COL_ASSIGNMENTS))
          ) %>%
          dplyr::filter(!dplyr::any_of(dplyr::c(
            pre_written_code, studying_midterms, tophat_quizzes,
            presentation_slides, handouts_notes, coding_own,
            live_coding, labs, ask_questions, assignments
          )))

        if (nrow(q_group) > 2) {
          alpha <- psych::alpha(q_group[, c("pre_written_code", "studying_midterms",
                                             "tophat_quizzes", "presentation_slides",
                                             "handouts_notes", "coding_own",
                                             "live_coding", "labs", "ask_questions",
                                             "assignments")])$total$alpha
          alpha_results <- rbind(alpha_results, data.frame(
            Group = "Learning Methods",
            Alpha = round(alpha, 3),
            Interpretation = ifelse(alpha >= 0.8, "Excellent",
                                     ifelse(alpha >= 0.7, "Good",
                                            ifelse(alpha >= 0.6, "Acceptable", "Poor"))),
            N = nrow(q_group),
            stringsAsFactors = FALSE
          ))
        }

        q_group <- data %>%
          dplyr::select(
            dplyr::all_of(COL_COMFORTABLE_SPEAKING),
            dplyr::all_of(COL_PART_OF_CLASS),
            dplyr::all_of(COL_FRIENDS_IMPORTANT),
            dplyr::all_of(COL_UNIVERSITY_COMMUNITY),
            dplyr::all_of(COL_EASY_MEET_PEOPLE)
          ) %>%
          dplyr::mutate(
            comfortable_speaking = extract_likert_value(dplyr::all_of(COL_COMFORTABLE_SPEAKING)),
            part_of_class = extract_likert_value(dplyr::all_of(COL_PART_OF_CLASS)),
            friends_important = extract_likert_value(dplyr::all_of(COL_FRIENDS_IMPORTANT)),
            university_community = extract_likert_value(dplyr::all_of(COL_UNIVERSITY_COMMUNITY)),
            easy_meet_people = extract_likert_value(dplyr::all_of(COL_EASY_MEET_PEOPLE))
          ) %>%
          dplyr::filter(!dplyr::any_of(dplyr::c(
            comfortable_speaking, part_of_class, friends_important,
            university_community, easy_meet_people
          )))

        if (nrow(q_group) > 2) {
          alpha <- psych::alpha(q_group[, c("comfortable_speaking", "part_of_class",
                                             "friends_important", "university_community",
                                             "easy_meet_people")])$total$alpha
          alpha_results <- rbind(alpha_results, data.frame(
            Group = "Community & Belonging",
            Alpha = round(alpha, 3),
            Interpretation = ifelse(alpha >= 0.8, "Excellent",
                                     ifelse(alpha >= 0.7, "Good",
                                            ifelse(alpha >= 0.6, "Acceptable", "Poor"))),
            N = nrow(q_group),
            stringsAsFactors = FALSE
          ))
        }

        return(alpha_results)

      }, error = function(e) {
        return(tibble::tibble())
      })
    })

    output$cronbach_alpha_table <- DT::renderDataTable({
      alpha_df <- cronbach_alpha()
      
      if (nrow(alpha_df) == 0) {
        return(DT::datatable(tibble::tibble()))
      }

      DT::datatable(
        alpha_df,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          searching = TRUE,
          ordering = TRUE,
          paging = TRUE,
          info = TRUE,
          lengthChange = FALSE
        ),
        rownames = FALSE,
        selection = "none",
        colnames = c("Question Group", "Cronbach's Alpha", "Interpretation", "N")
      )
    })

    interaction_effects <- reactive({
      tryCatch({
        data <- filtered_data()
        
        if (nrow(data) == 0) {
          return(tibble::tibble())
        }

        likert_data <- data %>%
          dplyr::select(
            dplyr::all_of(COL_CONTENT_RELEVANT),
            dplyr::all_of(COL_EXCITED_CONTENT),
            dplyr::all_of(COL_SATISFIED_FEEDBACK),
            dplyr::all_of(COL_APPLY_LEARNING),
            dplyr::all_of(COL_EASY_ASK_HELP),
            dplyr::all_of(COL_MEETING_GOALS),
            dplyr::all_of(COL_PRE_WRITTEN_CODE),
            dplyr::all_of(COL_STUDYING_MIDTERMS),
            dplyr::all_of(COL_TOPHAT_QUIZZES),
            dplyr::all_of(COL_PRESENTATION_SLIDES),
            dplyr::all_of(COL_HANDOUTS_NOTES),
            dplyr::all_of(COL_CODING_OWN),
            dplyr::all_of(COL_LIVE_CODING),
            dplyr::all_of(COL_LABS),
            dplyr::all_of(COL_ASK_QUESTIONS),
            dplyr::all_of(COL_ASSIGNMENTS),
            dplyr::all_of(COL_COMFORTABLE_SPEAKING),
            dplyr::all_of(COL_PART_OF_CLASS),
            dplyr::all_of(COL_FRIENDS_IMPORTANT),
            dplyr::all_of(COL_UNIVERSITY_COMMUNITY),
            dplyr::all_of(COL_EASY_MEET_PEOPLE)
          ) %>%
          dplyr::mutate(
            content_relevant = extract_likert_value(dplyr::all_of(COL_CONTENT_RELEVANT)),
            excited_content = extract_likert_value(dplyr::all_of(COL_EXCITED_CONTENT)),
            satisfied_feedback = extract_likert_value(dplyr::all_of(COL_SATISFIED_FEEDBACK)),
            apply_learning = extract_likert_value(dplyr::all_of(COL_APPLY_LEARNING)),
            easy_ask_help = extract_likert_value(dplyr::all_of(COL_EASY_ASK_HELP)),
            meeting_goals = extract_likert_value(dplyr::all_of(COL_MEETING_GOALS)),
            pre_written_code = extract_likert_value(dplyr::all_of(COL_PRE_WRITTEN_CODE)),
            studying_midterms = extract_likert_value(dplyr::all_of(COL_STUDYING_MIDTERMS)),
            tophat_quizzes = extract_likert_value(dplyr::all_of(COL_TOPHAT_QUIZZES)),
            presentation_slides = extract_likert_value(dplyr::all_of(COL_PRESENTATION_SLIDES)),
            handouts_notes = extract_likert_value(dplyr::all_of(COL_HANDOUTS_NOTES)),
            coding_own = extract_likert_value(dplyr::all_of(COL_CODING_OWN)),
            live_coding = extract_likert_value(dplyr::all_of(COL_LIVE_CODING)),
            labs = extract_likert_value(dplyr::all_of(COL_LABS)),
            ask_questions = extract_likert_value(dplyr::all_of(COL_ASK_QUESTIONS)),
            assignments = extract_likert_value(dplyr::all_of(COL_ASSIGNMENTS)),
            comfortable_speaking = extract_likert_value(dplyr::all_of(COL_COMFORTABLE_SPEAKING)),
            part_of_class = extract_likert_value(dplyr::all_of(COL_PART_OF_CLASS)),
            friends_important = extract_likert_value(dplyr::all_of(COL_FRIENDS_IMPORTANT)),
            university_community = extract_likert_value(dplyr::all_of(COL_UNIVERSITY_COMMUNITY)),
            easy_meet_people = extract_likert_value(dplyr::all_of(COL_EASY_MEET_PEOPLE))
          ) %>%
          dplyr::filter(!dplyr::any_of(dplyr::c(
            content_relevant, excited_content, satisfied_feedback,
            apply_learning, easy_ask_help, meeting_goals,
            pre_written_code, studying_midterms, tophat_quizzes,
            presentation_slides, handouts_notes, coding_own,
            live_coding, labs, ask_questions, assignments,
            comfortable_speaking, part_of_class, friends_important,
            university_community, easy_meet_people
          )))

        if (nrow(likert_data) == 0) {
          return(tibble::tibble())
        }

        interaction_df <- data.frame(
          Interaction = character(),
          FValue = numeric(),
          PValue = numeric(),
          Interpretation = character(),
          stringsAsFactors = FALSE
        )

        model1 <- lm(meeting_goals ~ content_relevant * pre_written_code, data = likert_data)
        summary1 <- summary(model1)$coefficients
        if (nrow(summary1) >= 3) {
          interaction_df <- rbind(interaction_df, data.frame(
            Interaction = "Content Relevance × Pre-written Code",
            FValue = round(summary1[3, 4], 4),
            PValue = round(summary1[3, 5], 4),
            Interpretation = ifelse(summary1[3, 5] < 0.05, "Significant",
                                    ifelse(summary1[3, 5] < 0.1, "Marginally significant", "Not significant")),
            stringsAsFactors = FALSE
          ))
        }

        model2 <- lm(meeting_goals ~ satisfied_feedback * ask_questions, data = likert_data)
        summary2 <- summary(model2)$coefficients
        if (nrow(summary2) >= 3) {
          interaction_df <- rbind(interaction_df, data.frame(
            Interaction = "Satisfied Feedback × Ask Questions",
            FValue = round(summary2[3, 4], 4),
            PValue = round(summary2[3, 5], 4),
            Interpretation = ifelse(summary2[3, 5] < 0.05, "Significant",
                                    ifelse(summary2[3, 5] < 0.1, "Marginally significant", "Not significant")),
            stringsAsFactors = FALSE
          ))
        }

        model3 <- lm(meeting_goals ~ excited_content * live_coding, data = likert_data)
        summary3 <- summary(model3)$coefficients
        if (nrow(summary3) >= 3) {
          interaction_df <- rbind(interaction_df, data.frame(
            Interaction = "Excited Content × Live Coding",
            FValue = round(summary3[3, 4], 4),
            PValue = round(summary3[3, 5], 4),
            Interpretation = ifelse(summary3[3, 5] < 0.05, "Significant",
                                    ifelse(summary3[3, 5] < 0.1, "Marginally significant", "Not significant")),
            stringsAsFactors = FALSE
          ))
        }

        return(interaction_df)

      }, error = function(e) {
        return(tibble::tibble())
      })
    })

    output$interaction_effects <- DT::renderDataTable({
      int_df <- interaction_effects()
      
      if (nrow(int_df) == 0) {
        return(DT::datatable(tibble::tibble()))
      }

      DT::datatable(
        int_df,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          searching = TRUE,
          ordering = TRUE,
          paging = TRUE,
          info = TRUE,
          lengthChange = FALSE
        ),
        rownames = FALSE,
        selection = "none",
        colnames = c("Interaction", "F-value", "p-value", "Interpretation")
      )
    })

    satisfaction_predictors <- reactive({
      tryCatch({
        data <- filtered_data()
        
        if (nrow(data) == 0) {
          return(tibble::tibble())
        }

        predictors <- data %>%
          dplyr::select(
            dplyr::all_of(COL_CONTENT_RELEVANT),
            dplyr::all_of(COL_EXCITED_CONTENT),
            dplyr::all_of(COL_SATISFIED_FEEDBACK),
            dplyr::all_of(COL_APPLY_LEARNING),
            dplyr::all_of(COL_EASY_ASK_HELP),
            dplyr::all_of(COL_MEETING_GOALS),
            dplyr::all_of(COL_PRE_WRITTEN_CODE),
            dplyr::all_of(COL_STUDYING_MIDTERMS),
            dplyr::all_of(COL_TOPHAT_QUIZZES),
            dplyr::all_of(COL_PRESENTATION_SLIDES),
            dplyr::all_of(COL_HANDOUTS_NOTES),
            dplyr::all_of(COL_CODING_OWN),
            dplyr::all_of(COL_LIVE_CODING),
            dplyr::all_of(COL_LABS),
            dplyr::all_of(COL_ASK_QUESTIONS),
            dplyr::all_of(COL_ASSIGNMENTS),
            dplyr::all_of(COL_COMFORTABLE_SPEAKING),
            dplyr::all_of(COL_PART_OF_CLASS),
            dplyr::all_of(COL_FRIENDS_IMPORTANT),
            dplyr::all_of(COL_UNIVERSITY_COMMUNITY),
            dplyr::all_of(COL_EASY_MEET_PEOPLE)
          ) %>%
          dplyr::mutate(
            content_relevant = extract_likert_value(dplyr::all_of(COL_CONTENT_RELEVANT)),
            excited_content = extract_likert_value(dplyr::all_of(COL_EXCITED_CONTENT)),
            satisfied_feedback = extract_likert_value(dplyr::all_of(COL_SATISFIED_FEEDBACK)),
            apply_learning = extract_likert_value(dplyr::all_of(COL_APPLY_LEARNING)),
            easy_ask_help = extract_likert_value(dplyr::all_of(COL_EASY_ASK_HELP)),
            meeting_goals = extract_likert_value(dplyr::all_of(COL_MEETING_GOALS)),
            pre_written_code = extract_likert_value(dplyr::all_of(COL_PRE_WRITTEN_CODE)),
            studying_midterms = extract_likert_value(dplyr::all_of(COL_STUDYING_MIDTERMS)),
            tophat_quizzes = extract_likert_value(dplyr::all_of(COL_TOPHAT_QUIZZES)),
            presentation_slides = extract_likert_value(dplyr::all_of(COL_PRESENTATION_SLIDES)),
            handouts_notes = extract_likert_value(dplyr::all_of(COL_HANDOUTS_NOTES)),
            coding_own = extract_likert_value(dplyr::all_of(COL_CODING_OWN)),
            live_coding = extract_likert_value(dplyr::all_of(COL_LIVE_CODING)),
            labs = extract_likert_value(dplyr::all_of(COL_LABS)),
            ask_questions = extract_likert_value(dplyr::all_of(COL_ASK_QUESTIONS)),
            assignments = extract_likert_value(dplyr::all_of(COL_ASSIGNMENTS)),
            comfortable_speaking = extract_likert_value(dplyr::all_of(COL_COMFORTABLE_SPEAKING)),
            part_of_class = extract_likert_value(dplyr::all_of(COL_PART_OF_CLASS)),
            friends_important = extract_likert_value(dplyr::all_of(COL_FRIENDS_IMPORTANT)),
            university_community = extract_likert_value(dplyr::all_of(COL_UNIVERSITY_COMMUNITY)),
            easy_meet_people = extract_likert_value(dplyr::all_of(COL_EASY_MEET_PEOPLE))
          ) %>%
          dplyr::filter(!dplyr::any_of(dplyr::c(
            content_relevant, excited_content, satisfied_feedback,
            apply_learning, easy_ask_help, meeting_goals,
            pre_written_code, studying_midterms, tophat_quizzes,
            presentation_slides, handouts_notes, coding_own,
            live_coding, labs, ask_questions, assignments,
            comfortable_speaking, part_of_class, friends_important,
            university_community, easy_meet_people
          )))

        if (nrow(predictors) == 0) {
          return(tibble::tibble())
        }

        outcome <- predictors$meeting_goals
        
        predictor_cols <- setdiff(names(predictors), "meeting_goals")
        
        model <- lm(meeting_goals ~ ., data = predictors[, predictor_cols])
        
        coef_summary <- summary(model)$coefficients
        
        predictors_df <- data.frame(
          Predictor = predictor_cols,
          Coefficient = round(coef_summary[, 1], 4),
          StdError = round(coef_summary[, 2], 4),
          tValue = round(coef_summary[, 3], 4),
          PValue = round(coef_summary[, 4], 4),
          Importance = numeric(),
          stringsAsFactors = FALSE
        )
        
        predictors_df$Importance <- round(abs(coef_summary[, 1]) / sum(abs(coef_summary[, 1])), 3)
        
        predictors_df <- predictors_df %>%
          dplyr::arrange(dplyr::desc(Importance))
        
        return(predictors_df)

      }, error = function(e) {
        return(tibble::tibble())
      })
    })

    output$satisfaction_drivers <- DT::renderDataTable({
      pred_df <- satisfaction_predictors()
      
      if (nrow(pred_df) == 0) {
        return(DT::datatable(tibble::tibble()))
      }

      DT::datatable(
        pred_df,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          searching = TRUE,
          ordering = TRUE,
          paging = TRUE,
          info = TRUE,
          lengthChange = FALSE
        ),
        rownames = FALSE,
        selection = "none",
        colnames = c("Predictor", "Coefficient", "Std Error", "t-value", "p-value", "Importance")
      )
    })

    return(list(
      selected_analysis = selected_analysis,
      correlation_matrix = correlation_matrix,
      regression_predictors = regression_predictors,
      cluster_analysis = cluster_analysis,
      section_comparison = section_comparison,
      effect_size = effect_size,
      cronbach_alpha = cronbach_alpha,
      interaction_effects = interaction_effects,
      satisfaction_predictors = satisfaction_predictors
    ))
  })
}
