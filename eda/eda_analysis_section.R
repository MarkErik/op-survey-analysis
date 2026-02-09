# Exploratory Data Analysis (EDA) Script by Section
# This script calculates descriptive statistics and generates histograms
# for all Likert scale-type questions, broken down by class section.

# Required libraries
library(ggplot2)
library(dplyr)
library(gridExtra)
library(scales)
library(stringr)

# =============================================================================
# DATA LOADING
# =============================================================================

load_data <- function() {
  tryCatch({
    df <- read.csv("../survey_data/exported_data.csv", stringsAsFactors = FALSE)
    # Clean column names
    names(df) <- tolower(names(df))
    # Remove any leading/trailing whitespace from character columns
    df <- df %>% mutate(across(where(is.character), str_trim))
    message("Data loaded successfully: ", nrow(df), " rows, ", ncol(df), " columns")
    return(df)
  }, error = function(e) {
    stop("Error loading data: ", e$message)
  })
}

# =============================================================================
# LIKERT SCALE DEFINITIONS
# =============================================================================

# Define Likert scale questions and their response mappings
likert_questions <- list(
  course = list(
    excited = list(
      column = "x.course..i.am.excited.about.the.content.and.material.that.i.m.learning",
      label = "Excited about content and material",
      scale = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
    ),
    relevant = list(
      column = "x.course..the.content.is.relevant.and.up.to.date",
      label = "Content is relevant and up to date",
      scale = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
    ),
    meeting_goals = list(
      column = "x.course..i.feel.like.i.am.meeting.the.goals.of.learning.python.in.this.course",
      label = "Meeting learning goals",
      scale = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
    ),
    apply_scenario = list(
      column = "x.course..i.feel.like.i.could.take.what.i.m.learning.and.apply.it.in.a.new.scenario",
      label = "Can apply learning to new scenarios",
      scale = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
    ),
    feedback = list(
      column = "x.course..i.m.satisfied.with.the.level.of.feedback.i.receive",
      label = "Satisfied with feedback level",
      scale = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
    ),
    ask_help = list(
      column = "x.course..it.s.easy.to.ask.for.help",
      label = "Easy to ask for help",
      scale = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
    )
  ),
  learning = list(
    pre_written_code = list(
      column = "x.learning..explanations.of.pre.written.code",
      label = "Explanations of pre-written code",
      scale = c("Doesn't contribute", "Somewhat contributes", "Contributes", "Very helpful", "Essential")
    ),
    live_coding = list(
      column = "x.learning..live.coding.by.the.professor",
      label = "Live coding by professor",
      scale = c("Doesn't contribute", "Somewhat contributes", "Contributes", "Very helpful", "Essential")
    ),
    slides = list(
      column = "x.learning..presentation.slides",
      label = "Presentation slides",
      scale = c("Doesn't contribute", "Somewhat contributes", "Contributes", "Very helpful", "Essential")
    ),
    handouts = list(
      column = "x.learning..post.class.handouts.and.notes",
      label = "Post-class handouts and notes",
      scale = c("Doesn't contribute", "Somewhat contributes", "Contributes", "Very helpful", "Essential")
    ),
    tophat_quizzes = list(
      column = "x.learning..tophat.quizzes",
      label = "TopHat quizzes",
      scale = c("Doesn't contribute", "Somewhat contributes", "Contributes", "Very helpful", "Essential")
    ),
    assignments = list(
      column = "x.learning..assignments",
      label = "Assignments",
      scale = c("Doesn't contribute", "Somewhat contributes", "Contributes", "Very helpful", "Essential")
    ),
    labs = list(
      column = "x.learning..labs",
      label = "Labs",
      scale = c("Doesn't contribute", "Somewhat contributes", "Contributes", "Very helpful", "Essential")
    ),
    ask_questions = list(
      column = "x.learning..being.able.to.ask.questions.of.the.professor.during.lecture",
      label = "Asking questions during lecture",
      scale = c("Doesn't contribute", "Somewhat contributes", "Contributes", "Very helpful", "Essential")
    ),
    studying_midterms = list(
      column = "x.learning..studying.for.midterms",
      label = "Studying for midterms",
      scale = c("Doesn't contribute", "Somewhat contributes", "Contributes", "Very helpful", "Essential")
    ),
    coding_own = list(
      column = "x.learning..coding.on.my.own",
      label = "Coding on my own",
      scale = c("Doesn't contribute", "Somewhat contributes", "Contributes", "Very helpful", "Essential")
    )
  ),
  community = list(
    friends_important = list(
      column = "x.community..making.friends.within.the.class.is.important.to.me",
      label = "Making friends is important",
      scale = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
    ),
    easy_meet = list(
      column = "x.community..it.s.easy.to.meet.new.people.within.the.class",
      label = "Easy to meet new people",
      scale = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
    ),
    part_of_class = list(
      column = "x.community..i.feel.like.i.am.a.part.of.this.class",
      label = "Feel part of this class",
      scale = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
    ),
    comfortable_speaking = list(
      column = "x.community..i.feel.comfortable.speaking.up.in.class",
      label = "Comfortable speaking in class",
      scale = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
    ),
    part_of_university = list(
      column = "x.community..i.feel.like.i.am.a.part.of.the.university.community",
      label = "Feel part of university community",
      scale = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
    )
  )
)

# =============================================================================
# STATISTICAL FUNCTIONS
# =============================================================================

# Convert text responses to numeric scores (1-5)
convert_to_numeric <- function(responses, scale) {
  # Remove all non-numeric characters to extract the score
  # Handles formats like "5 - Strongly Agree", "4", "3", etc.
  numeric_values <- sapply(responses, function(r) {
    if (is.na(r) || r == "") {
      return(NA)
    }
    
    # Extract numeric value by removing all non-numeric characters
    numeric_value <- as.numeric(gsub("[^0-9]", "", r))
    
    return(numeric_value)
  })
  
  return(as.numeric(numeric_values))
}

# Calculate mode (most frequent value)
calculate_mode <- function(x) {
  ux <- na.omit(unique(x))
  if (length(ux) == 0) return(NA)
  ux[which.max(tabulate(match(x, ux)))]
}

# Calculate comprehensive statistics for a question (by section)
calculate_statistics_by_section <- function(df, column_name, question_label, scale, section) {
  # Filter by section
  df_section <- df[df$section == section, ]
  
  # Check if column exists
  if (!column_name %in% names(df_section)) {
    return(list(
      question = question_label,
      column = column_name,
      section = section,
      status = "Column not found",
      n = 0,
      mean = NA,
      median = NA,
      mode = NA,
      sd = NA,
      se = NA,
      min = NA,
      max = NA,
      q1 = NA,
      q3 = NA,
      missing = NA
    ))
  }
  
  # Extract responses
  responses <- df_section[[column_name]]
  
  # Calculate basic counts
  total_responses <- length(responses)
  valid_responses <- sum(!is.na(responses) & responses != "")
  missing_responses <- total_responses - valid_responses
  
  if (valid_responses == 0) {
    return(list(
      question = question_label,
      column = column_name,
      section = section,
      status = "No valid responses",
      n = 0,
      mean = NA,
      median = NA,
      mode = NA,
      sd = NA,
      se = NA,
      min = NA,
      max = NA,
      q1 = NA,
      q3 = NA,
      missing = missing_responses
    ))
  }
  
  # Convert to numeric
  numeric_values <- convert_to_numeric(responses, scale)
  numeric_values <- numeric_values[!is.na(numeric_values)]
  
  if (length(numeric_values) == 0) {
    return(list(
      question = question_label,
      column = column_name,
      section = section,
      status = "No valid numeric conversions",
      n = valid_responses,
      mean = NA,
      median = NA,
      mode = NA,
      sd = NA,
      se = NA,
      min = NA,
      max = NA,
      q1 = NA,
      q3 = NA,
      missing = missing_responses
    ))
  }
  
  # Calculate statistics
  stats <- list(
    question = question_label,
    column = column_name,
    section = section,
    status = "OK",
    n = length(numeric_values),
    mean = round(mean(numeric_values), 3),
    median = round(median(numeric_values), 3),
    mode = calculate_mode(numeric_values),
    sd = round(sd(numeric_values), 3),
    se = round(sd(numeric_values) / sqrt(length(numeric_values)), 3),
    min = min(numeric_values),
    max = max(numeric_values),
    q1 = round(quantile(numeric_values, 0.25), 3),
    q3 = round(quantile(numeric_values, 0.75), 3),
    missing = missing_responses
  )
  
  return(stats)
}

# =============================================================================
# HISTOGRAM GENERATION
# =============================================================================

# Generate histogram for a single question (by section)
generate_histogram_by_section <- function(df, column_name, question_label, scale, section) {
  # Filter by section
  df_section <- df[df$section == section, ]
  
  # Check if column exists
  if (!column_name %in% names(df_section)) {
    return(NULL)
  }
  
  # Extract and convert responses
  responses <- df_section[[column_name]]
  numeric_values <- convert_to_numeric(responses, scale)
  numeric_values <- numeric_values[!is.na(numeric_values)]
  
  if (length(numeric_values) == 0) {
    return(NULL)
  }
  
  # Create frequency table
  freq_table <- table(factor(numeric_values, levels = 1:5))
  freq_df <- data.frame(
    score = 1:5,
    label = scale,
    count = as.numeric(freq_table),
    percentage = round(as.numeric(freq_table) / sum(freq_table) * 100, 1)
  )
  
  # Create histogram
  p <- ggplot(freq_df, aes(x = score, y = count, fill = factor(score))) +
    geom_col(width = 0.7) +
    geom_text(aes(label = paste0(count, " (", percentage, "%)")), 
              vjust = -0.5, size = 3) +
    scale_fill_manual(
      values = c("#e74c3c", "#e67e22", "#f1c40f", "#2ecc71", "#27ae60"),
      name = "Score",
      labels = scale
    ) +
    scale_x_continuous(
      breaks = 1:5,
      labels = scale,
      expand = expansion(mult = c(0.05, 0.05))
    ) +
    labs(
      title = paste0(question_label, " - ", section),
      x = "Response",
      y = "Count",
      subtitle = paste0("n = ", sum(freq_table))
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      legend.position = "none"
    )
  
  return(p)
}

# =============================================================================
# MAIN ANALYSIS
# =============================================================================

# Run complete EDA analysis by section
run_eda_analysis_by_section <- function() {
  message("\n========================================")
  message("EXPLORATORY DATA ANALYSIS BY SECTION")
  message("========================================\n")
  
  # Load data
  df <- load_data()
  
  # Get unique sections (excluding NA)
  sections <- unique(df$section)
  sections <- sections[!is.na(sections)]
  sections <- sort(sections)
  
  message("Found ", length(sections), " sections: ", paste(sections, collapse = ", "), "\n")
  
  # Initialize results storage
  all_stats <- list()
  all_plots <- list()
  
  # Process each category
  for (category in names(likert_questions)) {
    message("\n--- Processing ", toupper(category), " Questions ---\n")
    
    category_stats <- list()
    category_plots <- list()
    
    for (question_name in names(likert_questions[[category]])) {
      question_info <- likert_questions[[category]][[question_name]]
      
      message("Analyzing: ", question_info$label)
      
      question_stats <- list()
      question_plots <- list()
      
      # Process each section
      for (section in sections) {
        # Calculate statistics
        stats <- calculate_statistics_by_section(
          df = df,
          column_name = question_info$column,
          question_label = question_info$label,
          scale = question_info$scale,
          section = section
        )
        
        question_stats[[section]] <- stats
        
        # Generate histogram
        plot <- generate_histogram_by_section(
          df = df,
          column_name = question_info$column,
          question_label = question_info$label,
          scale = question_info$scale,
          section = section
        )
        
        if (!is.null(plot)) {
          question_plots[[section]] <- plot
        }
      }
      
      category_stats[[question_name]] <- question_stats
      category_plots[[question_name]] <- question_plots
    }
    
    all_stats[[category]] <- category_stats
    all_plots[[category]] <- category_plots
  }
  
  # Return results
  return(list(
    stats = all_stats,
    plots = all_plots,
    data = df,
    sections = sections
  ))
}

# =============================================================================
# OUTPUT FUNCTIONS
# =============================================================================

# Print statistics summary by section
print_statistics_summary_by_section <- function(results) {
  message("\n========================================")
  message("STATISTICS SUMMARY BY SECTION")
  message("========================================\n")
  
  for (category in names(results$stats)) {
    for (question_name in names(results$stats[[category]])) {
      question_label <- results$stats[[category]][[question_name]][[1]]$question
      
      message("\n### ", question_label, " ###\n")
      
      # Create data frame for this question across all sections
      stats_df <- do.call(rbind, lapply(results$stats[[category]][[question_name]], function(x) {
        data.frame(
          Section = x$section,
          N = x$n,
          Mean = x$mean,
          Median = x$median,
          Mode = x$mode,
          SD = x$sd,
          SE = x$se,
          Min = x$min,
          Max = x$max,
          Q1 = x$q1,
          Q3 = x$q3,
          Missing = x$missing,
          stringsAsFactors = FALSE
        )
      }))
      
      print(stats_df, row.names = FALSE)
      message("\n")
    }
  }
}

# Save statistics to CSV (by section)
save_statistics_csv_by_section <- function(results, output_file = "eda_statistics_by_section.csv") {
  all_stats_df <- data.frame()
  
  for (category in names(results$stats)) {
    for (question_name in names(results$stats[[category]])) {
      category_df <- do.call(rbind, lapply(results$stats[[category]][[question_name]], function(x) {
        data.frame(
          Category = category,
          Question = x$question,
          Column = x$column,
          Section = x$section,
          Status = x$status,
          N = x$n,
          Mean = x$mean,
          Median = x$median,
          Mode = x$mode,
          SD = x$sd,
          SE = x$se,
          Min = x$min,
          Max = x$max,
          Q1 = x$q1,
          Q3 = x$q3,
          Missing = x$missing,
          stringsAsFactors = FALSE
        )
      }))
      all_stats_df <- rbind(all_stats_df, category_df)
    }
  }
  
  write.csv(all_stats_df, output_file, row.names = FALSE)
  message("Statistics saved to: ", output_file)
}

# Save histograms to PDF (by section - all sections for each question on one page)
save_histograms_pdf_by_section <- function(results, output_file = "eda_histograms_by_section.pdf") {
  pdf(output_file, width = 14, height = 10)
  
  for (category in names(results$plots)) {
    for (question_name in names(results$plots[[category]])) {
      plots_list <- results$plots[[category]][[question_name]]
      
      # Filter out NULL plots
      plots_list <- plots_list[!sapply(plots_list, is.null)]
      
      if (length(plots_list) > 0) {
        # Arrange all section plots for this question in a grid
        grid.arrange(grobs = plots_list, ncol = 2, nrow = 3,
                     top = grid::textGrob(paste(toupper(category), "-", 
                                                results$stats[[category]][[question_name]][[1]]$question),
                                          gp = grid::gpar(fontsize = 14, fontface = "bold")))
      }
    }
  }
  
  dev.off()
  message("Histograms saved to: ", output_file)
}

# Save histograms as individual PNG files (by section)
save_histograms_png_by_section <- function(results, output_dir = "eda_histograms_by_section") {
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  for (category in names(results$plots)) {
    for (question_name in names(results$plots[[category]])) {
      for (section in names(results$plots[[category]][[question_name]])) {
        plot <- results$plots[[category]][[question_name]][[section]]
        
        if (!is.null(plot)) {
          # Create safe filename
          safe_question <- gsub("[^a-zA-Z0-9]", "_", question_name)
          safe_section <- gsub("[^a-zA-Z0-9]", "_", section)
          filename <- file.path(output_dir, paste0(category, "_", safe_question, "_", safe_section, ".png"))
          
          ggsave(
            filename = filename,
            plot = plot,
            width = 10,
            height = 6,
            dpi = 300
          )
        }
      }
    }
  }
  
  message("Histograms saved to: ", output_dir, "/")
}

# =============================================================================
# MAIN EXECUTION
# =============================================================================

# Run the analysis
results <- run_eda_analysis_by_section()

# Print summary to console
print_statistics_summary_by_section(results)

# Save outputs
save_statistics_csv_by_section(results)
save_histograms_pdf_by_section(results)
save_histograms_png_by_section(results)

message("\n========================================")
message("EDA ANALYSIS BY SECTION COMPLETE")
message("========================================")
message("\nGenerated files:")
message("  - eda_statistics_by_section.csv (summary statistics by section)")
message("  - eda_histograms_by_section.pdf (all histograms by section)")
message("  - eda_histograms_by_section/ (individual PNG files by section)")
message("\n")
