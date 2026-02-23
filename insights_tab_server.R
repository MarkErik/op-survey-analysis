# =============================================================================
# INSIGHTS_TAB_SERVER.R - Insights Tab Server Module
# CPSC Experience Survey Explorer Shiny Application
# =============================================================================

# Server module for Insights tab - statistical analysis and insights

insightsTabServer <- function(id, filteredData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Likert question columns (all categories combined)
    likertCols <- c(8:13, 14:23, 27:31)
    likertNames <- c("Content relevance", "Excitement", "Feedback satisfaction", "Apply learning", 
                     "Ask help", "Goal achievement", "Pre-written code", "Midterms", "TopHat", 
                     "Slides", "Handouts", "Coding own", "Live coding", "Labs", 
                     "Asking questions", "Assignments", "Comfort speaking up", 
                     "Feeling part of class", "Making friends important", "University community", "Meeting people")
    
    # Helper: Extract numeric Likert values
    extractLikertValues <- function(df, cols) {
      sapply(cols, function(col) {
        as.numeric(stringr::str_extract(as.character(df[[col]]), "[1-5]"))
      })
    }
    
    # 1. Correlation Matrix & Heatmap
    output$correlationHeatmap <- ggiraph::renderGirafe({
      req(filteredData())
      tryCatch({
        df <- filteredData()
        likertData <- extractLikertValues(df, likertCols)
        likertData <- likertData[complete.cases(likertData), ]
        corMatrix <- cor(likertData, method = "pearson", use = "pairwise.complete.obs")
        
        corLong <- reshape2::melt(corMatrix)
        names(corLong) <- c("Var1", "Var2", "Correlation")
        
        p <- ggplot2::ggplot(corLong, ggplot2::aes(x = Var1, y = Var2, fill = Correlation)) +
          ggplot2::geom_tile(color = "white") +
          ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", Correlation)), size = 3) +
          ggplot2::scale_fill_gradient2(low = "#d73027", mid = "#f7f7f7", high = "#1a9850", 
                                        midpoint = 0, limits = c(-1, 1)) +
          ggplot2::theme_minimal() +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                         panel.grid = ggplot2::element_blank())
        
        ggiraph::girafe(ggobj = p, options = list(ggiraph::opts_tooltip(css = "background-color:white;padding:5px;")))
      }, error = function(e) {
        ggiraph::girafe(ggplot2::ggplot() + ggplot2::labs(title = paste("Error:", e$message)))
      })
    })
    
    # Key Correlations Insights
    shiny::observe({
      req(filteredData())
      tryCatch({
        df <- filteredData()
        likertData <- extractLikertValues(df, likertCols)
        corMatrix <- cor(likertData, method = "pearson", use = "pairwise.complete.obs")
        
        corLong <- reshape2::melt(corMatrix)
        corLong <- corLong[corLong$Var1 != corLong$Var2, ]
        corLong <- corLong[order(-abs(corLong$Correlation)), ]
        
        topPositive <- head(corLong[corLong$Correlation > 0, ], 3)
        topNegative <- head(corLong[corLong$Correlation < 0, ], 3)
        
        posHtml <- paste(sapply(1:nrow(topPositive), function(i) {
          htmltools::tags$li(sprintf("%s & %s: r=%.2f", likertNames[topPositive$Var1[i]], 
                                     likertNames[topPositive$Var2[i]], topPositive$Correlation[i]))
        }), collapse = "")
        negHtml <- paste(sapply(1:nrow(topNegative), function(i) {
          htmltools::tags$li(sprintf("%s & %s: r=%.2f", likertNames[topNegative$Var1[i]], 
                                     likertNames[topNegative$Var2[i]], topNegative$Correlation[i]))
        }), collapse = "")
        
        htmltools::runJavaScript(paste0("document.getElementById('", ns("positiveCorrelationsList"), "').innerHTML = '", 
                                        htmltools::renderTags(htmltools::tags$div(posHtml))$html, "';"))
        htmltools::runJavaScript(paste0("document.getElementById('", ns("negativeCorrelationsList"), "').innerHTML = '",
                                        htmltools::renderTags(htmltools::tags$div(negHtml))$html, "';"))
      }, error = function(e) {})
    })
    
    # 2. Regression Analysis for Satisfaction Prediction
    output$predictorsTableBody <- shiny::renderUI({
      req(filteredData())
      tryCatch({
        df <- filteredData()
        likertData <- extractLikertValues(df, likertCols)
        likertData <- likertData[complete.cases(likertData), ]
        
        satCol <- 1  # First column is satisfaction (COL_COURSE_SATISFACTION_1)
        predictors <- likertData[, -satCol, drop = FALSE]
        outcome <- likertData[, satCol]
        
        model <- lm(outcome ~ ., data = as.data.frame(predictors))
        coefs <- coef(model)[-1]
        stdCoefs <- abs(coefs)
        relImportance <- round(100 * stdCoefs / sum(stdCoefs), 1)
        
        predOrder <- order(-stdCoefs)
        rows <- lapply(predOrder, function(i) {
          direction <- ifelse(coefs[i] > 0, "↑ Positive", "↓ Negative")
          htmltools::tags$tr(
            htmltools::tags$td(likertNames[i + 1]),
            htmltools::tags$td(htmltools::HTML(direction)),
            htmltools::tags$td(sprintf("%.3f", coefs[i])),
            htmltools::tags$td(sprintf("%.1f%%", relImportance[i]))
          )
        })
        htmltools::tags$div(rows)
      }, error = function(e) {
        htmltools::tags$p("Error computing regression")
      })
    })
    
    # 3. Student Segmentation (K-means Clustering)
    output$clusterProfilesPlot <- ggiraph::renderGirafe({
      req(filteredData())
      tryCatch({
        df <- filteredData()
        likertData <- extractLikertValues(df, likertCols)
        likertData <- likertData[complete.cases(likertData), ]
        
        n <- nrow(likertData)
        k <- min(3, max(2, floor(n / 20)))
        set.seed(42)
        km <- kmeans(scale(likertData), centers = k, nstart = 25)
        
        clusterMeans <- aggregate(likertData, by = list(km$cluster), FUN = mean)
        clusterLong <- reshape2::melt(clusterMeans, id.vars = "Group.1")
        names(clusterLong) <- c("Cluster", "Question", "MeanScore")
        clusterLong$Question <- likertNames[clusterLong$Question]
        
        p <- ggplot2::ggplot(clusterLong, ggplot2::aes(x = Question, y = MeanScore, fill = factor(Cluster))) +
          ggplot2::geom_bar(stat = "identity", position = "dodge") +
          ggplot2::coord_flip() +
          ggplot2::scale_fill_brewer(palette = "Set2") +
          ggplot2::theme_minimal() +
          ggplot2::theme(axis.text.y = ggplot2::element_text(size = 7), legend.position = "bottom")
        
        ggiraph::girafe(ggobj = p)
      }, error = function(e) {
        ggiraph::girafe(ggplot2::ggplot() + ggplot2::labs(title = paste("Error:", e$message)))
      })
    })
    
    shiny::observe({
      req(filteredData())
      tryCatch({
        df <- filteredData()
        likertData <- extractLikertValues(df, likertCols)
        likertData <- likertData[complete.cases(likertData), ]
        
        n <- nrow(likertData)
        k <- min(3, max(2, floor(n / 20)))
        set.seed(42)
        km <- kmeans(scale(likertData), centers = k, nstart = 25)
        
        clusterSizes <- table(km$cluster)
        clusterPct <- round(100 * clusterSizes / sum(clusterSizes), 1)
        
        sizesHtml <- paste(sapply(names(clusterSizes), function(i) {
          htmltools::tags$div(class = "segment-size-item",
            htmltools::tags$span(sprintf("Segment %s: %d students (%.1f%%)", i, clusterSizes[i], clusterPct[i]))
          )
        }), collapse = "")
        
        profilesHtml <- paste(sapply(1:k, function(i) {
          clusterData <- likertData[km$cluster == i, , drop = FALSE]
          means <- colMeans(clusterData, na.rm = TRUE)
          avgScore <- round(mean(means), 2)
          label <- if (avgScore >= 4) "High Engagement" else if (avgScore >= 3) "Moderate Engagement" else "Needs Support"
          htmltools::tags$div(class = "segment-profile-card",
            htmltools::tags$h5(sprintf("Segment %s: %s", i, label)),
            htmltools::tags$p(sprintf("Average Score: %.2f | Size: %.1f%%", avgScore, clusterPct[i]))
          )
        }), collapse = "")
        
        htmltools::runJavaScript(paste0("document.getElementById('", ns("segmentSizesContainer"), "').innerHTML = '",
                                        htmltools::renderTags(htmltools::tags$div(sizesHtml))$html, "';"))
        htmltools::runJavaScript(paste0("document.getElementById('", ns("segmentProfilesContainer"), "').innerHTML = '",
                                        htmltools::renderTags(htmltools::tags$div(profilesHtml))$html, "';"))
      }, error = function(e) {})
    })
    
    # 4. Section Comparison Analysis
    output$sectionComparisonResults <- shiny::renderUI({
      req(filteredData())
      tryCatch({
        df <- filteredData()
        sections <- unique(df$section[df$section != ""])
        
        if (length(sections) < 2) return(htmltools::tags$p("Need 2+ sections for comparison"))
        
        results <- lapply(1:length(likertCols), function(i) {
          col <- likertCols[i]
          name <- likertNames[i]
          values <- as.numeric(stringr::str_extract(as.character(df[[col]]), "[1-5]"))
          
          anovaResult <- tryCatch({
            model <- lm(values ~ df$section)
            anova(model)
          }, error = function(e) NULL)
          
          if (!is.null(anovaResult) && anovaResult$`Pr(>F)`[1] < 0.05) {
            htmltools::tags$div(class = "comparison-result significant",
              htmltools::tags$strong(name),
              htmltools::tags$p(sprintf("Significant difference (p=%.3f)", anovaResult$`Pr(>F)`[1]))
            )
          } else {
            htmltools::tags$div(class = "comparison-result",
              htmltools::tags$strong(name),
              htmltools::tags$p("No significant difference")
            )
          }
        })
        htmltools::tags$div(results)
      }, error = function(e) {
        htmltools::tags$p("Error in section comparison")
      })
    })
    
    # 5. Effect Size Analysis
    output$effectSizeResults <- shiny::renderUI({
      req(filteredData())
      tryCatch({
        df <- filteredData()
        sections <- unique(df$section[df$section != ""])
        
        if (length(sections) < 2) return(htmltools::tags$p("Need 2+ sections"))
        
        effectSizes <- lapply(1:length(likertCols), function(i) {
          col <- likertCols[i]
          name <- likertNames[i]
          values <- as.numeric(stringr::str_extract(as.character(df[[col]]), "[1-5]"))
          
          s1 <- values[df$section == sections[1]]
          s2 <- values[df$section == sections[2]]
          
          if (length(s1) > 1 && length(s2) > 1) {
            pooledSd <- sqrt(((length(s1)-1)*sd(s1)^2 + (length(s2)-1)*sd(s2)^2) / (length(s1)+length(s2)-2))
            cohensD <- if (pooledSd > 0) (mean(s1) - mean(s2)) / pooledSd else 0
            
            interpretation <- if (abs(cohensD) < 0.5) "Small" else if (abs(cohensD) < 0.8) "Medium" else "Large"
            
            htmltools::tags$div(class = "effect-size-item",
              htmltools::tags$span(sprintf("%s: d=%.2f (%s)", name, cohensD, interpretation))
            )
          }
        })
        htmltools::tags$div(effectSizes)
      }, error = function(e) {
        htmltools::tags$p("Error calculating effect sizes")
      })
    })
    
    # 6. Reliability Analysis (Cronbach's Alpha)
    output$reliabilityResults <- shiny::renderUI({
      req(filteredData())
      tryCatch({
        df <- filteredData()
        
        categories <- list(
          list(name = "Course Satisfaction", cols = 8:13),
          list(name = "Learning Methods", cols = 14:23),
          list(name = "Community & Belonging", cols = 27:31)
        )
        
        calcAlpha <- function(values) {
          k <- ncol(values)
          if (k < 2) return(NA)
          variances <- apply(values, 2, var, na.rm = TRUE)
          totalVar <- var(rowSums(values, na.rm = TRUE), na.rm = TRUE)
          alpha <- (k / (k - 1)) * (1 - sum(variances) / totalVar)
          return(alpha)
        }
        
        results <- lapply(categories, function(cat) {
          values <- extractLikertValues(df, cat$cols)
          alpha <- calcAlpha(values)
          
          interpretation <- if (is.na(alpha)) "N/A" else if (alpha >= 0.9) "Excellent" else if (alpha >= 0.8) "Good" 
                           else if (alpha >= 0.7) "Acceptable" else if (alpha >= 0.6) "Questionable" else "Poor"
          
          htmltools::tags$div(class = "reliability-card",
            htmltools::tags$h5(cat$name),
            htmltools::tags$p(sprintf("Cronbach's α: %.3f (%s)", alpha, interpretation))
          )
        })
        htmltools::tags$div(results)
      }, error = function(e) {
        htmltools::tags$p("Error calculating reliability")
      })
    })
  })
}