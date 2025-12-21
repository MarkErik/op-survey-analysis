$(document).ready(function() {
  // Function to handle clicks on y-axis labels
  function handleYAxisClick(plotId, plotData) {
    // Wait for the plot to be fully rendered
    setTimeout(function() {
      // Get the plot container
      var plotContainer = $('#' + plotId);
      
      // Try multiple selectors for y-axis labels to handle different ggplot2 output structures
      var selectors = [
        '.axis.y .tick text',
        '.y-axis .tick text',
        '.ytick text',
        '.tick text',
        'text[transform*="translate(0"]' // This targets y-axis labels specifically
      ];
      
      // Try each selector until we find the y-axis labels
      for (var i = 0; i < selectors.length; i++) {
        var yAxisLabels = plotContainer.find(selectors[i]);
        
        if (yAxisLabels.length > 0) {
          // Filter to only include labels that match our plot data
          yAxisLabels = yAxisLabels.filter(function() {
            var labelText = $(this).text().trim();
            for (var key in plotData) {
              if (plotData[key] === labelText) {
                return true;
              }
            }
            return false;
          });
          
          if (yAxisLabels.length > 0) {
            // Add click event to each y-axis label
            yAxisLabels.each(function() {
              var $label = $(this);
              
              // Make sure we're not adding multiple event handlers
              $label.off('click.yaxis').css('cursor', 'pointer');
              
              $label.on('click.yaxis', function(e) {
                e.preventDefault();
                e.stopPropagation();
                
                var labelText = $(this).text().trim();
                
                // Find the corresponding question key
                var questionKey = null;
                for (var key in plotData) {
                  if (plotData[key] === labelText) {
                    questionKey = key;
                    break;
                  }
                }
                
                if (questionKey) {
                  // Trigger a Shiny input event with the question key
                  Shiny.onInputChange('y_axis_click', {
                    question: questionKey,
                    label: labelText,
                    plotId: plotId,
                    nonce: Math.random() // Force reactivity
                  });
                }
                
                return false; // Prevent default behavior
              });
            });
            
            // Break out of the loop once we've successfully added handlers
            break;
          }
        }
      }
    }, 2000); // Wait longer for the plot to fully render
  }
  
  // Function to handle clicks on bars
  function handleBarClick(plotId, plotData) {
    // Wait for the plot to be fully rendered
    setTimeout(function() {
      // Get the plot container
      var plotContainer = $('#' + plotId);
      
      // Find the main plot area
      var plotArea = plotContainer.find('.mainPlot, .plot, .ggplot, .plot-container');
      if (plotArea.length === 0) {
        plotArea = plotContainer; // Fallback to the entire container
      }
      
      // Find all rectangles in the plot area
      var allRects = plotArea.find('rect');
      
      // Filter to find only the actual bars (not background, grid lines, etc.)
      var bars = allRects.filter(function() {
        var $rect = $(this);
        var width = parseFloat($rect.attr('width'));
        var height = parseFloat($rect.attr('height'));
        var fill = $rect.attr('fill');
        var stroke = $rect.attr('stroke');
        
        // Filter out elements that are likely not bars:
        // 1. Too small (likely grid lines or markers)
        // 2. No fill or transparent fill
        // 3. Have a stroke (likely borders or grid lines)
        return width > 10 && height > 10 &&
               fill && fill !== 'none' && fill !== '#ffffff' && fill !== 'white' &&
               (!stroke || stroke === 'none');
      });
      
      // Add click event to each bar
      bars.each(function() {
        var $bar = $(this);
        
        // Make sure we're not adding multiple event handlers
        $bar.off('click.bar').css('cursor', 'pointer');
        
        $bar.on('click.bar', function(e) {
          e.preventDefault();
          e.stopPropagation();
          
          // Get the bar's position and dimensions
          var y = parseFloat($(this).attr('y'));
          var height = parseFloat($(this).attr('height'));
          
          // Calculate the center of the bar
          var centerY = y + height / 2;
          
          // Find the closest y-axis label by position
          var closestLabel = null;
          var minDistance = Infinity;
          
          // Try multiple selectors for y-axis labels
          var labelSelectors = [
            '.axis.y .tick text',
            '.y-axis .tick text',
            '.ytick text',
            '.tick text',
            'text[transform*="translate(0"]'
          ];
          
          for (var i = 0; i < labelSelectors.length; i++) {
            var yAxisLabels = plotContainer.find(labelSelectors[i]);
            
            yAxisLabels.each(function() {
              var $label = $(this);
              var labelText = $label.text().trim();
              
              // Only consider labels that are in our plot data
              var isValidLabel = false;
              for (var key in plotData) {
                if (plotData[key] === labelText) {
                  isValidLabel = true;
                  break;
                }
              }
              
              if (isValidLabel) {
                // Get the position of the label
                var labelY = parseFloat($label.attr('y'));
                
                // If y attribute is not available or NaN, try to get it from the transform
                if (isNaN(labelY)) {
                  var transform = $label.attr('transform');
                  if (transform) {
                    var match = transform.match(/translate\([^,]+,\s*([^)]+)\)/);
                    if (match) {
                      labelY = parseFloat(match[1]);
                    }
                  }
                }
                
                if (!isNaN(labelY)) {
                  var distance = Math.abs(labelY - centerY);
                  
                  if (distance < minDistance) {
                    minDistance = distance;
                    closestLabel = labelText;
                  }
                }
              }
            });
          }
          
          if (closestLabel) {
            // Find the corresponding question key
            var questionKey = null;
            for (var key in plotData) {
              if (plotData[key] === closestLabel) {
                questionKey = key;
                break;
              }
            }
            
            if (questionKey) {
              // Trigger a Shiny input event with the question key and plot ID
              Shiny.onInputChange('bar_click', {
                question: questionKey,
                label: closestLabel,
                plotId: plotId,
                nonce: Math.random() // Force reactivity
              });
            }
          }
          
          return false; // Prevent default behavior
        });
      });
    }, 2000); // Wait longer for the plot to fully render
  }
  
  // Observe when plots are updated
  $(document).on('shiny:value', function(event) {
    if (event.name === 'response_distribution_plot') {
      // Get the plot data for distribution plot
      Shiny.addCustomMessageHandler('dist_plot_data', function(data) {
        handleYAxisClick('response_distribution_plot', data);
        handleBarClick('response_distribution_plot', data);
      });
      
      // Request the plot data
      Shiny.onInputChange('get_dist_plot_data', Math.random());
    }
    
    if (event.name === 'response_length_plot') {
      // Get the plot data for length plot
      Shiny.addCustomMessageHandler('length_plot_data', function(data) {
        handleYAxisClick('response_length_plot', data);
        handleBarClick('response_length_plot', data);
      });
      
      // Request the plot data
      Shiny.onInputChange('get_length_plot_data', Math.random());
    }
  });
});