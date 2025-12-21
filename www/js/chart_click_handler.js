$(document).ready(function() {
  // Function to handle clicks on y-axis labels only
  function handleYAxisClicks(plotId, plotData) {
    // Function to try to attach click handlers
    function tryAttachHandlers() {
      // Get the plot container
      var plotContainer = $('#' + plotId);
      
      if (plotContainer.length === 0) {
        console.log("Plot container not found:", plotId);
        return false;
      }
      
      // Create a mapping of question labels to question keys
      var labelToKey = {};
      for (var key in plotData) {
        labelToKey[plotData[key]] = key;
      }
      
      console.log("Question label to key mapping:", labelToKey);
      
      // Try multiple selectors for y-axis labels to handle different ggplot2 output structures
      var labelSelectors = [
        '.axis.y .tick text',
        '.y-axis .tick text',
        '.ytick text',
        '.tick text',
        'text[transform*="translate(0"]', // This targets y-axis labels specifically
        'g[transform*="translate(0"] text', // More specific selector for ggplot2
        'svg text' // Fallback to all text elements in SVG
      ];
      
      var yAxisLabels = $();
      
      // Try each selector until we find the y-axis labels
      for (var i = 0; i < labelSelectors.length; i++) {
        var potentialLabels = plotContainer.find(labelSelectors[i]);
        
        // Filter to only include labels that match our plot data
        var matchingLabels = potentialLabels.filter(function() {
          var text = $(this).text().trim();
          return labelToKey.hasOwnProperty(text);
        });
        
        if (matchingLabels.length > 0) {
          yAxisLabels = matchingLabels;
          console.log("Found y-axis labels with selector:", labelSelectors[i]);
          break;
        }
      }
      
      if (yAxisLabels.length === 0) {
        console.log("No y-axis labels found that match our question data");
        console.log("Available text elements:", plotContainer.find('text').map(function() {
          return $(this).text().trim();
        }).get());
        return false;
      }
      
      console.log("Found", yAxisLabels.length, "y-axis labels for click handling");
      
      // Add click event to each y-axis label
      yAxisLabels.each(function() {
        var $label = $(this);
        
        // Make sure we're not adding multiple event handlers
        $label.off('click.yaxis').css('cursor', 'pointer');
        
        $label.on('click.yaxis', function(e) {
          e.preventDefault();
          e.stopPropagation();
          
          var labelText = $(this).text().trim();
          var questionKey = labelToKey[labelText];
          
          if (questionKey) {
            console.log("Y-axis label clicked:", labelText, "Question key:", questionKey);
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
      
      return true; // Successfully attached handlers
    }
    
    // Try immediately
    if (tryAttachHandlers()) {
      return;
    }
    
    // If not successful, try multiple times with increasing delays
    var attempts = 0;
    var maxAttempts = 10;
    var baseDelay = 500;
    
    function attemptWithDelay() {
      attempts++;
      
      if (attempts > maxAttempts) {
        console.log("Max attempts reached for y-axis click handlers");
        return;
      }
      
      setTimeout(function() {
        if (!tryAttachHandlers()) {
          attemptWithDelay(); // Try again with longer delay
        }
      }, baseDelay * attempts);
    }
    
    attemptWithDelay();
  }
  
  // Set up custom message handlers once
  Shiny.addCustomMessageHandler('dist_plot_data', function(data) {
    console.log("Received distribution plot data:", data);
    handleYAxisClicks('response_distribution_plot', data);
  });
  
  Shiny.addCustomMessageHandler('length_plot_data', function(data) {
    console.log("Received length plot data:", data);
    handleYAxisClicks('response_length_plot', data);
  });
  
  // Observe when plots are updated
  $(document).on('shiny:value', function(event) {
    if (event.name === 'response_distribution_plot') {
      // Request the plot data
      Shiny.onInputChange('get_dist_plot_data', Math.random());
    }
    
    if (event.name === 'response_length_plot') {
      // Request the plot data
      Shiny.onInputChange('get_length_plot_data', Math.random());
    }
  });
});