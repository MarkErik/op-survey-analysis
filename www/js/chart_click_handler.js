/**
 * Chart Click Handler for Survey Explorer
 * Handles click events on y-axis labels of ggplot2 charts in Shiny
 */

$(document).ready(function() {
  // Configuration constants
  const CONFIG = {
    MAX_ATTEMPTS: 10,
    BASE_DELAY: 500,
    SELECTORS: [
      '.axis.y .tick text',
      '.y-axis .tick text',
      '.ytick text',
      '.tick text',
      'text[transform*="translate(0"]',
      'g[transform*="translate(0"] text',
      'svg text'
    ]
  };

  /**
   * Handles clicks on y-axis labels of a plot
   * @param {string} plotId - The ID of the plot container
   * @param {Object} plotData - Mapping of question keys to labels
   */
  function handleYAxisClicks(plotId, plotData) {
    /**
     * Attempts to attach click handlers to y-axis labels
     * @returns {boolean} True if handlers were attached successfully
     */
    function tryAttachHandlers() {
      const plotContainer = $(`#${plotId}`);
      
      if (plotContainer.length === 0) {
        console.log("Plot container not found:", plotId);
        return false;
      }
      
      // Create a mapping of question labels to question keys
      const labelToKey = {};
      for (const key in plotData) {
        labelToKey[plotData[key]] = key;
      }
      
      console.log("Question label to key mapping:", labelToKey);
      
      // Try to find y-axis labels using multiple selectors
      const yAxisLabels = findYAxisLabels(plotContainer, labelToKey);
      
      if (yAxisLabels.length === 0) {
        logAvailableTextElements(plotContainer);
        return false;
      }
      
      console.log(`Found ${yAxisLabels.length} y-axis labels for click handling`);
      
      // Add click event to each y-axis label
      attachClickHandlers(yAxisLabels, labelToKey);
      
      return true;
    }
    
    /**
     * Finds y-axis labels that match our plot data
     * @param {jQuery} plotContainer - The plot container element
     * @param {Object} labelToKey - Mapping of labels to keys
     * @returns {jQuery} Matching y-axis labels
     */
    function findYAxisLabels(plotContainer, labelToKey) {
      let yAxisLabels = $();
      
      // Try each selector until we find the y-axis labels
      for (const selector of CONFIG.SELECTORS) {
        const potentialLabels = plotContainer.find(selector);
        
        // Filter to only include labels that match our plot data
        const matchingLabels = potentialLabels.filter(function() {
          const text = $(this).text().trim();
          return labelToKey.hasOwnProperty(text);
        });
        
        if (matchingLabels.length > 0) {
          yAxisLabels = matchingLabels;
          console.log("Found y-axis labels with selector:", selector);
          break;
        }
      }
      
      return yAxisLabels;
    }
    
    /**
     * Logs available text elements for debugging
     * @param {jQuery} plotContainer - The plot container element
     */
    function logAvailableTextElements(plotContainer) {
      console.log("No y-axis labels found that match our question data");
      console.log("Available text elements:", plotContainer.find('text').map(function() {
        return $(this).text().trim();
      }).get());
    }
    
    /**
     * Attaches click handlers to y-axis labels
     * @param {jQuery} yAxisLabels - The y-axis label elements
     * @param {Object} labelToKey - Mapping of labels to keys
     */
    function attachClickHandlers(yAxisLabels, labelToKey) {
      yAxisLabels.each(function() {
        const $label = $(this);
        
        // Make sure we're not adding multiple event handlers
        $label.off('click.yaxis').css('cursor', 'pointer');
        
        $label.on('click.yaxis', function(e) {
          e.preventDefault();
          e.stopPropagation();
          
          const labelText = $(this).text().trim();
          const questionKey = labelToKey[labelText];
          
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
    }
    
    // Try to attach handlers immediately
    if (tryAttachHandlers()) {
      return;
    }
    
    // If not successful, try multiple times with increasing delays
    let attempts = 0;
    
    function attemptWithDelay() {
      attempts++;
      
      if (attempts > CONFIG.MAX_ATTEMPTS) {
        console.log("Max attempts reached for y-axis click handlers");
        return;
      }
      
      setTimeout(function() {
        if (!tryAttachHandlers()) {
          attemptWithDelay(); // Try again with longer delay
        }
      }, CONFIG.BASE_DELAY * attempts);
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