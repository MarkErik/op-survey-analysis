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
      'svg text',
      // More specific selectors for ggplot2
      '.axis .tick text',
      '.axis text',
      'g.tick text',
      'text.axis',
      'text.y',
      'text.y.axis',
      // Even more general selectors
      'text',
      'g text',
      'svg *'
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
        return false;
      }
      
      // Create a mapping of question labels to question keys
      const labelToKey = {};
      for (const key in plotData) {
        labelToKey[plotData[key]] = key;
      }
      
      // Try to find y-axis labels using multiple selectors
      const yAxisLabels = findYAxisLabels(plotContainer, labelToKey);
      
      if (yAxisLabels.length === 0) {
        return false;
      }
      
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
          
          // Skip empty text
          if (!text) return false;
          
          // Try exact match first
          if (labelToKey.hasOwnProperty(text)) {
            return true;
          }
          
          // If no exact match, try partial match (in case of truncation)
          for (const key in labelToKey) {
            const label = labelToKey[key];
            
            // Try different matching strategies
            if (text.includes(label) || label.includes(text)) {
              // Store the mapping for this partial match
              $(this).data('questionKey', key);
              return true;
            }
            
            // Try matching with cleaned text (remove special characters, extra spaces)
            const cleanText = text.replace(/[^\w\s]/g, '').replace(/\s+/g, ' ').trim();
            const cleanLabel = label.replace(/[^\w\s]/g, '').replace(/\s+/g, ' ').trim();
            
            if (cleanText.includes(cleanLabel) || cleanLabel.includes(cleanText)) {
              // Store the mapping for this partial match
              $(this).data('questionKey', key);
              return true;
            }
            
            // Try matching first few words (in case of truncation)
            const textWords = text.split(' ').slice(0, 3).join(' ');
            const labelWords = label.split(' ').slice(0, 3).join(' ');
            
            if (textWords.includes(labelWords) || labelWords.includes(textWords)) {
              // Store the mapping for this partial match
              $(this).data('questionKey', key);
              return true;
            }
          }
          
          return false;
        });
        
        if (matchingLabels.length > 0) {
          yAxisLabels = matchingLabels;
          break;
        }
      }
      
      return yAxisLabels;
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
          let questionKey = labelToKey[labelText];
          
          // If no exact match found, try to get the stored key from partial match
          if (!questionKey) {
            questionKey = $(this).data('questionKey');
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
    handleYAxisClicks('response_distribution_plot', data);
  });
  
  Shiny.addCustomMessageHandler('length_plot_data', function(data) {
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