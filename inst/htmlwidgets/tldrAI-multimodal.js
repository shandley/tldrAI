HTMLWidgets.widget({
  name: 'tldrAI_multimodal',
  type: 'output',
  
  factory: function(el, width, height) {
    // Initialize empty widget instance that will be extended
    const instance = {};
    
    // Initialize the visualization containers and tracking
    instance.visualizationsLoaded = {};
    
    return {
      renderValue: function(x) {
        // Insert the interface HTML
        el.innerHTML = x.interface;
        
        // Set up tab switching functionality
        setupTabs(el, x);
        
        // Load the first visualization (active by default)
        const firstTabType = x.tabs[0];
        loadVisualization(el, x.func_name, x.metadata, firstTabType);
      },
      
      resize: function(width, height) {
        // Adjust container size if needed
        el.style.width = width + 'px';
        el.style.height = height + 'px';
      }
    };
  }
});

/**
 * Set up the tabbed interface and click handlers
 */
function setupTabs(el, x) {
  // Get all tab elements
  const tabs = el.querySelectorAll('.tldrAI-tab');
  const tabPanels = el.querySelectorAll('.tldrAI-tab-panel');
  
  // Add click event listeners to tabs
  tabs.forEach(tab => {
    tab.addEventListener('click', function() {
      // Deactivate all tabs
      tabs.forEach(t => {
        t.classList.remove('tldrAI-tab-active');
        t.style.borderBottom = '3px solid transparent';
      });
      
      // Hide all panels
      tabPanels.forEach(panel => {
        panel.style.display = 'none';
      });
      
      // Activate clicked tab
      this.classList.add('tldrAI-tab-active');
      this.style.borderBottom = '3px solid #FBBC05';
      
      // Show corresponding panel
      const targetPanelId = this.getAttribute('data-target');
      const targetPanel = el.querySelector('#' + targetPanelId);
      targetPanel.style.display = 'block';
      
      // Get the visualization type
      const visType = targetPanelId.replace('panel-', '');
      
      // Load the visualization if not already loaded
      loadVisualization(el, x.func_name, x.metadata, visType);
    });
  });
}

/**
 * Load a visualization into its container
 */
function loadVisualization(el, funcName, metadata, visType) {
  // Get the visualization container
  const vizContainer = el.querySelector(`#viz-${visType}`);
  
  // Check if already loaded
  if (vizContainer.getAttribute('data-loaded') === 'true') {
    return;
  }
  
  // Show loading indicator
  const loadingDiv = el.querySelector(`#panel-${visType} .tldrAI-loading`);
  if (loadingDiv) {
    loadingDiv.style.display = 'flex';
  }
  
  // Use Shiny.renderDependencies or direct R call through HTMLWidgets.evaluateStringMember
  // to load the visualization
  try {
    // Using R's callback mechanism to generate visualization
    const callback = 'load_visualization_tab';
    
    // Prepare parameters
    const params = {
      func_name: funcName,
      metadata: metadata,
      vis_type: visType
    };
    
    // Call to R function
    if (typeof window.Shiny !== 'undefined') {
      // If in Shiny environment
      Shiny.setInputValue('tldrAI_multimodal_vizRequest', {
        func_name: funcName,
        metadata: metadata,
        vis_type: visType,
        container_id: vizContainer.id,
        callback: callback
      });
    } else {
      // Direct HTMLWidgets call (for non-Shiny use)
      HTMLWidgets.evaluateStringMember(callback, params, function(err, visualization) {
        if (err) {
          console.error("Error loading visualization:", err);
          showErrorInContainer(vizContainer, "Failed to load visualization");
        } else {
          // Insert visualization content
          vizContainer.innerHTML = visualization;
          vizContainer.setAttribute('data-loaded', 'true');
          
          // Hide loading indicator
          if (loadingDiv) {
            loadingDiv.style.display = 'none';
          }
        }
      });
    }
  } catch (error) {
    console.error("Error setting up visualization:", error);
    showErrorInContainer(vizContainer, "Failed to load visualization");
    
    // Hide loading indicator
    if (loadingDiv) {
      loadingDiv.style.display = 'none';
    }
  }
}

/**
 * Display an error message in the visualization container
 */
function showErrorInContainer(container, message) {
  container.innerHTML = `
    <div style="color: #D32F2F; text-align: center; padding: 20px;">
      <p style="font-size: 16px; margin-bottom: 10px;">${message}</p>
      <p>Please ensure required packages are installed for this visualization type.</p>
    </div>
  `;
  container.setAttribute('data-loaded', 'true');
}