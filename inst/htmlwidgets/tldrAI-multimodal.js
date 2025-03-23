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
        // Process and insert the interface HTML
        try {
          console.log("Rendering tldrAI multimodal interface");
          
          // If x.interface is an HTML string, parse it
          let interfaceContent = '';
          if (typeof x.interface === 'string') {
            // Fix for any malformed HTML by wrapping in a div
            interfaceContent = '<div>' + x.interface + '</div>';
            
            // Parse the HTML string
            const parser = new DOMParser();
            const doc = parser.parseFromString(interfaceContent, 'text/html');
            
            // Check if parsing succeeded
            if (doc.body) {
              // Clear the element first
              el.innerHTML = '';
              
              // Get the main container (should be the div we're looking for)
              const container = doc.querySelector('.tldrAI-multimodal-container');
              if (container) {
                el.appendChild(container);
              } else {
                // If not found, append all body elements as fallback
                Array.from(doc.body.childNodes).forEach(node => {
                  el.appendChild(node.cloneNode(true));
                });
              }
            } else {
              // Fallback for parse failure
              el.innerHTML = interfaceContent;
            }
          } else {
            // Direct fallback if not a string
            el.innerHTML = '<div class="tldrAI-error">Error rendering interface</div>';
          }
        } catch (error) {
          console.error("Error rendering interface:", error);
          el.innerHTML = '<div class="tldrAI-error">Error rendering interface: ' + error.message + '</div>';
        }
        
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
  
  // Get theme accent color (fallback to default if not provided)
  const accentColor = (x.theme === 'dark') ? '#FBBC05' : '#FBBC05';
  
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
      this.style.borderBottom = `3px solid ${accentColor}`;
      
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
  
  // Get theme from container (light or dark)
  const isDarkTheme = document.querySelector('.tldrAI-multimodal-container')?.style.backgroundColor === '#1E1E1E';
  
  // Colors based on theme
  const colors = {
    primary: isDarkTheme ? '#4285F4' : '#4285F4',
    secondary: isDarkTheme ? '#34A853' : '#34A853',
    accent: isDarkTheme ? '#FBBC05' : '#FBBC05',
    background: isDarkTheme ? '#2D2D2D' : '#F5F7F9',
    border: isDarkTheme ? '#444444' : '#DADCE0',
    text: isDarkTheme ? '#E8E8E8' : '#333333',
    codeBackground: isDarkTheme ? '#2D2D30' : '#F5F7F9',
    codeText: isDarkTheme ? '#D4D4D4' : '#24292E'
  };
  
  // Create a simple fallback visualization for testing
  let fallbackViz = "";
  
  if (visType === "diagram") {
    fallbackViz = `
      <div style="text-align: center; padding: 20px;">
        <h3 style="color: ${colors.primary};">Function Flow Diagram</h3>
        <div style="margin: 20px auto; max-width: 500px; border: 2px solid ${colors.primary}; border-radius: 8px; padding: 20px; background-color: ${colors.background};">
          <div style="display: flex; justify-content: space-between; align-items: center;">
            <div style="background: ${isDarkTheme ? '#1E3A5F' : '#E8F0FE'}; padding: 10px; border-radius: 4px; border: 1px solid ${colors.primary}; color: ${colors.text};">
              <strong>Input</strong><br>
              ${metadata.args ? metadata.args.join('<br>') : 'data'}
            </div>
            <div style="font-size: 24px; color: ${colors.primary};">→</div>
            <div style="background: ${colors.primary}; color: white; padding: 10px; border-radius: 4px;">
              <strong>${funcName.split('::').pop()}</strong>
            </div>
            <div style="font-size: 24px; color: ${colors.primary};">→</div>
            <div style="background: ${isDarkTheme ? '#1A3726' : '#E6F4EA'}; padding: 10px; border-radius: 4px; border: 1px solid ${colors.secondary}; color: ${colors.text};">
              <strong>Output</strong><br>
              ${metadata.returns || 'filtered data'}
            </div>
          </div>
        </div>
      </div>
    `;
  } else if (visType === "data_flow") {
    fallbackViz = `
      <div style="text-align: center; padding: 20px;">
        <h3 style="color: ${colors.primary};">Data Transformation</h3>
        <div style="display: flex; justify-content: space-between; margin-top: 20px;">
          <div style="flex: 1; text-align: left; border: 1px solid ${colors.border}; padding: 10px; border-radius: 4px; background-color: ${isDarkTheme ? '#2D2D2D' : 'white'};">
            <h4 style="color: ${colors.secondary}; margin-top: 0;">Before</h4>
            <table style="width: 100%; border-collapse: collapse; color: ${colors.text};">
              <thead style="background-color: ${isDarkTheme ? '#3D3D3D' : '#F1F3F4'};">
                <tr>
                  <th style="padding: 8px;">id</th>
                  <th style="padding: 8px;">name</th>
                  <th style="padding: 8px;">age</th>
                  <th style="padding: 8px;">score</th>
                </tr>
              </thead>
              <tbody>
                <tr><td style="padding: 8px;">1</td><td style="padding: 8px;">Alice</td><td style="padding: 8px;">25</td><td style="padding: 8px;">85</td></tr>
                <tr><td style="padding: 8px;">2</td><td style="padding: 8px;">Bob</td><td style="padding: 8px;">17</td><td style="padding: 8px;">92</td></tr>
                <tr><td style="padding: 8px;">3</td><td style="padding: 8px;">Charlie</td><td style="padding: 8px;">32</td><td style="padding: 8px;">78</td></tr>
                <tr><td style="padding: 8px;">4</td><td style="padding: 8px;">David</td><td style="padding: 8px;">41</td><td style="padding: 8px;">65</td></tr>
                <tr><td style="padding: 8px;">5</td><td style="padding: 8px;">Eve</td><td style="padding: 8px;">19</td><td style="padding: 8px;">91</td></tr>
              </tbody>
            </table>
          </div>
          <div style="display: flex; flex-direction: column; justify-content: center; align-items: center; margin: 0 20px;">
            <div style="background-color: ${colors.primary}; color: white; padding: 10px; border-radius: 4px; margin-bottom: 10px;">
              ${funcName.split('::').pop()}
            </div>
            <div style="font-size: 24px; color: ${colors.primary};">→</div>
          </div>
          <div style="flex: 1; text-align: left; border: 1px solid ${colors.border}; padding: 10px; border-radius: 4px; background-color: ${isDarkTheme ? '#2D2D2D' : 'white'};">
            <h4 style="color: #EA4335; margin-top: 0;">After</h4>
            <table style="width: 100%; border-collapse: collapse; color: ${colors.text};">
              <thead style="background-color: ${isDarkTheme ? '#3D3D3D' : '#F1F3F4'};">
                <tr>
                  <th style="padding: 8px;">id</th>
                  <th style="padding: 8px;">name</th>
                  <th style="padding: 8px;">age</th>
                  <th style="padding: 8px;">score</th>
                </tr>
              </thead>
              <tbody>
                <tr style="background-color: ${isDarkTheme ? '#3A3216' : '#FFF0E0'};"><td style="padding: 8px;">1</td><td style="padding: 8px;">Alice</td><td style="padding: 8px;">25</td><td style="padding: 8px;">85</td></tr>
                <tr style="background-color: ${isDarkTheme ? '#3A3216' : '#FFF0E0'};"><td style="padding: 8px;">3</td><td style="padding: 8px;">Charlie</td><td style="padding: 8px;">32</td><td style="padding: 8px;">78</td></tr>
                <tr style="background-color: ${isDarkTheme ? '#3A3216' : '#FFF0E0'};"><td style="padding: 8px;">4</td><td style="padding: 8px;">David</td><td style="padding: 8px;">41</td><td style="padding: 8px;">65</td></tr>
              </tbody>
            </table>
          </div>
        </div>
      </div>
    `;
  } else if (visType === "function_network") {
    fallbackViz = `
      <div style="text-align: center; padding: 20px;">
        <h3 style="color: ${colors.primary};">Function Network</h3>
        <div style="margin: 20px auto; max-width: 600px; border: 2px solid ${colors.primary}; border-radius: 8px; padding: 20px; background-color: ${colors.background};">
          <div style="display: flex; flex-wrap: wrap; justify-content: center; gap: 15px;">
            <div style="background: ${isDarkTheme ? '#1E3A5F' : '#E8F0FE'}; padding: 10px; border-radius: 4px; border: 1px solid ${colors.primary}; width: 120px; text-align: center; color: ${colors.text};">
              <strong>select()</strong>
            </div>
            <div style="background: ${isDarkTheme ? '#1E3A5F' : '#E8F0FE'}; padding: 10px; border-radius: 4px; border: 1px solid ${colors.primary}; width: 120px; text-align: center; color: ${colors.text};">
              <strong>arrange()</strong>
            </div>
            <div style="background: ${colors.primary}; color: white; padding: 10px; border-radius: 4px; width: 120px; text-align: center;">
              <strong>${funcName.split('::').pop()}</strong>
            </div>
            <div style="background: ${isDarkTheme ? '#1E3A5F' : '#E8F0FE'}; padding: 10px; border-radius: 4px; border: 1px solid ${colors.primary}; width: 120px; text-align: center; color: ${colors.text};">
              <strong>mutate()</strong>
            </div>
            <div style="background: ${isDarkTheme ? '#1E3A5F' : '#E8F0FE'}; padding: 10px; border-radius: 4px; border: 1px solid ${colors.primary}; width: 120px; text-align: center; color: ${colors.text};">
              <strong>summarize()</strong>
            </div>
          </div>
          <div style="margin-top: 30px; text-align: center; color: ${isDarkTheme ? '#BBBBBB' : '#666666'};">
            <p>Functions commonly used with ${funcName.split('::').pop()} in data analysis workflows</p>
          </div>
        </div>
      </div>
    `;
  } else if (visType === "code_highlight") {
    // Set code colors based on theme
    const codeColors = {
      keyword: isDarkTheme ? '#569CD6' : '#07a',
      function: isDarkTheme ? '#DCDCAA' : '#690',
      string: isDarkTheme ? '#CE9178' : '#690',
      number: isDarkTheme ? '#B5CEA8' : '#905',
      comment: isDarkTheme ? '#6A9955' : '#999',
      background: isDarkTheme ? '#1E1E1E' : '#F5F7F9',
      text: isDarkTheme ? '#D4D4D4' : '#333333'
    };
    
    fallbackViz = `
      <div style="text-align: center; padding: 20px;">
        <h3 style="color: ${colors.primary};">Code Example</h3>
        <div style="margin: 20px auto; max-width: 600px; text-align: left;">
          <pre style="background-color: ${codeColors.background}; padding: 15px; border-radius: 4px; overflow-x: auto; line-height: 1.5; font-family: 'Consolas', 'Monaco', monospace; color: ${codeColors.text};">
<span style="color: ${codeColors.keyword};">library</span>(<span style="color: ${codeColors.function};">dplyr</span>)

<span style="color: ${codeColors.comment};"># Sample data</span>
data <- <span style="color: ${codeColors.keyword};">data.frame</span>(
  id = <span style="color: ${codeColors.number};">1</span>:<span style="color: ${codeColors.number};">5</span>,
  name = <span style="color: ${codeColors.function};">c</span>(<span style="color: ${codeColors.string};">"Alice"</span>, <span style="color: ${codeColors.string};">"Bob"</span>, <span style="color: ${codeColors.string};">"Charlie"</span>, <span style="color: ${codeColors.string};">"David"</span>, <span style="color: ${codeColors.string};">"Eve"</span>),
  age = <span style="color: ${codeColors.function};">c</span>(<span style="color: ${codeColors.number};">25</span>, <span style="color: ${codeColors.number};">17</span>, <span style="color: ${codeColors.number};">32</span>, <span style="color: ${codeColors.number};">41</span>, <span style="color: ${codeColors.number};">19</span>),
  score = <span style="color: ${codeColors.function};">c</span>(<span style="color: ${codeColors.number};">85</span>, <span style="color: ${codeColors.number};">92</span>, <span style="color: ${codeColors.number};">78</span>, <span style="color: ${codeColors.number};">65</span>, <span style="color: ${codeColors.number};">91</span>)
)

<span style="color: ${codeColors.comment};"># Filter rows where age >= 21</span>
adults <- <span style="color: ${codeColors.keyword};">filter</span>(data, age >= <span style="color: ${codeColors.number};">21</span>)

<span style="color: ${codeColors.comment};"># Multiple conditions with logical operators</span>
high_scoring_adults <- <span style="color: ${codeColors.keyword};">filter</span>(data, 
  age >= <span style="color: ${codeColors.number};">21</span>, 
  score > <span style="color: ${codeColors.number};">75</span>
)

<span style="color: ${codeColors.comment};"># Using %in% operator</span>
selected_names <- <span style="color: ${codeColors.keyword};">filter</span>(data, 
  name %in% <span style="color: ${codeColors.function};">c</span>(<span style="color: ${codeColors.string};">"Alice"</span>, <span style="color: ${codeColors.string};">"Charlie"</span>)
)
          </pre>
        </div>
      </div>
    `;
  } else {
    fallbackViz = `
      <div style="text-align: center; padding: 40px;">
        <h3 style="color: ${colors.primary};">Visualization: ${visType}</h3>
        <p style="color: ${colors.text};">This visualization type requires additional packages:</p>
        <p><code style="background-color: ${colors.codeBackground}; color: ${colors.codeText}; padding: 2px 4px; border-radius: 3px;">install.packages(c("DiagrammeR", "visNetwork", "highlight"))</code></p>
      </div>
    `;
  }
  
  // Use fallback visualization while we fix the R function integration
  vizContainer.innerHTML = fallbackViz;
  vizContainer.setAttribute('data-loaded', 'true');
  
  // Hide loading indicator
  if (loadingDiv) {
    loadingDiv.style.display = 'none';
  }
  
  /* Temporarily commented out until R function integration is fixed
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
  */
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