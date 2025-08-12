# Script to add download buttons to all HTML files in the graphs directory

# CSS styles for download buttons
download_css <- '
        .download-buttons {
            display: flex;
            gap: 1rem;
            margin-top: 1rem;
            padding-top: 1rem;
            border-top: 1px solid #e5e7eb;
        }
        .download-btn {
            padding: 0.5rem 1rem;
            border-radius: 0.375rem;
            font-weight: 600;
            text-decoration: none;
            display: inline-flex;
            align-items: center;
            gap: 0.5rem;
            transition: all 0.2s;
        }
        .download-btn:hover {
            transform: translateY(-1px);
        }
        .btn-svg {
            background-color: #3b82f6;
            color: white;
        }
        .btn-svg:hover {
            background-color: #2563eb;
        }
        .btn-png {
            background-color: #10b981;
            color: white;
        }
        .btn-png:hover {
            background-color: #059669;
        }'

# HTML for download buttons
download_buttons_html <- '
            <div class="download-buttons">
                <button class="download-btn btn-svg" onclick="downloadSVG()">
                    <svg width="16" height="16" fill="currentColor" viewBox="0 0 16 16">
                        <path d="M.5 9.9a.5.5 0 0 1 .5.5v2.5a1 1 0 0 0 1 1h12a1 1 0 0 0 1-1v-2.5a.5.5 0 0 1 1 0v2.5a2 2 0 0 1-2 2H2a2 2 0 0 1-2-2v-2.5a.5.5 0 0 1 .5-.5z"/>
                        <path d="M7.646 11.854a.5.5 0 0 0 .708 0l3-3a.5.5 0 0 0-.708-.708L8.5 10.293V1.5a.5.5 0 0 0-1 0v8.793L5.354 8.146a.5.5 0 1 0-.708.708l3 3z"/>
                    </svg>
                    Download SVG
                </button>
                <button class="download-btn btn-png" onclick="downloadPNG()">
                    <svg width="16" height="16" fill="currentColor" viewBox="0 0 16 16">
                        <path d="M.5 9.9a.5.5 0 0 1 .5.5v2.5a1 1 0 0 0 1 1h12a1 1 0 0 0 1-1v-2.5a.5.5 0 0 1 1 0v2.5a2 2 0 0 1-2 2H2a2 2 0 0 1-2-2v-2.5a.5.5 0 0 1 .5-.5z"/>
                        <path d="M7.646 11.854a.5.5 0 0 0 .708 0l3-3a.5.5 0 0 0-.708-.708L8.5 10.293V1.5a.5.5 0 0 0-1 0v8.793L5.354 8.146a.5.5 0 1 0-.708.708l3 3z"/>
                    </svg>
                    Download PNG
                </button>
            </div>'

# JavaScript functions for downloads
download_js <- '
        // Function to show all edges (for downloads)
        function showAllEdges() {
            const svgElement = document.getElementById("graph-container").querySelector("svg");
            if (!svgElement) return;

            const paths = svgElement.querySelectorAll("path");
            const polygons = svgElement.querySelectorAll("polygon");

            // Show all paths and polygons
            paths.forEach(path => {
                path.style.display = "initial";
            });
            polygons.forEach(polygon => {
                polygon.style.display = "initial";
            });
        }

        // Function to restore filtered view
        function restoreFilteredView() {
            const checkboxes = document.querySelectorAll(".controls-panel input[type=\"checkbox\"]");
            const svgElement = document.getElementById("graph-container").querySelector("svg");
            if (!svgElement) return;

            const paths = svgElement.querySelectorAll("path");
            const polygons = svgElement.querySelectorAll("polygon");

            // Re-apply filters
            paths.forEach(path => {
                const strokeColor = path.getAttribute("stroke");
                const strokeDasharray = path.getAttribute("stroke-dasharray");
                let isVisible = false;

                checkboxes.forEach(checkbox => {
                    const filterColor = checkbox.dataset.color;
                    const filterStyle = checkbox.dataset.style;
                    const isChecked = checkbox.checked;

                    if (isChecked) {
                        if (filterStyle === "dashed" && strokeDasharray && strokeColor === filterColor) {
                            isVisible = true;
                        } else if (filterStyle === "solid" && !strokeDasharray && strokeColor === filterColor) {
                            isVisible = true;
                        } else if (filterStyle === "dotted" && strokeDasharray && strokeColor === filterColor) {
                            isVisible = true;
                        }
                    }
                });
                path.style.display = isVisible ? "initial" : "none";
            });

            polygons.forEach(polygon => {
                const fillColor = polygon.getAttribute("fill");
                let isVisible = false;

                checkboxes.forEach(checkbox => {
                    const filterColor = checkbox.dataset.color;
                    const isChecked = checkbox.checked;

                    if (isChecked && fillColor === filterColor) {
                        isVisible = true;
                    }
                });
                polygon.style.display = isVisible ? "initial" : "none";
            });
        }

        // Function to download SVG
        function downloadSVG() {
            showAllEdges();
            
            const svgElement = document.getElementById("graph-container").querySelector("svg");
            if (!svgElement) {
                alert("SVG element not found");
                return;
            }

            // Clone the SVG to avoid modifying the original
            const clonedSvg = svgElement.cloneNode(true);
            
            // Add XML declaration and SVG namespace
            const svgString = \'<?xml version="1.0" encoding="UTF-8"?>\\n\' + 
                             clonedSvg.outerHTML;
            
            // Create blob and download
            const blob = new Blob([svgString], { type: \'image/svg+xml\' });
            const url = URL.createObjectURL(blob);
            const a = document.createElement(\'a\');
            a.href = url;
            a.download = \'network_graph.svg\';
            document.body.appendChild(a);
            a.click();
            document.body.removeChild(a);
            URL.revokeObjectURL(url);
            
            // Restore filtered view
            setTimeout(restoreFilteredView, 100);
        }

        // Function to download PNG
        function downloadPNG() {
            showAllEdges();
            
            const svgElement = document.getElementById("graph-container").querySelector("svg");
            if (!svgElement) {
                alert("SVG element not found");
                return;
            }

            // Clone the SVG to avoid modifying the original
            const clonedSvg = svgElement.cloneNode(true);
            
            // Get SVG dimensions
            const bbox = clonedSvg.getBBox();
            const width = bbox.width + 40; // Add padding
            const height = bbox.height + 40; // Add padding
            
            // Set SVG dimensions
            clonedSvg.setAttribute(\'width\', width);
            clonedSvg.setAttribute(\'height\', height);
            clonedSvg.setAttribute(\'viewBox\', `${bbox.x - 20} ${bbox.y - 20} ${width} ${height}`);
            
            // Convert SVG to data URL
            const svgString = new XMLSerializer().serializeToString(clonedSvg);
            const svgDataUrl = \'data:image/svg+xml;base64,\' + btoa(unescape(encodeURIComponent(svgString)));
            
            // Create canvas and convert to PNG
            const canvas = document.createElement(\'canvas\');
            const ctx = canvas.getContext(\'2d\');
            const img = new Image();
            
            img.onload = function() {
                canvas.width = width;
                canvas.height = height;
                ctx.fillStyle = \'#f8fafc\'; // Background color
                ctx.fillRect(0, 0, width, height);
                ctx.drawImage(img, 0, 0);
                
                // Download PNG
                canvas.toBlob(function(blob) {
                    const url = URL.createObjectURL(blob);
                    const a = document.createElement(\'a\');
                    a.href = url;
                    a.download = \'network_graph.png\';
                    document.body.appendChild(a);
                    a.click();
                    document.body.removeChild(a);
                    URL.revokeObjectURL(url);
                }, \'image/png\');
                
                // Restore filtered view
                setTimeout(restoreFilteredView, 100);
            };
            
            img.src = svgDataUrl;
        }'

# Get all HTML files in the graphs directory
html_files <- list.files("graphs", pattern = "\\.html$", full.names = TRUE)

message(paste("Found", length(html_files), "HTML files to update"))

for (file in html_files) {
  message(paste("Processing:", basename(file)))
  
  # Read the HTML file
  html_content <- readLines(file)
  
  # Check if download buttons already exist
  if (any(grepl("download-buttons", html_content))) {
    message(paste("  Skipping", basename(file), "- download buttons already exist"))
    next
  }
  
  # Add CSS styles after the existing checkbox styles
  css_insert_pos <- which(grepl("checkbox-label input\\[type=\"checkbox\"\\]", html_content))[1]
  if (!is.na(css_insert_pos)) {
    # Find the end of the style section
    style_end <- which(grepl("^    </style>", html_content))[1]
    if (!is.na(style_end)) {
      html_content <- c(
        html_content[1:(style_end-1)],
        download_css,
        html_content[style_end:length(html_content)]
      )
    }
  }
  
  # Add download buttons after the filter controls
  filter_end <- which(grepl("</label>", html_content))
  if (length(filter_end) > 0) {
    last_filter <- filter_end[length(filter_end)]
    html_content <- c(
      html_content[1:last_filter],
      download_buttons_html,
      html_content[(last_filter+1):length(html_content)]
    )
  }
  
  # Add JavaScript functions before the closing script tag
  script_end <- which(grepl("^    </script>", html_content))
  if (length(script_end) > 0) {
    script_end <- script_end[length(script_end)]
    html_content <- c(
      html_content[1:(script_end-1)],
      download_js,
      html_content[script_end:length(html_content)]
    )
  }
  
  # Write the updated file
  writeLines(html_content, file)
  message(paste("  ✓ Updated:", basename(file)))
}

message("Done! All HTML files have been updated with download functionality.")