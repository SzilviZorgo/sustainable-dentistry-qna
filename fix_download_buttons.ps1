# Simple PowerShell script to add download buttons to HTML files

# Get all HTML files except case1 and case2 (which already have buttons)
$htmlFiles = Get-ChildItem "graphs" -Filter "*.html" | Where-Object { $_.Name -notlike "*case1*" -and $_.Name -notlike "*case2*" }

Write-Host "Found $($htmlFiles.Count) HTML files to update"

foreach ($file in $htmlFiles) {
    Write-Host "Processing: $($file.Name)"
    
    # Read the HTML file
    $content = Get-Content $file.FullName -Raw
    
    # Check if download buttons already exist
    if ($content -match "download-buttons") {
        Write-Host "  Skipping $($file.Name) - download buttons already exist"
        continue
    }
    
    # Add CSS styles before </style>
    $cssToAdd = @"
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
        }
"@
    
    $content = $content -replace '(\s+)</style>', "$cssToAdd`n$1</style>"
    
    # Add download buttons HTML after the last </label>
    $buttonsHtml = @"
            
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
            </div>
"@
    
    $content = $content -replace '(\s+)</label>(\s+)</div>', '$1</label>$2$buttonsHtml$1</div>'
    
    # Add JavaScript functions before </script>
    $jsToAdd = @"

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
            const checkboxes = document.querySelectorAll(".controls-panel input[type=`"checkbox`"]");
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
            const svgString = '<?xml version="1.0" encoding="UTF-8"?>\n' + 
                             clonedSvg.outerHTML;
            
            // Create blob and download
            const blob = new Blob([svgString], { type: 'image/svg+xml' });
            const url = URL.createObjectURL(blob);
            const a = document.createElement('a');
            a.href = url;
            a.download = 'network_graph.svg';
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
            clonedSvg.setAttribute('width', width);
            clonedSvg.setAttribute('height', height);
            clonedSvg.setAttribute('viewBox', `${bbox.x - 20} ${bbox.y - 20} ${width} ${height}`);
            
            // Convert SVG to data URL
            const svgString = new XMLSerializer().serializeToString(clonedSvg);
            const svgDataUrl = 'data:image/svg+xml;base64,' + btoa(unescape(encodeURIComponent(svgString)));
            
            // Create canvas and convert to PNG
            const canvas = document.createElement('canvas');
            const ctx = canvas.getContext('2d');
            const img = new Image();
            
            img.onload = function() {
                canvas.width = width;
                canvas.height = height;
                ctx.fillStyle = '#f8fafc'; // Background color
                ctx.fillRect(0, 0, width, height);
                ctx.drawImage(img, 0, 0);
                
                // Download PNG
                canvas.toBlob(function(blob) {
                    const url = URL.createObjectURL(blob);
                    const a = document.createElement('a');
                    a.href = url;
                    a.download = 'network_graph.png';
                    document.body.appendChild(a);
                    a.click();
                    document.body.removeChild(a);
                    URL.revokeObjectURL(url);
                }, 'image/png');
                
                // Restore filtered view
                setTimeout(restoreFilteredView, 100);
            };
            
            img.src = svgDataUrl;
        }
"@
    
    $content = $content -replace '(\s+)</script>', "$jsToAdd`n$1</script>"
    
    # Write the updated file
    Set-Content -Path $file.FullName -Value $content -Encoding UTF8
    Write-Host "  ✓ Updated: $($file.Name)"
}

Write-Host "Done! All HTML files have been updated with download functionality." 