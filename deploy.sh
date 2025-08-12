#!/bin/bash

echo "🚀 Sustainability Q&A Site Generator"
echo "====================================="

# Check if R is installed
if ! command -v Rscript &> /dev/null; then
    echo "❌ R is not installed. Please install R first."
    exit 1
fi

# Check if Graphviz is installed
if ! command -v neato &> /dev/null; then
    echo "❌ Graphviz is not installed. Please install Graphviz first."
    echo "   On Ubuntu/Debian: sudo apt-get install graphviz"
    echo "   On macOS: brew install graphviz"
    echo "   On Windows: Download from https://graphviz.org/"
    exit 1
fi

# Check if data directory exists
if [ ! -d "data" ]; then
    echo "❌ Data directory not found. Please create a 'data' directory and add your .rock files."
    exit 1
fi

# Check if .rock files exist
if [ ! "$(ls -A data/*.rock 2>/dev/null)" ]; then
    echo "❌ No .rock files found in data directory. Please add your .rock files."
    exit 1
fi

echo "✅ Prerequisites check passed"
echo ""

# Install R package if needed
echo "📦 Installing R dependencies..."
Rscript -e "if (!require(stringr)) install.packages('stringr', repos='https://cran.rstudio.com/')"

# Generate the site
echo "🔧 Generating site..."
Rscript generate_site.R

if [ $? -eq 0 ]; then
    echo ""
    echo "✅ Site generation completed successfully!"
    echo ""
    echo "📁 Generated files:"
    echo "   - public/index.html (landing page)"
    echo "   - public/graphs/ (individual case graphs)"
    echo "   - public/graphs/network_graph_combined.html (combined analysis)"
    echo ""
    echo "🚀 To deploy to GitLab Pages:"
    echo "   1. Commit and push these files to your GitLab repository"
    echo "   2. GitLab will automatically deploy to Pages"
    echo "   3. Your site will be available at: https://your-username.gitlab.io/your-repo-name/"
    echo ""
    echo "💡 The .gitlab-ci.yml file is already configured for simple deployment."
else
    echo "❌ Site generation failed. Please check the error messages above."
    exit 1
fi 