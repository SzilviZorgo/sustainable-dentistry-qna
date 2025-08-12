#!/usr/bin/env Rscript

# Simple script to generate all static files locally
# Run this once, then push the files to GitLab

library(stringr)

# Check if data directory exists
if (!dir.exists("data")) {
  stop("Data directory 'data' not found. Please create it and add your .rock files.")
}

# Read ROCK files
rock_files <- list.files('data', pattern = '\\.(rock)$', full.names = TRUE)

if (length(rock_files) == 0) {
  stop("No .rock files found in 'data' directory.")
}

message(paste("Found", length(rock_files), "ROCK files to process"))

# Create output directories
if (!dir.exists("graphs")) dir.create("graphs")
if (!dir.exists("public")) dir.create("public")

# Function to detect codes
detect_codes <- function(lines) {
  code_pattern <- '\\[\\[[^\\]]+->[^\\]]+\\|\\|[^\\]]+\\]\\]'
  matches <- str_extract_all(lines, code_pattern)
  codes <- unlist(matches)
  unique(codes)
}

# Function to remove measured codes
remove_measured_codes <- function(codes_list) {
  pattern <- '\\[\\[([^->]+)->([^|]+)\\|\\|'
  matches <- str_match(codes_list, pattern)
  originating <- matches[,2]
  receiving <- matches[,3]
  to_remove <- originating %in% c("Measured", "NotMeasured") |
    receiving %in% c("Measured", "NotMeasured")
  filtered <- codes_list[!to_remove]
  return(filtered)
}

# Function to convert codes to network format
convert_rock_codes_to_network <- function(codes_with_freq) {
  pattern <- '\\[\\[([^->]+)->([^|]+)\\|\\|([^|]+)\\|\\|([0-9]+)\\]\\]'
  matches <- str_match(codes_with_freq, pattern)
  
  if (nrow(matches) == 0) {
    stop("No valid ROCK codes found")
  }
  
  from_nodes <- unique(matches[,2])
  to_nodes <- unique(matches[,3])
  all_nodes <- unique(c(from_nodes, to_nodes))
  
  # Remove Measured and NotMeasured nodes
  all_nodes <- all_nodes[!all_nodes %in% c("Measured", "NotMeasured")]
  message(paste("  Nodes after removing Measured/NotMeasured:", paste(all_nodes, collapse = ", ")))
  
  node_frequencies <- table(c(matches[,2], matches[,3]))
  sorted_nodes <- names(sort(node_frequencies, decreasing = TRUE))
  
  inner_nodes <- sorted_nodes[1:min(4, length(sorted_nodes))]
  outer_nodes <- setdiff(sorted_nodes, inner_nodes)
  
  edge_types <- unique(na.omit(matches[,4]))
  
  # GLOBAL CONSISTENT EDGE TYPE STYLING (same across all cases)
  global_edge_styling <- list(
    "Analogue" = list(color = "blue", style = "solid", dir = "none"),
    "Affect" = list(color = "red", style = "dashed", dir = "forward"),
    "Manifest" = list(color = "green", style = "solid", dir = "forward")
  )
  
  # Use global styling for known edge types, fallback for unknown
  edge_type_styling <- list()
  for (edge_type in edge_types) {
    if (edge_type %in% names(global_edge_styling)) {
      edge_type_styling[[edge_type]] <- global_edge_styling[[edge_type]]
    } else {
      # Fallback for unknown edge types
      edge_type_styling[[edge_type]] <- list(color = "gray", style = "solid", dir = "forward")
    }
  }
  
  edges <- list()
  for (i in 1:nrow(matches)) {
    if (!is.na(matches[i,1])) {
      edge_type <- matches[i,4]
      frequency <- as.numeric(matches[i,5])
      
      if (edge_type %in% names(edge_type_styling)) {
        styling <- edge_type_styling[[edge_type]]
        color <- styling$color
        style <- styling$style
      } else {
        color <- "black"
        style <- "solid"
      }
      
      penwidth <- max(1, min(3, frequency))
      
      # Get direction for this edge type
      dir <- if (edge_type == "Analogue") "none" else "forward"
      
      edges[[length(edges) + 1]] <- list(
        from = matches[i,2],
        to = matches[i,3],
        edge_type = edge_type,
        frequency = frequency,
        color = color,
        style = style,
        penwidth = penwidth,
        dir = dir
      )
    }
  }
  
  return(list(
    inner_nodes = inner_nodes,
    outer_nodes = outer_nodes,
    edges = edges,
    edge_types = edge_types,
    edge_type_styling = edge_type_styling
  ))
}

# Function to generate HTML
generate_network_graph <- function(network_data, output_filename) {
  inner_nodes <- network_data$inner_nodes
  outer_nodes <- network_data$outer_nodes
  edges_data <- network_data$edges
  
  # Function to truncate long node labels intelligently
  truncate_label <- function(label, node_size = 1.2, font_size = 10) {
    # Calculate approximate characters that fit in the node
    # Based on font size and node width (1.2 inches = ~115px at 96 DPI)
    # Average character width is roughly 0.6 * font_size in pixels
    node_width_px <- node_size * 96  # Convert inches to pixels
    char_width_px <- font_size * 0.6  # Approximate character width
    max_chars <- floor((node_width_px * 0.8) / char_width_px)  # 80% of node width for safety
    
    # Ensure minimum and maximum reasonable limits
    max_chars <- max(6, min(max_chars, 15))  # Between 6 and 15 characters
    
    if (nchar(label) <= max_chars) {
      return(label)
    } else {
      # Truncate and add ellipsis
      return(paste0(substr(label, 1, max_chars - 3), "..."))
    }
  }
  
  # Advanced adaptive layout based on node count, edge density, and connectivity
  total_nodes <- length(inner_nodes) + length(outer_nodes)
  total_edges <- length(edges_data)
  
  # Calculate edge density (edges per node)
  edge_density <- if (total_nodes > 0) total_edges / total_nodes else 0
  
  # Calculate average node degree
  node_degrees <- table(unlist(lapply(edges_data, function(edge) c(edge$from, edge$to))))
  avg_degree <- if (length(node_degrees) > 0) mean(node_degrees) else 0
  
  message(paste("  Network stats: ", total_nodes, "nodes, ", total_edges, "edges, density=", round(edge_density, 2), ", avg_degree=", round(avg_degree, 2)))
  
  if (total_nodes <= 4) {
    # Very small networks: Use ultra-compact single circle
    message("  Using ultra-compact single circle for very small network (", total_nodes, " nodes)")
    all_nodes <- c(inner_nodes, outer_nodes)
    
    # Ultra-compact circle with edge-based scaling
    base_radius <- max(20, 15 * total_nodes)
    if (total_edges < total_nodes) {
      # Very sparse: even tighter
      compact_radius <- base_radius * 0.6
    } else {
      compact_radius <- base_radius
    }
    angles <- seq(0, 2 * pi, length.out = length(all_nodes) + 1)[1:length(all_nodes)]
    x_coords <- compact_radius * cos(angles)
    y_coords <- compact_radius * sin(angles)
    
    node_positions <- paste(sapply(seq_along(all_nodes), function(i) {
      sprintf('    %s [pos="%f,%f!", label="%s"];', 
              all_nodes[i], x_coords[i], y_coords[i], truncate_label(all_nodes[i]))
    }), collapse="\n")
    layout_engine <- "neato"
    
  } else if (total_nodes <= 8) {
    # Small networks: Ultra-compact circle with edge scaling
    message("  Using ultra-compact circle for small network (", total_nodes, " nodes)")
    all_nodes <- c(inner_nodes, outer_nodes)
    
    # Ultra-compact circle with edge-based scaling
    base_radius <- max(30, 25 * total_nodes)
    if (total_edges < total_nodes * 1.5) {
      # Sparse: very tight
      radius <- base_radius * 0.7
    } else {
      radius <- base_radius
    }
    angles <- seq(0, 2 * pi, length.out = length(all_nodes) + 1)[1:length(all_nodes)]
    x_coords <- radius * cos(angles)
    y_coords <- radius * sin(angles)
    
    node_positions <- paste(sapply(seq_along(all_nodes), function(i) {
      sprintf('    %s [pos="%f,%f!", label="%s"];', 
              all_nodes[i], x_coords[i], y_coords[i], truncate_label(all_nodes[i]))
    }), collapse="\n")
    layout_engine <- "neato"
    
  } else if (total_nodes <= 12) {
    # Medium networks: Two circles with ultra-tight spacing
    message("  Using ultra-tight two-circle layout for medium network (", total_nodes, " nodes)")
    
    # Ultra-tight spacing with edge-based scaling
    if (total_edges < total_nodes * 2) {
      # Sparse: very tight
      inner_radius <- 60
      outer_radius <- 100
    } else {
      # Dense: tight but readable
      inner_radius <- 80
      outer_radius <- 140
    }
    
    inner_positions <- paste(sapply(seq_along(inner_nodes), function(i) {
      angle <- (i-1) * 2 * pi / length(inner_nodes)
      sprintf('    %s [pos="%f,%f!", label="%s"];', 
              inner_nodes[i], inner_radius*cos(angle), inner_radius*sin(angle), truncate_label(inner_nodes[i]))
    }), collapse="\n")
    
    outer_positions <- paste(sapply(seq_along(outer_nodes), function(i) {
      angle <- (i-1) * 2 * pi / length(outer_nodes)
      sprintf('    %s [pos="%f,%f!", label="%s"];', 
              outer_nodes[i], outer_radius*cos(angle), outer_radius*sin(angle), truncate_label(outer_nodes[i]))
    }), collapse="\n")
    
    node_positions <- paste(inner_positions, outer_positions, sep = "\n")
    layout_engine <- "neato"
    
  } else {
    # Large networks: Concentric circles with ultra-tight spacing
    message("  Using ultra-tight concentric circles for large network (", total_nodes, " nodes)")
    
    # Ultra-tight spacing with edge-based scaling
    if (total_edges < total_nodes * 1.8) {
      # Sparse: very tight
      inner_radius <- 50
      outer_radius <- 100
    } else {
      # Dense: tight but readable
      inner_radius <- 70
      outer_radius <- 140
    }
    
    inner_positions <- paste(sapply(seq_along(inner_nodes), function(i) {
      angle <- (i-1) * 2 * pi / length(inner_nodes)
      sprintf('    %s [pos="%f,%f!", label="%s"];', 
              inner_nodes[i], inner_radius*cos(angle), inner_radius*sin(angle), truncate_label(inner_nodes[i]))
    }), collapse="\n")
    
    outer_positions <- paste(sapply(seq_along(outer_nodes), function(i) {
      angle <- (i-1) * 2 * pi / length(outer_nodes)
      sprintf('    %s [pos="%f,%f!", label="%s"];', 
              outer_nodes[i], outer_radius*cos(angle), outer_radius*sin(angle), truncate_label(outer_nodes[i]))
    }), collapse="\n")
    
    node_positions <- paste(inner_positions, outer_positions, sep = "\n")
    layout_engine <- "neato"
  }
  
  # Generate DOT script with dynamic layout and tighter spacing
  dot_script <- paste0('
digraph ROCK_network {
    layout="', layout_engine, '";
    overlap=false;
    splines="true";
    sep="+5,5";
    mindist=0.5;
    margin="0.5,0.5";
    K=0.3;
    node [shape=circle, fixedsize=true, width=1.2, height=1.2, fontname="Inter", fontsize=10, labelloc="c"];
    edge [dir=forward, fontname="Inter"];
',
    if (node_positions != "") node_positions else "",
    "\n",
    paste(sapply(edges_data, function(e) {
      dir_attr <- if (!is.null(e$dir) && e$dir == "none") 'dir="none"' else ""
      sprintf('    %s -> %s [color="%s", style="%s", penwidth=%d%s];', 
              e$from, e$to, e$color, e$style, e$penwidth,
              if (dir_attr != "") paste0(", ", dir_attr) else "")
    }), collapse = "\n"),
    "\n}"
  )
  
  # Write DOT file
  dot_file <- tempfile(fileext = ".dot")
  writeLines(dot_script, dot_file)
  
  # Generate SVG with dynamic layout engine
  svg_file <- tempfile(fileext = ".svg")
  system2(layout_engine, c("-Tsvg", "-o", svg_file, dot_file), stdout = TRUE, stderr = TRUE)
  
  # Read SVG content
  svg_content <- paste(readLines(svg_file), collapse = "\n")
  
  # Generate filter controls
  filter_controls <- ""
  if (length(network_data$edge_types) > 0) {
    edge_controls <- sapply(network_data$edge_types, function(edge_type) {
      if (edge_type %in% names(network_data$edge_type_styling)) {
        styling <- network_data$edge_type_styling[[edge_type]]
        sprintf('<label class="checkbox-label"><input type="checkbox" id="filter-%s" data-color="%s" data-style="%s" data-type="%s" checked><span class="text-%s-600 font-medium">%s</span></label>', 
                tolower(edge_type), styling$color, styling$style, edge_type, styling$color, edge_type)
      }
    })
    filter_controls <- paste(edge_controls, collapse = "\n")
  }
  
  # Read HTML template
  if (!file.exists("public/html_template.html")) {
    stop("public/html_template.html not found")
  }
  html_template <- paste(readLines("public/html_template.html"), collapse = "\n")
  
  # Create final HTML
  html_output <- gsub("%EDGE_TYPES%", paste(network_data$edge_types, collapse = ", "), html_template)
  html_output <- gsub("%FILTER_CONTROLS%", filter_controls, html_output)
  html_output <- gsub("%SVG_CONTENT%", svg_content, html_output)
  
  # Write output file
  writeLines(html_output, output_filename)
  message(paste("Generated:", output_filename))
}

# Process individual files
for (i in seq_along(rock_files)) {
  file_path <- rock_files[i]
  file_name <- basename(file_path)
  
  message(paste("Processing:", file_name))
  
  tryCatch({
    lines <- readLines(file_path, warn = FALSE)
    
    # Extract case identifier
    cid_line <- grep('^\\[\\[cid=', lines, value = TRUE)
    case_id <- if (length(cid_line) > 0) str_extract(cid_line, '(?<=\\[\\[cid=)[^\\]]+') else "unknown"
    
    # Extract codes
    codes_in_file <- detect_codes(lines)
    if (length(codes_in_file) == 0) next
    
    # Add frequency values
    codes_with_freq <- paste0(substring(codes_in_file, 1, nchar(codes_in_file)-2), '||1]]')
    codes_with_freq <- remove_measured_codes(codes_with_freq)
    
    # Check if any codes remain after filtering
    if (length(codes_with_freq) == 0) {
      message(paste("  ⚠ No valid codes remaining after filtering Measured/NotMeasured codes"))
      # Remove existing graph file if it exists
      output_filename <- file.path("public", "graphs", paste0("graph_case_", case_id, ".html"))
      if (file.exists(output_filename)) {
        file.remove(output_filename)
        message(paste("  🗑️ Removed existing graph file:", output_filename))
      }
      next
    }
    
    # Convert to network
    network_data <- convert_rock_codes_to_network(codes_with_freq)
    
    # Generate HTML
    output_filename <- file.path("public", "graphs", paste0("graph_case_", case_id, ".html"))
    generate_network_graph(network_data, output_filename)
    
  }, error = function(e) {
    message(paste("Error processing", file_name, ":", e$message))
  })
}

# Generate combined analysis
message("Generating combined analysis...")

# Collect all codes from all files
all_codes <- c()
for (file_path in rock_files) {
  lines <- readLines(file_path, warn = FALSE)
  codes <- detect_codes(lines)
  all_codes <- c(all_codes, codes)
}

if (length(all_codes) > 0) {
  # Calculate frequencies
  code_frequencies <- table(all_codes)
  
  # Create codes with frequencies
  global_codes_with_freq <- paste0('[[', substring(names(code_frequencies), 3, nchar(names(code_frequencies))-2), '||', code_frequencies, ']]')
  global_codes_with_freq <- remove_measured_codes(global_codes_with_freq)
  
  # Convert to network
  combined_network_data <- convert_rock_codes_to_network(global_codes_with_freq)
  
  # Generate combined HTML
  generate_network_graph(combined_network_data, file.path("public", "graphs", "network_graph_combined.html"))
}

# Copy files to public directory
message("Copying files to public directory...")
file.copy("index.html", "public/", overwrite = TRUE)
file.copy("html_template.html", "public/", overwrite = TRUE)
file.copy("graphs", "public/", recursive = TRUE, overwrite = TRUE)

message("Site generation complete! Files are ready in the public/ directory.")
message("Push these files to GitLab to deploy to Pages.") 