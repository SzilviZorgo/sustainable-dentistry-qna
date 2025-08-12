# Step 1: 
## Load library
library(stringr)

## Check if data directory exists
if (!dir.exists("data")) {
  stop("Data directory 'data' not found. Please create it and add your data files.")
}

## Read data (must have .rock extension in data directory)
rock_files <- list.files('data', pattern = '\\.(rock)$', full.names = TRUE)

if (length(rock_files) == 0) {
  stop("No .rock files found in 'data' directory. Please add .rock files to process.")
}

message(paste("Found", length(rock_files), "ROCK files to process:"))
for (file in rock_files) {
  message(paste("  -", basename(file)))
}

# Step 2: Print file names and case identifiers
cat('Files to be processed and their case identifiers:\n')
case_ids <- c()
for (file in rock_files) {
  lines <- readLines(file, warn = FALSE)
  # Find line with the case identifier
  cid_line <- grep('^\\[\\[cid=', lines, value = TRUE)
  cid <- if (length(cid_line) > 0) str_extract(cid_line, '(?<=\\[\\[cid=)[^\\]]+') else NA
  case_ids <- c(case_ids, cid)
  cat(basename(file), ':', cid, '\n')
}

# Step 3: Extract unique codes from a file
detect_codes <- function(lines) {
  ## Pattern: [[origin->receiving||edgeType]]
  code_pattern <- '\\[\\[[^\\]]+->[^\\]]+\\|\\|[^\\]]+\\]\\]'
  matches <- str_extract_all(lines, code_pattern)
  codes <- unlist(matches)
  unique(codes)
}

## Print list of files and codes
cat('\nList of files and the codes that appear in them:\n')
file_codes <- list()
for (i in seq_along(rock_files)) {
  lines <- readLines(rock_files[i], warn = FALSE)
  codes <- detect_codes(lines)
  file_codes[[basename(rock_files[i])]] <- codes
  cat(basename(rock_files[i]), ':', if (length(codes) > 0) paste(codes, collapse = ', ') else 'No codes found', '\n')
}

# Step 4: Create list of unique codes
all_codes <- unique(unlist(file_codes))
cat('\nAll unique codes across all files:\n')
print(all_codes)

# ## To check which file a code appears in
# my_code <- "[[ESupport->EClinic||Man]]"
# files_with_code <- names(file_codes)[sapply(file_codes, function(codes) my_code %in% codes)]
# print(files_with_code)

# Step 5: Check code presence in each file and calculate frequency
code_presence_matrix <- matrix(0, nrow = length(all_codes), ncol = length(rock_files),
                               dimnames = list(all_codes, basename(rock_files)))
for (i in seq_along(rock_files)) {
  codes_in_file <- file_codes[[basename(rock_files[i])]]
  code_presence_matrix[all_codes %in% codes_in_file, i] <- 1
}

# Step 6: Calculate frequency (number of files in which each code appears)
code_frequencies <- rowSums(code_presence_matrix)

cat('\nCode frequencies (number of files in which each code appears):\n')
for (i in seq_along(all_codes)) {
  cat(all_codes[i], ':', code_frequencies[i], '\n')
}

# Step 7: Add code values (frequency) to each code identifier and order by descending value
global_codes_with_freq <- paste0('[[', substring(all_codes, 3, nchar(all_codes)-2), '||', code_frequencies, ']]')
## Extract values
values_for_sort <- code_frequencies
order_idx <- order(values_for_sort, decreasing = TRUE)
global_codes_with_freq <- global_codes_with_freq[order_idx]
cat('\nCodes with frequency values as complete code identifiers (ordered by value descending):\n')
cat(global_codes_with_freq, sep = "\n")

# Step 8: Validate codes
{
  global_codes_with_freq <- trimws(global_codes_with_freq)
  validate_codes <- function(codes_with_freq) {
    # Working pattern: allow any character except [ in originating, receiving, and edgeType fields
    pattern <- '^\\[\\[[^\\[]+->[^\\[]+\\|\\|[^\\[]+\\|\\|[0-9]+\\]\\]$'
    valid_pattern <- grepl(pattern, codes_with_freq)
    all_have_value <- !is.na(codes_with_freq) & nchar(codes_with_freq) > 0
    all_unique <- length(unique(codes_with_freq)) == length(codes_with_freq)
    
    cat('\nValidation results for codes with frequency values:\n')
    if (all(valid_pattern)) {
      cat('All code identifiers adhere to the required pattern.\n')
    } else {
      cat('Some code identifiers do NOT adhere to the required pattern.\n')
      cat('Explanation: The following code identifiers do not match the expected format [[originatingCode->receivingCode||edgeType||value]]:\n')
      cat(paste0('>', codes_with_freq[!valid_pattern], '<\n'))
    }
    if (all(all_have_value)) {
      cat('All code identifiers have values (zero is valid).\n')
    } else {
      cat('Some code identifiers are missing values.\n')
      cat('Explanation: The following code identifiers are missing a value (should be an integer, zero is valid):\n')
      print(codes_with_freq[!all_have_value])
    }
    if (all_unique) {
      cat('All code identifiers are unique.\n')
    } else {
      cat('Some code identifiers are NOT unique.\n')
      cat('Explanation: The following code identifiers are duplicated in the list:\n')
      print(codes_with_freq[duplicated(codes_with_freq)])
    }
  }
  validate_codes(global_codes_with_freq)
}


# Step 9: Removal code identifiers, if applicable (user input needed)
{
  remove_measured_codes <- function(codes_list) {
    # Pattern to extract originating and receiving nodes
    pattern <- '\\[\\[([^->]+)->([^|]+)\\|\\|'
    matches <- str_match(codes_list, pattern)
    originating <- matches[,2]
    receiving <- matches[,3]
    to_remove <- originating %in% c("Measured", "NotMeasured") |
      receiving %in% c("Measured", "NotMeasured")
    filtered <- codes_list[!to_remove]
    cat('\nCodes after removing those with "Measured" or "NotMeasured" as originating or receiving node:\n')
    cat(filtered, sep = "\n")
    return(filtered)
  }
  global_codes_with_freq <- remove_measured_codes(global_codes_with_freq)
}


# Step 10: Extract and print all unique edgeTypes from code identifiers
{
  extract_edge_types <- function(codes_list) {
    # Pattern: [[originatingCode->receivingCode||edgeType||value]]
    pattern <- '\\[\\[[^->]+->[^|]+\\|\\|([^|]+)\\|\\|[0-9]+\\]\\]'
    matches <- str_match(codes_list, pattern)
    edge_types <- unique(na.omit(matches[,2]))
    cat('\nAll unique edgeTypes in the code identifiers:\n')
    print(edge_types)
    return(edge_types)
  }
  extract_edge_types(global_codes_with_freq)
}


##################################################################################



# Function to convert ROCK codes to network format
convert_rock_codes_to_network <- function(codes_with_freq) {
  # Extract nodes and edges from ROCK codes
  # Pattern: [[origin->receiving||edgeType||frequency]]
  
  # Extract all unique nodes
  pattern <- '\\[\\[([^->]+)->([^|]+)\\|\\|([^|]+)\\|\\|([0-9]+)\\]\\]'
  matches <- str_match(codes_with_freq, pattern)
  
  if (nrow(matches) == 0) {
    stop("No valid ROCK codes found to convert to network format")
  }
  
  # Extract nodes
  from_nodes <- unique(matches[,2])
  to_nodes <- unique(matches[,3])
  all_nodes <- unique(c(from_nodes, to_nodes))
  
  # Define inner and outer nodes (you may need to adjust this logic)
  # For now, we'll put the most frequent nodes in the inner circle
  node_frequencies <- table(c(matches[,2], matches[,3]))
  sorted_nodes <- names(sort(node_frequencies, decreasing = TRUE))
  
  # Take top 4 nodes for inner circle, rest for outer
  inner_nodes <- sorted_nodes[1:min(4, length(sorted_nodes))]
  outer_nodes <- setdiff(sorted_nodes, inner_nodes)
  
  # Extract unique edge types from the data
  edge_types <- unique(na.omit(matches[,4]))
  message(paste("  Found edge types:", paste(edge_types, collapse = ", ")))
  
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
  
  # Create edges list
  edges <- list()
  for (i in 1:nrow(matches)) {
    if (!is.na(matches[i,1])) {
      edge_type <- matches[i,4]
      frequency <- as.numeric(matches[i,5])
      
      # Get styling for this edge type
      if (edge_type %in% names(edge_type_styling)) {
        styling <- edge_type_styling[[edge_type]]
        color <- styling$color
        style <- styling$style
      } else {
        # Default styling for unknown edge types
        color <- "black"
        style <- "solid"
      }
      
      # Penwidth based on frequency
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



# Create graphs directory if it doesn't exist
if (!dir.exists("graphs")) {
  dir.create("graphs")
  message("Created 'graphs' directory")
}

# Process each ROCK file individually
individual_results <- list()
file_names <- tools::file_path_sans_ext(basename(rock_files))

message(paste("Starting to process", length(rock_files), "ROCK files..."))

for (i in seq_along(rock_files)) {
  file_path <- rock_files[i]
  file_name <- file_names[i]
  
  message(paste("\nProcessing file", i, "of", length(rock_files), ":", basename(file_path)))
  
  tryCatch({
    # Read the ROCK file
    lines <- readLines(file_path, warn = FALSE)
    
    # Extract case identifier
    cid_line <- grep('^\\[\\[cid=', lines, value = TRUE)
    case_id <- if (length(cid_line) > 0) str_extract(cid_line, '(?<=\\[\\[cid=)[^\\]]+') else "unknown"
    message(paste("  Case ID:", case_id))
    
    # Extract codes from this file
    codes_in_file <- detect_codes(lines)
    message(paste("  Found", length(codes_in_file), "codes"))
    
    if (length(codes_in_file) == 0) {
      message(paste("  ⚠ No codes found in", basename(file_path)))
      next
    }
    
    # Add frequency values to codes (for individual files, frequency = 1)
    # Original format: [[origin->receiving||edgeType]]
    # Target format: [[origin->receiving||edgeType||1]]
    codes_with_freq <- paste0(substring(codes_in_file, 1, nchar(codes_in_file)-2), '||1]]')
    message(paste("  Added frequency values to codes"))
    
    # Remove codes with "Measured" or "NotMeasured" nodes (same as global processing)
    codes_with_freq <- remove_measured_codes(codes_with_freq)
    message(paste("  Removed Measured/NotMeasured codes, remaining:", length(codes_with_freq)))
    
    # Convert to network format
    network_data <- convert_rock_codes_to_network(codes_with_freq)
    message(paste("  Converted to network with", length(network_data$inner_nodes), "inner nodes,", length(network_data$outer_nodes), "outer nodes,", length(network_data$edges), "edges"))
    
    # Show edge types found
    edge_types <- unique(sapply(network_data$edges, function(edge) edge$edge_type))
    message(paste("  Edge types found:", paste(edge_types, collapse = ", ")))
    
    # Ensure network_data includes edge type information for HTML generation
    if (is.null(network_data$edge_types)) {
      network_data$edge_types <- edge_types
    }
    if (is.null(network_data$edge_type_styling)) {
      # Create edge type styling for individual files
      colors <- c("blue", "red", "green", "orange", "purple", "brown", "pink", "gray")
      styles <- c("solid", "dashed", "dotted")
      edge_type_styling <- list()
      for (i in seq_along(edge_types)) {
        edge_type_styling[[edge_types[i]]] <- list(
          color = colors[((i-1) %% length(colors)) + 1],
          style = styles[((i-1) %% length(styles)) + 1]
        )
      }
      network_data$edge_type_styling <- edge_type_styling
    }
    
    # Generate individual HTML file with case identifier
    output_filename <- file.path("graphs", paste0("graph_case_", case_id, ".html"))
    result <- generate_network_graph(network_data, output_filename)
    
    individual_results[[file_name]] <- list(
      file_path = file_path,
      case_id = case_id,
      codes_in_file = codes_in_file,
      network_data = network_data,
      html_result = result
    )
    
    message(paste("  ✓ Generated:", output_filename))
    
  }, error = function(e) {
    message(paste("  ✗ Error processing", basename(file_path), ":", e$message))
    message(paste("  Error details:", toString(e)))
  })
}

message(paste("\nProcessed", length(individual_results), "files successfully"))

# Function to generate network graph from codes_with_freq input
generate_network_graph <- function(codes_with_freq, output_filename = "network_graph.html") {
  # Extract nodes and edges from codes_with_freq
  # Assuming codes_with_freq contains the network data
  # You may need to adjust this based on the actual structure of codes_with_freq
  
  # Extract nodes (assuming they're in codes_with_freq$inner_nodes and codes_with_freq$outer_nodes)
  if (is.null(codes_with_freq$inner_nodes)) {
    stop("codes_with_freq must contain an 'inner_nodes' element")
  }
  
  if (is.null(codes_with_freq$outer_nodes)) {
    stop("codes_with_freq must contain an 'outer_nodes' element")
  }
  
  # Extract edges (assuming they're in codes_with_freq$edges)
  if (is.null(codes_with_freq$edges)) {
    stop("codes_with_freq must contain an 'edges' element")
  }
  
  # Get inner and outer nodes from the input
  inner_nodes <- codes_with_freq$inner_nodes
  outer_nodes <- codes_with_freq$outer_nodes
  
  # Get edges from the input
  edges_data <- codes_with_freq$edges
  
  # Validate that all nodes are defined
  all_defined_nodes <- unique(c(inner_nodes, outer_nodes))
  
  # Extract all nodes mentioned in edges
  all_nodes_in_edges <- unique(unlist(lapply(edges_data, function(edge) {
    c(edge$from, edge$to)
  })))
  
  missing_nodes <- setdiff(all_nodes_in_edges, all_defined_nodes)
  if (length(missing_nodes) > 0) {
    stop(paste("Error: The following nodes from your edges are not assigned to an inner or outer circle:", paste(missing_nodes, collapse=", ")))
  }
  
  # Generate DOT script template with dynamic edges
  dot_script_template <- '
digraph ROCK_network {
    layout="%s";
    overlap=false;
    splines="true";
    sep="+5,5";
    mindist=0.5;
    margin="0.5,0.5";
    K=0.3;

    node [shape=circle, fixedsize=true, width=1.2, height=1.2, fontname="Inter", fontsize=10, labelloc="c"];

    %s

    edge [dir=forward, fontname="Inter"];

    %s
}
'

# Generate edge definitions from the input data
edge_definitions <- character(length(edges_data))
for (i in seq_along(edges_data)) {
  edge <- edges_data[[i]]
  dir_attr <- if (!is.null(edge$dir) && edge$dir == "none") 'dir="none"' else ""
  edge_definitions[i] <- sprintf('%s -> %s [color="%s", style="%s", penwidth=%d%s];', 
                                 edge$from, edge$to, edge$color, edge$style, edge$penwidth,
                                 if (dir_attr != "") paste0(", ", dir_attr) else "")
}
edges_string <- paste(edge_definitions, collapse = "\n    ")

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
  
  # Function to get circle positions
  get_circle_positions <- function(node_names, radius, center_x = 0, center_y = 0) {
    num_nodes <- length(node_names)
    angles <- seq(0, 2 * pi, length.out = num_nodes + 1)[1:num_nodes]
    x_coords <- center_x + radius * cos(angles)
    y_coords <- center_y + radius * sin(angles)
    
    node_definitions <- character(num_nodes)
    for (i in seq_along(node_names)) {
      # Truncate the node label if it's too long
      truncated_name <- truncate_label(node_names[i])
      node_definitions[i] <- sprintf('%s [pos="%f,%f!", label="%s"];', 
                                   node_names[i], x_coords[i], y_coords[i], truncated_name)
    }
    return(paste(node_definitions, collapse = "\n"))
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
    all_node_positions <- get_circle_positions(all_nodes, compact_radius)
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
    all_node_positions <- get_circle_positions(all_nodes, radius)
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
    
    inner_node_positions <- get_circle_positions(inner_nodes, inner_radius)
    outer_node_positions <- get_circle_positions(outer_nodes, outer_radius)
    all_node_positions <- paste(inner_node_positions, outer_node_positions, sep = "\n")
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
    
    inner_node_positions <- get_circle_positions(inner_nodes, inner_radius)
    outer_node_positions <- get_circle_positions(outer_nodes, outer_radius)
    all_node_positions <- paste(inner_node_positions, outer_node_positions, sep = "\n")
    layout_engine <- "neato"
  }

dot_script_string <- sprintf(dot_script_template, layout_engine, all_node_positions, edges_string)

input_dot_file <- tempfile(pattern = "ROCK_network_", fileext = ".dot")
output_svg_file <- tempfile(pattern = "ROCK_network_", fileext = ".svg")

message(paste("Writing DOT script to temporary file:", input_dot_file))
writeLines(dot_script_string, input_dot_file)

command_executable <- layout_engine

args <- c(
  "-Tsvg",
  "-o", output_svg_file,
  input_dot_file
)

message(paste("Executing Graphviz command:", command_executable, paste(args, collapse=" ")))
result <- system2(command_executable, args, stdout = TRUE, stderr = TRUE)

if (!is.null(attr(result, "status")) && attr(result, "status") != 0) {
  warning("Graphviz command failed. Please check Graphviz installation and PATH, and the DOT syntax.")
  message("Graphviz Error Output (stderr/stdout):")
  print(result)
  stop("Graph rendering failed.")
} else {
  message(paste("SVG graph successfully generated to temporary file:", output_svg_file))
  
  svg_content <- readLines(output_svg_file)
  svg_content_string <- paste(svg_content, collapse = "\n")
  
      # Generate dynamic filter controls based on edge types
    filter_controls <- ""
    if (!is.null(codes_with_freq$edge_types) && length(codes_with_freq$edge_types) > 0) {
      edge_type_controls <- ""
      for (edge_type in codes_with_freq$edge_types) {
        if (edge_type %in% names(codes_with_freq$edge_type_styling)) {
          styling <- codes_with_freq$edge_type_styling[[edge_type]]
          color <- styling$color
          style <- styling$style
          
          # Create SVG line for preview
          svg_line <- if (style == "dashed") {
            sprintf('<svg width="40" height="2" class="ml-2"><line x1="0" y1="1" x2="40" y2="1" stroke="%s" stroke-width="2" stroke-dasharray="5,5"/></svg>', color)
          } else if (style == "dotted") {
            sprintf('<svg width="40" height="2" class="ml-2"><line x1="0" y1="1" x2="40" y2="1" stroke="%s" stroke-width="2" stroke-dasharray="2,2"/></svg>', color)
          } else {
            sprintf('<svg width="40" height="2" class="ml-2"><line x1="0" y1="1" x2="40" y2="1" stroke="%s" stroke-width="2"/></svg>', color)
          }
          
          edge_type_controls <- paste0(edge_type_controls, sprintf('
                <label class="checkbox-label">
                    <input type="checkbox" id="filter-%s" data-color="%s" data-style="%s" data-type="%s" checked>
                    <span class="text-%s-600 font-medium">%s (%s %s)</span>
                    %s
                </label>', 
                tolower(edge_type), color, style, edge_type, color, edge_type, 
                tools::toTitleCase(color), tools::toTitleCase(style), svg_line))
        }
      }
      filter_controls <- sprintf('
            <div class="grid grid-cols-1 sm:grid-cols-2 gap-4">
                %s
            </div>', edge_type_controls)
    } else {
      # Fallback if no edge types found
      filter_controls <- '
            <div class="grid grid-cols-1 sm:grid-cols-2 gap-4">
                <label class="checkbox-label">
                    <input type="checkbox" id="filter-all" data-color="all" data-style="all" checked>
                    <span class="text-gray-800 font-medium">All Edges</span>
                </label>
            </div>'
    }
    
      # Read HTML template from separate file
  html_template_file <- "html_template.html"
  if (!file.exists(html_template_file)) {
    stop("HTML template file 'html_template.html' not found. Please ensure it exists in the working directory.")
  }
  html_template <- readLines(html_template_file)
      # Create the final HTML by replacing placeholders
    html_output_string <- paste(html_template, collapse = "\n")
    html_output_string <- gsub("%EDGE_TYPES%", 
                              if (!is.null(codes_with_freq$edge_types)) paste(codes_with_freq$edge_types, collapse = ", ") else "None found", 
                              html_output_string)
    html_output_string <- gsub("%FILTER_CONTROLS%", filter_controls, html_output_string)
    html_output_string <- gsub("%SVG_CONTENT%", svg_content_string, html_output_string)
  
  # Define the path for your output HTML file
  output_html_file <- output_filename
  
  # Save the HTML content to the specified file
  writeLines(html_output_string, output_html_file)
  
  message(paste("Interactive HTML graph saved to:", normalizePath(output_html_file)))
  
  # Return the HTML content as a string for further use if needed
  return(html_output_string)
}
}

# Example usage:
# codes_with_freq <- list(
#   inner_nodes = c("DValue", "ACost", "PMotivation", "EClinic"),
#   outer_nodes = c("PEducation", "EAvailable", "AQuality", "AEfficiency",
#                   "ELaw", "DSit", "EOthers", "AOutdated", "PTrust",
#                   "ABenefit", "ESupport", "ERegional", "DPersonal"),
#   edges = list(
#     list(from = "PEducation", to = "PMotivation", color = "blue", style = "dashed", penwidth = 5),
#     list(from = "ACost", to = "EClinic", color = "blue", style = "dashed", penwidth = 4),
#     # ... more edges
#   )
# )
# 
# result <- generate_network_graph(codes_with_freq)

# Test the function with sample data
codes_with_freq <- list(
  inner_nodes = c("DValue", "ACost", "PMotivation", "EClinic"),
  outer_nodes = c("PEducation", "EAvailable", "AQuality", "AEfficiency",
                  "ELaw", "DSit", "EOthers", "AOutdated", "PTrust",
                  "ABenefit", "ESupport", "ERegional", "DPersonal"),
  edges = list(
    list(from = "PEducation", to = "PMotivation", color = "blue", style = "dashed", penwidth = 5),
    list(from = "ACost", to = "EClinic", color = "blue", style = "dashed", penwidth = 4),
    list(from = "EAvailable", to = "ACost", color = "red", style = "solid", penwidth = 3),
    list(from = "EAvailable", to = "AQuality", color = "black", style = "solid", penwidth = 3),
    list(from = "ACost", to = "EAvailable", color = "blue", style = "dashed", penwidth = 2),
    list(from = "ACost", to = "DValue", color = "black", style = "solid", penwidth = 2),
    list(from = "EClinic", to = "DValue", color = "red", style = "solid", penwidth = 2),
    list(from = "AEfficiency", to = "ACost", color = "black", style = "solid", penwidth = 2),
    list(from = "ELaw", to = "AQuality", color = "black", style = "solid", penwidth = 2),
    list(from = "AEfficiency", to = "DValue", color = "black", style = "solid", penwidth = 2),
    list(from = "DSit", to = "DValue", color = "blue", style = "dashed", penwidth = 2),
    list(from = "ACost", to = "EOthers", color = "blue", style = "dashed", penwidth = 2),
    list(from = "ACost", to = "PMotivation", color = "blue", style = "dashed", penwidth = 2),
    list(from = "EClinic", to = "AOutdated", color = "blue", style = "dashed", penwidth = 1),
    list(from = "AOutdated", to = "PTrust", color = "blue", style = "dashed", penwidth = 1),
    list(from = "EAvailable", to = "AEfficiency", color = "black", style = "solid", penwidth = 1),
    list(from = "DValue", to = "EOthers", color = "black", style = "solid", penwidth = 1),
    list(from = "ELaw", to = "ACost", color = "blue", style = "dashed", penwidth = 1),
    list(from = "PEducation", to = "PTrust", color = "red", style = "solid", penwidth = 1),
    list(from = "EAvailable", to = "AOutdated", color = "red", style = "solid", penwidth = 1),
    list(from = "EOthers", to = "PEducation", color = "blue", style = "dashed", penwidth = 1),
    list(from = "DValue", to = "ABenefit", color = "black", style = "solid", penwidth = 1),
    list(from = "EOthers", to = "ACost", color = "blue", style = "dashed", penwidth = 1),
    list(from = "EClinic", to = "AEfficiency", color = "red", style = "solid", penwidth = 1),
    list(from = "PTrust", to = "PMotivation", color = "blue", style = "dashed", penwidth = 1),
    list(from = "ABenefit", to = "ACost", color = "black", style = "solid", penwidth = 1),
    list(from = "DValue", to = "PMotivation", color = "black", style = "solid", penwidth = 1),
    list(from = "ESupport", to = "PEducation", color = "blue", style = "dashed", penwidth = 1),
    list(from = "ESupport", to = "EClinic", color = "red", style = "solid", penwidth = 1),
    list(from = "ELaw", to = "DSit", color = "blue", style = "dashed", penwidth = 1),
    list(from = "ESupport", to = "PMotivation", color = "black", style = "solid", penwidth = 1),
    list(from = "EOthers", to = "PMotivation", color = "blue", style = "dashed", penwidth = 1),
    list(from = "AEfficiency", to = "AQuality", color = "black", style = "solid", penwidth = 1),
    list(from = "ESupport", to = "ACost", color = "black", style = "solid", penwidth = 1),
    list(from = "DValue", to = "AEfficiency", color = "black", style = "solid", penwidth = 1),
    list(from = "AEfficiency", to = "EOthers", color = "blue", style = "dashed", penwidth = 1),
    list(from = "DValue", to = "EOthers", color = "blue", style = "dashed", penwidth = 1),
    list(from = "EClinic", to = "DValue", color = "blue", style = "dashed", penwidth = 1),
    list(from = "DValue", to = "DPersonal", color = "black", style = "solid", penwidth = 1),
    list(from = "AQuality", to = "DValue", color = "blue", style = "dashed", penwidth = 1),
    list(from = "EAvailable", to = "EClinic", color = "red", style = "solid", penwidth = 1),
    list(from = "AEfficiency", to = "EClinic", color = "blue", style = "dashed", penwidth = 1),
    list(from = "EOthers", to = "ACost", color = "red", style = "solid", penwidth = 1),
    list(from = "ERegional", to = "EClinic", color = "blue", style = "dashed", penwidth = 1),
    list(from = "EOthers", to = "ABenefit", color = "black", style = "solid", penwidth = 1),
    list(from = "ACost", to = "PMotivation", color = "red", style = "solid", penwidth = 1),
    list(from = "ERegional", to = "ACost", color = "blue", style = "dashed", penwidth = 1),
    list(from = "ERegional", to = "EOthers", color = "red", style = "solid", penwidth = 1),
    list(from = "EClinic", to = "ACost", color = "blue", style = "dashed", penwidth = 1),
    list(from = "DValue", to = "ACost", color = "black", style = "solid", penwidth = 1)
  )
)

# Generate combined output for all ROCK files using global frequency analysis
if (length(individual_results) > 0) {
  message("\nGenerating combined output for all ROCK files using global frequency analysis...")
  
  # Use the global frequency analysis from earlier in the script
  # global_codes_with_freq contains the codes with their actual frequencies across all files
  combined_network_data <- convert_rock_codes_to_network(global_codes_with_freq)
  
  # Show edge types found in combined data
  edge_types <- unique(sapply(combined_network_data$edges, function(edge) edge$edge_type))
  message(paste("  Edge types found in combined data:", paste(edge_types, collapse = ", ")))
  
  # Generate combined HTML in graphs directory
  combined_result <- generate_network_graph(combined_network_data, file.path("graphs", "network_graph_combined.html"))
  
  message("✓ Generated combined output: graphs/network_graph_combined.html")
  message(paste("Combined network contains:", length(combined_network_data$inner_nodes), "inner nodes,", length(combined_network_data$outer_nodes), "outer nodes, and", length(combined_network_data$edges), "edges"))
} else {
  message("No files were successfully processed, skipping combined output generation.")
}

# Also generate the original sample data output for comparison
message("\nGenerating sample data output for comparison...")
codes_with_freq <- list(
  inner_nodes = c("DValue", "ACost", "PMotivation", "EClinic"),
  outer_nodes = c("PEducation", "EAvailable", "AQuality", "AEfficiency",
                  "ELaw", "DSit", "EOthers", "AOutdated", "PTrust",
                  "ABenefit", "ESupport", "ERegional", "DPersonal"),
  edges = list(
    list(from = "PEducation", to = "PMotivation", color = "blue", style = "dashed", penwidth = 5),
    list(from = "ACost", to = "EClinic", color = "blue", style = "dashed", penwidth = 4),
    list(from = "EAvailable", to = "ACost", color = "red", style = "solid", penwidth = 3),
    list(from = "EAvailable", to = "AQuality", color = "black", style = "solid", penwidth = 3),
    list(from = "ACost", to = "EAvailable", color = "blue", style = "dashed", penwidth = 2),
    list(from = "ACost", to = "DValue", color = "black", style = "solid", penwidth = 2),
    list(from = "EClinic", to = "DValue", color = "red", style = "solid", penwidth = 2),
    list(from = "AEfficiency", to = "ACost", color = "black", style = "solid", penwidth = 2),
    list(from = "ELaw", to = "AQuality", color = "black", style = "solid", penwidth = 2),
    list(from = "AEfficiency", to = "DValue", color = "black", style = "solid", penwidth = 2),
    list(from = "DSit", to = "DValue", color = "blue", style = "dashed", penwidth = 2),
    list(from = "ACost", to = "EOthers", color = "blue", style = "dashed", penwidth = 2),
    list(from = "ACost", to = "PMotivation", color = "blue", style = "dashed", penwidth = 2),
    list(from = "EClinic", to = "AOutdated", color = "blue", style = "dashed", penwidth = 1),
    list(from = "AOutdated", to = "PTrust", color = "blue", style = "dashed", penwidth = 1),
    list(from = "EAvailable", to = "AEfficiency", color = "black", style = "solid", penwidth = 1),
    list(from = "DValue", to = "EOthers", color = "black", style = "solid", penwidth = 1),
    list(from = "ELaw", to = "ACost", color = "blue", style = "dashed", penwidth = 1),
    list(from = "PEducation", to = "PTrust", color = "red", style = "solid", penwidth = 1),
    list(from = "EAvailable", to = "AOutdated", color = "red", style = "solid", penwidth = 1),
    list(from = "EOthers", to = "PEducation", color = "blue", style = "dashed", penwidth = 1),
    list(from = "DValue", to = "ABenefit", color = "black", style = "solid", penwidth = 1),
    list(from = "EOthers", to = "ACost", color = "blue", style = "dashed", penwidth = 1),
    list(from = "EClinic", to = "AEfficiency", color = "red", style = "solid", penwidth = 1),
    list(from = "PTrust", to = "PMotivation", color = "blue", style = "dashed", penwidth = 1),
    list(from = "ABenefit", to = "ACost", color = "black", style = "solid", penwidth = 1),
    list(from = "DValue", to = "PMotivation", color = "black", style = "solid", penwidth = 1),
    list(from = "ESupport", to = "PEducation", color = "blue", style = "dashed", penwidth = 1),
    list(from = "ESupport", to = "EClinic", color = "red", style = "solid", penwidth = 1),
    list(from = "ELaw", to = "DSit", color = "blue", style = "dashed", penwidth = 1),
    list(from = "ESupport", to = "PMotivation", color = "black", style = "solid", penwidth = 1),
    list(from = "EOthers", to = "PMotivation", color = "blue", style = "dashed", penwidth = 1),
    list(from = "AEfficiency", to = "AQuality", color = "black", style = "solid", penwidth = 1),
    list(from = "ESupport", to = "ACost", color = "black", style = "solid", penwidth = 1),
    list(from = "DValue", to = "AEfficiency", color = "black", style = "solid", penwidth = 1),
    list(from = "AEfficiency", to = "EOthers", color = "blue", style = "dashed", penwidth = 1),
    list(from = "DValue", to = "EOthers", color = "blue", style = "dashed", penwidth = 1),
    list(from = "EClinic", to = "DValue", color = "blue", style = "dashed", penwidth = 1),
    list(from = "DValue", to = "DPersonal", color = "black", style = "solid", penwidth = 1),
    list(from = "AQuality", to = "DValue", color = "blue", style = "dashed", penwidth = 1),
    list(from = "EAvailable", to = "EClinic", color = "red", style = "solid", penwidth = 1),
    list(from = "AEfficiency", to = "EClinic", color = "blue", style = "dashed", penwidth = 1),
    list(from = "EOthers", to = "ACost", color = "red", style = "solid", penwidth = 1),
    list(from = "ERegional", to = "EClinic", color = "blue", style = "dashed", penwidth = 1),
    list(from = "EOthers", to = "ABenefit", color = "black", style = "solid", penwidth = 1),
    list(from = "ACost", to = "PMotivation", color = "red", style = "solid", penwidth = 1),
    list(from = "ERegional", to = "ACost", color = "blue", style = "dashed", penwidth = 1),
    list(from = "ERegional", to = "EOthers", color = "red", style = "solid", penwidth = 1),
    list(from = "EClinic", to = "ACost", color = "blue", style = "dashed", penwidth = 1),
    list(from = "DValue", to = "ACost", color = "black", style = "solid", penwidth = 1)
  )
)

# Execute the function to generate the HTML document
result <- generate_network_graph(codes_with_freq, "network_graph_sample.html")
message("✓ Generated sample data output: network_graph_sample.html")