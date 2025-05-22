# /home/ubuntu/autodoc_tool/R/quarto_renderer.R
# Handles Quarto document generation and rendering

# Function to generate Mermaid syntax for a call graph
# Input: call_graph_data (list with 'nodes' and 'edges')
# Output: Mermaid syntax string
generate_mermaid_call_graph <- function(call_graph_data) {
  if (is.null(call_graph_data) || length(call_graph_data$nodes) == 0) {
    return("graph TD;\n  A[No call graph data available];")
  }

  mermaid_string <- "graph TD;\n"
  
  # Define nodes
  # Ensure node IDs are Mermaid-friendly (no special chars like ::, -, .)
  # Replace :: with __, and other problematic chars with _
  sanitize_mermaid_id <- function(id) {
    id <- gsub("::", "__", id)
    id <- gsub("[^a-zA-Z0-9_]", "_", id) # Replace non-alphanumeric (excluding _) with _
    # Ensure it doesn't start with a number if that's an issue, though Mermaid is usually flexible
    if (grepl("^[0-9]", id)) {
        id <- paste0("n",id) # prefix with 'n' if starts with a number
    }
    return(id)
  }
  
  node_map <- list() # To map original names to sanitized IDs
  for (node_name in unique(call_graph_data$nodes)) {
    sanitized_id <- sanitize_mermaid_id(node_name)
    node_map[[node_name]] <- sanitized_id
    # Node label will be the original name, ID is sanitized
    mermaid_string <- paste0(mermaid_string, "  ", sanitized_id, "[\"", node_name, "\"];\n")
  }
  
  # Define edges
  if (length(call_graph_data$edges) > 0) {
    for (edge in call_graph_data$edges) {
      caller_orig <- edge$caller
      callee_orig <- edge$callee
      
      # Use sanitized IDs from node_map
      caller_id <- node_map[[caller_orig]]
      callee_id <- node_map[[callee_orig]]
      
      if (!is.null(caller_id) && !is.null(callee_id)) {
        mermaid_string <- paste0(mermaid_string, "  ", caller_id, " --> ", callee_id, ";\n")
      } else {
        # This case should ideally not happen if all nodes from edges are in call_graph_data$nodes
        cat(paste("Warning: Missing node ID for caller '", caller_orig, "' or callee '", callee_orig, "'\n"))
      }
    }
  }
  
  return(mermaid_string)
}


# Function to render documentation using Quarto
# Takes generated content, output path, format, and call graph data
render_quarto_document <- function(qmd_content_list, output_dir, output_filename_base, quarto_format = "html", project_name = "Project", call_graph_data = NULL) {
  # Ensure output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Construct the full path for the .qmd file
  qmd_file_name <- paste0(output_filename_base, ".qmd")
  qmd_file_path <- file.path(output_dir, qmd_file_name)
  
  # Prepare Quarto YAML header
  doc_title <- if (!is.null(project_name) && nzchar(project_name)) {
    paste("Code Documentation for Project:", project_name)
  } else {
    "Generated Code Documentation"
  }
  
  yaml_header <- paste0(
    "---",
    "\ntitle: \"", doc_title, "\"",
    "\nformat: ", quarto_format,
    "\neditor: visual", # or source, depending on preference
    "\ntoc: true",
    "\ntoc-depth: 3",
    "\n---",
    "\n\n"
  )
  
  full_qmd_content <- yaml_header
  
  # Add Mermaid call graph section if data is available
  if (!is.null(call_graph_data) && length(call_graph_data$nodes) > 0) {
    mermaid_syntax <- generate_mermaid_call_graph(call_graph_data)
    mermaid_section <- paste0(
      "## Function Call Graph\n\n",
      "```mermaid\n",
      mermaid_syntax,
      "\n```\n\n"
    )
    full_qmd_content <- paste0(full_qmd_content, mermaid_section)
  }
  
  # Add main documentation content (function descriptions etc.)
  if (length(qmd_content_list) > 0) {
    full_qmd_content <- paste0(full_qmd_content, "## Function Documentation\n\n")
    for (item in qmd_content_list) {
      # Ensure item is a character string
      if(is.list(item) && !is.null(item$content_block)) { # Assuming item might be a list with a content_block
        item_content <- item$content_block
      } else if (is.character(item)) {
        item_content <- item
      } else {
        item_content <- "" # Skip if not in expected format
      }
      full_qmd_content <- paste0(full_qmd_content, item_content, "\n\n---\n\n")
    }
  }
  
  # Write the .qmd file
  tryCatch({
    writeLines(full_qmd_content, qmd_file_path)
    cat(paste("Quarto input file written to:", qmd_file_path, "\n"))
  }, error = function(e) {
    stop(paste("Error writing .qmd file:", e$message))
  })
  
  # Render the Quarto document
  # Ensure Quarto CLI is available in PATH
  output_file_full_path <- ""
  tryCatch({
    # quarto::quarto_render uses the CLI. It needs to be in PATH.
    # The output file will be in the same directory as the qmd_file_path by default.
    quarto::quarto_render(qmd_file_path, output_format = quarto_format, quiet = FALSE)
    
    # Determine the output file name (e.g., .html, .pdf)
    rendered_output_filename <- paste0(output_filename_base, ".", quarto_format)
    output_file_full_path <- file.path(output_dir, rendered_output_filename)
    
    if (file.exists(output_file_full_path)) {
      cat(paste("Successfully rendered Quarto document to:", output_file_full_path, "\n"))
      return(output_file_full_path)
    } else {
      # Sometimes quarto_render might not throw an error but file is not created, or name is different
      # List files to see what was created
      actual_files <- list.files(output_dir)
      cat("Quarto rendering finished, but expected output file not found. Files in output dir:", paste(actual_files, collapse=", "), "\n")
      # Try to find a file with the base name and correct extension
      potential_matches <- actual_files[startsWith(actual_files, output_filename_base) & endsWith(actual_files, paste0(".", quarto_format))]
      if(length(potential_matches) == 1){
        output_file_full_path <- file.path(output_dir, potential_matches[1])
        cat(paste("Found rendered document at:", output_file_full_path, "\n"))
        return(output_file_full_path)
      } else {
         stop(paste("Quarto rendering did not produce the expected output file:", rendered_output_filename))
      }
    }
  }, error = function(e) {
    # Attempt to get more detailed error from Quarto if possible
    cat(paste("Error during Quarto rendering:", e$message, "\n"))
    # Check if a log file was created by Quarto
    quarto_log_file <- sub("\\.qmd$", "_quarto.log", qmd_file_path)
    if(file.exists(quarto_log_file)){
        cat("--- Quarto Log ---\n")
        cat(readLines(quarto_log_file), sep="\n")
        cat("--- End Quarto Log ---\n")
    }
    stop(paste("Error during Quarto rendering:", e$message))
  })
}

