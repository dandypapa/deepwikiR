# /home/ubuntu/autodoc_tool/R/code_analyzer.R
# Handles R code analysis, including function extraction and call graph generation

# Load necessary libraries
if (!requireNamespace("CodeDepends", quietly = TRUE)) {
  stop("Package CodeDepends is not installed. Please install it.")
}
library(CodeDepends)
library(stringr)

# Function to extract basic elements (functions) from R code content
extract_r_code_elements <- function(file_path, code_content) {
  elements <- list()
  tryCatch({
    parsed_code <- parse(text = code_content, keep.source = TRUE)
    if (length(parsed_code) == 0) return(list(functions = list(), call_graph_data = list()))

    for (i in seq_along(parsed_code)) {
      expr <- parsed_code[[i]]
      if (is.call(expr) && (as.character(expr[[1]]) %in% c("<-", "="))) {
        if (length(expr) == 3 && is.call(expr[[3]]) && as.character(expr[[3]][[1]]) == "function") {
          func_name <- as.character(expr[[2]])
          func_signature <- deparse(expr[[3]][[2]], width.cutoff = 500L)
          func_signature_clean <- stringr::str_squish(stringr::str_replace_all(func_signature, "\n", " "))
          
          elements[[length(elements) + 1]] <- list(
            name = func_name,
            type = "function",
            signature = paste0("(", func_signature_clean, ")"),
            # Use paste with collapse for deparse to avoid nlines issues
            code_block = paste(deparse(expr, width.cutoff = 500L), collapse = "\n"), 
            file_path = file_path,
            description = "" 
          )
        }
      }
    }
  }, error = function(e) {
    cat(paste("Error parsing R code in", file_path, ":", e$message, "\n"))
  })
  return(list(functions = elements, call_graph_data = list()))
}

# Function to analyze R script(s) and generate a call graph
# Takes a list of absolute file_paths and their content (code_details)
generate_r_call_graph <- function(r_file_paths_from_config, project_base_path) {
  all_edges <- list()
  all_nodes <- character(0)
  
  if (length(r_file_paths_from_config) == 0) {
    return(list(nodes = character(0), edges = list()))
  }

  tryCatch({
    defined_functions_map <- list() # file_path_relative -> list of function names
    all_defined_function_names <- character(0)

    # First pass: identify all defined functions and their relative paths
    for(relative_fp in r_file_paths_from_config){
        absolute_fp <- file.path(project_base_path, relative_fp)
        if (!file.exists(absolute_fp)) {
            warning(paste("File not found for call graph generation:", absolute_fp))
            next
        }
        file_content <- readLines(absolute_fp, warn = FALSE, encoding = "UTF-8")
        parsed_file_code <- parse(text = file_content, keep.source = TRUE)
        defined_in_file <- character(0)
        for (expr in parsed_file_code) {
            if (is.call(expr) && (as.character(expr[[1]]) %in% c("<-", "="))) {
                if (length(expr) == 3 && is.call(expr[[3]]) && as.character(expr[[3]][[1]]) == "function") {
                    func_name <- as.character(expr[[2]])
                    defined_in_file <- c(defined_in_file, func_name)
                    # Use relative path for unique ID to match how elements are stored
                    all_defined_function_names <- c(all_defined_function_names, paste0(relative_fp, "::", func_name))
                }
            }
        }
        if(length(defined_in_file) > 0) defined_functions_map[[relative_fp]] <- defined_in_file
    }
    all_defined_function_names <- unique(all_defined_function_names)
    all_nodes <- all_defined_function_names # Start with defined functions as nodes

    # Second pass: find calls within each defined function
    for(relative_fp in r_file_paths_from_config){
        absolute_fp <- file.path(project_base_path, relative_fp)
        if (!file.exists(absolute_fp) || is.null(defined_functions_map[[relative_fp]])) next
        
        file_content <- readLines(absolute_fp, warn = FALSE, encoding = "UTF-8")
        parsed_file_code <- parse(text = file_content, keep.source = TRUE)
        
        for (expr in parsed_file_code) {
            if (is.call(expr) && (as.character(expr[[1]]) %in% c("<-", "="))) {
                if (length(expr) == 3 && is.call(expr[[3]]) && as.character(expr[[3]][[1]]) == "function") {
                    caller_name <- as.character(expr[[2]])
                    caller_id <- paste0(relative_fp, "::", caller_name)
                    
                    find_calls_in_body <- function(body_expr) {
                        calls <- character(0)
                        if (is.call(body_expr)) {
                            called_func_char <- deparse(body_expr[[1]])
                            if (grepl("^[a-zA-Z0-9_.]+(::[a-zA-Z0-9_.]+)?$", called_func_char)) {
                                calls <- c(calls, called_func_char)
                            }
                            for (k in 2:length(body_expr)) {
                                calls <- c(calls, find_calls_in_body(body_expr[[k]]))
                            }
                        } else if (is.recursive(body_expr)) { 
                            for (sub_expr in body_expr) {
                                calls <- c(calls, find_calls_in_body(sub_expr))
                            }
                        }
                        return(unique(calls))
                    }
                    
                    if (length(expr[[3]]) >= 3) {
                        body_calls <- find_calls_in_body(expr[[3]][[3]])
                        for (callee_name_raw in body_calls) {
                            callee_id <- callee_name_raw
                            is_local_call <- FALSE
                            # Check if callee is one of the defined functions
                            for(search_relative_fp in names(defined_functions_map)){
                                if(callee_name_raw %in% defined_functions_map[[search_relative_fp]]){
                                    callee_id <- paste0(search_relative_fp, "::", callee_name_raw)
                                    is_local_call <- TRUE
                                    break
                                }
                            }
                            if (! (callee_id %in% all_nodes)) {
                                all_nodes <- c(all_nodes, callee_id)
                            }
                            all_edges[[length(all_edges) + 1]] <- list(caller = caller_id, callee = callee_id)
                        }
                    }
                }
            }
        }
    }
    
    all_edges <- unique(all_edges)
    all_nodes <- unique(all_nodes)

  }, error = function(e) {
    cat(paste("Error generating R call graph:", e$message, "\n"))
    return(list(nodes = character(0), edges = list()))
  })
  
  return(list(nodes = all_nodes, edges = all_edges))
}

# Main analysis function called by main.R
analyze_r_repository_code <- function(code_details, project_base_path) {
  all_elements <- list()
  # file_paths here are relative to project_base_path
  all_relative_file_paths <- names(code_details)
  
  for (relative_file_path in all_relative_file_paths) {
    code_content <- code_details[[relative_file_path]]
    if (is.null(code_content) || nchar(code_content) == 0) next
    
    file_elements_result <- extract_r_code_elements(relative_file_path, code_content)
    if (length(file_elements_result$functions) > 0) {
      all_elements <- c(all_elements, file_elements_result$functions)
    }
  }
  
  r_file_relative_paths <- all_relative_file_paths[endsWith(all_relative_file_paths, ".R") | endsWith(all_relative_file_paths, ".r")]
  # Pass the project_base_path for constructing absolute paths when reading files for call graph
  call_graph_result <- generate_r_call_graph(r_file_relative_paths, project_base_path)
  
  return(list(
    extracted_elements = all_elements,
    call_graph = call_graph_result
  ))
}

