# /home/ubuntu/autodoc_tool/R/code_analyzer.R
# Handles R code analysis, including function extraction and call graph generation

#' @importFrom stringr str_squish str_replace_all
NULL

#' Extract R Code Elements (Functions)
#'
#' @description
#' Parses R code content from a single file to extract function definitions.
#' It identifies functions defined using `<-` or `=` assignments.
#'
#' @param file_path Character string. The relative path to the R file being processed.
#'        Used for associating extracted elements with their source file.
#' @param code_content Character string. The actual R code content of the file.
#'
#' @return A list containing:
#'   \item{functions}{A list of extracted function elements. Each element is a list with:
#'     \itemize{
#'       \item `name`: Name of the function.
#'       \item `type`: "function".
#'       \item `signature`: Formal arguments of the function as a string.
#'       \item `code_block`: The complete code block of the function definition.
#'       \item `file_path`: The `file_path` argument.
#'       \item `description`: An empty string, to be filled later by LLM.
#'     }}
#'   \item{call_graph_data}{An empty list (placeholder, as call graph generation is separate).}
#' @noRd
extract_r_code_elements <- function(file_path, code_content) {
  elements <- list()
  tryCatch({
    # Suppress "incomplete final line" warning if it's not critical for parsing
    parsed_code <- suppressWarnings(parse(text = code_content, keep.source = TRUE))
    if (length(parsed_code) == 0) return(list(functions = list(), call_graph_data = list()))

    for (i in seq_along(parsed_code)) {
      expr <- parsed_code[[i]]
      if (is.call(expr) && (as.character(expr[[1]]) %in% c("<-", "="))) {
        if (length(expr) == 3 && is.call(expr[[3]]) && as.character(expr[[3]][[1]]) == "function") {
          func_name <- as.character(expr[[2]])
          # Ensure func_signature is captured robustly
          func_signature_raw <- expr[[3]][[2]]
          func_signature <- paste(deparse(func_signature_raw, width.cutoff = 500L), collapse = " ")
          func_signature_clean <- stringr::str_squish(stringr::str_replace_all(func_signature, "\n", " "))
          
          elements[[length(elements) + 1]] <- list(
            name = func_name,
            type = "function",
            signature = paste0("(", func_signature_clean, ")"),
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
  return(list(functions = elements, call_graph_data = list())) # call_graph_data is placeholder here
}

#' Generate R Call Graph
#'
#' @description
#' Analyzes a set of R scripts to build a static call graph. It identifies function
#' definitions and the calls made within them.
#'
#' @details
#' The process involves two passes:
#' 1. Identify all functions defined across the provided R files.
#' 2. For each defined function, parse its body to find calls to other functions
#'    (both within the analyzed set and potentially external/library functions).
#' Function identifiers in the graph are typically prefixed with their relative file path
#' (e.g., "R/utils.R::my_helper").
#'
#' @param r_file_paths_from_config Character vector. A list of relative file paths
#'        (from project root) of R scripts to analyze.
#' @param project_base_path Character string. The absolute path to the project's root directory.
#'        Used to construct absolute paths for reading files.
#'
#' @return A list representing the call graph, with two elements:
#'   \item{nodes}{A character vector of unique function identifiers (node names) found or called.}
#'   \item{edges}{A list of edges. Each edge is a list with `caller` and `callee`
#'                (using the function identifiers from `nodes`).}
#' @noRd
generate_r_call_graph <- function(r_file_paths_from_config, project_base_path) {
  all_edges <- list()
  all_nodes <- character(0)
  
  if (length(r_file_paths_from_config) == 0) {
    return(list(nodes = character(0), edges = list()))
  }

  tryCatch({
    defined_functions_map <- list()
    all_defined_function_names <- character(0)

    for(relative_fp in r_file_paths_from_config){
        absolute_fp <- file.path(project_base_path, relative_fp)
        if (!file.exists(absolute_fp)) {
            warning(paste("File not found for call graph generation:", absolute_fp))
            next
        }
        file_content <- readLines(absolute_fp, warn = FALSE, encoding = "UTF-8")
        # Suppress "incomplete final line" warning
        parsed_file_code <- suppressWarnings(parse(text = file_content, keep.source = TRUE))
        defined_in_file <- character(0)
        for (expr in parsed_file_code) {
            if (is.call(expr) && (as.character(expr[[1]]) %in% c("<-", "="))) {
                if (length(expr) == 3 && is.call(expr[[3]]) && as.character(expr[[3]][[1]]) == "function") {
                    func_name <- as.character(expr[[2]])
                    defined_in_file <- c(defined_in_file, func_name)
                    all_defined_function_names <- c(all_defined_function_names, paste0(relative_fp, "::", func_name))
                }
            }
        }
        if(length(defined_in_file) > 0) defined_functions_map[[relative_fp]] <- defined_in_file
    }
    all_defined_function_names <- unique(all_defined_function_names)
    all_nodes <- all_defined_function_names

    for(relative_fp in r_file_paths_from_config){
        absolute_fp <- file.path(project_base_path, relative_fp)
        # Check if file exists AND had functions defined (otherwise defined_functions_map[[relative_fp]] might be NULL)
        if (!file.exists(absolute_fp) || is.null(defined_functions_map[[relative_fp]])) next
        
        file_content <- readLines(absolute_fp, warn = FALSE, encoding = "UTF-8")
        parsed_file_code <- suppressWarnings(parse(text = file_content, keep.source = TRUE))
        
        for (expr in parsed_file_code) {
            if (is.call(expr) && (as.character(expr[[1]]) %in% c("<-", "="))) {
                if (length(expr) == 3 && is.call(expr[[3]]) && as.character(expr[[3]][[1]]) == "function") {
                    caller_name <- as.character(expr[[2]])
                    caller_id <- paste0(relative_fp, "::", caller_name)
                    
                    # Recursive helper to find calls within a function body
                    find_calls_in_body <- function(body_expr) {
                        calls <- character(0)
                        if (is.call(body_expr)) {
                            # Attempt to deparse the called function name/expression
                            # This handles simple function calls, namespaced calls (pkg::fun), and complex calls (obj$method)
                            called_func_char <- deparse(body_expr[[1]], width.cutoff = 500L)[1]
                            # Basic filter for valid-looking function names or calls
                            if (grepl("^[a-zA-Z0-9_.]+(::[a-zA-Z0-9_.]+)?(\\$[a-zA-Z0-9_.]+)?$", called_func_char)) {
                                calls <- c(calls, called_func_char)
                            }
                            # Recurse on arguments
                            for (k in 2:length(body_expr)) {
                                calls <- c(calls, find_calls_in_body(body_expr[[k]]))
                            }
                        } else if (is.recursive(body_expr) && !is.symbol(body_expr)) {
                            # Recurse on list-like structures or language objects, but not symbols
                            for (sub_expr in body_expr) {
                                calls <- c(calls, find_calls_in_body(sub_expr))
                            }
                        }
                        return(unique(calls))
                    }
                    
                    if (length(expr[[3]]) >= 3) { # Ensure there is a body
                        body_calls <- find_calls_in_body(expr[[3]][[3]]) # expr[[3]][[3]] is the function body
                        for (callee_name_raw in body_calls) {
                            callee_id <- callee_name_raw
                            # Check if callee is one of the defined functions in the project
                            # This logic attempts to resolve if the raw call name matches a defined function
                            is_project_defined_call <- FALSE
                            for(search_relative_fp in names(defined_functions_map)){
                                if(callee_name_raw %in% defined_functions_map[[search_relative_fp]]){
                                    callee_id <- paste0(search_relative_fp, "::", callee_name_raw)
                                    is_project_defined_call <- TRUE
                                    break
                                }
                            }
                            # If not a project-defined call, it might be a namespaced call already (e.g. pkg::fun)
                            # or a base R function. Add it to nodes if not already there.
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
    
    all_edges <- unique(all_edges) # Remove duplicate edges
    all_nodes <- unique(all_nodes) # Ensure all nodes are unique

  }, error = function(e) {
    cat(paste("Error generating R call graph:", e$message, "\n"))
    # Return empty graph on error
    return(list(nodes = character(0), edges = list()))
  })
  
  return(list(nodes = all_nodes, edges = all_edges))
}

#' Analyze R Repository Code
#'
#' @description
#' Main analysis function for an R code repository. It orchestrates the extraction
#' of code elements (primarily functions), generation of a call graph, and listing
#' of directory contents.
#'
#' @param code_details A named list where keys are relative file paths (from project root)
#'        and values are the character string content of these files. This is typically
#'        the output of `acquire_code_content`.
#' @param project_base_path Character string. The absolute path to the project's root directory.
#'
#' @return A list containing:
#'   \item{extracted_elements}{A list of function elements extracted from the R files (output of `extract_r_code_elements`).}
#'   \item{call_graph}{A list representing the call graph (output of `generate_r_call_graph`), with `nodes` and `edges`.}
#'   \item{directory_contents}{A named list where names are directory paths (relative to `project_base_path`,
#'         e.g., "R/", ".", "src/utils/") and values are character vectors of basenames of files in that directory.}
#' @noRd
analyze_r_repository_code <- function(code_details, project_base_path) {
  all_elements <- list()
  all_relative_file_paths <- names(code_details)
  
  for (relative_file_path in all_relative_file_paths) {
    code_content <- code_details[[relative_file_path]]
    if (is.null(code_content) || !nzchar(code_content)) next # Use nzchar for empty string check
    
    # Consider only .R or .r files for R-specific element extraction
    if (endsWith(tolower(relative_file_path), ".r")) {
      file_elements_result <- extract_r_code_elements(relative_file_path, code_content)
      if (length(file_elements_result$functions) > 0) {
        all_elements <- c(all_elements, file_elements_result$functions)
      }
    }
  }
  
  r_file_relative_paths <- all_relative_file_paths[endsWith(tolower(all_relative_file_paths), ".r")]
  call_graph_result <- generate_r_call_graph(r_file_relative_paths, project_base_path)
  
  directory_contents <- list()
  all_relative_file_paths_for_dirs <- names(code_details)

  for (relative_file_path in all_relative_file_paths_for_dirs) {
    dir_name <- dirname(relative_file_path)

    if (dir_name == ".") {
      dir_key <- "."
    } else {
      dir_key <- gsub("\\\\", "/", dir_name)
      if (!endsWith(dir_key, "/")) {
        dir_key <- paste0(dir_key, "/")
      }
    }

    base_name <- basename(relative_file_path)

    if (is.null(directory_contents[[dir_key]])) {
      directory_contents[[dir_key]] <- list()
    }
    # Append basename to the list for this directory key
    directory_contents[[dir_key]][[length(directory_contents[[dir_key]]) + 1]] <- base_name
  }

  for (dir_key in names(directory_contents)) {
    directory_contents[[dir_key]] <- unique(unlist(directory_contents[[dir_key]]))
  }

  return(list(
    extracted_elements = all_elements,
    call_graph = call_graph_result,
    directory_contents = directory_contents
  ))
}
