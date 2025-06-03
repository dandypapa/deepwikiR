# /home/ubuntu/autodoc_tool/R/quarto_renderer.R

#' @importFrom quarto quarto_render
NULL

#' Generate Mermaid Syntax for Call Graph
#'
#' @title Create Mermaid Code for Call Graph Visualization
#' @description Converts call graph data (nodes and edges) into Mermaid graph syntax (TD - top-down).
#' It sanitizes node names to be Mermaid-friendly.
#'
#' @param call_graph_data A list containing:
#'   \item{nodes}{A character vector of unique function identifiers (node names).}
#'   \item{edges}{A list of edges, where each edge is a list with `caller` and `callee` identifiers.}
#'
#' @return Character string. The Mermaid syntax for the call graph. Returns a
#'         simple "No data" graph if input is NULL or has no nodes.
#'
#' @details
#' Node sanitization involves:
#' - Replacing "::" with "__".
#' - Replacing other non-alphanumeric characters (excluding "_") with "_".
#' - Prefixing with "n" if the ID starts with a number.
#'
#' @examples
#' \dontrun{
#' graph_data <- list(
#'   nodes = c("fileA.R::func1", "fileB.R::func2", "external_lib::fun3"),
#'   edges = list(list(caller = "fileA.R::func1", callee = "fileB.R::func2"))
#' )
#' mermaid_code <- generate_mermaid_call_graph(graph_data)
#' cat(mermaid_code)
#' }
#' @noRd
generate_mermaid_call_graph <- function(call_graph_data) {
  if (is.null(call_graph_data) || length(call_graph_data$nodes) == 0) {
    return("graph TD;\n  A[No call graph data available];")
  }

  mermaid_string <- "graph TD;\n"
  
  sanitize_mermaid_id <- function(id) {
    id <- gsub("::", "__", id, fixed = TRUE) # Use fixed = TRUE for literal "::"
    id <- gsub("[^a-zA-Z0-9_]", "_", id)
    if (grepl("^[0-9]", id)) {
        id <- paste0("n",id)
    }
    return(id)
  }
  
  node_map <- list()
  for (node_name in unique(call_graph_data$nodes)) {
    sanitized_id <- sanitize_mermaid_id(node_name)
    node_map[[node_name]] <- sanitized_id
    mermaid_string <- paste0(mermaid_string, "  ", sanitized_id, "[\"", node_name, "\"];\n")
  }
  
  if (length(call_graph_data$edges) > 0) {
    for (edge in call_graph_data$edges) {
      caller_orig <- edge$caller
      callee_orig <- edge$callee
      
      caller_id <- node_map[[caller_orig]]
      callee_id <- node_map[[callee_orig]]
      
      if (!is.null(caller_id) && !is.null(callee_id)) {
        mermaid_string <- paste0(mermaid_string, "  ", caller_id, " --> ", callee_id, ";\n")
      } else {
        cat(paste("Warning: Missing node ID for caller '", caller_orig, "' or callee '", callee_orig, "'\n"))
      }
    }
  }
  
  return(mermaid_string)
}

#' Render Quarto Document
#'
#' @title Generate and Render Quarto Documentation
#' @description Constructs a Quarto (`.qmd`) document from provided content sections
#' (directory summaries, call graph, code element documentation) and then renders
#' it to the specified output format (e.g., HTML, PDF).
#'
#' @param qmd_content_list A list of character strings or list-like items. Each item represents a
#'        section of documentation for a code element (e.g., function). If an item is a list,
#'        it's expected to have a `content_block` element containing the QMD content.
#' @param output_dir Character string. The directory where the `.qmd` file and the
#'        rendered output will be saved.
#' @param output_filename_base Character string. The base name for the output files
#'        (e.g., "my_project_docs", which will result in "my_project_docs.qmd").
#' @param quarto_format Character string. The target output format for Quarto
#'        (e.g., "html", "pdf"). Default is "html".
#' @param project_name Character string. The name of the project, used in the document title.
#'        Default is "Project".
#' @param call_graph_data A list or NULL. Data for generating a Mermaid call graph,
#'        as returned by `generate_r_call_graph`. If `NULL` or empty, the call graph
#'        section is omitted.
#' @param directory_summaries A named list or NULL. Where names are directory paths
#'        and values are LLM-generated summary strings for each directory. If `NULL`
#'        or empty, the directory overview section is omitted.
#' @param verbose Logical. If `TRUE`, prints status messages during processing.
#'        Default is `TRUE`.
#'
#' @return Character string. The absolute path to the rendered output file
#'         (e.g., ".../my_project_docs.html"). Stops with an error if Quarto
#'         rendering fails.
#'
#' @details
#' The function performs these steps:
#' 1. Ensures the `output_dir` exists.
#' 2. Prepares a Quarto YAML header (title, format, TOC settings).
#' 3. Sequentially adds sections to the QMD content:
#'    - Directory Overview (if `directory_summaries` provided).
#'    - Function Call Graph (if `call_graph_data` provided).
#'    - Detailed Code Element Documentation (from `qmd_content_list`).
#' 4. Writes the complete content to a `.qmd` file.
#' 5. Calls `quarto::quarto_render()` to render the `.qmd` file.
#' 6. Checks for the existence of the rendered output file and returns its path.
#'
#' @examples
#' \dontrun{
#' # Assume qmd_sections, out_dir, etc., are defined
#' # render_quarto_document(
#' #   qmd_content_list = list("### Func A\nDetails..."),
#' #   output_dir = "output_docs",
#' #   output_filename_base = "MyLib",
#' #   project_name = "My Awesome Library",
#' #   call_graph_data = list(nodes=c("A"),edges=list()),
#' #   directory_summaries = list("./ (Root)" = "This is the main directory.")
#' # )
#' }
#' @importFrom quarto quarto_render
#' @noRd
render_quarto_document <- function(
  qmd_content_list,
  output_dir,
  output_filename_base,
  quarto_format = "html",
  project_name = "Project",
  call_graph_data = NULL,
  directory_summaries = NULL,
  verbose = TRUE
) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  qmd_file_name <- paste0(output_filename_base, ".qmd")
  qmd_file_path <- file.path(output_dir, qmd_file_name)
  
  doc_title <- if (!is.null(project_name) && nzchar(project_name)) {
    paste("Code Documentation for Project:", project_name)
  } else {
    "Generated Code Documentation"
  }
  
  yaml_header <- paste0(
    "---",
    "\ntitle: \"", doc_title, "\"",
    "\nformat: ", quarto_format,
    "\neditor: visual",
    "\ntoc: true",
    "\ntoc-depth: 3",
    "\n---",
    "\n\n"
  )
  
  full_qmd_content_lines <- list(yaml_header)

  if (!is.null(directory_summaries) && length(directory_summaries) > 0) {
    dir_summary_qmd <- list("\n## Directory Overview\n")
    sorted_dir_names <- sort(names(directory_summaries))

    for (dir_path in sorted_dir_names) {
      summary_text <- directory_summaries[[dir_path]]
      display_dir_path <- if (dir_path == ".") "./ (Project Root)" else dir_path

      dir_summary_qmd[[length(dir_summary_qmd) + 1]] <- paste0(
        "\n### Directory: `", display_dir_path, "`\n\n",
        summary_text, "\n"
      )
    }
    dir_summary_qmd[[length(dir_summary_qmd) + 1]] <- "\n---\n"
    full_qmd_content_lines <- c(full_qmd_content_lines, dir_summary_qmd)
  }
  
  if (!is.null(call_graph_data) && length(call_graph_data$nodes) > 0) {
    mermaid_syntax <- generate_mermaid_call_graph(call_graph_data) # Internal
    mermaid_section <- list(
      "\n## Function Call Graph\n\n",
      "```mermaid\n",
      mermaid_syntax,
      "\n```\n\n",
      "---\n"
    )
    full_qmd_content_lines <- c(full_qmd_content_lines, mermaid_section)
  }
  
  if (length(qmd_content_list) > 0) {
    element_docs_qmd <- list("\n## Detailed Code Element Documentation\n")
    for (item in qmd_content_list) {
      item_content <- ""
      if(is.list(item) && !is.null(item$content_block)) {
        item_content <- item$content_block
      } else if (is.character(item)) {
        item_content <- item
      }

      if (nzchar(item_content)) {
        element_docs_qmd[[length(element_docs_qmd) + 1]] <- paste0("\n", item_content, "\n\n---\n")
      }
    }
    full_qmd_content_lines <- c(full_qmd_content_lines, element_docs_qmd)
  }
  
  final_qmd_content_str <- paste(unlist(full_qmd_content_lines), collapse = "")
  tryCatch({
    writeLines(final_qmd_content_str, qmd_file_path)
    if (verbose) cat(paste("Quarto input file written to:", qmd_file_path, "\n"))
  }, error = function(e) {
    stop(paste("Error writing .qmd file:", e$message))
  })
  
  output_file_full_path <- ""
  tryCatch({
    quarto::quarto_render(qmd_file_path, output_format = quarto_format, quiet = !verbose) # Use !verbose for quiet
    
    rendered_output_filename <- paste0(output_filename_base, ".", quarto_format)
    output_file_full_path <- file.path(output_dir, rendered_output_filename)
    
    if (file.exists(output_file_full_path)) {
      if(verbose) cat(paste("Successfully rendered Quarto document to:", output_file_full_path, "\n")) # Check verbose
      return(output_file_full_path)
    } else {
      actual_files <- list.files(output_dir)
      if(verbose) cat("Quarto rendering finished, but expected output file not found. Files in output dir:", paste(actual_files, collapse=", "), "\n")
      potential_matches <- actual_files[startsWith(actual_files, output_filename_base) & endsWith(actual_files, paste0(".", quarto_format))]
      if(length(potential_matches) == 1){
        output_file_full_path <- file.path(output_dir, potential_matches[1])
        if(verbose) cat(paste("Found rendered document at:", output_file_full_path, "\n"))
        return(output_file_full_path)
      } else {
         stop(paste("Quarto rendering did not produce the expected output file:", rendered_output_filename))
      }
    }
  }, error = function(e) {
    if(verbose) cat(paste("Error during Quarto rendering:", e$message, "\n"))
    quarto_log_file <- sub("\\.qmd$", "_quarto.log", qmd_file_path, fixed = TRUE)
    if(file.exists(quarto_log_file)){
        if(verbose) cat("--- Quarto Log ---\n")
        if(verbose) cat(readLines(quarto_log_file), sep="\n")
        if(verbose) cat("--- End Quarto Log ---\n")
    }
    stop(paste("Error during Quarto rendering:", e$message))
  })
}
