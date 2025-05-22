# /home/ubuntu/autodoc_tool/R/deepwikiR.R

#' Generate Documentation for a Single Project
#'
#' @title Generate Single Project Documentation
#' @description Analyzes a specified R code project and generates its documentation.
#' This function processes a single project configuration, handling code acquisition,
#' analysis (function extraction, call graph, directory structure), LLM-based
#' documentation generation for code elements and directories, and finally,
#' rendering the documentation using Quarto. It also supports exporting data
#' for fine-tuning LLMs.
#'
#' @param project_config A list. A fully resolved and validated configuration object for a single project.
#'        This object is typically one of the elements from the `projects` list returned by
#'        `load_and_validate_config`. It should contain details like `project_name`,
#'        `code_source`, `output_dir`, `llm_settings`, etc.
#' @param llm_api_key Character string or NULL. An optional LLM API key to override the
#'        one specified in environment variables for this project's LLM calls. Default is `NULL`.
#' @param verbose Logical. If `TRUE`, detailed status messages will be printed during
#'        the documentation generation process. Default is `TRUE`.
#'
#' @return Character string or NULL. The absolute path to the main rendered documentation
#'         file for the project (e.g., an HTML or PDF file). Returns `NULL` if
#'         documentation generation fails or is skipped (e.g., no code files found).
#'
#' @details
#' The function performs the following steps:
#' 1. Sets an LLM API key if provided via `llm_api_key`, overriding environment variables for this call.
#' 2. Acquires code content using `acquire_code_content()`.
#' 3. Analyzes the code structure (elements, call graph, directory contents) using `analyze_r_repository_code()`.
#' 4. Generates documentation for individual code elements using `generate_docs_with_llm()`.
#' 5. Generates summaries for directories using `generate_directory_summaries_with_llm()`.
#' 6. Exports data for fine-tuning if configured, using `export_for_finetuning()`.
#' 7. Renders the final documentation using `render_quarto_document()`.
#' 8. Saves all analysis data (elements, graph, summaries, etc.) to an RDS file.
#'
#' @examples
#' \dontrun{
#' # This function is typically called by run_cli. For direct use:
#' sample_project_conf <- list(
#'   project_name = "MyRProject",
#'   code_source = list(local_path = "./path/to/my_r_project_code", language_hints = list("R")),
#'   output_dir = "output/MyRProjectDocs",
#'   output_filename_base = "my_r_project_documentation",
#'   quarto_format = "html",
#'   llm_settings = list(
#'     provider = "openai", model = "gpt-3.5-turbo",
#'     api_details = list(api_key_env_var = "OPENAI_API_KEY")
#'   ),
#'   export_finetuning_data = list(enabled = FALSE)
#' )
#' # Ensure the necessary helper functions like acquire_code_content are available.
#' # generate_repo_docs(sample_project_conf, verbose = TRUE)
#' }
#'
#' @export
#' @importFrom tools file_path_sans_ext
generate_repo_docs <- function(project_config, llm_api_key = NULL, verbose = TRUE) {
  
  if (verbose) cat(paste0("--- Starting documentation generation for project: ", project_config$project_name, " ---\n"))

  if (!is.null(llm_api_key) && 
      !is.null(project_config$llm_settings) &&
      !is.null(project_config$llm_settings$api_details$api_key_env_var) &&
      project_config$llm_settings$api_details$api_key_env_var != "") {
    
    api_key_env_var_name <- project_config$llm_settings$api_details$api_key_env_var
    orig_api_key_value <- Sys.getenv(api_key_env_var_name, unset = NA) 
    
    env_vars_to_set <- list()
    env_vars_to_set[[api_key_env_var_name]] <- llm_api_key
    do.call(Sys.setenv, env_vars_to_set)
    
    on.exit({
      if (verbose) cat(paste("Restoring original API key for env var:", api_key_env_var_name, "\n"))
      env_vars_to_restore <- list()
      if (is.na(orig_api_key_value)) { 
        Sys.unsetenv(api_key_env_var_name)
      } else {
        env_vars_to_restore[[api_key_env_var_name]] <- orig_api_key_value
        do.call(Sys.setenv, env_vars_to_restore)
      }
    }, add = TRUE) 
    
    if (verbose) cat(paste("CLI API key override applied for env var:", api_key_env_var_name, "\n"))
  }
  
  if (verbose) cat("Acquiring code content...\n")
  code_details <- acquire_code_content(project_config$code_source) # Internal, not exported
  
  if (length(code_details) == 0) {
    warning(paste("No code files found for project:", project_config$project_name, "- skipping documentation generation for this project."))
    return(NULL) 
  }
  
  if (verbose) cat(paste("Found", length(code_details), "files for project", project_config$project_name, "\n"))
  
  project_base_path <- NULL
  if (!is.null(project_config$code_source$local_path)) {
    project_base_path <- normalizePath(project_config$code_source$local_path, mustWork = FALSE)
  } else if (!is.null(project_config$code_source$git_repo)) {
    project_base_path <- getwd() 
    warning("Git repo path resolution might need refinement in generate_repo_docs.")
  } else {
     stop(paste("No local_path or git_repo specified in code_source for project:", project_config$project_name))
  }
  
  if (verbose) cat("Analyzing code structure...\n")
  analysis_result <- analyze_r_repository_code(code_details, project_base_path) # Internal
  extracted_elements <- analysis_result$extracted_elements
  call_graph_data <- analysis_result$call_graph
  directory_contents <- analysis_result$directory_contents 
  
  if (verbose) {
    cat(paste("Found", length(extracted_elements), "code elements.\n"))
    cat(paste("Call graph contains", length(call_graph_data$nodes), "nodes and", 
              length(call_graph_data$edges), "edges.\n"))
    if (!is.null(directory_contents)) {
        cat(paste("Found directory content information for", length(directory_contents), "directories.\n"))
    }
  }
  
  llm_generated_docs <- list() 
  if (length(extracted_elements) > 0) {
    if (!is.null(project_config$llm_settings) && !is.null(project_config$llm_settings$provider)) {
      if (verbose) cat("Generating documentation for individual code elements using LLM...\n")
      llm_generated_docs <- generate_docs_with_llm( # Internal (from llm_interactor.R)
        extracted_elements, 
        project_config$llm_settings, 
        project_config$code_source$language_hints
      )
    } else {
      if (verbose) cat("LLM settings or provider not specified. Skipping LLM documentation generation for elements.\n")
      llm_generated_docs <- lapply(extracted_elements, function(el) {
        el$description <- "(LLM documentation generation for element skipped due to missing configuration)"
        return(el)
      })
    }
  }

  directory_summaries <- NULL 
  if (!is.null(directory_contents) && length(directory_contents) > 0) {
    if (!is.null(project_config$llm_settings) && !is.null(project_config$llm_settings$provider)) {
      if (verbose) cat("Generating directory summaries with LLM...\n")
      directory_summaries <- generate_directory_summaries_with_llm( # Internal (from llm_interactor.R)
        directory_contents = directory_contents,
        code_details = code_details, 
        llm_generated_docs = llm_generated_docs, 
        llm_config = project_config$llm_settings, 
        language_hints = project_config$code_source$language_hints,
        project_name = project_config$project_name,
        verbose = verbose
      )
      if (verbose) cat(paste("Generated", length(directory_summaries), "directory summaries.\n"))
    } else {
      if (verbose) cat("LLM settings or provider not specified. Skipping directory summary generation.\n")
      directory_summaries <- lapply(names(directory_contents), function(dir_name) {
        "(LLM summary generation for directory skipped due to missing configuration)"
      })
      names(directory_summaries) <- names(directory_contents)
    }
  } else {
    if (verbose) cat("Skipping directory summary generation as no directory contents were found.\n")
  }

  export_settings <- project_config$export_finetuning_data
  if (!is.null(export_settings) && is.list(export_settings) && isTRUE(export_settings$enabled)) {
    if (length(llm_generated_docs) > 0) {
      output_filepath <- file.path(project_config$output_dir, export_settings$output_filename)
      if (verbose) {
        cat(paste("Exporting data for fine-tuning to:", output_filepath, 
                  "in format:", export_settings$format, "\n"))
      }
      
      tryCatch({
        if (!exists("export_for_finetuning", mode = "function")) {
            utils_path <- "R/export_utils.R" 
            if (file.exists(utils_path)) {
                if (verbose) cat(paste("Sourcing export_utils.R from:", utils_path, "\n"))
                source(utils_path, local = TRUE) 
            } else {
                warning("R/export_utils.R not found. Cannot export fine-tuning data.")
            }
        }

        if (exists("export_for_finetuning", mode = "function")) {
            export_for_finetuning( # Assumed to be exported from export_utils.R
              documented_elements = llm_generated_docs,
              output_filepath = output_filepath,
              format = export_settings$format,
              project_name = project_config$project_name,
              verbose = verbose
            )
        } else {
            warning("export_for_finetuning function not found after attempting to source. Skipping export.")
        }
      }, error = function(e) {
        warning(paste("Failed to export fine-tuning data for project", project_config$project_name, ":", e$message))
      })
    } else {
      if (verbose) cat("No documented elements available to export for fine-tuning.\n")
    }
  } else {
    if (verbose) cat("Fine-tuning data export is disabled for this project.\n")
  }
  
  if (verbose) cat("Preparing Quarto content and rendering...\n")
  
  qmd_document_sections <- list()
  if (length(llm_generated_docs) > 0) { 
    for (doc_item in llm_generated_docs) {
      desc_text <- doc_item$description %||% "(No description generated)"
      
      section_content <- paste0(
        "### `", doc_item$name, "`\n\n",
        "**Type:** ", doc_item$type, "\n\n",
        "**Signature:** `", doc_item$name, doc_item$signature, "`\n\n",
        "**File:** `", doc_item$file_path, "`\n\n", 
        "**Description:**\n", desc_text, "\n\n",
        "**Code:**\n",
        "```", tolower(project_config$code_source$language_hints[[1]] %||% "code"), "\n",
        paste(doc_item$code_block, collapse = "\n"),
        "\n```\n"
      )
      qmd_document_sections[[length(qmd_document_sections) + 1]] <- section_content
    }
  } else {
      if (verbose) cat("No documentable elements found or generated. Output document may be empty.\n")
  }
  
  output_file_path <- render_quarto_document( # Internal (from quarto_renderer.R)
    qmd_content_list = qmd_document_sections,
    output_dir = project_config$output_dir,
    output_filename_base = project_config$output_filename_base,
    quarto_format = project_config$quarto_format,
    project_name = project_config$project_name, 
    call_graph_data = call_graph_data,
    directory_summaries = directory_summaries,
    verbose = verbose
  )
  
  if (verbose) cat("Saving analysis data (RDS)...\n")
  
  analysis_output_data <- list(
    project_name = project_config$project_name,
    language_hints = project_config$code_source$language_hints,
    documented_elements = llm_generated_docs, 
    call_graph = call_graph_data,
    directory_contents = directory_contents, 
    directory_summaries = directory_summaries, 
    timestamp = Sys.time()
  )
  
  base_name_for_rds <- project_config$output_filename_base
  if (is.null(base_name_for_rds) || base_name_for_rds == "") {
    base_name_for_rds <- project_config$project_name 
  }
  
  rds_filename <- paste0(tools::file_path_sans_ext(base_name_for_rds), "_analysis_data.rds")
  rds_filepath <- file.path(project_config$output_dir, rds_filename)
  
  tryCatch({
    if (!dir.exists(project_config$output_dir)) {
      dir.create(project_config$output_dir, recursive = TRUE, showWarnings = FALSE)
      if (verbose) cat(paste("Created output directory for RDS:", project_config$output_dir, "\n"))
    }
    saveRDS(analysis_output_data, file = rds_filepath)
    if (verbose) cat(paste("Analysis data successfully saved to:", rds_filepath, "\n"))
  }, error = function(e) {
    warning(paste("Failed to save analysis data for project", project_config$project_name, "to", rds_filepath, ":", e$message))
  })

  if (verbose) {
    cat(paste("Documentation for project", project_config$project_name, "rendered to:", output_file_path, "\n"))
    cat(paste("Cumulative tokens consumed so far:", get_total_tokens_consumed(), "\n")) # Internal (llm_interactor.R)
  }
  
  if (verbose) cat(paste0("--- Finished documentation generation for project: ", project_config$project_name, " ---\n"))
  return(output_file_path)
}


#' Calculate Relative Path
#'
#' @title Calculate Relative File Path
#' @description Computes a relative file path from a base directory (`from_dir`)
#' to a target path (`to_path`). This is a simplified implementation.
#'
#' @param from_dir Character string. The absolute path of the base directory.
#' @param to_path Character string. The absolute path of the target file or directory.
#'
#' @return Character string. The relative path from `from_dir` to `to_path`.
#'         If `to_path` is not directly under `from_dir` or if a simple relative
#'         path cannot be determined (e.g., different drives, complex traversals),
#'         it might return the absolute `to_path` or a partially relativized path
#'         and issue a warning.
#'
#' @details The function normalizes paths and primarily handles cases where `to_path`
#'          is a subdirectory of `from_dir`. It also attempts to find common
#'          path prefixes to create relative paths like "subdir/file.html".
#'          It does not currently construct complex relative paths like `../../file.html`.
#'          For more robust relative path calculations, consider using `fs::path_rel`.
#'
#' @examples
#' \dontrun{
#' from <- "/abs/path/docs"
#' to <- "/abs/path/docs/projectA/index.html"
#' calculate_relative_path(from, to) # Should be "projectA/index.html"
#'
#' from_complex <- "/abs/path/output"
#' to_complex <- "/abs/another/path/file.txt"
#' calculate_relative_path(from_complex, to_complex) # Might warn and return absolute path
#' }
#' @noRd
calculate_relative_path <- function(from_dir, to_path) {
  from_dir <- normalizePath(from_dir, mustWork = FALSE)
  to_path <- normalizePath(to_path, mustWork = FALSE)

  if (startsWith(to_path, paste0(from_dir, .Platform$file.sep))) {
    return(substring(to_path, nchar(from_dir) + 2)) 
  } else {
    warning(paste("Cannot easily calculate relative path from", from_dir, "to", to_path, 
                  ". Using basename or absolute path. Consider 'fs' package for robust relative paths."))
    from_parts <- unlist(strsplit(from_dir, .Platform$file.sep))
    to_parts <- unlist(strsplit(to_path, .Platform$file.sep))
    
    common_len <- 0
    for (k in 1:min(length(from_parts), length(to_parts))) {
      if (from_parts[k] == to_parts[k]) {
        common_len <- common_len + 1
      } else {
        break
      }
    }
    
    if (common_len > 0) {
      to_rel_parts <- to_parts[(common_len + 1):length(to_parts)]
      if (length(to_rel_parts) > 0) {
        return(paste(to_rel_parts, collapse = .Platform$file.sep))
      }
    }
    return(to_path) 
  }
}


#' Generate Master Index Page for Multiple Projects
#'
#' @title Create a Hub Page for Multiple Project Documentations
#' @description Generates a main `index.qmd` (and renders it to HTML) that serves as a
#' hub, linking to the documentation pages of multiple individual projects.
#'
#' @param generated_docs_summary A list of summaries for each successfully generated
#'        project document. Each element in the list should be a sub-list with:
#'        \itemize{
#'          \item `name`: Character string, the name of the project.
#'          \item `path`: Character string, the absolute path to the main rendered
#'                        documentation file of the project (e.g., ".../projectA_docs.html").
#'          \item `output_dir`: Character string, the absolute path to the output
#'                              directory of the project's documentation.
#'        }
#' @param global_output_dir_root Character string. The absolute path to the root
#'        directory where the master `index.qmd` and its rendered HTML file
#'        should be saved.
#' @param verbose Logical. If `TRUE`, prints status messages during generation.
#'        Default is `TRUE`.
#'
#' @return Invisibly returns the character string path to the generated master
#'         `index.qmd` file. Returns `NULL` if no projects are provided or if
#'         `global_output_dir_root` is not specified.
#'
#' @details The function creates an `index.qmd` file with a title and a list of
#'          links. It attempts to make these links relative to `global_output_dir_root`.
#'          If `quarto` package is available, it also renders this QMD file to HTML.
#'
#' @examples
#' \dontrun{
#' summaries <- list(
#'   list(name = "Project Alpha", path = "/docs/proj_alpha/alpha.html", output_dir="/docs/proj_alpha"),
#'   list(name = "Project Beta", path = "/docs/proj_beta/beta.html", output_dir="/docs/proj_beta")
#' )
#' generate_master_index_page(summaries, "/docs/hub", verbose = TRUE)
#' # This would create /docs/hub/index.qmd and /docs/hub/index.html
#' }
#' @importFrom quarto quarto_render
#' @noRd 
generate_master_index_page <- function(generated_docs_summary, global_output_dir_root, verbose = TRUE) {
  if (length(generated_docs_summary) < 1 || is.null(global_output_dir_root) || global_output_dir_root == "") {
    if (verbose) cat("Not enough projects or no global output directory root specified. Skipping master index page generation.\n")
    return(invisible(NULL))
  }

  if (!dir.exists(global_output_dir_root)) {
    if (verbose) cat(paste("Creating global output directory for master index:", global_output_dir_root, "\n"))
    dir.create(global_output_dir_root, recursive = TRUE, showWarnings = FALSE)
  }

  index_qmd_path <- file.path(global_output_dir_root, "index.qmd")
  index_content <- c(
    "---",
    "title: \"Project Documentation Hub\"",
    "format: html", 
    "---",
    "",
    "## Generated Project Documentations",
    "",
    "Below is a list of all generated project documentations:",
    ""
  )

  list_items <- c()
  for (doc_summary in generated_docs_summary) {
    relative_doc_path <- calculate_relative_path(global_output_dir_root, doc_summary$path) # Internal
    
    is_sub_path <- startsWith(normalizePath(doc_summary$output_dir, mustWork=FALSE), 
                              normalizePath(global_output_dir_root, mustWork=FALSE))

    if (!is_sub_path && grepl("^(/|[A-Za-z]:)", relative_doc_path)) { 
        list_items <- c(list_items, paste0("* ", doc_summary$name, " (Documentation at: `", doc_summary$path, "`) - External path, cannot make relative link easily."))
    } else {
        list_items <- c(list_items, paste0("* [", doc_summary$name, "](", relative_doc_path, ")"))
    }
  }
  
  if (length(list_items) > 0) {
    index_content <- c(index_content, list_items)
  } else {
    index_content <- c(index_content, "No projects were processed or linked.")
  }

  tryCatch({
    writeLines(index_content, index_qmd_path)
    if (verbose) cat(paste("Master index QMD file saved to:", index_qmd_path, "\n"))

    if (requireNamespace("quarto", quietly = TRUE)) {
      quarto::quarto_render(input = index_qmd_path, output_format = "html", quiet = !verbose)
      rendered_index_file <- sub("\\.qmd$", ".html", index_qmd_path) 
      if (verbose) cat(paste("Master index page rendered to:", rendered_index_file, "\n"))
    } else {
      warning("Quarto package not available. Master index page QMD created but not rendered.")
    }
  }, error = function(e) {
    warning(paste("Failed to create or render master index page:", e$message))
    return(invisible(NULL))
  })
  
  invisible(index_qmd_path)
}


#' Interact with an Analyzed Code Repository
#'
#' @title Interactive Q&A with Code Repository
#' @description Launches an interactive command-line session to allow users to ask
#' questions about a previously analyzed code repository. The analysis data
#' (including documented elements) must have been saved to an `.rds` file.
#'
#' @param analysis_data_path Character string. The path to the `_analysis_data.rds`
#'        file generated by a previous run of `generate_repo_docs`.
#' @param config_path Character string. The path to the main `config.json` file
#'        that was used for the initial analysis. This is needed to load LLM settings.
#' @param verbose Logical. If `TRUE`, prints detailed messages during the launch
#'        and execution of the interactive script. Default is `TRUE`.
#'
#' @return Invisibly returns the status code from the system command that executed
#'         the interactive script. Typically, `0` for success. The function primarily
#'         operates via side effects (launching an interactive session).
#'
#' @details This function constructs a system command to execute the `R/interactive_mode.R`
#'          script using `Rscript`. It passes the paths to the analysis data and
#'          configuration file as command-line arguments to the script.
#'          The `interactive_mode.R` script then handles the Q&A session.
#'
#' @examples
#' \dontrun{
#' # Assuming analysis data and config exist:
#' # rds_file <- "output/MyProjectDocs/my_project_docs_analysis_data.rds"
#' # config_file <- "inst/config/sample_config.json" # Or your project's config
#' # if(file.exists(rds_file) && file.exists(config_file)) {
#' #   chat_with_repo(
#' #     analysis_data_path = rds_file,
#' #     config_path = config_file
#' #   )
#' # } else {
#' #  cat("Required RDS or config file not found for chat_with_repo example.\n")
#' # }
#' }
#' @export
chat_with_repo <- function(analysis_data_path, config_path, verbose = TRUE) {
  if (!file.exists(analysis_data_path)) {
    stop(paste("Analysis data file not found:", analysis_data_path))
  }
  if (!file.exists(config_path)) {
    stop(paste("Configuration file not found:", config_path))
  }

  rscript_path <- file.path(R.home("bin"), "Rscript")
  interactive_script_path <- "R/interactive_mode.R" # Assumes running from project root

  if (!file.exists(interactive_script_path)) {
    # Attempt to locate via system.file if run as part of an installed package
    pkg_script_path <- system.file("R", "interactive_mode.R", package = "deepwikiR")
    if (nzchar(pkg_script_path) && file.exists(pkg_script_path)) {
        interactive_script_path <- pkg_script_path
    } else {
        stop(paste("interactive_mode.R script not found at:", interactive_script_path,
                   "and not found in deepwikiR package structure via system.file().",
                   "Ensure it is in the R/ directory of your project or package structure."))
    }
  }

  cmd_args <- c(
    shQuote(interactive_script_path),
    "--data_file", shQuote(normalizePath(analysis_data_path)),
    "--config_file", shQuote(normalizePath(config_path))
  )
  
  full_cmd <- paste(shQuote(rscript_path), paste(cmd_args, collapse = " "))

  if (verbose) {
    cat("Launching interactive Q&A mode for your repository...\n")
    cat("Command to be executed:\n", full_cmd, "\n\n")
  }

  status <- system(full_cmd, intern = FALSE, wait = TRUE) 

  if (status != 0) {
    warning(paste("Interactive mode script execution failed with status:", status))
  } else {
    if (verbose) cat("Interactive session ended.\n")
  }
  
  invisible(status) 
}


#' Run DeepwikiR Command-Line Interface (CLI)
#'
#' @title DeepwikiR Command-Line Interface
#' @description Provides a CLI for `deepwikiR` to generate documentation or
#' interact with an analyzed repository. This function parses command-line
#' arguments and dispatches to the appropriate internal functions.
#'
#' @param args Character vector. Command-line arguments, typically obtained from
#'        `commandArgs(trailingOnly = TRUE)`.
#'
#' @return Invisibly returns the result of the dispatched operation. For 'generate'
#'         mode, this is a list of paths to the generated documents. For 'chat' mode,
#'         it's the status of the system call. Returns `NULL` if argument parsing
#'         fails or if a mode doesn't produce a specific path output.
#'
#' @details
#' The CLI supports two main modes:
#' - **`generate`**: Analyzes code and generates documentation.
#'   Requires `--config` (path to JSON configuration file).
#'   Optional: `--api-key` (override LLM API key).
#' - **`chat`**: Starts an interactive Q&A session.
#'   Requires `--data_file` (path to `_analysis_data.rds`) and
#'   `--config_file` (path to JSON configuration file).
#'
#' A global `--quiet` flag can be used to suppress verbose logging for all modes.
#'
#' @examples
#' \dontrun{
#' # To run from command line (assuming this function is part of an executable script):
#' # Rscript path/to/deepwikiR_script.R generate --config myconfig.json
#' # Rscript path/to/deepwikiR_script.R chat --data_file analysis.rds --config myconfig.json
#'
#' # To call from R (less common for CLI runners):
#' # run_cli(c("generate", "--config", "inst/config/sample_config.json", "--quiet"))
#' }
#' @importFrom argparse ArgumentParser
#' @noRd # Typically not called directly by users as a package function
run_cli <- function(args = commandArgs(trailingOnly = TRUE)) {
  parser <- argparse::ArgumentParser(
    description = "deepwikiR - Automate code repository documentation and interaction."
  )
  parser$add_argument(
    "--quiet", 
    action = "store_true", 
    default = FALSE, 
    help = "Global quiet mode (suppresses verbose logging)."
  )
  
  subparsers <- parser$add_subparsers(
    title = "mode",
    dest = "mode",
    help = "Operation mode: 'generate' documentation or 'chat' with repository."
  )

  generate_parser <- subparsers$add_parser(
    "generate", 
    help = "Analyze code repository and generate documentation."
  )
  generate_parser$add_argument(
    "--config", 
    type = "character", 
    required = TRUE, 
    help = "Path to the JSON configuration file."
  )
  generate_parser$add_argument( # --output-dir is effectively handled by global_output_dir_root in config now
    "--api-key", 
    type = "character", 
    default = NULL, 
    help = "LLM API key (optional, overrides environment variables)."
  )

  chat_parser <- subparsers$add_parser(
    "chat", 
    help = "Interactively query a previously analyzed code repository."
  )
  chat_parser$add_argument(
    "--data_file", 
    type = "character", 
    required = TRUE, 
    help = "Path to the _analysis_data.rds file."
  )
  chat_parser$add_argument(
    "--config_file", 
    type = "character", 
    required = TRUE, 
    help = "Path to the main config.json file (for LLM settings)."
  )
  
  parsed_args <- tryCatch({
    parser$parse_args(args)
  }, error = function(e) {
    message("Error parsing arguments: ", e$message)
    parser$print_help()
    return(invisible(NULL))
  })
  
  if (is.null(parsed_args) || is.null(parsed_args$mode)) {
      if(length(args) > 0 && !any(c("generate", "chat") %in% args)) {
          message("No valid mode ('generate' or 'chat') specified or invalid arguments.")
      }
      parser$print_help()
      return(invisible(NULL))
  }

  current_verbose_mode <- !parsed_args$quiet
  
  if (parsed_args$mode == "generate") {
    if (is.null(parsed_args$config)) { # Should be caught by 'required=TRUE' but good practice
        message("Error: --config is required for 'generate' mode.")
        generate_parser$print_help()
        return(invisible(NULL))
    }
    
    if (current_verbose_mode) cat("Loading and validating multi-project configuration...\n")
    
    config_data <- tryCatch({
        load_and_validate_config(parsed_args$config) # Internal
    }, error = function(e) {
        message(paste("Error loading or validating configuration:", e$message))
        return(NULL)
    })

    if (is.null(config_data)) {
        return(invisible(NULL)) 
    }

    global_settings <- config_data$global_settings
    project_configs <- config_data$projects

    if (is.null(project_configs) || length(project_configs) == 0) {
      message("No projects found in the configuration file.")
      return(invisible(NULL))
    }

    generated_docs_summary <- list()
    total_projects <- length(project_configs)
    if (current_verbose_mode) cat(paste("Found", total_projects, "project(s) to process.\n"))

    for (i in seq_along(project_configs)) {
      single_project_conf <- project_configs[[i]]
      if (current_verbose_mode) {
        cat(paste0("\nProcessing project ", i, " of ", total_projects, ": ", single_project_conf$project_name, "\n"))
        cat(paste0(rep("-", 40), collapse=""), "\n")
      }
      
      output_file_path <- generate_repo_docs( # Exported
        project_config = single_project_conf,
        llm_api_key = parsed_args$`api-key`, 
        verbose = current_verbose_mode
      )
      
      if (!is.null(output_file_path) && file.exists(output_file_path)) {
        generated_docs_summary[[length(generated_docs_summary) + 1]] <- list(
          name = single_project_conf$project_name,
          path = normalizePath(output_file_path, mustWork = FALSE), 
          output_dir = normalizePath(single_project_conf$output_dir, mustWork = FALSE) 
        )
        if (current_verbose_mode) cat(paste("Successfully generated documentation for project:", single_project_conf$project_name, "at", output_file_path, "\n"))
      } else {
        if (current_verbose_mode) warning(paste("Documentation generation failed or returned no path for project:", single_project_conf$project_name, "\n"))
      }
    }

    if (current_verbose_mode) {
      cat(paste0("\n", rep("=", 40), collapse=""), "\n")
      cat("Summary of documentation generation:\n")
      if (length(generated_docs_summary) > 0) {
        for (summary_item in generated_docs_summary) {
          cat(paste("  - Project:", summary_item$name, "\n    Documentation:", summary_item$path, "\n"))
        }
      } else {
        cat("  No documents were generated successfully.\n")
      }
      cat(paste0(rep("=", 40), collapse=""), "\n")
    }
    
    if (length(generated_docs_summary) > 0 && 
        !is.null(global_settings$output_dir_root) && nzchar(global_settings$output_dir_root)) {
      if (current_verbose_mode) cat("\nGenerating master index page...\n")
      generate_master_index_page( # Internal
        generated_docs_summary = generated_docs_summary,
        global_output_dir_root = global_settings$output_dir_root, 
        verbose = current_verbose_mode
      )
    } else {
        if (current_verbose_mode) {
            if (length(generated_docs_summary) <=0) cat("Skipping master index: no projects successfully processed.\n")
            else if (is.null(global_settings$output_dir_root) || !nzchar(global_settings$output_dir_root)) cat("Skipping master index: global_settings$output_dir_root not set.\n")
        }
    }
    
    if (current_verbose_mode) cat("\nAll processing complete.\n")
    invisible(lapply(generated_docs_summary, function(x) x$path)) 

  } else if (parsed_args$mode == "chat") {
    if (is.null(parsed_args$data_file) || is.null(parsed_args$config_file)) {
        message("Error: --data_file and --config_file are required for 'chat' mode.")
        chat_parser$print_help()
        return(invisible(NULL))
    }
    chat_status <- chat_with_repo( # Exported
      analysis_data_path = parsed_args$data_file,
      config_path = parsed_args$config_file,
      verbose = current_verbose_mode
    )
    invisible(chat_status) 
  } else {
    message(paste("Unknown mode:", parsed_args$mode))
    parser$print_help()
    invisible(NULL)
  }
}
