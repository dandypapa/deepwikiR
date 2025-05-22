# /home/ubuntu/autodoc_tool/R/main.R

#' Main Execution Script for DeepwikiR (CLI Entry Point)
#'
#' @description
#' This script serves as the primary command-line interface (CLI) entry point for
#' the `deepwikiR` package when it's not run as an installed package directly
#' via `Rscript -e "deepwikiR::run_cli(...)"`. It sources necessary modules,
#' parses command-line arguments (currently expecting only `--config` for a single
#' project setup, which is now outdated), and orchestrates the documentation
#' generation process by calling functions from the sourced modules.
#'
#' **Note:** The primary CLI logic, including multi-project support and different modes
#' (generate, chat), is now handled by `run_cli()` in `R/deepwikiR.R`. This `main.R`
#' script is more of a legacy single-project runner or a development entry point.
#' For standard package use or multi-project features, `run_cli()` is preferred.
#'
#' @details
#' The script performs the following actions:
#' 1. Sets up the R library path if `R_LIBS_USER` is not defined.
#' 2. Sources core modules of the `deepwikiR` application.
#' 3. Parses command-line arguments. It expects a `--config` argument specifying
#'    the path to a JSON configuration file.
#' 4. Calls `load_and_validate_config` (which now expects a multi-project format,
#'    so this script might error if given an old single-project config).
#' 5. Calls `acquire_code_content`, `analyze_r_repository_code`, `generate_docs_with_llm`,
#'    and `render_quarto_document` in sequence to process the *first* project
#'    defined in the configuration if the config were multi-project.
#'
#' This script is NOT part of the installable package's exported functions.
#' It's intended for direct execution via `Rscript R/main.R --config <path_to_config>`.
#'
#' @section Command-Line Usage:
#' \preformatted{
#' Rscript R/main.R --config path/to/your_config.json
#' }
#'
#' @seealso \code{\link{run_cli}} for the recommended CLI interface.
#' @keywords internal
#' @noRd
#' @importFrom argparse ArgumentParser
main_script_runner <- function() {
  # This function encapsulates the script's logic for clarity,
  # though R scripts often run code directly.
  cat("Loading libraries and source modules...\n")

  if (Sys.getenv("R_LIBS_USER") == "") {
    user_lib_path <- "/home/ubuntu/R/library" 
    if (!dir.exists(user_lib_path)) dir.create(user_lib_path, recursive = TRUE, showWarnings = FALSE)
    Sys.setenv(R_LIBS_USER = user_lib_path)
    .libPaths(c(user_lib_path, .libPaths()))
  }

  source_module <- function(file_path) {
    tryCatch({
      source(file_path, local = FALSE) 
      cat(paste("Successfully sourced:", file_path, "\n"))
      return(TRUE)
    }, error = function(e) {
      cat(paste("Error sourcing", file_path, ":", e$message, "\n"))
      stop(e) 
      return(FALSE)
    })
  }

  # These paths assume main.R is in the R/ directory and other files are relative to it.
  # If running from project root, paths should be "R/input_config_handler.R", etc.
  # For consistency, let's assume this script is run from the project root.
  base_path <- if (basename(getwd()) == "R") ".." else "." 
  
  core_modules <- c(
    file.path(base_path, "R/input_config_handler.R"),
    file.path(base_path, "R/code_acquisition_parser.R"), 
    file.path(base_path, "R/code_analyzer.R"),        
    file.path(base_path, "R/llm_interactor.R"),         
    file.path(base_path, "R/quarto_renderer.R"),
    file.path(base_path, "R/export_utils.R") # Added export_utils
  )

  all_sourced <- TRUE
  for (mod in core_modules) {
    if (!source_module(mod)) {
      all_sourced <- FALSE # Mark failure but continue trying to source others
    }
  }
  if(!all_sourced) {
      stop(paste("Failed to load one or more critical modules. Exiting."))
  }
  # Also source deepwikiR.R for generate_repo_docs if it's not part of the loop
  if(!source_module(file.path(base_path, "R/deepwikiR.R"))) {
      stop(paste("Failed to load R/deepwikiR.R. Exiting."))
  }


  parser <- argparse::ArgumentParser(description = "Automated Code Documentation Generator (Legacy Single Project Runner)")
  parser$add_argument("--config", type = "character", required = TRUE, help = "Path to the JSON configuration file")
  
  # Capture actual command line arguments.
  # When run with Rscript R/main.R --config ..., commandArgs(trailingOnly=TRUE) works.
  # If sourced, args might be different. This is designed for Rscript.
  cli_args <- commandArgs(trailingOnly = TRUE)
  
  # Check if arguments are what we expect for this script.
  # This is a simple check; run_cli in deepwikiR.R has more robust parsing.
  if (!("--config" %in% cli_args)) {
    parser$print_help()
    stop("Missing --config argument. This script is intended for command-line execution with a config file.", call. = FALSE)
  }
  args <- parser$parse_args(args = cli_args)


  cat(paste("Starting documentation generation with config:", args$config, "\n"))

  # 1. Load and validate configuration
  # Note: load_and_validate_config now returns a list with global_settings and projects list
  full_config_data <- load_and_validate_config(args$config)
  if (is.null(full_config_data) || is.null(full_config_data$projects) || length(full_config_data$projects) == 0) {
    stop("Configuration loading failed or no projects found. Exiting.")
  }
  
  # This legacy main.R script will process ONLY THE FIRST project from the config.
  # For multi-project processing, use run_cli from R/deepwikiR.R
  config <- full_config_data$projects[[1]]
  if (length(full_config_data$projects) > 1) {
      warning("Multiple projects found in configuration. This main.R script will only process the first project: ", config$project_name, 
              ". For multi-project processing, use the main CLI entry point (e.g., Rscript -e 'deepwikiR::run_cli(\"generate\", \"--config\", \"",args$config,"\")').")
  }
  cat("Configuration for project '", config$project_name, "' loaded successfully.\n")


  # Call generate_repo_docs for the first project
  # The generate_repo_docs function now handles most of the logic internally.
  # The llm_api_key override is not handled by this legacy script's direct args,
  # but could be if generate_parser was used fully.
  output_file_path <- generate_repo_docs(
    project_config = config,
    llm_api_key = NULL, # No direct CLI arg for this in main.R
    verbose = TRUE      # Defaulting to verbose for this script
  )

  if (!is.null(output_file_path) && file.exists(output_file_path)) {
    cat(paste("Documentation for project", config$project_name, "rendered to:", output_file_path, "\n"))
  } else {
    warning(paste("Documentation generation may have failed for project", config$project_name, "as no valid output path was returned."))
  }
  
  cat(paste("Documentation generation process completed. Total tokens consumed:", get_total_tokens_consumed(), "\n"))
  
  if (!is.null(output_file_path)) print(output_file_path)
}

# This ensures main_script_runner() is called only when Rscript R/main.R is used.
if (sys.nframe() == 0L && !interactive()) {
  # Check if this script is being run directly by Rscript
  # sys.calls()[[1]][[1]] might be R CMD BATCH or similar, not robust
  # A common check is if sys.nframe() == 0L or if programName matches Rscript
  # For simplicity, if not interactive and sys.nframe is 0, assume Rscript execution.
  # For more robustness, one might check commandArgs() patterns.
  
  # The following condition attempts to identify if the script is being run via Rscript
  # This logic is a bit heuristic.
  # `getOption(" milyen_script_running")` is not standard.
  # A common way: check if sys.parent() is the global environment (0)
  # and if the script name matches.
  # However, for `Rscript file.R`, sys.nframe() is 0.
  
  # The condition `!interactive()` is often sufficient for scripts.
  # `sys.nframe() == 0L` is true for Rscript.
  
  # No, this is not the way. `run_cli` is the main entry point for CLI.
  # This file (main.R) if executed with Rscript, should probably just call run_cli.
  # However, the existing structure of main.R is a direct runner.
  # Let's wrap its current logic in a function and call it.
  main_script_runner()
}
