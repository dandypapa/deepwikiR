# Demo script for processing the deepwikiR repository itself with deepwikiR
# This script demonstrates self-documentation capabilities.

cat("Starting deepwikiR self-documentation demo...\n")

# Determine project root dynamically
if (requireNamespace("here", quietly = TRUE)) {
  project_root <- here::here()
} else {
  project_root <- "." # Assumes script is run from project root or paths are relative
  warning("Package 'here' not found. Assuming current directory is project root.")
}

# Source necessary functions if not running as part of a loaded package
# In a real package usage, these would be deepwikiR::function_name
if(!exists("load_and_validate_config", mode="function") || !exists("generate_repo_docs", mode="function")) {
  # Source key files. This assumes a specific relative structure.
  # Adjust if main.R or another entry point handles sourcing more globally.
  cat("Sourcing required functions from R/ directory (if not already loaded)...\n")

  # Construct paths relative to the determined project_root
  input_config_handler_path <- file.path(project_root, "R", "input_config_handler.R")
  deepwikiR_script_core_path <- file.path(project_root, "R", "deepwikiR.R") # Main functions like generate_repo_docs

  # Other dependencies are sourced by deepwikiR.R or other sourced files.
  # We primarily need load_and_validate_config and generate_repo_docs to be available.

  if(file.exists(input_config_handler_path)) {
    source(input_config_handler_path, local = FALSE) # Source into global for wider availability in demo
    cat(paste("Sourced:", input_config_handler_path, "\n"))
  } else {
    warning(paste("Could not find R/input_config_handler.R at:", input_config_handler_path))
  }

  if(file.exists(deepwikiR_script_core_path)) {
    source(deepwikiR_script_core_path, local = FALSE) # This will source other R/ files
    cat(paste("Sourced:", deepwikiR_script_core_path, "\n"))
  } else {
    warning(paste("Could not find R/deepwikiR.R at:", deepwikiR_script_core_path))
  }

  # Re-check if essential functions are now available
  if(!exists("load_and_validate_config", mode="function") || !exists("generate_repo_docs", mode="function")) {
      stop("Essential functions (load_and_validate_config, generate_repo_docs) still not found after attempting to source R/ files. Please ensure deepwikiR is correctly loaded or installed, or that this demo script is run from the project root.")
  }
}


config_file_path <- file.path(project_root, "inst/examples/deepwikiR-self-demo-config.json")

if (!file.exists(config_file_path)) {
  stop(paste("Self-demo configuration file not found at:", config_file_path,
             "\nPlease ensure you are running this script from the root of the deepwikiR project, or adjust project_root definition."))
}

cat(paste("Loading self-demo configuration from:", config_file_path, "\n"))
config_data <- NULL
tryCatch({
  config_data <- load_and_validate_config(config_file_path)
}, error = function(e) {
  stop(paste("Failed to load or validate self-demo configuration:", e$message))
})

if (is.null(config_data) || is.null(config_data$projects) || length(config_data$projects) == 0) {
  stop("No projects found in the self-demo configuration.")
}

# Assuming the deepwikiR self-demo project is the first one in the config
self_demo_project_conf <- config_data$projects[[1]]

cat(paste("Starting documentation generation for project:", self_demo_project_conf$project_name, "\n"))
cat("This will process the deepwikiR codebase itself. This may take a while...\n")
cat("Please ensure your OPENAI_API_KEY (or the key specified in the config) is set as an environment variable.\n")


output_doc_path <- NULL
tryCatch({
  # Ensure API key is set via environment variable as specified in config
  # The llm_interactor should pick it up from Sys.getenv(api_key_env_var)

  output_doc_path <- generate_repo_docs(project_config = self_demo_project_conf, verbose = TRUE)

  if (!is.null(output_doc_path) && file.exists(output_doc_path)) {
    cat(paste("\nDocumentation generation complete for", self_demo_project_conf$project_name, "!\n"))
    # output_dir in config is relative, make it absolute for user display
    abs_output_dir <- normalizePath(file.path(project_root, self_demo_project_conf$output_dir), mustWork = FALSE)
    cat(paste("Output saved in directory:", abs_output_dir, "\n"))
    cat(paste("Main document:", normalizePath(output_doc_path, mustWork = FALSE), "\n"))
  } else {
    warning("Documentation generation for self-demo did not return a valid output file path.")
  }

}, error = function(e) {
  cat(paste("Error during documentation generation for", self_demo_project_conf$project_name, ":\n"))
  print(e)
  cat("\nEnsure your API key is correctly set and has credit/access.\n")
})

# Provide instructions for chat mode if documentation and RDS file were generated
if (!is.null(output_doc_path) && file.exists(output_doc_path)) {
  # Construct the expected path for the RDS file based on project config
  # output_dir is relative to global_output_dir_root, which is output/deepwikiR_demos
  # project_config$output_dir is "deepwikiR-self-docs"
  # So, the actual output_dir for RDS is project_root/output/deepwikiR_demos/deepwikiR-self-docs

  # The generate_repo_docs function saves RDS in project_config$output_dir,
  # which load_and_validate_config resolves.
  # So, self_demo_project_conf$output_dir is already the correct absolute path for the project's output.

  analysis_rds_filename <- paste0(tools::file_path_sans_ext(self_demo_project_conf$output_filename_base), "_analysis_data.rds")
  analysis_rds_path <- file.path(self_demo_project_conf$output_dir, analysis_rds_filename) # Should be absolute from config

  # Path to deepwikiR's main CLI script (R/deepwikiR.R)
  deepwikiR_cli_script_path <- file.path(project_root, "R/deepwikiR.R")

  if(file.exists(analysis_rds_path)) {
    cat(paste0("\nTo chat with the ", self_demo_project_conf$project_name,
               " repository's generated documentation (from project root of deepwikiR), you can run:\n"))

    # Use normalized paths for the command for clarity and robustness
    cmd_data_file <- shQuote(normalizePath(analysis_rds_path))
    cmd_config_file <- shQuote(normalizePath(config_file_path))
    cmd_script_path <- shQuote(deepwikiR_cli_script_path) # This is relative to where user runs it

    cat(paste0("Rscript ", cmd_script_path, " chat --data_file ",
               cmd_data_file, " --config_file ", cmd_config_file, "\n\n"))
  } else {
    cat(paste("\nAnalysis data file (", normalizePath(analysis_rds_path), ") not found. Chat mode cannot be demonstrated.\n"))
  }
}

cat("deepwikiR self-demo finished.\n")
