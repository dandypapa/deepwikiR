# Demo script for processing the r-lib/cli repository with deepwikiR
# Ensure functions are available (e.g. by running devtools::load_all() or from installed package)
# For non-package use, sourcing might be required if not already done by a master script.
# We assume functions like load_and_validate_config and generate_repo_docs are accessible.

cat("Starting deepwikiR demo for r-lib/cli repository...\n")

# Define paths (assuming script is run from project root or paths are adjusted)
# If this script is in inst/examples, relative paths need to be set from there or use here::here()
# For simplicity, this demo script might be best run interactively from project root,
# or paths adjusted if run via Rscript inst/examples/run_rlib_cli_demo.R

# Determine project root dynamically (if possible, otherwise assume standard execution)
# This is a common pattern for scripts within inst/
if (requireNamespace("here", quietly = TRUE)) {
   project_root <- here::here()
} else {
   # Fallback if 'here' is not available - assumes script is run from project root
   # or that relative paths from current dir to R/ and inst/ are valid.
   project_root <- "." 
   warning("Package 'here' not found. Assuming current directory is project root or paths are relative from here.")
}

# Source necessary functions if not running as part of a loaded package
# This is primarily for making the demo script runnable in more contexts.
# In a real package usage, these would be deepwikiR::function_name
if(!exists("load_and_validate_config", mode="function") || !exists("generate_repo_docs", mode="function")) {
   cat("Sourcing required functions from R/ directory...\n")
   # Source all R files in the R directory. This is a broad approach for a demo.
   # A more targeted sourcing might be needed if function names clash or for efficiency.
   r_files <- list.files(file.path(project_root, "R"), pattern = "\\.R$", full.names = TRUE)
   if (length(r_files) == 0 && project_root == ".") { # If "." is root and no R/ found, try one level up for R/
        r_files <- list.files(file.path("..", "R"), pattern = "\\.R$", full.names = TRUE)
        if (length(r_files) > 0) { # if files found one level up, adjust project_root for config path
            project_root <- ".."
            cat("Adjusted project_root to '..' for finding R/ files and config.\n")
        }
   }

   for (r_file in r_files) {
     tryCatch({
       source(r_file, local = FALSE) # Source into global env for wider availability in demo
       cat(paste("Sourced:", r_file, "\n"))
     }, error = function(e) {
       warning(paste("Error sourcing", r_file, ":", e$message))
     })
   }
   # Re-check if essential functions are now available
   if(!exists("load_and_validate_config", mode="function") || !exists("generate_repo_docs", mode="function")) {
       stop("Essential functions (load_and_validate_config, generate_repo_docs) still not found after attempting to source R/ files. Please ensure deepwikiR is correctly loaded or installed.")
   }
}


config_file_path <- file.path(project_root, "inst/examples/r-lib-cli-config.json")

if (!file.exists(config_file_path)) {
  stop(paste("Demo configuration file not found:", config_file_path, 
             "\nPlease ensure you are running this script from the root of the deepwikiR project, or adjust project_root definition."))
}

cat(paste("Loading configuration from:", config_file_path, "\n"))
config_data <- NULL
tryCatch({
  config_data <- load_and_validate_config(config_file_path)
}, error = function(e) {
  stop(paste("Failed to load or validate configuration:", e$message))
})

if (is.null(config_data) || is.null(config_data$projects) || length(config_data$projects) == 0) {
  stop("No projects found in the configuration.")
}

# Assuming the r-lib/cli project is the first one in the config
rlib_cli_project_conf <- config_data$projects[[1]]

cat(paste("Starting documentation generation for project:", rlib_cli_project_conf$project_name, "\n"))
cat("This may take a while as it involves cloning the repository and LLM processing...\n")
cat("Please ensure your OPENAI_API_KEY (or the key specified in the config) is set as an environment variable.\n")

output_doc_path <- NULL
tryCatch({
  # The llm_interactor should pick up the API key from Sys.getenv(api_key_env_var)
  
  output_doc_path <- generate_repo_docs(project_config = rlib_cli_project_conf, verbose = TRUE)
  cat(paste("\nDocumentation generation complete for", rlib_cli_project_conf$project_name, "!\n"))
  cat(paste("Output saved in directory:", normalizePath(rlib_cli_project_conf$output_dir), "\n"))
  if(!is.null(output_doc_path)) cat(paste("Main document:", normalizePath(output_doc_path), "\n"))
}, error = function(e) {
  cat(paste("Error during documentation generation for", rlib_cli_project_conf$project_name, ":\n"))
  print(e)
  cat("\nEnsure your API key is correctly set and has credit/access.\n")
})

if (!is.null(output_doc_path)) {
   analysis_rds_filename <- paste0(tools::file_path_sans_ext(rlib_cli_project_conf$output_filename_base), "_analysis_data.rds")
   analysis_rds_path <- file.path(rlib_cli_project_conf$output_dir, analysis_rds_filename)
   
   # Construct path to deepwikiR's CLI entry point (assuming it's R/deepwikiR.R)
   # This path needs to be relative to where the user *runs* this demo script from,
   # or absolute if the package is installed. For this demo, assume relative from project root.
   deepwikiR_cli_script_path <- file.path(project_root, "R/deepwikiR.R") # Path to the CLI script

   if(file.exists(analysis_rds_path)) {
       cat(paste0("\nTo chat with the ", rlib_cli_project_conf$project_name, 
                  " repository (from project root of deepwikiR), you can run:\n"))
       # Ensure paths are correctly quoted and normalized for command line use
       cmd_data_file <- shQuote(normalizePath(analysis_rds_path))
       cmd_config_file <- shQuote(normalizePath(config_file_path))
       cmd_script_path <- shQuote(deepwikiR_cli_script_path) # No normalization, it's relative to project root

       cat(paste0("Rscript ", cmd_script_path, " chat --data_file ", 
                  cmd_data_file, " --config_file ", cmd_config_file, "\n\n"))
       
       # Example of directly calling chat_with_repo if preferred:
       # This assumes deepwikiR functions are loaded (e.g., via devtools::load_all())
       # cat("Alternatively, from an R session (after loading deepwikiR functions):\n")
       # cat(paste0("deepwikiR::chat_with_repo(analysis_data_path = \"", normalizePath(analysis_rds_path), 
       #            "\", config_path = \"", normalizePath(config_file_path), "\")\n"))
   } else {
       cat(paste("\nAnalysis data file (", analysis_rds_path, ") not found. Chat mode cannot be demonstrated.\n"))
   }
}

cat("Demo finished.\n")
