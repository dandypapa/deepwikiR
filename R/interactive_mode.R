#!/usr/bin/env Rscript

# R/interactive_mode.R - Interactive Q&A about the R codebase

#' @importFrom argparse ArgumentParser
#' @importFrom jsonlite fromJSON
NULL

#' Build Context from Documented Elements for LLM
#'
#' @title Construct Context String from Documentation
#' @description Combines documentation from various code elements into a single
#' string to be used as context for an LLM in a Q&A session.
#'
#' @param documented_elements A list of documented code elements. Each element is
#'        expected to be a list with at least `name`, `type`, `file_path`,
#'        `signature`, `code_block`, and `description` (or `documentation_text`).
#' @param max_context_length Numeric. The approximate maximum character length for
#'        the generated context string. Default is 15,000 characters.
#'
#' @return Character string. A formatted string containing the details of
#'         documented elements, truncated if it exceeds `max_context_length`.
#'         Returns a message if no elements are provided or if context cannot be built.
#'
#' @details The function iterates through `documented_elements`, formatting each
#'          one into a block containing its name, type, file path, signature,
#'          code, and description. It stops adding blocks if the total character
#'          count exceeds `max_context_length`.
#'
#' @examples
#' \dontrun{
#' # Sample elements (simplified)
#' elements <- list(
#'   list(name="func1", type="function", file_path="R/file1.R", signature="()",
#'        code_block="func1 <- function() { print('hi') }", description="Prints hi"),
#'   list(name="func2", type="function", file_path="R/file2.R", signature="(x)",
#'        code_block="func2 <- function(x) { x + 1 }", description="Increments x")
#' )
#' context_str <- build_context_from_docs(elements)
#' # cat(context_str)
#' }
#' @noRd
build_context_from_docs <- function(documented_elements, max_context_length = 15000) {
  context_blocks <- c()
  current_length <- 0

  if (is.null(documented_elements) || length(documented_elements) == 0) {
    return("No documented elements provided to build context.\n")
  }

  for (element in documented_elements) {
    element_desc <- element$description 
    if (is.null(element_desc) || element_desc == "") {
        element_desc <- element$documentation_text 
    }
    if (is.null(element_desc) || element_desc == "") {
        element_desc <- "(No description available)"
    }

    block <- paste0(
      "Item: ", element$name, "\n",
      "Type: ", element$type, "\n",
      "File: ", element$file_path, "\n",
      "Signature: ", element$name, element$signature, "\n", 
      "Code:\n",
      "```r\n", # Assuming R code, could be parameterized if needed
      paste(element$code_block, collapse = "\n"), "\n",
      "```\n",
      "Description:\n",
      element_desc, "\n",
      "---\n"
    )
    
    block_length <- nchar(block)
    if (current_length + block_length > max_context_length && length(context_blocks) > 0) {
      cat(paste0("Context length limit (", max_context_length, " chars) reached. Context may be truncated.\n"))
      break 
    }
    
    context_blocks <- c(context_blocks, block)
    current_length <- current_length + block_length
  }
  
  if (length(context_blocks) == 0) {
    return("Could not build any context from the provided documented elements (possibly all too large or empty).\n")
  }
  
  return(paste(context_blocks, collapse = ""))
}

#' Main Function for Interactive Q&A Mode
#'
#' @title Run Interactive Codebase Q&A Session
#' @description This function is the main entry point for the interactive Q&A mode.
#' It parses command-line arguments for data and configuration files, loads them,
#' builds a context from the analyzed code, and then enters a loop to take user
#' questions and provide LLM-generated answers.
#'
#' @details
#' The function expects two command-line arguments:
#' - `--data_file`: Path to the `_analysis_data.rds` file containing code analysis results.
#' - `--config_file`: Path to the main project `config.json` file for LLM settings.
#'
#' It sources `R/llm_interactor.R` for LLM communication. The Q&A loop continues
#' until the user types "exit", "quit", or "q". Token consumption is tracked
#' and reported for the session.
#'
#' This function is intended to be called when `R/interactive_mode.R` is executed
#' as a script.
#'
#' @note This function itself is not exported as it's the main part of a script.
#'       The script execution is guarded by `if (!interactive()) { main() }`.
#' @keywords internal
#' @noRd
main_interactive_mode <- function() { # Renamed from main to avoid conflict if sourced
  parser <- argparse::ArgumentParser(description = "Interactive Q&A about an R codebase using LLM.")
  parser$add_argument("--data_file", type = "character", required = TRUE,
                      help = "Path to the _analysis_data.rds file.")
  parser$add_argument("--config_file", type = "character", required = TRUE,
                      help = "Path to the main project config.json file.")
  
  # When run via Rscript, commandArgs(trailingOnly=TRUE) provides the arguments
  # When sourced for testing, you might pass args manually or use default NULL
  args <- parser$parse_args(args = commandArgs(trailingOnly = TRUE))

  cat("Loading analysis data from:", args$data_file, "\n")
  if (!file.exists(args$data_file)) {
    stop("Analysis data file not found: ", args$data_file)
  }
  analysis_data <- tryCatch({
    readRDS(args$data_file)
  }, error = function(e) {
    stop("Error loading RDS file: ", e$message)
  })
  
  cat("Loading configuration from:", args$config_file, "\n")
  if (!file.exists(args$config_file)) {
    stop("Configuration file not found: ", args$config_file)
  }
  
  # project_config here refers to the *entire* multi-project config file.
  # We need to find the LLM settings, likely from global_settings or a specific project.
  # For simplicity, this script will assume LLM settings are directly accessible or within global_settings.
  # A more robust approach would be to identify which project the .rds file belongs to if ambiguous.
  full_project_config <- tryCatch({
    jsonlite::fromJSON(args$config_file, simplifyVector = FALSE) 
  }, error = function(e) {
    stop("Error loading JSON config file: ", e$message)
  })

  # Attempt to get LLM settings:
  # 1. From a specific project if analysis_data contains a project_name that matches one in config.
  # 2. Fallback to global_settings$default_llm_settings.
  # 3. Error if no LLM settings can be clearly identified.
  llm_settings <- NULL
  current_project_name_from_data <- analysis_data$project_name %||% NULL

  if (!is.null(current_project_name_from_data) && !is.null(full_project_config$projects)) {
    for (proj_conf_item in full_project_config$projects) {
      if (identical(proj_conf_item$project_name, current_project_name_from_data)) {
        # If this project has its own LLM settings, use them.
        # Otherwise, they should have been merged with defaults during load_and_validate_config.
        llm_settings <- proj_conf_item$llm_settings 
        break
      }
    }
  }
  
  if (is.null(llm_settings) && !is.null(full_project_config$global_settings$default_llm_settings)) {
    cat("Using default LLM settings from global_settings for interactive mode.\n")
    llm_settings <- full_project_config$global_settings$default_llm_settings
  }
  
  if (is.null(llm_settings) || is.null(llm_settings$provider)) { # Check for provider specifically
    stop("LLM settings (including provider) could not be resolved from the configuration file for the interactive session.")
  }


  project_name_display <- current_project_name_from_data %||% "this R project"
  documented_elements <- analysis_data$documented_elements

  cat("\nWelcome to the Interactive R Codebase Q&A for project:", project_name_display, "\n")
  cat("Type 'exit', 'quit', or 'q' to end the session.\n\n")

  session_tokens_consumed <- 0

  cat("Building codebase context for the LLM...\n")
  # Pass only documented_elements to build_context_from_docs
  code_context_str <- build_context_from_docs(documented_elements) 
  if (nchar(code_context_str) < 100) { 
      cat("Warning: The generated context is very short. Answers may be limited.\n")
  }
  cat("Context built. Ready for questions.\n\n")

  # Source LLM interactor functions - ensure path is correct relative to script execution
  # If R/interactive_mode.R is run from project root, then "R/llm_interactor.R" is correct.
  # If run from R/ itself, then "llm_interactor.R"
  # Assuming execution from project root as per instructions for deepwikiR::run_cli
  base_path_for_source <- if (basename(getwd()) == "R") "." else "R" 
  llm_interactor_script_path <- file.path(base_path_for_source, "llm_interactor.R")

  if (!file.exists(llm_interactor_script_path)) {
      # Try to find it relative to this script's path if possible
      # This is more robust if the script is called from different locations
      try_path <- tryCatch(dirname(sys.frame(1)$ofile), warning=function(w) NULL) # Path of current script
      if (!is.null(try_path)) llm_interactor_script_path <- file.path(try_path, "llm_interactor.R")
  }
  
  if (!file.exists(llm_interactor_script_path)) {
    stop(paste("LLM Interactor script not found. Tried:", llm_interactor_script_path, 
               "Please ensure R/llm_interactor.R is accessible."))
  }
  source(llm_interactor_script_path, local = TRUE) # Source into local env for safety

  while (TRUE) {
    user_question <- readline(prompt = paste0("[", project_name_display, "] Ask a question (or 'exit'): "))
    user_question_lower <- tolower(trimws(user_question))

    if (user_question_lower %in% c("exit", "quit", "q")) {
      cat("Exiting interactive session.\n")
      break
    }

    if (user_question_lower == "") {
      next
    }

    system_prompt <- paste0(
      "You are an AI assistant helping a user understand their R codebase for the project named '", project_name_display, "'.\n",
      "The user will ask a question. Use the following context, which includes documented functions and code elements from their project, to answer the question.\n",
      "If the answer cannot be found in the provided context, clearly state that. Do not make up information outside of this context.\n",
      "Focus on providing answers relevant to the user's R code.\n\n",
      "Context from the codebase:\n",
      "---\n",
      code_context_str,
      "---\n"
    )
    
    full_prompt <- paste(system_prompt, "User Question:", user_question, sep = "\n")

    cat("\nSending question to LLM...\n")
    
    llm_response <- invoke_llm_completion(
      prompt = full_prompt,
      llm_config = llm_settings, 
      element_name = paste("interactive_question_for", project_name_display) 
    )

    if (llm_response$status == "success") {
      cat("\nLLM Response:\n", llm_response$content, "\n\n")
      if (!is.null(llm_response$tokens_used) && is.numeric(llm_response$tokens_used)) {
        session_tokens_consumed <- session_tokens_consumed + llm_response$tokens_used
        # Use increment_tokens_consumed from sourced llm_interactor.R if it updates a global counter
        # For session-specific, direct addition is fine as done here.
        cat(paste0("Approx. tokens for this exchange: ", round(llm_response$tokens_used), 
                   ". Session total: ", round(session_tokens_consumed), "\n\n"))
      }
    } else {
      cat("\nError from LLM:\n", llm_response$content, "\n\n")
    }
  }

  cat(paste0("Total tokens consumed during this interactive session: ", round(session_tokens_consumed), "\n"))
  cat("Thank you for using the Interactive R Codebase Q&A!\n")
}

# Script Execution Guard
# This ensures main_interactive_mode() is called only when Rscript R/interactive_mode.R is used.
# It checks if the script is the top-level frame (sys.nframe() == 0L) 
# and if the session is not interactive (implying Rscript execution).
if (sys.nframe() == 0L && !interactive()) {
  main_interactive_mode()
}
