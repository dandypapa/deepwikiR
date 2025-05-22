#!/usr/bin/env Rscript

# R/interactive_mode.R - Interactive Q&A about the R codebase

# --- Dependencies ---
if (!requireNamespace("argparse", quietly = TRUE)) {
  install.packages("argparse", repos = "http://cran.us.r-project.org")
}
library(argparse)

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  install.packages("jsonlite", repos = "http://cran.us.r-project.org")
}
library(jsonlite)

# Source LLM interactor functions
# Assuming the script is run from the project root directory (e.g., ./R/interactive_mode.R)
llm_interactor_path <- "R/llm_interactor.R"
if (!file.exists(llm_interactor_path)) {
  stop(paste("LLM Interactor script not found at:", llm_interactor_path, 
             "Please ensure you are running from the project root or adjust the path."))
}
source(llm_interactor_path)

# --- Helper Functions ---

#' Build a context string from documented code elements.
#'
#' @param documented_elements A list of documented code elements.
#' @param max_context_length Approximate maximum character length for the context.
#' @return A single string containing formatted documentation for code elements.
build_context_from_docs <- function(documented_elements, max_context_length = 15000) {
  context_blocks <- c()
  current_length <- 0

  if (is.null(documented_elements) || length(documented_elements) == 0) {
    return("No documented elements provided to build context.\n")
  }

  for (element in documented_elements) {
    # Use 'description' if available (from LLM), otherwise 'documentation_text' or a fallback
    element_desc <- element$description # This is the field populated by generate_docs_with_llm
    if (is.null(element_desc) || element_desc == "") {
        element_desc <- element$documentation_text # Fallback if any
    }
    if (is.null(element_desc) || element_desc == "") {
        element_desc <- "(No description available)"
    }

    block <- paste0(
      "Item: ", element$name, "\n",
      "Type: ", element$type, "\n",
      "File: ", element$file_path, "\n",
      "Signature: ", element$name, element$signature, "\n", # Assuming signature doesn't include name
      "Code:\n",
      "```r\n",
      paste(element$code_block, collapse = "\n"), "\n",
      "```\n",
      "Description:\n",
      element_desc, "\n",
      "---\n"
    )
    
    block_length <- nchar(block)
    if (current_length + block_length > max_context_length && length(context_blocks) > 0) {
      # If adding this block exceeds max length, and we already have some context, stop.
      # A more sophisticated strategy could be implemented here (e.g., prioritizing, summarizing).
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

# --- Main Application Logic ---
main <- function() {
  parser <- ArgumentParser(description = "Interactive Q&A about an R codebase using LLM.")
  parser$add_argument("--data_file", type = "character", required = TRUE,
                      help = "Path to the _analysis_data.rds file.")
  parser$add_argument("--config_file", type = "character", required = TRUE,
                      help = "Path to the main project config.json file.")
  
  args <- parser$parse_args()

  # --- Load Data and Configuration ---
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
  project_config <- tryCatch({
    jsonlite::fromJSON(args$config_file, simplifyVector = FALSE) # Keep structure
  }, error = function(e) {
    stop("Error loading JSON config file: ", e$message)
  })

  llm_settings <- project_config$llm_settings
  if (is.null(llm_settings)) {
    stop("LLM settings not found in the configuration file.")
  }

  project_name <- analysis_data$project_name %||% "this R project"
  documented_elements <- analysis_data$documented_elements
  # call_graph <- analysis_data$call_graph # Not used in this version but available

  cat("\nWelcome to the Interactive R Codebase Q&A for project:", project_name, "\n")
  cat("Type 'exit', 'quit', or 'q' to end the session.\n\n")

  session_tokens_consumed <- 0

  # --- Build Full Context (once per session for now) ---
  cat("Building codebase context for the LLM...\n")
  code_context_str <- build_context_from_docs(documented_elements)
  if (nchar(code_context_str) < 100) { # Arbitrary small number to check if context is minimal
      cat("Warning: The generated context is very short. Answers may be limited.\n")
  }
  cat("Context built. Ready for questions.\n\n")

  # --- Interactive Loop ---
  while (TRUE) {
    user_question <- readline(prompt = paste0("[", project_name, "] Ask a question (or 'exit'): "))
    user_question_lower <- tolower(trimws(user_question))

    if (user_question_lower %in% c("exit", "quit", "q")) {
      cat("Exiting interactive session.\n")
      break
    }

    if (user_question_lower == "") {
      next
    }

    # Construct the prompt
    system_prompt <- paste0(
      "You are an AI assistant helping a user understand their R codebase for the project named '", project_name, "'.\n",
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
    
    # Call LLM
    # Ensure llm_settings is correctly passed. invoke_llm_completion expects 'llm_config'
    llm_response <- invoke_llm_completion(
      prompt = full_prompt,
      llm_config = llm_settings, 
      element_name = paste("interactive_question_for", project_name) # For logging/tracking
    )

    if (llm_response$status == "success") {
      cat("\nLLM Response:\n", llm_response$content, "\n\n")
      if (!is.null(llm_response$tokens_used) && is.numeric(llm_response$tokens_used)) {
        session_tokens_consumed <- session_tokens_consumed + llm_response$tokens_used
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

# --- Script Execution ---
if (!interactive()) {
  # This check ensures the main function runs only when Rscript is used.
  # If you source this file in an interactive R session, main() won't run automatically.
  main()
}
