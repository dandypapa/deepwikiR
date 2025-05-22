# /home/ubuntu/autodoc_tool/R/llm_interactor.R
# Handles interaction with Large Language Models for generating documentation

# Ensure necessary libraries are loaded (e.g., tidyllm, httr, jsonlite)
# These should be installed via pak in install_dependencies.R or Dockerfile
if (!requireNamespace("tidyllm", quietly = TRUE)) {
  stop("Package tidyllm is not installed. Please install it.")
}
if (!requireNamespace("httr", quietly = TRUE)) {
  stop("Package httr is not installed. Please install it.")
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Package jsonlite is not installed. Please install it.")
}

library(tidyllm)
library(httr)
library(jsonlite)

# Global token counter (simple version)
.total_tokens_consumed <- 0

get_total_tokens_consumed <- function() {
  return(.total_tokens_consumed)
}

increment_tokens_consumed <- function(tokens) {
  if (is.numeric(tokens) && length(tokens) == 1 && !is.na(tokens)) {
    .total_tokens_consumed <<- .total_tokens_consumed + tokens
  } else {
    warning("Invalid token count provided to increment_tokens_consumed.")
  }
}

# Helper function to safely get nested list elements
# TODO: Consider moving to a general utils file if used elsewhere
safe_get_nested <- function(lst, ...) {
  keys <- list(...)
  for (key in keys) {
    if (is.list(lst) && key %in% names(lst)) {
      lst <- lst[[key]]
    } else {
      return(NULL)
    }
  }
  return(lst)
}

# Provider-specific handler for tidyllm (OpenAI-compatible)
# Inputs: prompt, model_name, max_tokens, temperature, api_key, api_base_url
# Returns a list with status, content, and tokens_used
handle_tidyllm_completion <- function(prompt, model_name, max_tokens, temperature, api_key, api_base_url, element_name = "current element") {
  orig_openai_api_key <- Sys.getenv("OPENAI_API_KEY", unset = NA)
  orig_openai_api_base <- Sys.getenv("OPENAI_API_BASE", unset = NA)

  Sys.setenv(OPENAI_API_KEY = api_key)
  if (!is.null(api_base_url) && nzchar(api_base_url)) {
    Sys.setenv(OPENAI_API_BASE = api_base_url)
  } else {
    # Ensure OPENAI_API_BASE is unset if api_base_url is NULL or empty
    # Sys.unsetenv("OPENAI_API_BASE") # tidyllm might default if not set
  }
  
  # Restore original env vars on exit, whether success or error
  on.exit({
    if (is.na(orig_openai_api_key)) Sys.unsetenv("OPENAI_API_KEY") else Sys.setenv(OPENAI_API_KEY = orig_openai_api_key)
    if (is.na(orig_openai_api_base)) Sys.unsetenv("OPENAI_API_BASE") else Sys.setenv(OPENAI_API_BASE = orig_openai_api_base)
  })

  result <- list(status = "error", content = "Initialization error", tokens_used = 0)

  tryCatch({
    response <- tidyllm::complete(
      prompt = prompt,
      model = model_name,
      max_tokens = max_tokens,
      temperature = temperature
      # system_prompt can be added if needed
    )

    if (is.data.frame(response) && "completion" %in% names(response) && nrow(response) > 0) {
      generated_text <- response$completion[1]
      # Rough token estimation (prompt + completion), assumes 1 token ~ 4 chars
      # TODO: More accurate token counting if tidyllm provides it (e.g., from response headers or attributes)
      prompt_tokens <- nchar(prompt) / 4 
      completion_tokens <- nchar(generated_text) / 4
      total_tokens_for_request <- prompt_tokens + completion_tokens
      
      result$status <- "success"
      result$content <- stringr::str_trim(generated_text)
      result$tokens_used <- total_tokens_for_request
    } else {
      warning_msg <- paste("LLM response for", element_name, "was not in the expected format or was empty via tidyllm.")
      warning(warning_msg)
      result$content <- warning_msg
    }
  }, error = function(e) {
    error_msg <- paste("Error calling tidyllm for element", element_name, ":", e$message)
    warning(error_msg)
    result$content <- error_msg
  })
  
  return(result)
}

# Generic function to invoke LLM completion
# Inputs: prompt, llm_config (list with provider, model, etc.), element_name
# Returns a list with status, content, and tokens_used
invoke_llm_completion <- function(prompt, llm_config, element_name = "current element") {
  provider <- safe_get_nested(llm_config, "provider")
  if (is.null(provider)) {
    return(list(status = "error", content = "LLM provider not specified in llm_config.", tokens_used = 0))
  }

  model_name <- safe_get_nested(llm_config, "model") %||% "gpt-3.5-turbo" # Default if not specified
  max_tokens <- safe_get_nested(llm_config, "max_tokens_per_request") %||% 150
  temperature <- safe_get_nested(llm_config, "temperature") %||% 0.2
  
  # Get API key from environment variable specified in config
  api_key_env_var <- safe_get_nested(llm_config, "api_details", "api_key_env_var")
  if (is.null(api_key_env_var)) {
      return(list(status = "error", content = "API key environment variable name not specified in llm_config$api_details.", tokens_used = 0))
  }
  api_key <- Sys.getenv(api_key_env_var, unset = "")
  if (api_key == "") {
    warning_msg <- paste("API key from env var '", api_key_env_var, "' is not set. LLM calls might fail for provider '", provider, "'.", sep="")
    # It's a warning because some providers might not need a key or use other auth methods handled by their SDKs
    warning(warning_msg) 
    # Depending on strictness, could return an error here:
    # return(list(status = "error", content = warning_msg, tokens_used = 0))
  }

  api_base_url <- safe_get_nested(llm_config, "api_details", "api_base_url") # Can be NULL

  # Switch based on provider
  if (provider == "openai" || provider == "tidyllm") { # Assuming "openai" uses tidyllm for now
    cat(paste("Using tidyllm handler for provider:", provider, "for element:", element_name, "\n"))
    return(handle_tidyllm_completion(
      prompt = prompt,
      model_name = model_name,
      max_tokens = as.integer(max_tokens), # Ensure integer type
      temperature = as.numeric(temperature), # Ensure numeric type
      api_key = api_key,
      api_base_url = api_base_url,
      element_name = element_name
    ))
  } else if (provider == "another_provider") {
    # Placeholder for another provider's handler
    # return(handle_another_completion(...))
    return(list(status = "error", content = paste("Provider '", provider, "' not yet supported.", sep=""), tokens_used = 0))
  } else {
    return(list(status = "error", content = paste("Unknown LLM provider specified:", provider), tokens_used = 0))
  }
}

# Function to generate documentation for code elements using an LLM
# Takes a list of extracted code elements, LLM settings from config, and language hints
# Returns the list of elements, augmented with LLM-generated descriptions
generate_docs_with_llm <- function(extracted_elements, llm_settings, language_hints) {
  if (is.null(llm_settings) || is.null(safe_get_nested(llm_settings, "provider"))) {
    cat("LLM settings or provider not specified. Skipping LLM documentation generation.\n")
    for (i in seq_along(extracted_elements)) {
      if (is.null(extracted_elements[[i]]$description) || extracted_elements[[i]]$description == "") {
        extracted_elements[[i]]$description <- "(LLM description not generated due to missing configuration)"
      }
    }
    return(extracted_elements)
  }

  cat(paste("Using LLM API Provider:", safe_get_nested(llm_settings, "provider"), "\n"))

  # Loop through each code element and generate a description
  for (i in seq_along(extracted_elements)) {
    element <- extracted_elements[[i]]
    cat(paste("Generating docs for element:", element$name, "in file:", element$file_path, "\n"))

    # Construct a prompt for the LLM
    lang_hint_str <- paste(language_hints, collapse = ", ")
    prompt <- paste0(
      "You are an expert programmer tasked with generating concise documentation for code elements.",
      "\nFor the following ", lang_hint_str, " ", element$type, " named `", element$name, "`",
      " with signature `", element$name, element$signature, "`", # Assuming element$signature does not include name
      " found in file `", element$file_path, "`,",
      "\nCode block:\n```", tolower(language_hints[[1]] %||% "r"), "\n",
      paste(element$code_block, collapse = "\n"),
      "\n```\n",
      "\nPlease provide a brief, one-paragraph explanation of its purpose and functionality. ",
      "Focus on what it does, its main inputs, and its primary outputs or side effects. ",
      "Do not repeat the code block or the signature in your explanation. Be very concise."
    )

    # Call the generic LLM invocation function
    # llm_settings itself is passed as llm_config
    llm_response <- invoke_llm_completion(
      prompt = prompt,
      llm_config = llm_settings, # llm_settings from main config matches structure for llm_config
      element_name = element$name
    )

    if (llm_response$status == "success") {
      extracted_elements[[i]]$description <- llm_response$content
      if (!is.null(llm_response$tokens_used) && is.numeric(llm_response$tokens_used)) {
        increment_tokens_consumed(llm_response$tokens_used)
        cat(paste("Generated description for", element$name, ". Approx tokens for this request:", round(llm_response$tokens_used), "\n"))
      } else {
        cat(paste("Generated description for", element$name, ". Token count not available.\n"))
      }
    } else {
      warning_msg <- paste("LLM description generation failed for", element$name, ":", llm_response$content)
      warning(warning_msg)
      extracted_elements[[i]]$description <- paste("(LLM description generation failed:", llm_response$content, ")")
    }

    # Respect rate limits if any (simple delay)
    rate_limit_delay <- safe_get_nested(llm_settings, "rate_limit_delay_sec")
    if (!is.null(rate_limit_delay) && is.numeric(rate_limit_delay) && rate_limit_delay > 0) {
      Sys.sleep(rate_limit_delay)
    }
  }
  
  return(extracted_elements)
}


#' Generate Summaries for Each Directory in a Project using an LLM
#'
#' Iterates through directories, compiles context about their contents (files and documented elements),
#' and calls an LLM to generate a high-level summary for each directory.
#'
#' @param directory_contents A named list where names are directory paths (keys like "R/", ".", "src/utils/")
#'                           and values are character vectors of basenames of files in that directory.
#'                           Output from `analyze_r_repository_code`.
#' @param code_details A named list where names are relative file paths to project root and values
#'                     are the string content of those files.
#' @param llm_generated_docs A list of documentation objects for individual code elements (functions, etc.),
#'                           each object should include at least `name`, `type`, `file_path` (relative to project root),
#'                           and `description`.
#' @param llm_config The LLM settings object, as passed from the main configuration.
#'                   Expected to contain provider, model, api_details (including api_key_env_var), etc.
#' @param language_hints A character vector providing language context (e.g., c("R", "script")).
#' @param project_name The name of the project, used in prompts.
#' @param verbose Logical, whether to print detailed messages during processing. Default TRUE.
#' @return A named list where names are directory path keys and values are the LLM-generated summaries.
#' @export
generate_directory_summaries_with_llm <- function(
  directory_contents,
  code_details, # Currently not used for code snippets to manage prompt length, but available
  llm_generated_docs,
  llm_config,
  language_hints, # Currently not used in dir summary prompt, but available
  project_name,
  verbose = TRUE
) {
  
  directory_summaries <- list()
  
  if (is.null(llm_config) || is.null(safe_get_nested(llm_config, "provider"))) {
    if (verbose) cat("LLM settings or provider not specified. Skipping directory summary generation.\n")
    return(directory_summaries)
  }

  # --- Helper for Context Compilation ---
  compile_directory_context <- function(dir_path_key, files_in_dir, all_docs, max_context_char = 15000) {
    context_parts <- list()
    current_char_count <- 0

    # Header
    header <- paste0("Context for directory '", dir_path_key, "' in project '", project_name, "':\n")
    context_parts[[length(context_parts) + 1]] <- header
    current_char_count <- current_char_count + nchar(header)

    # File list
    file_list_str <- paste0("This directory contains the following files: ", paste(files_in_dir, collapse=", "), "\n\n")
    context_parts[[length(context_parts) + 1]] <- file_list_str
    current_char_count <- current_char_count + nchar(file_list_str)
    
    # Documented elements in this directory
    elements_in_dir_context <- c()
    # Normalize dir_path_key for matching: remove trailing slash if present, except for "."
    normalized_dir_path_for_match <- if (dir_path_key == ".") "." else sub("/$", "", dir_path_key)

    for (element_doc in all_docs) {
      if (current_char_count >= max_context_char) break
      
      # element_doc$file_path is relative to project root e.g. "R/utils.R" or "main.R"
      element_dir_name <- dirname(element_doc$file_path) # "R" or "."
      element_base_name <- basename(element_doc$file_path) # "utils.R" or "main.R"

      if (element_dir_name == normalized_dir_path_for_match && element_base_name %in% files_in_dir) {
        element_snippet <- paste0(
          "Element Name: ", element_doc$name, " (Type: ", element_doc$type, ")\n",
          "File: ", element_doc$file_path, "\n", # Relative to project root
          "Description: ", element_doc$description %||% "(No description available)", "\n---\n"
        )
        if (current_char_count + nchar(element_snippet) <= max_context_char) {
          elements_in_dir_context <- c(elements_in_dir_context, element_snippet)
          current_char_count <- current_char_count + nchar(element_snippet)
        } else {
          if (verbose) cat(paste("Context limit reached while adding elements for dir:", dir_path_key, "\n"))
          break 
        }
      }
    }
    if (length(elements_in_dir_context) > 0) {
      context_parts[[length(context_parts) + 1]] <- "\nKey documented elements in this directory:\n"
      context_parts <- c(context_parts, elements_in_dir_context)
    } else {
      context_parts[[length(context_parts) + 1]] <- "\nNo specific documented code elements found or included for this directory in the context.\n"
    }
    
    return(paste(unlist(context_parts), collapse=""))
  }
  # --- End of Helper ---

  if (verbose) cat(paste("Starting directory summary generation for project:", project_name, "\n"))

  for (dir_path_key in names(directory_contents)) {
    if (verbose) cat(paste("Processing directory:", dir_path_key, "\n"))
    
    files_in_dir <- directory_contents[[dir_path_key]]
    
    compiled_context <- compile_directory_context(
      dir_path_key = dir_path_key,
      files_in_dir = files_in_dir,
      all_docs = llm_generated_docs 
    )
    
    system_prompt_dir_summary <- paste0(
      "You are an expert software engineering assistant. Your task is to provide a high-level summary of a code directory based on the context provided.\n",
      "Focus on the overall purpose, primary responsibilities, and key functionalities encapsulated within this directory.\n",
      "Do not merely list the files or functions. Synthesize the information into a coherent summary.\n",
      "If the directory appears to be for utility functions, testing, data, documentation, configuration, or a specific module/feature, please state that clearly.\n",
      "Aim for a concise summary of 1-3 paragraphs."
    )
    
    full_prompt_for_dir <- paste0(
      system_prompt_dir_summary,
      "\n\nBased on the following context about directory '", dir_path_key, "' in project '", project_name, 
      "', please generate the summary.\n\nContext:\n---\n",
      compiled_context,
      "\n---\n\nSummary:"
    )

    if (verbose) cat(paste("  Sending request to LLM for directory:", dir_path_key, "\n"))
    
    llm_response <- invoke_llm_completion(
      prompt = full_prompt_for_dir,
      llm_config = llm_config,
      element_name = paste("Summary for dir:", dir_path_key, "in project:", project_name)
    )

    if (llm_response$status == "success") {
      directory_summaries[[dir_path_key]] <- stringr::str_trim(llm_response$content)
      if (verbose) {
        cat(paste("  Successfully generated summary for directory:", dir_path_key, "\n"))
        # cat(paste("    Summary:", directory_summaries[[dir_path_key]], "\n")) # Can be too verbose
      }
      if (!is.null(llm_response$tokens_used) && is.numeric(llm_response$tokens_used)) {
        increment_tokens_consumed(llm_response$tokens_used)
        if (verbose) cat(paste("    Approx. tokens for this directory summary:", round(llm_response$tokens_used), "\n"))
      }
    } else {
      warning_msg <- paste("LLM summary generation failed for directory", dir_path_key, ":", llm_response$content)
      warning(warning_msg)
      directory_summaries[[dir_path_key]] <- paste("(LLM summary generation failed:", llm_response$content, ")")
    }

    # Respect rate limits
    rate_limit_delay <- safe_get_nested(llm_config, "rate_limit_delay_sec")
    if (!is.null(rate_limit_delay) && is.numeric(rate_limit_delay) && rate_limit_delay > 0) {
      if (verbose) cat(paste("  Respecting rate limit, sleeping for", rate_limit_delay, "seconds.\n"))
      Sys.sleep(rate_limit_delay)
    }
  }
  
  if (verbose) cat(paste("Finished directory summary generation for project:", project_name, "\n"))
  return(directory_summaries)
}

