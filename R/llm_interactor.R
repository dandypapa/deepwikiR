# /home/ubuntu/autodoc_tool/R/llm_interactor.R

#' @importFrom tidyllm complete
#' @importFrom httr POST content
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom stringr str_trim
NULL

#' Get Total Tokens Consumed
#'
#' @title Retrieve LLM Token Consumption
#' @description Returns the total number of tokens estimated to have been consumed
#' by LLM interactions during the current R session.
#'
#' @return Numeric. The total tokens consumed.
#' @export
#' @examples
#' \dontrun{
#' # Initialize counter or after some LLM calls
#' total_tokens <- get_total_tokens_consumed()
#' cat("Total tokens used so far:", total_tokens, "\n")
#' }
get_total_tokens_consumed <- function() {
  return(.total_tokens_consumed)
}

#' Increment Tokens Consumed
#'
#' @title Increment LLM Token Counter
#' @description Adds a specified number of tokens to the session's total token consumption counter.
#' Issues a warning if the input is not a single numeric value.
#'
#' @param tokens Numeric. The number of tokens to add to the total.
#' @return Invisible NULL. Modifies the global token counter (`.total_tokens_consumed`)
#'         as a side effect.
#' @noRd
increment_tokens_consumed <- function(tokens) {
  if (is.numeric(tokens) && length(tokens) == 1 && !is.na(tokens)) {
    .total_tokens_consumed <<- .total_tokens_consumed + tokens
  } else {
    warning("Invalid token count provided to increment_tokens_consumed.")
  }
}

#' Safely Get Nested List Elements
#'
#' @title Access Nested List Elements Safely
#' @description Retrieves an element from a nested list structure using a sequence of keys.
#' If any key along the path does not exist, or if the structure is not a list
#' at any point, it returns `NULL` instead of throwing an error.
#'
#' @param lst The list from which to retrieve the element.
#' @param ... A sequence of character strings or numeric indices representing the
#'        path of keys to the desired element.
#' @return The value of the nested element if found, otherwise `NULL`.
#' @noRd
#' @examples
#' \dontrun{
#' my_list <- list(a = list(b = list(c = 10)))
#' safe_get_nested(my_list, "a", "b", "c") # Returns 10
#' safe_get_nested(my_list, "a", "x", "c") # Returns NULL
#' }
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

#' Handle TidyLLM API Completion
#'
#' @title Process LLM Completion via TidyLLM
#' @description Sends a prompt to an OpenAI-compatible LLM API using the `tidyllm` package.
#' It manages API key and base URL environment variables for the duration of the call.
#'
#' @param prompt Character string. The prompt to send to the LLM.
#' @param model_name Character string. The identifier of the LLM model to use.
#' @param max_tokens Integer. The maximum number of tokens to generate in the completion.
#' @param temperature Numeric. The sampling temperature for generation (controls randomness).
#' @param api_key Character string. The API key for the LLM service.
#' @param api_base_url Character string or NULL. The base URL for the LLM API endpoint.
#' @param element_name Character string. A descriptive name for the item being processed
#'        (e.g., function name, directory path), used for logging and error messages.
#'        Default is "current element".
#'
#' @return A list with:
#'   \item{status}{"success" or "error".}
#'   \item{content}{The generated text if successful, or an error message.}
#'   \item{tokens_used}{An estimated number of tokens used for the request (prompt + completion).}
#' @noRd
handle_tidyllm_completion <- function(prompt, model_name, max_tokens, temperature, api_key, api_base_url, element_name = "current element") {
  orig_openai_api_key <- Sys.getenv("OPENAI_API_KEY", unset = NA)
  orig_openai_api_base <- Sys.getenv("OPENAI_API_BASE", unset = NA)

  Sys.setenv(OPENAI_API_KEY = api_key)
  if (!is.null(api_base_url) && nzchar(api_base_url)) {
    Sys.setenv(OPENAI_API_BASE = api_base_url)
  } 
  
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
    )

    if (is.data.frame(response) && "completion" %in% names(response) && nrow(response) > 0) {
      generated_text <- response$completion[1]
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

#' Invoke LLM Completion (Generic)
#'
#' @title Generic LLM Completion Invocation
#' @description A generic wrapper to call different LLM providers for text completion.
#' It reads provider-specific details from `llm_config` and routes the request
#' to the appropriate handler (e.g., `handle_tidyllm_completion`).
#'
#' @param prompt Character string. The prompt to send to the LLM.
#' @param llm_config A list. Configuration for the LLM, including `provider`, `model`,
#'        `max_tokens_per_request`, `temperature`, and `api_details` (which should
#'        contain `api_key_env_var` and optionally `api_base_url`).
#' @param element_name Character string. A descriptive name for the item being processed,
#'        used for logging. Default is "current element".
#'
#' @return A list with:
#'   \item{status}{"success" or "error".}
#'   \item{content}{The generated text if successful, or an error message.}
#'   \item{tokens_used}{An estimated number of tokens used for the request.}
#' @noRd
invoke_llm_completion <- function(prompt, llm_config, element_name = "current element") {
  provider <- safe_get_nested(llm_config, "provider")
  if (is.null(provider)) {
    return(list(status = "error", content = "LLM provider not specified in llm_config.", tokens_used = 0))
  }

  model_name <- safe_get_nested(llm_config, "model") %||% "gpt-3.5-turbo" 
  max_tokens <- safe_get_nested(llm_config, "max_tokens_per_request") %||% 150
  temperature <- safe_get_nested(llm_config, "temperature") %||% 0.2
  
  api_key_env_var <- safe_get_nested(llm_config, "api_details", "api_key_env_var")
  if (is.null(api_key_env_var)) {
      return(list(status = "error", content = "API key environment variable name not specified in llm_config$api_details.", tokens_used = 0))
  }
  api_key <- Sys.getenv(api_key_env_var, unset = "")
  if (api_key == "") {
    warning_msg <- paste("API key from env var '", api_key_env_var, "' is not set. LLM calls might fail for provider '", provider, "'.", sep="")
    warning(warning_msg) 
  }

  api_base_url <- safe_get_nested(llm_config, "api_details", "api_base_url") 

  if (provider == "openai" || provider == "tidyllm") { 
    cat(paste("Using tidyllm handler for provider:", provider, "for element:", element_name, "\n"))
    return(handle_tidyllm_completion(
      prompt = prompt,
      model_name = model_name,
      max_tokens = as.integer(max_tokens), 
      temperature = as.numeric(temperature), 
      api_key = api_key,
      api_base_url = api_base_url,
      element_name = element_name
    ))
  } else if (provider == "another_provider") {
    return(list(status = "error", content = paste("Provider '", provider, "' not yet supported.", sep=""), tokens_used = 0))
  } else {
    return(list(status = "error", content = paste("Unknown LLM provider specified:", provider), tokens_used = 0))
  }
}

#' Generate Documentation for Code Elements via LLM
#'
#' @title Generate LLM Documentation for Code Elements
#' @description Iterates through a list of extracted code elements (e.g., functions),
#' constructs a detailed prompt for each, and calls an LLM to generate a
#' documentation string (description).
#'
#' @param extracted_elements A list of code elements. Each element is a list
#'        expected to contain `name`, `type`, `signature`, `file_path`, and `code_block`.
#' @param llm_settings A list. Configuration for the LLM, as defined in the main
#'        project config (e.g., `project_config$llm_settings`). Passed to `invoke_llm_completion`.
#' @param language_hints A character vector. Hints about the programming language(s)
#'        (e.g., `c("R", "function")`) to include in the LLM prompt.
#'
#' @return The input `extracted_elements` list, with each element augmented with a
#'         `description` field containing the LLM-generated text or an error/placeholder message.
#' @noRd
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

  for (i in seq_along(extracted_elements)) {
    element <- extracted_elements[[i]]
    cat(paste("Generating docs for element:", element$name, "in file:", element$file_path, "\n"))

    lang_hint_str <- paste(language_hints, collapse = ", ")
    prompt <- paste0(
      "You are an expert programmer tasked with generating concise documentation for code elements.",
      "\nFor the following ", lang_hint_str, " ", element$type, " named `", element$name, "`",
      " with signature `", element$name, element$signature, "`", 
      " found in file `", element$file_path, "`,",
      "\nCode block:\n```", tolower(language_hints[[1]] %||% "r"), "\n",
      paste(element$code_block, collapse = "\n"),
      "\n```\n",
      "\nPlease provide a brief, one-paragraph explanation of its purpose and functionality. ",
      "Focus on what it does, its main inputs, and its primary outputs or side effects. ",
      "Do not repeat the code block or the signature in your explanation. Be very concise."
    )

    llm_response <- invoke_llm_completion(
      prompt = prompt,
      llm_config = llm_settings, 
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

    rate_limit_delay <- safe_get_nested(llm_settings, "rate_limit_delay_sec")
    if (!is.null(rate_limit_delay) && is.numeric(rate_limit_delay) && rate_limit_delay > 0) {
      Sys.sleep(rate_limit_delay)
    }
  }
  
  return(extracted_elements)
}


#' Generate Summaries for Each Directory via LLM
#'
#' @title Generate LLM Directory Summaries
#' @description Iterates through directories within a project, compiles context about
#' their contents (including file names and summaries of documented code elements
#' within those files), and calls an LLM to generate a high-level summary for each directory.
#'
#' @param directory_contents A named list where names are directory paths (keys like
#'        "R/", ".", "src/utils/") relative to the project root, and values are
#'        character vectors of basenames of files in that directory. This is part
#'        of the output from `analyze_r_repository_code`.
#' @param code_details A named list where names are relative file paths to project root
#'        and values are the string content of those files. (Currently not used for
#'        code snippets in prompts to manage prompt length, but available for future use).
#' @param llm_generated_docs A list of documentation objects for individual code
#'        elements (e.g., functions), as returned by `generate_docs_with_llm`. Each
#'        object should include `name`, `type`, `file_path` (relative to project root),
#'        and `description`.
#' @param llm_config The LLM settings object from the project configuration, passed
#'        to `invoke_llm_completion`.
#' @param language_hints A character vector providing language context (e.g., `c("R", "script")`).
#'        (Currently not used in directory summary prompts, but available).
#' @param project_name Character string. The name of the project, used in prompts.
#' @param verbose Logical. If `TRUE`, prints detailed status messages. Default is `TRUE`.
#'
#' @return A named list where names are directory path keys (e.g., "R/", ".") and
#'         values are the LLM-generated summary strings for each directory.
#'         If LLM calls fail for a directory, the value will be an error message.
#' @export
#' @examples
#' \dontrun{
#' # Assume directory_contents, llm_generated_docs, llm_config, etc. are populated
#' # summaries <- generate_directory_summaries_with_llm(
#' #   directory_contents,
#' #   code_details_placeholder, # Can be an empty list if not used by context compiler
#' #   llm_docs_for_elements,
#' #   project_llm_settings,
#' #   project_language_hints,
#' #   "MyAwesomeProject"
#' # )
#' # print(summaries[["R/"]])
#' }
generate_directory_summaries_with_llm <- function(
  directory_contents,
  code_details, 
  llm_generated_docs,
  llm_config,
  language_hints, 
  project_name,
  verbose = TRUE
) {
  
  directory_summaries <- list()
  
  if (is.null(llm_config) || is.null(safe_get_nested(llm_config, "provider"))) {
    if (verbose) cat("LLM settings or provider not specified. Skipping directory summary generation.\n")
    return(directory_summaries)
  }

  compile_directory_context <- function(dir_path_key, files_in_dir, all_docs, max_context_char = 15000) {
    context_parts <- list()
    current_char_count <- 0

    header <- paste0("Context for directory '", dir_path_key, "' in project '", project_name, "':\n")
    context_parts[[length(context_parts) + 1]] <- header
    current_char_count <- current_char_count + nchar(header)

    file_list_str <- paste0("This directory contains the following files: ", paste(files_in_dir, collapse=", "), "\n\n")
    context_parts[[length(context_parts) + 1]] <- file_list_str
    current_char_count <- current_char_count + nchar(file_list_str)
    
    elements_in_dir_context <- c()
    normalized_dir_path_for_match <- if (dir_path_key == ".") "." else sub("/$", "", dir_path_key)

    for (element_doc in all_docs) {
      if (current_char_count >= max_context_char) break
      
      element_dir_name <- dirname(element_doc$file_path) 
      element_base_name <- basename(element_doc$file_path) 

      if (element_dir_name == normalized_dir_path_for_match && element_base_name %in% files_in_dir) {
        element_snippet <- paste0(
          "Element Name: ", element_doc$name, " (Type: ", element_doc$type, ")\n",
          "File: ", element_doc$file_path, "\n", 
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

    rate_limit_delay <- safe_get_nested(llm_config, "rate_limit_delay_sec")
    if (!is.null(rate_limit_delay) && is.numeric(rate_limit_delay) && rate_limit_delay > 0) {
      if (verbose) cat(paste("  Respecting rate limit, sleeping for", rate_limit_delay, "seconds.\n"))
      Sys.sleep(rate_limit_delay)
    }
  }
  
  if (verbose) cat(paste("Finished directory summary generation for project:", project_name, "\n"))
  return(directory_summaries)
}
