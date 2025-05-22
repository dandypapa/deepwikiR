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
  .total_tokens_consumed <<- .total_tokens_consumed + tokens
}

# Function to generate documentation for code elements using an LLM
# Takes a list of extracted code elements and LLM settings from config
# Returns the list of elements, augmented with LLM-generated descriptions
generate_docs_with_llm <- function(extracted_elements, llm_settings, language_hints) {
  if (is.null(llm_settings) || is.null(llm_settings$api_provider)) {
    cat("LLM settings or API provider not specified. Skipping LLM documentation generation.\n")
    # Return elements with empty descriptions
    for (i in seq_along(extracted_elements)) {
      if (is.null(extracted_elements[[i]]$description) || extracted_elements[[i]]$description == "") {
        extracted_elements[[i]]$description <- "(LLM description not generated due to missing configuration)"
      }
    }
    return(extracted_elements)
  }

  cat(paste("Using LLM API Provider:", llm_settings$api_provider, "\n"))

  # Initialize tidyllm client based on provider
  # This part needs to be robust to handle different providers and their auth
  # For SiliconCloud (OpenAI compatible)
  api_key <- Sys.getenv(llm_settings$api_key_env_var %||% "SILICONCLOUD_API_KEY")
  if (api_key == "") {
    warning(paste("API key environment variable", llm_settings$api_key_env_var %||% "SILICONCLOUD_API_KEY", "not set. LLM calls will likely fail."))
  }

  # Set up the tidyllm model parameters
  # The model string should be in the format expected by tidyllm for the specific provider
  # For OpenAI-compatible APIs, it might just be the model name.
  # tidyllm uses `openai_api_key` and `openai_api_base` for custom OpenAI-like services.
  
  # Store original env vars to restore later
  orig_openai_api_key <- Sys.getenv("OPENAI_API_KEY")
  orig_openai_api_base <- Sys.getenv("OPENAI_API_BASE")
  
  Sys.setenv(OPENAI_API_KEY = api_key)
  if (!is.null(llm_settings$api_base_url)) {
    Sys.setenv(OPENAI_API_BASE = llm_settings$api_base_url)
  }
  
  llm_model_name <- llm_settings$model %||% "gpt-3.5-turbo" # Default model if not specified
  max_tokens_per_request <- llm_settings$max_tokens_per_request %||% 150
  temperature <- llm_settings$temperature %||% 0.2

  # Loop through each code element and generate a description
  for (i in seq_along(extracted_elements)) {
    element <- extracted_elements[[i]]
    cat(paste("Generating docs for element:", element$name, "in file:", element$file_path, "\n"))

    # Construct a prompt for the LLM
    # Language hint can be used here, e.g. "R function"
    lang_hint <- paste(language_hints, collapse=", ")
    prompt <- paste0(
      "You are an expert programmer tasked with generating concise documentation for code elements.",
      "\nFor the following ", lang_hint, " ", element$type, " named `", element$name, "`",
      " with signature `", element$name, element$signature, "`",
      " found in file `", element$file_path, "`,",
      "\nCode block:\n```", tolower(language_hints[[1]] %||% "r"), "\n",
      paste(element$code_block, collapse="\n"),
      "\n```\n",
      "\nPlease provide a brief, one-paragraph explanation of its purpose and functionality. ",
      "Focus on what it does, its main inputs, and its primary outputs or side effects. ",
      "Do not repeat the code block or the signature in your explanation. Be very concise."
    )
    
    # Use tidyllm::complete for OpenAI-compatible APIs
    # Ensure the model string is correctly formatted for tidyllm if it needs provider prefix
    # For direct OpenAI-compatible, just model name should work if OPENAI_API_BASE is set.
    tryCatch({
      response <- tidyllm::complete(
        prompt = prompt,
        model = llm_model_name, 
        max_tokens = max_tokens_per_request,
        temperature = temperature
        # system_prompt = "You are a helpful assistant that writes code documentation." # Optional
      )
      
      # Assuming response is a character string (the completion)
      # tidyllm::complete usually returns a tibble, so we need to extract the text
      if (is.data.frame(response) && "completion" %in% names(response) && nrow(response) > 0) {
        generated_text <- response$completion[1]
        extracted_elements[[i]]$description <- stringr::str_trim(generated_text)
        
        # Estimate token usage (very rough for now, tidyllm might provide this)
        # OpenAI typically counts prompt + completion. A simple rule of thumb: 1 token ~ 4 chars.
        prompt_tokens <- nchar(prompt) / 4
        completion_tokens <- nchar(generated_text) / 4
        increment_tokens_consumed(prompt_tokens + completion_tokens)
        cat(paste("Generated description for", element$name, ". Approx tokens for this request:", round(prompt_tokens + completion_tokens), "\n"))
        
      } else {
        warning(paste("LLM response for", element$name, "was not in the expected format or was empty."))
        extracted_elements[[i]]$description <- "(LLM description generation failed or returned empty)"
      }
      
      # Respect rate limits if any (simple delay)
      if (!is.null(llm_settings$rate_limit_delay_sec) && llm_settings$rate_limit_delay_sec > 0) {
        Sys.sleep(llm_settings$rate_limit_delay_sec)
      }
      
    }, error = function(e) {
      warning(paste("Error calling LLM for element", element$name, ":", e$message))
      extracted_elements[[i]]$description <- "(LLM description generation failed due to an error)"
    })
  }
  
  # Restore original env vars
  Sys.setenv(OPENAI_API_KEY = orig_openai_api_key)
  Sys.setenv(OPENAI_API_BASE = orig_openai_api_base)
  
  return(extracted_elements)
}

