# R/export_utils.R - Utility functions for exporting data

#' Export Documented Elements for Fine-tuning
#'
#' Formats extracted code elements and their LLM-generated documentation
#' into a JSONL file suitable for fine-tuning language models.
#'
#' @param documented_elements A list of documented code elements. Each element 
#'        is expected to be a list containing at least \code{type}, \code{name}, 
#'        \code{signature}, \code{file_path}, \code{code_block}, and \code{description}.
#' @param output_filepath The full path (including filename) for the output JSONL file.
#' @param format The desired output format. Currently supports "jsonl_chat" and "jsonl_prompt_completion".
#' @param project_name Optional. The name of the project, to be included in system prompts.
#' @param verbose Logical. If TRUE, prints status messages.
#' @return Invisible NULL. Writes to a file as a side effect.
#' @export
#' @importFrom jsonlite toJSON
export_for_finetuning <- function(
    documented_elements, 
    output_filepath, 
    format = "jsonl_chat",
    project_name = NULL,
    verbose = TRUE
) {

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("The 'jsonlite' package is required but not installed. Please install it.")
  }

  if (is.null(documented_elements) || length(documented_elements) == 0) {
    if (verbose) cat("No documented elements provided. Nothing to export.\n")
    return(invisible(NULL))
  }

  # Ensure output directory exists
  output_dir <- dirname(output_filepath)
  if (!dir.exists(output_dir)) {
    if (verbose) cat(paste("Creating output directory:", output_dir, "\n"))
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  }

  con <- NULL # Initialize connection variable

  tryCatch({
    if (format == "jsonl_chat") {
      if (verbose) cat(paste0("Opening file for writing in 'jsonl_chat' format: ", output_filepath, "\n"))
      con <- file(output_filepath, "w", encoding = "UTF-8")
      on.exit({
        if (!is.null(con) && isOpen(con)) {
          if (verbose) cat("Closing file connection (on.exit)...\n")
          close(con)
        }
      }, add = TRUE) # Add = TRUE to not overwrite other on.exit calls

      for (i in seq_along(documented_elements)) {
        element <- documented_elements[[i]]
        
        # Validate required fields for this element
        required_fields <- c("type", "name", "signature", "file_path", "code_block", "description")
        missing_fields <- setdiff(required_fields, names(element))
        if (length(missing_fields) > 0) {
          if (verbose) {
            warning(paste0("Skipping element #", i, " (name: '", element$name %||% "NA",
                           "') due to missing fields: ", paste(missing_fields, collapse = ", "), "\n"))
          }
          next # Skip this element
        }

        system_prompt_content <- paste0(
          "You are an expert R programming assistant",
          if (!is.null(project_name) && nzchar(project_name)) paste0(" for the project '", project_name, "'") else "",
          ". Explain the following R code accurately and concisely."
        )
        
        user_prompt_content <- paste0(
          "Explain the R ", element$type, " named `", element$name, 
          "` with signature `", element$name, element$signature, 
          "` found in file `", element$file_path, 
          "`. Code:\n```r\n", 
          paste(element$code_block, collapse = "\n"), "\n```"
        )
        
        assistant_content <- element$description %||% ""

        json_data <- list(
          messages = list(
            list(role = "system", content = system_prompt_content),
            list(role = "user", content = user_prompt_content),
            list(role = "assistant", content = assistant_content)
          )
        )
        
        json_string <- jsonlite::toJSON(json_data, auto_unbox = TRUE, pretty = FALSE)
        writeLines(json_string, con)
      }
      if (verbose) cat(paste("Successfully exported", length(documented_elements), "elements to:", output_filepath, "\n"))

    } else if (format == "jsonl_prompt_completion") {
      if (verbose) cat(paste0("Opening file for writing in 'jsonl_prompt_completion' format: ", output_filepath, "\n"))
      con <- file(output_filepath, "w", encoding = "UTF-8")
      on.exit({
        if (!is.null(con) && isOpen(con)) {
          if (verbose) cat("Closing file connection (on.exit)...\n")
          close(con)
        }
      }, add = TRUE)

      for (i in seq_along(documented_elements)) {
        element <- documented_elements[[i]]

        required_fields <- c("type", "name", "code_block", "description") # Signature and file_path less critical for basic prompt
        missing_fields <- setdiff(required_fields, names(element))
        if (length(missing_fields) > 0) {
          if (verbose) {
            warning(paste0("Skipping element #", i, " (name: '", element$name %||% "NA",
                           "') for prompt_completion due to missing fields: ", paste(missing_fields, collapse = ", "), "\n"))
          }
          next 
        }
        
        # Construct a simpler prompt, assuming the system message is handled elsewhere or implicitly
        prompt_content <- paste0(
          "Explain the R ", element$type, " named `", element$name, "`.",
          if (!is.null(element$signature) && nzchar(element$signature)) paste0(" Signature: `", element$name, element$signature, "`.") else "",
          if (!is.null(element$file_path) && nzchar(element$file_path)) paste0(" File: `", element$file_path, "`.") else "",
          "\nCode:\n```r\n", 
          paste(element$code_block, collapse = "\n"), "\n```\n\nExplanation:"
        )
        
        completion_content <- element$description %||% ""

        json_data <- list(
          prompt = prompt_content,
          completion = completion_content
        )
        
        json_string <- jsonlite::toJSON(json_data, auto_unbox = TRUE, pretty = FALSE)
        writeLines(json_string, con)
      }
      if (verbose) cat(paste("Successfully exported", length(documented_elements), "elements to:", output_filepath, "\n"))

    } else {
      warning(paste("Invalid export format specified:", format, 
                    ". Supported formats are 'jsonl_chat' and 'jsonl_prompt_completion'. No file written."))
    }
  }, error = function(e) {
    if (verbose) cat(paste("Error during export:", e$message, "\n"))
    # Ensure connection is closed on error too, though on.exit should handle it.
    if (!is.null(con) && isOpen(con)) {
      close(con)
      con <- NULL # Nullify to prevent double-close by on.exit if it runs after error
    }
    stop(e) # Re-throw the error after logging
  }, finally = {
    if (!is.null(con) && isOpen(con)) {
      if (verbose) cat("Closing file connection (finally block)...\n")
      close(con)
    }
  })
  
  return(invisible(NULL))
}
