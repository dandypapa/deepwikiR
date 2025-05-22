# /home/ubuntu/autodoc_tool/R/input_config_handler.R

#' @importFrom jsonlite fromJSON
NULL

#' Deep Merge Settings Lists
#'
#' @title Deeply Merge Two Lists
#' @description Recursively merges two lists (e.g., configuration settings).
#' For elements that are themselves named lists, the function merges them recursively.
#' For all other elements (or if one corresponding element is not a list), the value
#' from `project_specifics` takes precedence.
#'
#' @param global_defaults A list. The base list of default settings.
#' @param project_specifics A list. The list of settings that will override
#'        and augment `global_defaults`.
#'
#' @return A list. The merged list.
#'
#' @examples
#' \dontrun{
#' defaults <- list(a = 1, b = list(x = 10, y = 20), d = 4)
#' specifics <- list(b = list(y = 25, z = 30), c = 3)
#' merged <- deep_merge_settings(defaults, specifics)
#' # merged would be: list(a = 1, b = list(x = 10, y = 25, z = 30), d = 4, c = 3)
#' }
#' @noRd
deep_merge_settings <- function(global_defaults, project_specifics) {
  if (is.null(project_specifics)) return(global_defaults)
  if (is.null(global_defaults)) return(project_specifics)

  merged <- global_defaults
  for (name in names(project_specifics)) {
    if (is.list(project_specifics[[name]]) && !is.null(names(project_specifics[[name]])) && 
        is.list(global_defaults[[name]]) && !is.null(names(global_defaults[[name]]))) {
      merged[[name]] <- deep_merge_settings(global_defaults[[name]], project_specifics[[name]])
    } else {
      merged[[name]] <- project_specifics[[name]]
    }
  }
  return(merged)
}

#' Check if Path is Absolute
#'
#' @title Determine if a File Path is Absolute
#' @description Checks if a given file path string appears to be an absolute path.
#'
#' @param path Character string. The file path to check.
#'
#' @return Logical. `TRUE` if the path starts with `/`, `~`, or a Windows-style
#'         drive letter (e.g., `C:/`), `FALSE` otherwise.
#'
#' @details This provides a basic cross-platform check. It may not cover all
#'          edge cases for absolute paths on all operating systems.
#'
#' @examples
#' \dontrun{
#' is_absolute_path("/usr/bin/R") # TRUE
#' is_absolute_path("~/documents") # TRUE
#' is_absolute_path("C:/Windows") # TRUE
#' is_absolute_path("./relative/path") # FALSE
#' is_absolute_path("relative/path") # FALSE
#' }
#' @noRd
is_absolute_path <- function(path) {
  grepl("^(/|~|[A-Za-z]:[/\\\\])", path)
}

#' Load and Validate Multi-Project Configuration
#'
#' @title Load, Validate, and Process Multi-Project Configuration
#' @description This function loads a project configuration from a JSON file,
#' validates its structure and content, merges project-specific settings with
#' global defaults, resolves paths, and sets defaults for optional parameters.
#' It is designed to handle a configuration file that defines one or more projects.
#'
#' @param config_path Character string. The path to the JSON configuration file.
#'
#' @return A list containing two main elements:
#'   \item{global_settings}{A list of settings that apply globally unless
#'         overridden by a specific project.}
#'   \item{projects}{A list of processed and validated configuration objects,
#'         one for each project defined in the configuration file.}
#' The function will stop with an error if critical validation checks fail.
#'
#' @details
#' The function performs several key operations:
#' - Parses the JSON configuration file.
#' - Validates the top-level structure (e.g., presence of a `projects` array).
#' - For each project:
#'   - Merges its `llm_settings` with `global_settings$default_llm_settings`.
#'   - Resolves its `output_dir` (making it absolute if `global_settings$output_dir_root` is provided and the project's path is relative).
#'   - Validates required keys (`project_name`, `code_source`, `output_dir`, `output_filename_base`).
#'   - Validates `code_source` (must have `local_path` or `git_repo`).
#'   - Validates `llm_settings` (e.g., presence of `provider` if LLM features are used).
#'   - Sets default values for `quarto_format` ("html") and `code_source$language_hints` (list("unknown")).
#'   - Validates `export_finetuning_data` settings, applying defaults if necessary.
#'
#' @examples
#' \dontrun{
#' # Assuming "inst/config/sample_config.json" is a valid multi-project config:
#' # config_data <- load_and_validate_config("inst/config/sample_config.json")
#' # print(str(config_data, max.level = 2))
#' }
#' @importFrom jsonlite fromJSON
#' @noRd 
load_and_validate_config <- function(config_path) {
  cat(paste("Attempting to load multi-project configuration from:", config_path, "\n"))
  if (!file.exists(config_path)) {
    stop(paste("Configuration file not found:", config_path))
  }
  
  full_config <- NULL
  tryCatch({
    config_content <- readLines(config_path, warn = FALSE, encoding="UTF-8")
    full_config <- jsonlite::fromJSON(paste(config_content, collapse="\n"), simplifyVector = FALSE)
    cat("Multi-project configuration file parsed successfully.\n")
  }, error = function(e) {
    stop(paste("Error parsing JSON configuration file:", e$message))
  })

  if (is.null(full_config$projects) || !is.list(full_config$projects) || length(full_config$projects) == 0) {
    stop("Configuration error: 'projects' array is missing or empty.")
  }

  global_settings <- full_config$global_settings %||% list()
  default_llm_settings <- global_settings$default_llm_settings %||% list()
  output_dir_root <- global_settings$output_dir_root %||% NULL

  processed_projects <- list()

  for (i in seq_along(full_config$projects)) {
    project_config <- full_config$projects[[i]]
    project_name_for_log <- project_config$project_name %||% paste("Unnamed Project #", i)
    cat(paste("Processing project:", project_name_for_log, "\n"))

    if (!is.null(project_config$llm_settings) || length(default_llm_settings) > 0) {
        project_config$llm_settings <- deep_merge_settings(default_llm_settings, project_config$llm_settings)
    }
    
    if (is.null(project_config$output_dir)) {
        if (!is.null(output_dir_root)) {
            project_config$output_dir <- file.path(output_dir_root, project_name_for_log)
            cat(paste("Project output_dir not set, defaulting to global_root + project_name:", project_config$output_dir, "\n"))
        } else {
            project_config$output_dir <- project_name_for_log 
            cat(paste("Project output_dir not set and no global_root, defaulting to project_name relative to CWD:", project_config$output_dir, "\n"))
        }
    } else {
        if (!is_absolute_path(project_config$output_dir) && !is.null(output_dir_root)) {
            project_config$output_dir <- file.path(output_dir_root, project_config$output_dir)
            cat(paste("Resolved relative project output_dir with global_root:", project_config$output_dir, "\n"))
        }
    }
    project_config$output_dir <- normalizePath(project_config$output_dir, mustWork = FALSE)

    required_project_keys <- c("project_name", "code_source", "output_dir", "output_filename_base")
    missing_project_keys <- setdiff(required_project_keys, names(project_config))
    if (length(missing_project_keys) > 0) {
      stop(paste("Project", project_name_for_log, "is missing required keys:", paste(missing_project_keys, collapse = ", ")))
    }

    if (is.null(project_config$code_source$local_path) && is.null(project_config$code_source$git_repo)) {
      stop(paste("Project", project_name_for_log, "error: code_source must contain either 'local_path' or 'git_repo'."))
    }
    
    if (!is.null(project_config$llm_settings) && is.null(project_config$llm_settings$provider)) {
       warning(paste("Project", project_name_for_log, ": LLM settings are present but API provider not specified. LLM features might fail or use defaults if available at call site."))
    } else if (is.null(project_config$llm_settings)) {
        cat(paste("Project", project_name_for_log, ": No LLM settings specified (neither project-specific nor global defaults). LLM-based documentation will be skipped.\n"))
        project_config$llm_settings <- list(provider = NULL) 
    }

    if (is.null(project_config$quarto_format)) {
      project_config$quarto_format <- "html" 
      cat(paste("Project", project_name_for_log, ": Quarto format not specified, defaulting to 'html'.\n"))
    }
    if (is.null(project_config$code_source$language_hints)) {
      project_config$code_source$language_hints <- list("unknown") 
      cat(paste("Project", project_name_for_log, ": code_source$language_hints not specified, defaulting to 'unknown'.\n"))
    }
     if (is.null(safe_get_nested(project_config, "llm_settings", "api_details", "api_key_env_var")) && 
         !is.null(safe_get_nested(project_config, "llm_settings", "provider"))) {
        warning(paste("Project", project_name_for_log, ": llm_settings$api_details$api_key_env_var is not set. LLM calls may fail if an API key is required by the provider ('", project_config$llm_settings$provider, "')."))
    }

    export_settings <- project_config$export_finetuning_data %||% list(enabled = FALSE) 

    if (!is.list(export_settings)) { 
        warning(paste("Project", project_name_for_log, ": 'export_finetuning_data' is not a list. Defaulting to disabled."))
        export_settings <- list(enabled = FALSE)
    }
    
    if (is.null(export_settings$enabled)) {
        if (length(export_settings) > 0 && !is.null(names(export_settings))) { 
             warning(paste("Project", project_name_for_log, ": 'export_finetuning_data$enabled' is missing. Defaulting to FALSE."))
        }
        export_settings$enabled <- FALSE 
    } else if (!is.logical(export_settings$enabled) || length(export_settings$enabled) != 1) {
        warning(paste("Project", project_name_for_log, ": 'export_finetuning_data$enabled' is not a single boolean. Defaulting to FALSE."))
        export_settings$enabled <- FALSE
    }

    if (export_settings$enabled) {
      if (is.null(export_settings$output_filename) || !is.character(export_settings$output_filename) || 
          length(export_settings$output_filename) != 1 || !nzchar(trimws(export_settings$output_filename))) {
        stop(paste("Project", project_name_for_log, ": 'export_finetuning_data$output_filename' must be a non-empty string when enabled is true."))
      }
      
      supported_formats <- c("jsonl_chat", "jsonl_prompt_completion")
      if (is.null(export_settings$format)) {
        export_settings$format <- "jsonl_chat"
        cat(paste("Project", project_name_for_log, ": 'export_finetuning_data$format' not specified, defaulting to 'jsonl_chat'.\n"))
      } else if (!is.character(export_settings$format) || length(export_settings$format) != 1 || 
                 !(export_settings$format %in% supported_formats)) {
        warning(paste("Project", project_name_for_log, ": Invalid 'export_finetuning_data$format' specified ('", 
                      export_settings$format, "'). Supported formats are: ", 
                      paste(supported_formats, collapse=", "), ". Defaulting to 'jsonl_chat'."))
        export_settings$format <- "jsonl_chat"
      }
    } else {
      export_settings$output_filename <- NULL
      export_settings$format <- NULL
    }
    project_config$export_finetuning_data <- export_settings

    processed_projects[[length(processed_projects) + 1]] <- project_config
    cat(paste("Successfully processed and validated project:", project_name_for_log, "\n"))
  }
  
  cat("All projects in configuration processed and validated successfully.\n")
  return(list(global_settings = global_settings, projects = processed_projects))
}
