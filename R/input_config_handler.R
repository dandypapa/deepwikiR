# Helper function to deeply merge two lists (e.g., settings objects)
# project_specific settings override global_defaults
deep_merge_settings <- function(global_defaults, project_specifics) {
  if (is.null(project_specifics)) return(global_defaults)
  if (is.null(global_defaults)) return(project_specifics)

  merged <- global_defaults
  for (name in names(project_specifics)) {
    if (is.list(project_specifics[[name]]) && !is.null(names(project_specifics[[name]])) && 
        is.list(global_defaults[[name]]) && !is.null(names(global_defaults[[name]]))) {
      # If both are named lists, recurse
      merged[[name]] <- deep_merge_settings(global_defaults[[name]], project_specifics[[name]])
    } else {
      # Otherwise, project_specifics takes precedence
      merged[[name]] <- project_specifics[[name]]
    }
  }
  return(merged)
}

# Helper function to check if a path is absolute
is_absolute_path <- function(path) {
  # Simple check: starts with / or ~ or X:/ or X:\ (for Windows)
  # This might need to be more robust for all OS cases.
  grepl("^(/|~|[A-Za-z]:[/\\\\])", path)
}

#' 加载并验证多项目配置
#'
#' 从JSON文件加载并验证多项目配置信息。
#' 它处理全局设置并将它们与每个项目的特定设置合并。
#'
#' @param config_path 配置文件路径
#' @return 一个列表，包含 `global_settings` 和处理过的 `projects` 列表。
#'         验证失败则停止执行并报错。
#' @keywords internal
load_and_validate_config <- function(config_path) {
  cat(paste("Attempting to load multi-project configuration from:", config_path, "\n"))
  if (!file.exists(config_path)) {
    stop(paste("Configuration file not found:", config_path))
  }
  
  full_config <- NULL
  tryCatch({
    config_content <- readLines(config_path, warn = FALSE)
    # Use simplifyVector = FALSE to preserve list structure for single-element arrays if they occur
    full_config <- jsonlite::fromJSON(paste(config_content, collapse="\n"), simplifyVector = FALSE)
    cat("Multi-project configuration file parsed successfully.\n")
  }, error = function(e) {
    stop(paste("Error parsing JSON configuration file:", e$message))
  })

  # Validate top-level structure
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

    # --- Merge LLM settings ---
    if (!is.null(project_config$llm_settings) || length(default_llm_settings) > 0) {
        project_config$llm_settings <- deep_merge_settings(default_llm_settings, project_config$llm_settings)
    }
    
    # --- Resolve output_dir ---
    if (is.null(project_config$output_dir)) {
        if (!is.null(output_dir_root)) {
            project_config$output_dir <- file.path(output_dir_root, project_name_for_log)
            cat(paste("Project output_dir not set, defaulting to global_root + project_name:", project_config$output_dir, "\n"))
        } else {
            project_config$output_dir <- project_name_for_log # Relative to working dir
            cat(paste("Project output_dir not set and no global_root, defaulting to project_name relative to CWD:", project_config$output_dir, "\n"))
        }
    } else {
        if (!is_absolute_path(project_config$output_dir) && !is.null(output_dir_root)) {
            project_config$output_dir <- file.path(output_dir_root, project_config$output_dir)
            cat(paste("Resolved relative project output_dir with global_root:", project_config$output_dir, "\n"))
        }
        # If absolute, it remains as is. If relative and no output_dir_root, it's relative to CWD.
    }
    # Ensure output_dir is normalized
    project_config$output_dir <- normalizePath(project_config$output_dir, mustWork = FALSE)


    # --- Validate individual project config ---
    # Most of these checks are essential for the tool to function.
    required_project_keys <- c("project_name", "code_source", "output_dir", "output_filename_base")
    missing_project_keys <- setdiff(required_project_keys, names(project_config))
    if (length(missing_project_keys) > 0) {
      stop(paste("Project", project_name_for_log, "is missing required keys:", paste(missing_project_keys, collapse = ", ")))
    }

    if (is.null(project_config$code_source$local_path) && is.null(project_config$code_source$git_repo)) {
      stop(paste("Project", project_name_for_log, "error: code_source must contain either 'local_path' or 'git_repo'."))
    }
    
    # Validate llm_settings if they are present and provider is specified
    if (!is.null(project_config$llm_settings) && is.null(project_config$llm_settings$provider)) {
       # This can be a warning if LLM use is optional for a project, or an error if not.
       # For now, let's make it a warning if llm_settings exists but provider is missing.
       # If llm_settings is entirely missing, it implies no LLM for this project (unless defaults apply fully)
       warning(paste("Project", project_name_for_log, ": LLM settings are present but API provider not specified. LLM features might fail or use defaults if available at call site."))
    } else if (is.null(project_config$llm_settings)) {
        cat(paste("Project", project_name_for_log, ": No LLM settings specified (neither project-specific nor global defaults). LLM-based documentation will be skipped.\n"))
        # Create an empty llm_settings object to avoid errors later if parts of the code expect it
        project_config$llm_settings <- list(provider = NULL) 
    }


    # Set defaults for optional parameters if not present after merge
    if (is.null(project_config$quarto_format)) {
      project_config$quarto_format <- "html" # Default Quarto format
      cat(paste("Project", project_name_for_log, ": Quarto format not specified, defaulting to 'html'.\n"))
    }
    if (is.null(project_config$code_source$language_hints)) {
      # Default language_hints, can be an empty list or a generic hint
      project_config$code_source$language_hints <- list("unknown") 
      cat(paste("Project", project_name_for_log, ": code_source$language_hints not specified, defaulting to 'unknown'.\n"))
    }
     if (is.null(project_config$llm_settings$api_details$api_key_env_var) && !is.null(project_config$llm_settings$provider)) {
        warning(paste("Project", project_name_for_log, ": llm_settings$api_details$api_key_env_var is not set. LLM calls may fail if an API key is required by the provider ('", project_config$llm_settings$provider, "')."))
    }

    # --- Validate and set defaults for export_finetuning_data ---
    export_settings <- project_config$export_finetuning_data %||% list(enabled = FALSE) # Default to disabled if section is missing

    if (!is.list(export_settings)) { # Handles cases where it's not a list or NULL
        warning(paste("Project", project_name_for_log, ": 'export_finetuning_data' is not a list. Defaulting to disabled."))
        export_settings <- list(enabled = FALSE)
    }
    
    # Validate 'enabled' field or default it
    if (is.null(export_settings$enabled)) {
        if (length(export_settings) > 0 && !is.null(names(export_settings))) { # If section exists but 'enabled' is missing
             warning(paste("Project", project_name_for_log, ": 'export_finetuning_data$enabled' is missing. Defaulting to FALSE."))
        }
        export_settings$enabled <- FALSE # Default to FALSE
    } else if (!is.logical(export_settings$enabled) || length(export_settings$enabled) != 1) {
        warning(paste("Project", project_name_for_log, ": 'export_finetuning_data$enabled' is not a single boolean. Defaulting to FALSE."))
        export_settings$enabled <- FALSE
    }

    if (export_settings$enabled) {
      # Validate 'output_filename'
      if (is.null(export_settings$output_filename) || !is.character(export_settings$output_filename) || 
          length(export_settings$output_filename) != 1 || !nzchar(trimws(export_settings$output_filename))) {
        stop(paste("Project", project_name_for_log, ": 'export_finetuning_data$output_filename' must be a non-empty string when enabled is true."))
      }
      
      # Validate 'format' or default it
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
      # If not enabled, ensure other fields are NULL or not problematic
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
