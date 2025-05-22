#' 加载并验证配置
#'
#' 从JSON文件加载并验证配置信息。
#' 检查必要的配置项是否存在，并设置默认值。
#'
#' @param config_path 配置文件路径
#' @return 加载并验证后的配置列表，验证失败则返回NULL
#' @keywords internal
load_and_validate_config <- function(config_path) {
  cat(paste("Attempting to load configuration from:", config_path, "\n"))
  if (!file.exists(config_path)) {
    stop(paste("Configuration file not found:", config_path))
    return(NULL)
  }
  
  config <- NULL
  tryCatch({
    config_content <- readLines(config_path, warn = FALSE)
    config <- jsonlite::fromJSON(paste(config_content, collapse="\n"))
    cat("Configuration file parsed successfully.\n")
  }, error = function(e) {
    stop(paste("Error parsing JSON configuration file:", e$message))
    return(NULL)
  })
  
  # Basic validation (presence of essential keys)
  required_keys <- c("code_source", "llm_settings", "output_dir", "output_filename_base")
  missing_keys <- setdiff(required_keys, names(config))
  
  if (length(missing_keys) > 0) {
    stop(paste("Missing required keys in configuration:", paste(missing_keys, collapse = ", ")))
    return(NULL)
  }
  
  # Validate code_source (either local_path or git_repo must be present)
  if (is.null(config$code_source$local_path) && is.null(config$code_source$git_repo)) {
    stop("Configuration error: code_source must contain either 'local_path' or 'git_repo'.")
    return(NULL)
  }
  
  # Validate llm_settings
  if (is.null(config$llm_settings$api_provider)) {
    warning("LLM API provider not specified in config, LLM features might be limited or use defaults.")
    # Set a default or handle appropriately if this is critical
  }
  
  # Set defaults for optional parameters if not present
  if (is.null(config$quarto_format)) {
    config$quarto_format <- "html"
    cat("Quarto format not specified, defaulting to 'html'.\n")
  }
  if (is.null(config$project_name)) {
    config$project_name <- "Untitled Project"
     cat("Project name not specified, defaulting to 'Untitled Project'.\n")
  }
  if (is.null(config$code_source$language_hints)) {
    config$code_source$language_hints <- list("R") # Default to R if not specified
  }
  
  cat("Configuration loaded and validated successfully.\n")
  return(config)
}
