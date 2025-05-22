#' 获取代码内容
#'
#' 从指定的源（本地路径或Git仓库）获取代码内容。
#' 返回一个列表，其中键是文件路径，值是文件内容字符串。
#'
#' @param code_source_config 代码源配置，包含local_path或git_repo
#' @return 代码内容列表，键为文件路径，值为文件内容
#' @keywords internal
acquire_code_content <- function(code_source_config) {
  code_details <- list()
  
  if (!is.null(code_source_config$local_path)) {
    local_path <- code_source_config$local_path
    cat(paste("Acquiring code from local path:", local_path, "\n"))
    
    if (!dir.exists(local_path)) {
      stop(paste("Local path specified in config does not exist:", local_path))
      return(list())
    }
    
    # List all files recursively (or based on config if more specific rules are added later)
    # For now, let's assume R files (.R, .r) are of primary interest for R-specific analysis
    # but we should probably fetch all text-based files for broader applicability
    # We can filter later in the analysis stage if needed.
    files_to_process <- list.files(local_path, recursive = TRUE, full.names = TRUE, no.. = TRUE)
    
    if (length(files_to_process) == 0) {
      cat(paste("No files found in local path:", local_path, "\n"))
      return(list())
    }
    
    for (file_path in files_to_process) {
      if (dir.exists(file_path)) next # Skip directories
      
      # Basic check for text files (can be improved)
      # For now, let's try to read all files and handle errors if they are binary
      tryCatch({
        # Ensure file is not too large to avoid memory issues (e.g., > 10MB)
        if (file.info(file_path)$size > 10 * 1024 * 1024) {
            cat(paste("Skipping large file ( > 10MB ):", file_path, "\n"))
            next
        }
        content <- paste(readLines(file_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
        # Use relative path from the provided local_path as the key for consistency
        relative_file_path <- sub(paste0("^", normalizePath(local_path), "/"), "", normalizePath(file_path))
        code_details[[relative_file_path]] <- content
      }, error = function(e) {
        cat(paste("Could not read file (possibly binary or encoding issue):", file_path, "-", e$message, "\n"))
      })
    }
    
  } else if (!is.null(code_source_config$git_repo)) {
    git_repo_url <- code_source_config$git_repo
    target_dir <- tempfile(pattern = "git_clone_")
    dir.create(target_dir)
    cat(paste("Cloning git repository:", git_repo_url, "to", target_dir, "\n"))
    
    # Ensure git2r is installed (should be handled by pak)
    if (!requireNamespace("git2r", quietly = TRUE)) {
      stop("Package git2r is not installed. Please install it to use git_repo feature.")
    }
    
    tryCatch({
      repo <- git2r::clone(git_repo_url, target_dir)
      cat("Repository cloned successfully.\n")
      
      # Now list files from the cloned directory
      files_to_process <- list.files(target_dir, recursive = TRUE, full.names = TRUE, no.. = TRUE)
      for (file_path in files_to_process) {
        if (dir.exists(file_path) || grepl("/\\.git/", file_path)) next # Skip directories and .git folder contents
        
        tryCatch({
          if (file.info(file_path)$size > 10 * 1024 * 1024) {
            cat(paste("Skipping large file ( > 10MB ):", file_path, "\n"))
            next
          }
          content <- paste(readLines(file_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
          relative_file_path <- sub(paste0("^", normalizePath(target_dir), "/"), "", normalizePath(file_path))
          code_details[[relative_file_path]] <- content
        }, error = function(e) {
          cat(paste("Could not read file from cloned repo:", file_path, "-", e$message, "\n"))
        })
      }
      # Clean up cloned directory
      unlink(target_dir, recursive = TRUE, force = TRUE)
      cat(paste("Cleaned up temporary clone directory:", target_dir, "\n"))
      
    }, error = function(e) {
      stop(paste("Error cloning or processing git repository:", e$message))
      if (dir.exists(target_dir)) unlink(target_dir, recursive = TRUE, force = TRUE)
      return(list())
    })
    
  } else {
    stop("Configuration error: code_source must specify either 'local_path' or 'git_repo'.")
    return(list())
  }
  
  cat(paste("Acquired content for", length(code_details), "files.\n"))
  return(code_details)
}
