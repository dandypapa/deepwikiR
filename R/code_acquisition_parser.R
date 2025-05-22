#' Acquire Code Content from Various Sources
#'
#' @description
#' Fetches code content from a specified source, which can be either a local directory
#' or a remote Git repository. It reads text files, excluding very large files and
#' specific patterns (like `.git` folder contents).
#'
#' @param code_source_config A list containing configuration for the code source.
#'   It must include either `local_path` (character string for a local directory)
#'   or `git_repo` (character string for a Git repository URL). The list can also
#'   contain optional `include_patterns` and `exclude_patterns` (though these are
#'   not explicitly used in the current file listing logic of this function but
#'   are part of a broader convention).
#'
#' @return A named list where keys are file paths (relative to the root of the
#'   source, e.g., "R/script.R") and values are character strings representing
#'   the content of these files. Returns an empty list if no files are found or
#'   if the source is inaccessible.
#'
#' @details
#' - For `local_path`, the function lists all files recursively.
#' - For `git_repo`, the function clones the repository into a temporary directory,
#'   reads the files, and then cleans up the temporary directory.
#' - Files larger than 10MB are skipped.
#' - Errors during file reading (e.g., for binary files) are caught and reported,
#'   but do not stop the processing of other files.
#'
#' @examples
#' \dontrun{
#' # Example for a local path
#' local_config <- list(local_path = "./sample_code_project")
#' # Ensure "./sample_code_project" exists and has files.
#' # code_content_local <- acquire_code_content(local_config)
#' # print(names(code_content_local))
#'
#' # Example for a Git repository
#' # git_config <- list(git_repo = "https://github.com/user/my_r_project.git")
#' # code_content_git <- acquire_code_content(git_config)
#' # print(names(code_content_git))
#' }
#'
#' @importFrom utils file.info
#' @importFrom git2r clone
#' @noRd
acquire_code_content <- function(code_source_config) {
  code_details <- list()
  
  if (!is.null(code_source_config$local_path)) {
    local_path <- code_source_config$local_path
    cat(paste("Acquiring code from local path:", local_path, "\n"))
    
    if (!dir.exists(local_path)) {
      stop(paste("Local path specified in config does not exist:", local_path))
      return(list())
    }
    
    files_to_process <- list.files(local_path, recursive = TRUE, full.names = TRUE, no.. = TRUE)
    
    if (length(files_to_process) == 0) {
      cat(paste("No files found in local path:", local_path, "\n"))
      return(list())
    }
    
    for (file_path in files_to_process) {
      if (dir.exists(file_path)) next 
      
      tryCatch({
        if (utils::file.info(file_path)$size > 10 * 1024 * 1024) {
            cat(paste("Skipping large file ( > 10MB ):", file_path, "\n"))
            next
        }
        content <- paste(readLines(file_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
        relative_file_path <- sub(paste0("^", normalizePath(local_path), .Platform$file.sep), "", normalizePath(file_path))
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
    
    if (!requireNamespace("git2r", quietly = TRUE)) {
      stop("Package git2r is not installed. Please install it to use git_repo feature.")
    }
    
    tryCatch({
      repo <- git2r::clone(git_repo_url, target_dir)
      cat("Repository cloned successfully.\n")
      
      files_to_process <- list.files(target_dir, recursive = TRUE, full.names = TRUE, no.. = TRUE)
      for (file_path in files_to_process) {
        if (dir.exists(file_path) || grepl(paste0(.Platform$file.sep, ".git", .Platform$file.sep), file_path, fixed = TRUE)) next 
        
        tryCatch({
          if (utils::file.info(file_path)$size > 10 * 1024 * 1024) {
            cat(paste("Skipping large file ( > 10MB ):", file_path, "\n"))
            next
          }
          content <- paste(readLines(file_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
          relative_file_path <- sub(paste0("^", normalizePath(target_dir), .Platform$file.sep), "", normalizePath(file_path))
          code_details[[relative_file_path]] <- content
        }, error = function(e) {
          cat(paste("Could not read file from cloned repo:", file_path, "-", e$message, "\n"))
        })
      }
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
