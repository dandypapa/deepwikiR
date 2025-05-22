# 主函数文件，提供包的主要功能接口

#' 生成单个项目的代码仓库文档
#'
#' 分析指定项目的代码仓库并生成文档。
#' 此函数由 `run_cli` 在处理多项目配置时为每个项目调用。
#'
#' @param project_config 单个项目的完整配置对象。
#' @param llm_api_key （可选）用于覆盖此项目环境变量的LLM API密钥。
#' @param verbose 是否显示详细日志（默认为TRUE）。
#' @return 生成文档的主文件路径。
#' @export # Still export if direct single project processing is useful
#' @examples
#' \dontrun{
#' # This function is typically called by run_cli after loading a multi-project config.
#' # For direct use, you would need a fully resolved project_config object:
#' # sample_project_conf <- list(
#' #   project_name = "MyProject",
#' #   code_source = list(local_path = "./my_project_code", language_hints = list("R")),
#' #   output_dir = "output/MyProjectDocs",
#' #   output_filename_base = "my_project_docs",
#' #   quarto_format = "html",
#' #   llm_settings = list(provider = "openai", model = "gpt-3.5-turbo", 
#' #                       api_details=list(api_key_env_var="OPENAI_API_KEY"))
#' # )
#' # generate_repo_docs(sample_project_conf, verbose = TRUE)
#' }
generate_repo_docs <- function(project_config, llm_api_key = NULL, verbose = TRUE) {
  
  # project_config is assumed to be fully resolved and validated by load_and_validate_config
  
  if (verbose) cat(paste0("--- Starting documentation generation for project: ", project_config$project_name, " ---\n"))

  # 设置API密钥（如果指定）
  # This overrides any environment variable specifically for this project's LLM calls.
  # The actual environment variable name is within project_config$llm_settings$api_details$api_key_env_var
  if (!is.null(llm_api_key) && 
      !is.null(project_config$llm_settings) &&
      !is.null(project_config$llm_settings$api_details$api_key_env_var) &&
      project_config$llm_settings$api_details$api_key_env_var != "") {
    
    api_key_env_var_name <- project_config$llm_settings$api_details$api_key_env_var
    orig_api_key_value <- Sys.getenv(api_key_env_var_name, unset = NA) # NA to distinguish from ""
    
    env_vars_to_set <- list()
    env_vars_to_set[[api_key_env_var_name]] <- llm_api_key
    do.call(Sys.setenv, env_vars_to_set)
    
    # Ensure original environment variable is restored on exit
    on.exit({
      if (verbose) cat(paste("Restoring original API key for env var:", api_key_env_var_name, "\n"))
      env_vars_to_restore <- list()
      if (is.na(orig_api_key_value)) { # Was originally unset
        Sys.unsetenv(api_key_env_var_name)
      } else {
        env_vars_to_restore[[api_key_env_var_name]] <- orig_api_key_value
        do.call(Sys.setenv, env_vars_to_restore)
      }
    }, add = TRUE) # add = TRUE is important if other on.exit calls exist
    
    if (verbose) cat(paste("CLI API key override applied for env var:", api_key_env_var_name, "\n"))
  }
  
  # 1. 获取代码内容
  if (verbose) cat("Acquiring code content...\n")
  code_details <- acquire_code_content(project_config$code_source)
  
  if (length(code_details) == 0) {
    warning(paste("No code files found for project:", project_config$project_name, "- skipping documentation generation for this project."))
    return(NULL) # Or throw an error if this is critical
  }
  
  if (verbose) cat(paste("Found", length(code_details), "files for project", project_config$project_name, "\n"))
  
  # 确定项目基础路径
  project_base_path <- NULL
  if (!is.null(project_config$code_source$local_path)) {
    project_base_path <- normalizePath(project_config$code_source$local_path, mustWork = FALSE)
  } else if (!is.null(project_config$code_source$git_repo)) {
    # If it's a git repo, it would have been cloned to a temp path by acquire_code_content
    # and that temp path should be in project_config$code_source$local_path
    # This part might need refinement if acquire_code_content doesn't update local_path
    project_base_path <- getwd() # Fallback, ideally acquire_code_content provides the path
    warning("Git repo path resolution might need refinement in generate_repo_docs.")
  } else {
     stop(paste("No local_path or git_repo specified in code_source for project:", project_config$project_name))
  }
  
  # 2. 分析代码结构
  if (verbose) cat("Analyzing code structure...\n")
  analysis_result <- analyze_r_repository_code(code_details, project_base_path)
  extracted_elements <- analysis_result$extracted_elements
  call_graph_data <- analysis_result$call_graph
  directory_contents <- analysis_result$directory_contents # Extract directory_contents
  
  if (verbose) {
    cat(paste("Found", length(extracted_elements), "code elements.\n"))
    cat(paste("Call graph contains", length(call_graph_data$nodes), "nodes and", 
              length(call_graph_data$edges), "edges.\n"))
    if (!is.null(directory_contents)) {
        cat(paste("Found directory content information for", length(directory_contents), "directories.\n"))
    }
  }
  
  # 3. 使用LLM生成文档 (for individual elements)
  llm_generated_docs <- list() # Keep this name for consistency in analysis_output_data
  if (length(extracted_elements) > 0) {
    if (!is.null(project_config$llm_settings) && !is.null(project_config$llm_settings$provider)) {
      if (verbose) cat("Generating documentation for individual code elements using LLM...\n")
      # The generate_docs_with_llm function handles API key retrieval from env vars
      # which might have been set by the override logic above.
      llm_generated_docs <- generate_docs_with_llm(
        extracted_elements, 
        project_config$llm_settings, 
        project_config$code_source$language_hints
      )
    } else {
      if (verbose) cat("LLM settings or provider not specified. Skipping LLM documentation generation for elements.\n")
      # Populate with placeholders if LLM is skipped
      llm_generated_docs <- lapply(extracted_elements, function(el) {
        el$description <- "(LLM documentation generation for element skipped due to missing configuration)"
        return(el)
      })
    }
  }

  # 3b. 使用LLM生成目录摘要
  directory_summaries <- NULL # Initialize
  if (!is.null(directory_contents) && length(directory_contents) > 0) {
    if (!is.null(project_config$llm_settings) && !is.null(project_config$llm_settings$provider)) {
      if (verbose) cat("Generating directory summaries with LLM...\n")
      directory_summaries <- generate_directory_summaries_with_llm(
        directory_contents = directory_contents,
        code_details = code_details, 
        llm_generated_docs = llm_generated_docs, # Use the docs generated in the previous step
        llm_config = project_config$llm_settings, 
        language_hints = project_config$code_source$language_hints,
        project_name = project_config$project_name,
        verbose = verbose
      )
      if (verbose) cat(paste("Generated", length(directory_summaries), "directory summaries.\n"))
    } else {
      if (verbose) cat("LLM settings or provider not specified. Skipping directory summary generation.\n")
      # Optionally create placeholder summaries
      directory_summaries <- lapply(names(directory_contents), function(dir_name) {
        "(LLM summary generation for directory skipped due to missing configuration)"
      })
      names(directory_summaries) <- names(directory_contents)
    }
  } else {
    if (verbose) cat("Skipping directory summary generation as no directory contents were found.\n")
  }

  # 3c. 导出用于微调的数据 (Export for fine-tuning)
  export_settings <- project_config$export_finetuning_data
  if (!is.null(export_settings) && is.list(export_settings) && isTRUE(export_settings$enabled)) {
    if (length(llm_generated_docs) > 0) {
      output_filepath <- file.path(project_config$output_dir, export_settings$output_filename)
      if (verbose) {
        cat(paste("Exporting data for fine-tuning to:", output_filepath, 
                  "in format:", export_settings$format, "\n"))
      }
      
      tryCatch({
        # Ensure R/export_utils.R is sourced if not running as a package
        if (!exists("export_for_finetuning", mode = "function")) {
            utils_path <- "R/export_utils.R" # Adjust if path differs, e.g. from package root
            if (file.exists(utils_path)) {
                if (verbose) cat(paste("Sourcing export_utils.R from:", utils_path, "\n"))
                source(utils_path, local = TRUE) # Source into local environment if preferred
            } else {
                warning("R/export_utils.R not found. Cannot export fine-tuning data.")
            }
        }

        if (exists("export_for_finetuning", mode = "function")) {
            export_for_finetuning(
              documented_elements = llm_generated_docs,
              output_filepath = output_filepath,
              format = export_settings$format,
              project_name = project_config$project_name,
              verbose = verbose
            )
        } else {
            warning("export_for_finetuning function not found after attempting to source. Skipping export.")
        }
      }, error = function(e) {
        warning(paste("Failed to export fine-tuning data for project", project_config$project_name, ":", e$message))
      })
    } else {
      if (verbose) cat("No documented elements available to export for fine-tuning.\n")
    }
  } else {
    if (verbose) cat("Fine-tuning data export is disabled for this project.\n")
  }
  
  # 4. 准备Quarto内容并渲染
  if (verbose) cat("Preparing Quarto content and rendering...\n")
  
  qmd_document_sections <- list()
  # Use llm_generated_docs as it contains the descriptions (either from LLM or placeholder)
  if (length(llm_generated_docs) > 0) { 
    for (doc_item in llm_generated_docs) {
      # Use 'description' which is populated by LLM or placeholder
      desc_text <- doc_item$description %||% "(No description generated)"
      
      section_content <- paste0(
        "### `", doc_item$name, "`\n\n",
        "**Type:** ", doc_item$type, "\n\n",
        "**Signature:** `", doc_item$name, doc_item$signature, "`\n\n",
        "**File:** `", doc_item$file_path, "`\n\n", # file_path is relative to project_base_path
        "**Description:**\n", desc_text, "\n\n",
        "**Code:**\n",
        "```", તોlower(project_config$code_source$language_hints[[1]] %||% "code"), "\n",
        paste(doc_item$code_block, collapse = "\n"),
        "\n```\n"
      )
      qmd_document_sections[[length(qmd_document_sections) + 1]] <- section_content
    }
  } else {
      if (verbose) cat("No documentable elements found or generated. Output document may be empty.\n")
  }
  
  # 渲染文档
  # output_dir in project_config is already resolved to an absolute path
  output_file_path <- render_quarto_document(
    qmd_content_list = qmd_document_sections,
    output_dir = project_config$output_dir,
    output_filename_base = project_config$output_filename_base,
    quarto_format = project_config$quarto_format,
    project_name = project_config$project_name, # Ensure this is project_config$project_name
    call_graph_data = call_graph_data,
    directory_summaries = directory_summaries # New argument
  )
  
  # 保存分析数据
  if (verbose) cat("Saving analysis data (RDS)...\n")
  
  analysis_output_data <- list(
    project_name = project_config$project_name,
    language_hints = project_config$code_source$language_hints,
    documented_elements = llm_generated_docs, 
    call_graph = call_graph_data,
    directory_contents = directory_contents, # Add this
    directory_summaries = directory_summaries, # Add this
    timestamp = Sys.time()
  )
  
  base_name_for_rds <- project_config$output_filename_base
  if (is.null(base_name_for_rds) || base_name_for_rds == "") {
    base_name_for_rds <- project_config$project_name # Should always have a project_name
  }
  
  rds_filename <- paste0(tools::file_path_sans_ext(base_name_for_rds), "_analysis_data.rds")
  rds_filepath <- file.path(project_config$output_dir, rds_filename)
  
  tryCatch({
    # output_dir should have been created by render_quarto_document, but ensure robust
    if (!dir.exists(project_config$output_dir)) {
      dir.create(project_config$output_dir, recursive = TRUE, showWarnings = FALSE)
      if (verbose) cat(paste("Created output directory for RDS:", project_config$output_dir, "\n"))
    }
    saveRDS(analysis_output_data, file = rds_filepath)
    if (verbose) cat(paste("Analysis data successfully saved to:", rds_filepath, "\n"))
  }, error = function(e) {
    warning(paste("Failed to save analysis data for project", project_config$project_name, "to", rds_filepath, ":", e$message))
  })

  if (verbose) {
    cat(paste("Documentation for project", project_config$project_name, "rendered to:", output_file_path, "\n"))
    # Token consumption is global; might need per-project if reset, or just report total at the end of all projects.
    # For now, get_total_tokens_consumed() will give cumulative for all projects processed so far in the session.
    cat(paste("Cumulative tokens consumed so far:", get_total_tokens_consumed(), "\n"))
  }
  
  if (verbose) cat(paste0("--- Finished documentation generation for project: ", project_config$project_name, " ---\n"))
  return(output_file_path)
}


#' Helper function to calculate relative path
#' This is a simplified version. For robust solution, fs::path_rel is better.
calculate_relative_path <- function(from_dir, to_path) {
  # Ensure paths are normalized and absolute for comparison
  from_dir <- normalizePath(from_dir, mustWork = FALSE)
  to_path <- normalizePath(to_path, mustWork = FALSE)

  # If to_path is not within from_dir, return to_path (or handle as error/absolute)
  # This simple version assumes to_path is "under" from_dir for typical use cases.
  if (startsWith(to_path, paste0(from_dir, .Platform$file.sep))) {
    return(substring(to_path, nchar(from_dir) + 2)) # +2 for separator and start
  } else {
    # If not directly under, it might be more complex (e.g. ../../) or on different drive
    # For now, if it's not a simple subdirectory, return the absolute path or a placeholder
    # This is a limitation of not using a dedicated path library like 'fs'.
    warning(paste("Cannot easily calculate relative path from", from_dir, "to", to_path, 
                  ". Using basename or absolute path. Consider 'fs' package for robust relative paths."))
    # Fallback: return just the filename if they share a parent up to some level,
    # or the full path if completely different. For this hub, we assume project docs
    # are in subdirectories of global_output_dir_root.
    # If global_output_dir_root is /a/b and to_path is /a/b/c/doc.html, rel_path is c/doc.html
    # If global_output_dir_root is /a/b and to_path is /x/y/z/doc.html, this logic fails.
    
    # A slightly more robust attempt for common prefix
    from_parts <- unlist(strsplit(from_dir, .Platform$file.sep))
    to_parts <- unlist(strsplit(to_path, .Platform$file.sep))
    
    # Remove common prefix
    common_len <- 0
    for (k in 1:min(length(from_parts), length(to_parts))) {
      if (from_parts[k] == to_parts[k]) {
        common_len <- common_len + 1
      } else {
        break
      }
    }
    
    if (common_len > 0) {
      # Path parts relative to the common ancestor
      # from_rel_parts <- from_parts[(common_len + 1):length(from_parts)] # Not used here to go up
      to_rel_parts <- to_parts[(common_len + 1):length(to_parts)]
      
      # Number of '..' needed is length(from_parts) - common_len
      # For index.qmd at global_output_dir_root, from_parts are the parts of global_output_dir_root itself.
      # So, if global_output_dir_root = /a/b, and to_path = /a/b/c/d.html
      # common_len = 2 (for 'a' and 'b'). from_parts = c('a','b'). to_parts = c('a','b','c','d.html')
      # to_rel_parts = c('c', 'd.html'). This is what we need.
      if (length(to_rel_parts) > 0) {
        return(paste(to_rel_parts, collapse = .Platform$file.sep))
      }
    }
    # Fallback if no commonality or complex case
    return(to_path) # Return absolute path as fallback
  }
}


#' Generate a master index page for multiple projects
#'
#' Creates an index.qmd file that links to the documentation of multiple projects.
#'
#' @param generated_docs_summary A list of summaries for each generated document.
#'        Each summary is a list with `name`, `path`, and `output_dir`.
#' @param global_output_dir_root The root directory where the master index should be saved.
#' @param verbose Logical, whether to print verbose messages.
#' @return Invisibly returns the path to the generated master index QMD file, or NULL.
#' @keywords internal
generate_master_index_page <- function(generated_docs_summary, global_output_dir_root, verbose = TRUE) {
  if (length(generated_docs_summary) < 1 || is.null(global_output_dir_root) || global_output_dir_root == "") {
    if (verbose) cat("Not enough projects or no global output directory root specified. Skipping master index page generation.\n")
    return(invisible(NULL))
  }

  if (!dir.exists(global_output_dir_root)) {
    if (verbose) cat(paste("Creating global output directory for master index:", global_output_dir_root, "\n"))
    dir.create(global_output_dir_root, recursive = TRUE, showWarnings = FALSE)
  }

  index_qmd_path <- file.path(global_output_dir_root, "index.qmd")
  index_content <- c(
    "---",
    "title: \"Project Documentation Hub\"",
    "format: html", # Defaulting to html for the hub
    "---",
    "",
    "## Generated Project Documentations",
    "",
    "Below is a list of all generated project documentations:",
    ""
  )

  list_items <- c()
  for (doc_summary in generated_docs_summary) {
    # Calculate relative path from global_output_dir_root to the project's main document file
    # doc_summary$path is the absolute path to the main *rendered file* (e.g., .html, .pdf)
    relative_doc_path <- calculate_relative_path(global_output_dir_root, doc_summary$path)
    
    # Check if path calculation was successful enough (i.e., it's not the absolute path)
    # or if the project's output_dir is indeed a subdirectory of global_output_dir_root
    is_sub_path <- startsWith(normalizePath(doc_summary$output_dir, mustWork=FALSE), 
                              normalizePath(global_output_dir_root, mustWork=FALSE))

    if (!is_sub_path && grepl("^(/|[A-Za-z]:)", relative_doc_path)) { # If it's still an absolute path
        list_items <- c(list_items, paste0("* ", doc_summary$name, " (Documentation at: `", doc_summary$path, "`) - External path, cannot make relative link easily."))
    } else {
        list_items <- c(list_items, paste0("* [", doc_summary$name, "](", relative_doc_path, ")"))
    }
  }
  
  if (length(list_items) > 0) {
    index_content <- c(index_content, list_items)
  } else {
    index_content <- c(index_content, "No projects were processed or linked.")
  }

  tryCatch({
    writeLines(index_content, index_qmd_path)
    if (verbose) cat(paste("Master index QMD file saved to:", index_qmd_path, "\n"))

    # Render the master index page
    if (requireNamespace("quarto", quietly = TRUE)) {
      quarto::quarto_render(input = index_qmd_path, output_format = "html", quiet = !verbose)
      rendered_index_file <- sub("\\.qmd$", ".html", index_qmd_path) # Assume HTML output
      if (verbose) cat(paste("Master index page rendered to:", rendered_index_file, "\n"))
    } else {
      warning("Quarto package not available. Master index page QMD created but not rendered.")
    }
  }, error = function(e) {
    warning(paste("Failed to create or render master index page:", e$message))
    return(invisible(NULL))
  })
  
  invisible(index_qmd_path)
}


#' 与已分析的代码仓库进行交互式问答
#'
#' 启动一个交互式会话，允许用户就先前通过 `generate_repo_docs` 分析并保存了
#' `_analysis_data.rds` 文件的代码仓库提问。
#'
#' @param analysis_data_path 指向 `_analysis_data.rds` 文件的路径 (character)。
#' @param config_path 指向用于分析的主 `config.json` 文件的路径 (character)。
#' @param verbose 是否打印详细消息 (logical, default `TRUE`)。
#' @return 该函数通常不直接返回值，因为它会启动一个外部进程。
#'         它会打印外部脚本的输出。
#' @export
#' @examples
#' \dontrun{
#' # 假设你已经运行了 generate_repo_docs 并且有以下文件：
#' # output/my_project_analysis_data.rds
#' # config/my_config.json
#' 
#' # chat_with_repo(
#' #   analysis_data_path = "output/my_project_analysis_data.rds",
#' #   config_path = "config/my_config.json"
#' # )
#' }
chat_with_repo <- function(analysis_data_path, config_path, verbose = TRUE) {
  # Validate paths
  if (!file.exists(analysis_data_path)) {
    stop(paste("Analysis data file not found:", analysis_data_path))
  }
  if (!file.exists(config_path)) {
    stop(paste("Configuration file not found:", config_path))
  }

  # Construct command
  rscript_path <- file.path(R.home("bin"), "Rscript")
  # For development, assume R/interactive_mode.R is in the R/ directory relative to project root
  # For installed package: system.file("R", "interactive_mode.R", package = "deepwikiR")
  interactive_script_path <- "R/interactive_mode.R" 
  # Fallback if system.file is preferred and package is installed, but we'll use relative for dev
  # if (interactive_script_path == "" || !file.exists(interactive_script_path)) {
  #   interactive_script_path <- "R/interactive_mode.R" # Relative path for dev
  # }

  if (!file.exists(interactive_script_path)) {
    stop(paste("interactive_mode.R script not found at:", interactive_script_path,
               "Ensure it is in the R/ directory of your project or package structure."))
  }

  cmd_args <- c(
    shQuote(interactive_script_path),
    "--data_file", shQuote(analysis_data_path),
    "--config_file", shQuote(config_path)
  )
  
  full_cmd <- paste(shQuote(rscript_path), paste(cmd_args, collapse = " "))

  if (verbose) {
    cat("Launching interactive Q&A mode for your repository...\n")
    cat("Command to be executed:\n", full_cmd, "\n\n")
  }

  # Execute command
  # Using system2 for better control over stdout/stderr if needed, but system is fine.
  status <- system(full_cmd, intern = FALSE, wait = TRUE) # wait = TRUE is default for system

  if (status != 0) {
    warning(paste("Interactive mode script execution failed with status:", status))
  } else {
    if (verbose) cat("Interactive session ended.\n")
  }
  
  invisible(status) # Return status invisibly
}


#' 运行命令行界面
#'
#' 这个函数提供了一个命令行界面，用于从终端运行deepwikiR。
#' 它不应该被直接调用，而是通过R脚本或系统命令调用。
#'
#' @param args 命令行参数
#' @return 不可见返回值，用于内部处理
#' @keywords internal
run_cli <- function(args = commandArgs(trailingOnly = TRUE)) {
  # Main parser
  parser <- argparse::ArgumentParser(
    description = "deepwikiR - 自动化代码仓库文档生成与交互工具"
  )
  parser$add_argument(
    "--quiet", 
    action = "store_true", 
    default = FALSE, 
    help = "全局安静模式，不显示详细日志 (适用于所有子命令)"
  )
  
  subparsers <- parser$add_subparsers(
    title = "mode",
    dest = "mode",
    help = "选择操作模式: 'generate' (生成文档) 或 'chat' (与仓库聊天)"
  )

  # Subparser for 'generate'
  generate_parser <- subparsers$add_parser(
    "generate", 
    help = "分析代码仓库并生成文档"
  )
  generate_parser$add_argument(
    "--config", 
    type = "character", 
    required = TRUE, 
    help = "配置文件路径（JSON格式）"
  )
  generate_parser$add_argument(
    "--output-dir", 
    type = "character", 
    default = NULL, 
    help = "输出目录（可选，覆盖配置文件中的设置）"
  )
  generate_parser$add_argument(
    "--api-key", 
    type = "character", 
    default = NULL, 
    help = "大语言模型API密钥（可选，覆盖环境变量）"
  )
  # Note: --quiet is a global argument now

  # Subparser for 'chat'
  chat_parser <- subparsers$add_parser(
    "chat", 
    help = "与先前分析过的代码仓库进行交互式问答"
  )
  chat_parser$add_argument(
    "--data_file", 
    type = "character", 
    required = TRUE, 
    help = "指向 _analysis_data.rds 文件的路径"
  )
  chat_parser$add_argument(
    "--config_file", 
    type = "character", 
    required = TRUE, 
    help = "指向用于分析的主 config.json 文件的路径"
  )
  # Note: --quiet is a global argument now
  
  # Parse arguments
  # If no arguments are provided, argparse shows help. If mode is missing, it also shows help.
  if (length(args) == 0) {
    # Insert 'generate' as default mode if a config file is present in common locations
    # Or simply print help. For now, let's require a mode.
    # parser$print_help()
    # return(invisible(NULL))
    # No, argparse handles this if 'mode' is required implicitly by subparsers.
  }

  parsed_args <- tryCatch({
    parser$parse_args(args)
  }, error = function(e) {
    # Handle cases where no mode is specified or invalid arguments for a mode
    # argparse usually prints help and exits, but if it errors out differently:
    message("Error parsing arguments: ", e$message)
    parser$print_help()
    return(invisible(NULL))
  })
  
  if (is.null(parsed_args) || is.null(parsed_args$mode)) {
      # This might happen if parsing fails and returns NULL, or if no mode was given.
      # Argparse usually exits before this if a mode is required by add_subparsers.
      # If execution reaches here, it means parsing didn't complete as expected or mode is missing.
      if(length(args) > 0 && !any(c("generate", "chat") %in% args)) {
          # If some args were given but not a valid mode, show help.
          message("No valid mode ('generate' or 'chat') specified or invalid arguments.")
      }
      parser$print_help()
      return(invisible(NULL))
  }


  # Execute based on mode
  if (parsed_args$mode == "generate") {
    if (is.null(parsed_args$config)) {
        message("Error: --config is required for 'generate' mode.")
        generate_parser$print_help()
        return(invisible(NULL))
    }
    
    # Load the multi-project configuration
    current_verbose_mode <- !parsed_args$quiet
    if (current_verbose_mode) cat("Loading and validating multi-project configuration...\n")
    
    config_data <- tryCatch({
        load_and_validate_config(parsed_args$config)
    }, error = function(e) {
        message(paste("Error loading or validating configuration:", e$message))
        return(NULL)
    })

    if (is.null(config_data)) {
        return(invisible(NULL)) # Error message already printed by load_and_validate_config or here
    }

    global_settings <- config_data$global_settings
    project_configs <- config_data$projects

    if (is.null(project_configs) || length(project_configs) == 0) {
      message("No projects found in the configuration file.")
      return(invisible(NULL))
    }

    generated_docs_summary <- list()
    total_projects <- length(project_configs)
    if (current_verbose_mode) cat(paste("Found", total_projects, "project(s) to process.\n"))

    for (i in seq_along(project_configs)) {
      single_project_conf <- project_configs[[i]]
      if (current_verbose_mode) {
        cat(paste0("\nProcessing project ", i, " of ", total_projects, ": ", single_project_conf$project_name, "\n"))
        cat(paste0(rep("-", 40), collapse=""), "\n")
      }
      
      # API key override from CLI applies to all projects if provided
      # generate_repo_docs handles its own on.exit for this key
      output_file_path <- generate_repo_docs(
        project_config = single_project_conf,
        llm_api_key = parsed_args$`api-key`, # Pass CLI override
        verbose = current_verbose_mode
      )
      
      if (!is.null(output_file_path) && file.exists(output_file_path)) {
        generated_docs_summary[[length(generated_docs_summary) + 1]] <- list(
          name = single_project_conf$project_name,
          path = normalizePath(output_file_path, mustWork = FALSE), # Store absolute path
          output_dir = normalizePath(single_project_conf$output_dir, mustWork = FALSE) # Store absolute path
        )
        if (current_verbose_mode) cat(paste("Successfully generated documentation for project:", single_project_conf$project_name, "at", output_file_path, "\n"))
      } else {
        if (current_verbose_mode) warning(paste("Documentation generation failed or returned no path for project:", single_project_conf$project_name, "\n"))
      }
    }

    if (current_verbose_mode) {
      cat(paste0("\n", rep("=", 40), collapse=""), "\n")
      cat("Summary of documentation generation:\n")
      if (length(generated_docs_summary) > 0) {
        for (summary_item in generated_docs_summary) {
          cat(paste("  - Project:", summary_item$name, "\n    Documentation:", summary_item$path, "\n"))
        }
      } else {
        cat("  No documents were generated successfully.\n")
      }
      cat(paste0(rep("=", 40), collapse=""), "\n")
    }
    
    # Generate master index page if multiple projects were processed and global_output_dir_root is set
    if (length(generated_docs_summary) > 0 && # Changed from > 1 to > 0, index for one project is fine.
        !is.null(global_settings$output_dir_root) && nzchar(global_settings$output_dir_root)) {
      if (current_verbose_mode) cat("\nGenerating master index page...\n")
      generate_master_index_page(
        generated_docs_summary = generated_docs_summary,
        global_output_dir_root = global_settings$output_dir_root, # This is already resolved/absolute
        verbose = current_verbose_mode
      )
    } else {
        if (current_verbose_mode) {
            if (length(generated_docs_summary) <=0) cat("Skipping master index: no projects successfully processed.\n")
            else if (is.null(global_settings$output_dir_root) || !nzchar(global_settings$output_dir_root)) cat("Skipping master index: global_settings$output_dir_root not set.\n")
        }
    }
    
    if (current_verbose_mode) cat("\nAll processing complete.\n")
    invisible(lapply(generated_docs_summary, function(x) x$path)) # Return list of paths

  } else if (parsed_args$mode == "chat") {
    if (is.null(parsed_args$data_file) || is.null(parsed_args$config_file)) {
        message("Error: --data_file and --config_file are required for 'chat' mode.")
        chat_parser$print_help()
        return(invisible(NULL))
    }
    chat_with_repo(
      analysis_data_path = parsed_args$data_file,
      config_path = parsed_args$config_file,
      verbose = !parsed_args$quiet
    )
    invisible(NULL) # chat_with_repo doesn't return a path
  } else {
    # Should not happen if subparsers are set up correctly
    message(paste("未知模式:", parsed_args$mode))
    parser$print_help()
    invisible(NULL)
  }
}
