# 主函数文件，提供包的主要功能接口

#' 生成代码仓库文档
#'
#' 这是deepwikiR包的主函数，用于自动分析R代码仓库并生成文档。
#' 该函数支持从本地路径或Git仓库获取代码，使用大语言模型生成文档，
#' 并通过Mermaid图表可视化函数调用关系。
#'
#' @param config_path 配置文件路径，JSON格式
#' @param output_dir 输出目录，如果为NULL则使用配置文件中的设置
#' @param llm_api_key 大语言模型API密钥，如果为NULL则使用环境变量中的设置
#' @param verbose 是否显示详细日志，默认为TRUE
#' @return 生成的文档文件路径
#' @export
#' @examples
#' \dontrun{
#' # 使用配置文件生成文档
#' config_file <- system.file("config", "sample_config.json", package = "deepwikiR")
#' generate_repo_docs(config_file)
#' 
#' # 指定输出目录
#' generate_repo_docs(config_file, output_dir = "~/my_docs")
#' }
generate_repo_docs <- function(config_path, output_dir = NULL, llm_api_key = NULL, verbose = TRUE) {
  # 验证参数
  if (!file.exists(config_path)) {
    stop("配置文件不存在: ", config_path)
  }
  
  # 加载配置
  config <- load_and_validate_config(config_path)
  
  # 覆盖配置中的输出目录（如果指定）
  if (!is.null(output_dir)) {
    config$output_dir <- output_dir
    if (verbose) cat(paste("输出目录已设置为:", output_dir, "\n"))
  }
  
  # 设置API密钥（如果指定）
  if (!is.null(llm_api_key) && !is.null(config$llm_settings)) {
    # 保存原始环境变量
    orig_api_key <- Sys.getenv(config$llm_settings$api_key_env_var, "")
    # 设置新的环境变量
    Sys.setenv(setNames(llm_api_key, config$llm_settings$api_key_env_var))
    # 确保函数退出时恢复原始环境变量
    on.exit(Sys.setenv(setNames(orig_api_key, config$llm_settings$api_key_env_var)))
    
    if (verbose) cat("已设置API密钥环境变量\n")
  }
  
  # 执行文档生成流程
  if (verbose) cat("开始文档生成流程...\n")
  
  # 1. 获取代码内容
  if (verbose) cat("获取代码内容...\n")
  code_details <- acquire_code_content(config$code_source)
  
  if (length(code_details) == 0) {
    stop("未找到代码文件，请检查配置")
  }
  
  if (verbose) cat(paste("找到", length(code_details), "个文件\n"))
  
  # 确定项目基础路径
  project_base_path <- NULL
  if (!is.null(config$code_source$local_path)) {
    project_base_path <- normalizePath(config$code_source$local_path, mustWork = FALSE)
  } else {
    # 对于Git仓库，使用当前工作目录作为基础路径
    project_base_path <- getwd()
  }
  
  # 2. 分析代码结构
  if (verbose) cat("分析代码结构...\n")
  analysis_result <- analyze_r_repository_code(code_details, project_base_path)
  extracted_elements <- analysis_result$extracted_elements
  call_graph_data <- analysis_result$call_graph
  
  if (verbose) {
    cat(paste("找到", length(extracted_elements), "个代码元素\n"))
    cat(paste("调用图包含", length(call_graph_data$nodes), "个节点和", 
              length(call_graph_data$edges), "条边\n"))
  }
  
  # 3. 使用LLM生成文档
  llm_generated_docs <- list()
  if (length(extracted_elements) > 0) {
    if (verbose) cat("使用大语言模型生成文档...\n")
    llm_generated_docs <- generate_docs_with_llm(
      extracted_elements, 
      config$llm_settings, 
      config$code_source$language_hints
    )
  }
  
  # 4. 准备Quarto内容并渲染
  if (verbose) cat("准备Quarto内容并渲染...\n")
  
  # 准备文档部分
  qmd_document_sections <- list()
  if (length(llm_generated_docs) > 0) {
    for (doc_item in llm_generated_docs) {
      section_content <- paste0(
        "### `", doc_item$name, "`\n\n",
        "**类型:** ", doc_item$type, "\n\n",
        "**签名:** `", doc_item$name, doc_item$signature, "`\n\n",
        "**文件:** `", doc_item$file_path, "`\n\n",
        "**描述:**\n", doc_item$documentation_text, "\n\n",
        "**代码:**\n",
        "```r\n",
        paste(doc_item$code_block, collapse = "\n"),
        "\n```\n"
      )
      qmd_document_sections[[length(qmd_document_sections) + 1]] <- section_content
    }
  }
  
  # 渲染文档
  output_file_path <- render_quarto_document(
    qmd_content_list = qmd_document_sections,
    output_dir = config$output_dir,
    output_filename_base = config$output_filename_base,
    quarto_format = config$quarto_format,
    project_name = config$project_name,
    call_graph_data = call_graph_data
  )
  
  if (verbose) {
    cat(paste("文档已渲染到:", output_file_path, "\n"))
    cat(paste("文档生成完成。总消耗token数:", get_total_tokens_consumed(), "\n"))
  }
  
  # 返回生成的文档路径
  return(output_file_path)
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
  # 使用argparse包解析命令行参数
  parser <- argparse::ArgumentParser(
    description = "deepwikiR - 自动化代码仓库文档生成工具"
  )
  
  parser$add_argument(
    "--config", 
    type = "character", 
    required = TRUE, 
    help = "配置文件路径（JSON格式）"
  )
  
  parser$add_argument(
    "--output-dir", 
    type = "character", 
    default = NULL, 
    help = "输出目录（可选，覆盖配置文件中的设置）"
  )
  
  parser$add_argument(
    "--api-key", 
    type = "character", 
    default = NULL, 
    help = "大语言模型API密钥（可选，覆盖环境变量）"
  )
  
  parser$add_argument(
    "--quiet", 
    action = "store_true", 
    default = FALSE, 
    help = "安静模式，不显示详细日志"
  )
  
  # 解析参数
  parsed_args <- parser$parse_args(args)
  
  # 调用主函数
  result <- generate_repo_docs(
    config_path = parsed_args$config,
    output_dir = parsed_args$`output-dir`,
    llm_api_key = parsed_args$`api-key`,
    verbose = !parsed_args$quiet
  )
  
  # 打印结果路径
  cat(paste("文档已生成:", result, "\n"))
  
  # 返回不可见结果
  invisible(result)
}
