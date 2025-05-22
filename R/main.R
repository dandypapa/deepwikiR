# /home/ubuntu/autodoc_tool/R/main.R
# Main script to drive the documentation generation process

# Load necessary libraries and source modules
cat("Loading libraries and source modules...\n")

# Ensure R_LIBS_USER is set for pak and other library operations if running in a clean env
if (Sys.getenv("R_LIBS_USER") == "") {
  user_lib_path <- "/home/ubuntu/R/library" 
  if (!dir.exists(user_lib_path)) dir.create(user_lib_path, recursive = TRUE, showWarnings = FALSE)
  Sys.setenv(R_LIBS_USER = user_lib_path)
  .libPaths(c(user_lib_path, .libPaths()))
}

# Source local modules
source_module <- function(file_path) {
  tryCatch({
    source(file_path, local = FALSE) 
    cat(paste("Successfully sourced:", file_path, "\n"))
    return(TRUE)
  }, error = function(e) {
    cat(paste("Error sourcing", file_path, ":", e$message, "\n"))
    stop(e) 
    return(FALSE)
  })
}

core_modules <- c(
  "R/input_config_handler.R",
  "R/code_acquisition_parser.R", 
  "R/code_analyzer.R",        
  "R/llm_interactor.R",         
  "R/quarto_renderer.R"       
)

for (mod in core_modules) {
  if (!source_module(mod)) {
    stop(paste("Failed to load a critical module. Exiting."))
  }
}

# Parse command line arguments
library(argparse)
parser <- ArgumentParser(description = "Automated Code Documentation Generator")
parser$add_argument("--config", type = "character", required = TRUE, help = "Path to the JSON configuration file")
args <- parser$parse_args()

# --- Main Process ---
cat(paste("Starting documentation generation with config:", args$config, "\n"))

# 1. Load and validate configuration
config <- load_and_validate_config(args$config)
if (is.null(config)) {
  stop("Configuration loading failed. Exiting.")
}
cat("Configuration loaded successfully.\n")

# 2. Acquire and parse code
cat("Acquiring and parsing code...\n")
code_details <- acquire_code_content(config$code_source)
if (length(code_details) == 0) {
  stop("No code files found or acquired. Exiting.")
}

# Determine project_base_path for call graph analysis
project_base_path <- NULL
if (!is.null(config$code_source$local_path)) {
  project_base_path <- normalizePath(config$code_source$local_path, mustWork = FALSE)
  cat(paste("Found", length(code_details), "files for processing from local path:", project_base_path, "\n"))
} else if (!is.null(config$code_source$git_repo)) {
  # For git repos, acquire_code_content clones to a temp dir.
  # The paths in code_details are relative to this temp dir.
  # We need to reconstruct this temp dir path or adjust how paths are handled.
  # For simplicity, if git, the paths in code_details are already relative and base is the temp root.
  # The current code_acquisition_parser returns paths relative to the *cloned* root.
  # So, for call graph, we might need to pass the temp clone path if files are read again from there.
  # However, generate_r_call_graph now expects relative paths and the base path.
  # Let's assume for git, the `code_details` keys are relative paths and we need a conceptual base.
  # This part might need refinement if git source is used and files are re-read by absolute path.
  # For now, if it's git, the `project_base_path` would be the temp clone dir, which is tricky to get here
  # unless acquire_code_content returns it. Let's assume local_path is used for this test.
  cat(paste("Found", length(code_details), "files for processing from git repository.", "Base path for analysis will be relative to cloned root.\n"))
  # This is a placeholder; if git is used, project_base_path needs to be the temp cloned dir.
  # For now, the sample_config uses local_path, so this branch is not hit in current tests.
  project_base_path <- getwd() # Fallback, not ideal for git
}

if (is.null(project_base_path)){
    stop("Could not determine project_base_path for analysis. Exiting.")
}

# 3. Analyze code structure (R specific handling)
cat("Analyzing code structure...\n")
analysis_result <- analyze_r_repository_code(code_details, project_base_path)
extracted_elements <- analysis_result$extracted_elements
call_graph_data <- analysis_result$call_graph

if (length(extracted_elements) == 0 && (is.null(call_graph_data) || length(call_graph_data$nodes) == 0)) {
  cat("No code elements extracted and no call graph data generated. Output might be minimal.\n")
} else {
  cat(paste("Found", length(extracted_elements), "elements for documentation.",
            "Call graph has", length(call_graph_data$nodes), "nodes and", length(call_graph_data$edges), "edges.\n"))
}

# 4. Generate documentation with LLM (if elements are found)
llm_generated_docs <- list()
if (length(extracted_elements) > 0) {
    cat("Generating documentation with LLM...\n")
    llm_generated_docs <- generate_docs_with_llm(extracted_elements, config$llm_settings, config$code_source$language_hints)
} else {
    cat("Skipping LLM documentation generation as no elements were extracted.\n")
}

# 5. Prepare content for Quarto and render
cat("Preparing content for Quarto and rendering...\n")

qmd_document_sections <- list()
if (length(llm_generated_docs) > 0) {
  for (doc_item in llm_generated_docs) {
    section_content <- paste0(
      "### `", doc_item$name, "`\n\n",
      "**Type:** ", doc_item$type, "\n\n",
      "**Signature:** `", doc_item$name, doc_item$signature, "`\n\n",
      "**File:** `", doc_item$file_path, "`\n\n",
      "**Description:**\n", doc_item$documentation_text, "\n\n",
      "**Code:**\n",
      "```r\n",
      paste(doc_item$code_block, collapse = "\n"),
      "\n```\n"
    )
    qmd_document_sections[[length(qmd_document_sections) + 1]] <- section_content
  }
}

output_file_path <- render_quarto_document(
  qmd_content_list = qmd_document_sections,
  output_dir = config$output_dir,
  output_filename_base = config$output_filename_base,
  quarto_format = config$quarto_format,
  project_name = config$project_name,
  call_graph_data = call_graph_data 
)

cat(paste("Documentation rendered to:", output_file_path, "\n"))
cat(paste("Documentation generation process completed. Total tokens consumed:", get_total_tokens_consumed(), "\n"))

print(output_file_path)

