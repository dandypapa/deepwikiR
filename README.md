# deepwikiR: Automated R Code Repository Documentation and Exploration Tool

[CI Badge Placeholder]
<!-- Replace with actual badge if/when available -->

## Overview

`deepwikiR` is an advanced tool for automated documentation generation and exploration of R code repositories. It combines static code analysis, Large Language Model (LLM) powered insights, and the Quarto publishing framework to help developers understand and document R projects more effectively.

The system can process local R projects or clone them from Git repositories. It extracts information about functions, generates narrative documentation using LLMs, visualizes call graphs, summarizes directory contents, and even offers an interactive chat mode to ask questions about your codebase.

## Key Features

*   **Automated Code Documentation:** Generates detailed documentation for R functions and other code elements using LLMs.
*   **Call Graph Visualization:** Creates Mermaid diagrams to visualize function and module dependencies.
*   **Multi-Project Support:** Process multiple R projects from a single, unified configuration file, with a master index page for outputs.
*   **Directory-Level Summaries:** Provides LLM-generated summaries for the contents and role of each directory within your project.
*   **Interactive Code Chat:** Engage in a Q&A session with an LLM about your analyzed codebase directly from the command line.
*   **Fine-tuning Data Export:** Export documented code elements in JSONL format, suitable for fine-tuning your own LLMs.
*   **Quarto Integration:** Produces well-formatted and customizable documentation using Quarto.
*   **R Package Structure:** Designed with R package best practices in mind, using Roxygen2 for inline code documentation.

## Installation

Currently, `deepwikiR` is under development. To use it, you would typically clone this repository and ensure all dependencies listed in the `DESCRIPTION` file are installed.

```R
# Conceptual installation from GitHub (once available)
# remotes::install_github("your_username/deepwikiR")

# Or, after cloning:
# Ensure all dependencies from DESCRIPTION are installed, then build/install locally.
# For example, using devtools:
# devtools::install() 
```

## Quick Start

1.  **Configuration:**
    *   Copy or create a configuration JSON file. A sample can be found at `inst/config/sample_config.json`.
    *   This file defines global settings and a list of projects to process. For each project, specify:
        *   `project_name`
        *   `code_source` (e.g., `local_path` or `git_repo`, include/exclude patterns, language hints)
        *   `output_dir` (where documentation will be saved)
        *   `output_filename_base` (base name for output files)
        *   `llm_settings` (provider, model, API key environment variable, etc.)
        *   Optional: `export_finetuning_data` settings.
        *   Refer to `inst/config/sample_config.json` for detailed structure.

2.  **Generate Documentation:**
    *   **Using the Command Line Interface (CLI):**
        The primary way to run `deepwikiR` is via the Rscript command pointing to `R/deepwikiR.R`.
        ```bash
        Rscript R/deepwikiR.R generate --config path/to/your_config.json
        ```
        (Ensure `R/deepwikiR.R` is executable or use `Rscript path/to/deepwikiR_project_root/R/deepwikiR.R ...`)
    *   **From an R Session:**
        ```R
        # Assuming deepwikiR is loaded or functions are sourced
        # Load the multi-project config
        config_data <- deepwikiR::load_and_validate_config("path/to/your_config.json")
        
        # Iterate and generate for each project
        if (!is.null(config_data$projects)) {
          for (proj_conf in config_data$projects) {
            deepwikiR::generate_repo_docs(project_config = proj_conf, verbose = TRUE)
          }
        }
        ```

3.  **Chat with Your Code:**
    *   After generating documentation for a project, an `_analysis_data.rds` file is saved in its output directory.
    *   **Using the CLI:**
        ```bash
        Rscript R/deepwikiR.R chat --data_file path/to/project_output/_analysis_data.rds --config_file path/to/your_config.json 
        ```
    *   **From an R Session:**
        ```R
        deepwikiR::chat_with_repo(
          analysis_data_path = "path/to/project_output/_analysis_data.rds",
          config_path = "path/to/your_config.json"
        )
        ```

---

### Self-Documentation Demo

You can also see `deepwikiR` document its own codebase. This provides another comprehensive example of its capabilities.

1.  **Configuration:**
    *   The configuration for this demo is in `inst/examples/deepwikiR-self-demo-config.json`. It's set up to process the `deepwikiR` source files themselves.

2.  **Run the Self-Demo:**
    *   Execute the following script from the project root:
      ```bash
      Rscript inst/examples/run_deepwikiR_self_demo.R
      ```
    *   This will generate documentation for `deepwikiR` into the `output/deepwikiR_demos/deepwikiR-self-docs/` directory (by default).

3.  **Explore and Chat:**
    *   Browse the generated HTML documentation.
    *   Use the chat mode with the generated `_analysis_data.rds` file for the self-demo to ask questions about `deepwikiR`'s own code.
---

4.  **View Output:**
    *   Generated Quarto documents (HTML, PDF, etc.) will be in the specified `output_dir` for each project.
    *   If multiple projects are processed with a `global_settings$output_dir_root`, an `index.html` will be created there, linking to all project documentations.

## Fine-tuning Data Export

If configured in your `config.json` (`export_finetuning_data: { "enabled": true, ... }`), `deepwikiR` will also export a JSONL file containing prompt-completion pairs based on your code and its generated documentation. This can be used to fine-tune custom language models.

## License

This project is licensed under the MIT License. See the `LICENSE` file for details.
(Note: Ensure a `LICENSE` file with MIT license text exists in the repo).
