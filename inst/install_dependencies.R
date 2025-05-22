# install_dependencies.R
# Script to install R package dependencies, avoiding shell interpretation issues.

# Set user library path
user_lib_path <- "/home/ubuntu/R/library"
if (Sys.getenv("R_LIBS_USER") == "") {
  Sys.setenv(R_LIBS_USER = user_lib_path)
}
if (!dir.exists(user_lib_path)) {
  dir.create(user_lib_path, recursive = TRUE, showWarnings = FALSE)
}
.libPaths(c(user_lib_path, .libPaths()))

# Install pak if not already installed
if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak", lib = Sys.getenv("R_LIBS_USER"), repos = "https://cloud.r-project.org/")
}

# Define list of packages to install
packages_to_install <- c(
  "CodeDepends", 
  "httr", 
  "jsonlite", 
  "tidyllm", 
  "argparse", 
  "stringr", 
  "quarto", 
  "git2r" # Added git2r as it's used in code_acquisition_parser.R
)

# Install packages using pak
cat("Installing required R packages using pak...\n")
pak::pkg_install(packages_to_install, lib = Sys.getenv("R_LIBS_USER"))

cat("R package installation attempt finished.\n")

# Verify installations
for (pkg in packages_to_install) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    cat(paste(pkg, "installed successfully.\n"))
  } else {
    cat(paste("Failed to install", pkg, ".\n"))
    # Optionally, quit with error if a critical package fails
    # if (pkg %in% c("CodeDepends", "httr", "jsonlite", "tidyllm", "quarto")) {
    #   quit(status = 1)
    # }
  }
}

