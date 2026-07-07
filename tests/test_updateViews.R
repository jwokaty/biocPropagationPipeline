#!/usr/bin/env Rscript

# Test script for updatePackageViews and updateVIEWS functions
# This script tests the basic functionality without requiring external dependencies

source("../R/utils.R")
source("../R/makeViews.R")
source("../R/updateViews.R")

# Test package_version function
cat("Testing package_version function...\n")
print(package_version("3.19.0", "3.18.0"))  # Should be TRUE
print(package_version("3.18.0", "3.19.0"))  # Should be FALSE
print(package_version("1.2.3", "1.2.3"))    # Should be FALSE
print(package_version("1.2.4", "1.2.3"))    # Should be TRUE

# Test getUni function
cat("\nTesting getUni function...\n")
print(getUni("devel"))    # Should be "bioc"
print(getUni("release"))  # Should be "bioc-release"

# Test filterPropagationCandidates with mock data
cat("\nTesting filterPropagationCandidates with mock data...\n")

# Create mock jobs data
mock_jobs <- data.frame(
    Package = c("pkg1", "pkg2", "pkg3", "pkg4"),
    Version = c("1.2.0", "1.3.0", "1.1.0", "2.0.0"),
    `R CMD check` = c("OK", "ERROR", "NOTE", "OK"),
    `Binary builds` = c("OK", "OK", "FAIL", "OK"),
    `Vignette builds` = c("OK", "OK", "OK", "FAIL"),
    Unsupported = c(FALSE, FALSE, FALSE, TRUE),
    RemoteSha = c("abc1234567", "def2345678", "ghi3456789", "jkl4567890"),
    stringsAsFactors = FALSE
)

# Mock getBiocVersion to return a fixed version
getBiocVersion <- function(branch) {
    "1.0.0"
}

# Test filtering
filtered <- filterPropagationCandidates(mock_jobs, "devel")
cat("Filtered packages:", paste(filtered$Package, collapse=", "), "\n")

# Test updateVIEWS function with mock data
cat("\nTesting updateVIEWS function with mock data...\n")

# Create a temporary directory with mock package JSONs
temp_dir <- tempdir()
mock_package_dir <- file.path(temp_dir, "mock_devel")
dir.create(mock_package_dir, recursive = TRUE)

# Create mock package JSON files
mock_pkg1 <- list(
    Package = "testPkg1",
    Version = "1.0.0",
    Title = "Test Package 1",
    Description = "A test package",
    git_last_commit = "abc1234567",
    git_url = "https://github.com/test/testPkg1"
)

mock_pkg2 <- list(
    Package = "testPkg2",
    Version = "2.0.0",
    Title = "Test Package 2",
    Description = "Another test package",
    git_last_commit = "def2345678",
    git_url = "https://github.com/test/testPkg2"
)

# Write mock JSON files
jsonlite::write_json(mock_pkg1, file.path(mock_package_dir, "testPkg1.json"))
jsonlite::write_json(mock_pkg2, file.path(mock_package_dir, "testPkg2.json"))

# Test updateVIEWS
views_data <- updateVIEWS(mock_package_dir, file.path(mock_package_dir, "VIEWS.json"), "devel", dry_run = TRUE)
cat("VIEWS data contains", length(views_data), "packages\n")
cat("Package names:", paste(names(views_data), collapse=", "), "\n")

# Check that additional fields were added
if (length(views_data) > 0) {
    first_pkg <- views_data[[1]]
    cat("First package has .source field:", !is.null(first_pkg$.source), "\n")
    cat("First package has .last_updated field:", !is.null(first_pkg$.last_updated), "\n")
}

# Clean up
unlink(mock_package_dir, recursive = TRUE)

cat("\nAll tests completed.\n")
