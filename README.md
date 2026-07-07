# biocPropagationPipeline

A Bioconductor propagation pipeline that monitors R-universe for newly built packages, identifies packages meeting propagation criteria, and updates package VIEWS accordingly.

## Features

- **Monitors** R-universe (`bioc.r-universe.dev` for devel, `bioc-release.r-universe.dev` for release) for newly built packages
- **Identifies** packages meeting propagation criteria using `biocUniTools::get_jobs()`
- **Updates** individual `devel/{package}.json` and `release/{package}.json` files with new package metadata
- **Rebuilds** `VIEWS.json` for each branch by aggregating all package JSONs with additional metadata
- **Automated** via GitHub Actions (runs every 6 hours)

## Propagation Criteria

Packages must meet all of the following criteria to be propagated:

- ✅ R CMD check passed (OK/NOTE)
- ✅ Binary builds passed (at least one platform)
- ✅ Vignette builds passed
- ✅ Not unsupported
- ✅ Version > current Bioconductor version
- ✅ SHA changed (7-char `RemoteSha` vs `git_last_commit` in VIEWS)

## Usage

### Manual Execution

```r
# Source the required files
source("R/utils.R")
source("R/makeViews.R")
source("R/updateViews.R")

# Step 1: Update individual package JSON files
result <- updatePackageViews("devel", "devel")

# Step 2: Update the aggregated VIEWS.json file
views_data <- updateVIEWS("devel", "devel/VIEWS.json", "devel")

# Same for release branch
result <- updatePackageViews("release", "release")
views_data <- updateVIEWS("release", "release/VIEWS.json", "release")
```

### Dry Run Mode

```r
# Test without making changes
result <- updatePackageViews("devel", "devel", dry_run = TRUE)
views_data <- updateVIEWS("devel", "devel/VIEWS.json", "devel", dry_run = TRUE)
```

## Repository Structure

```
biocPropagationPipeline/
├── R/                          # Pipeline code
│   ├── utils.R                 # Utility functions
│   ├── makeViews.R             # VIEW preparation functions
│   ├── fetchArtifacts.R       # Artifact fetching functions
│   └── updateViews.R           # Main propagation logic
│       ├── updatePackageViews() # Updates individual package JSONs
│       └── updateVIEWS()       # Creates/updates aggregated VIEWS.json
├── devel/                      # Devel package JSONs + VIEWS.json
├── release/                    # Release package JSONs + VIEWS.json
├── .github/workflows/          # GitHub Actions workflows
│   └── propagate.yml           # Scheduled propagation workflow
├── tests/                      # Test scripts
│   └── test_updateViews.R      # Test script for update functions
└── README.md
```

## Workflow

1. Scheduled GitHub Actions job runs every 6 hours
2. `updatePackageViews()` fetches R-universe jobs and updates individual package JSON files
3. `updateVIEWS()` aggregates all package JSONs into VIEWS.json with additional metadata
4. Changes are committed back to the repository

## Functions

### Main Functions

- **`updatePackageViews(branch, package_dir, dry_run)`**: Updates individual package JSON files
  - Fetches R-universe jobs via `biocUniTools::get_jobs()`
  - Filters packages using propagation criteria
  - Compares SHAs with Bioconductor VIEWS
  - Creates/updates individual `{package}.json` files
  - Removes JSON files for packages no longer meeting criteria

- **`updateVIEWS(package_dir, views_path, branch, dry_run)`**: Creates/updates aggregated VIEWS.json
  - Reads all individual package JSON files from `package_dir`
  - Adds additional metadata fields to each package entry:
    - `.source`: Source of the data ("r-universe")
    - `.source_url`: URL to the R-universe package page
    - `.last_updated`: Timestamp of last update
    - `.propagation_status`: Propagation status ("propagated")
    - `git_branch`: Branch ("devel" or "release")
  - Sorts packages alphabetically
  - Writes aggregated data to VIEWS.json

- **`filterPropagationCandidates(jobs, branch)`**: Filters packages based on propagation criteria
- **`getBiocVersion(branch)`**: Gets current Bioconductor version for a branch
- **`getBiocViews(branch)`**: Fetches Bioconductor VIEWS for comparison
- **`determinePackageType(pkg, branch)`**: Determines package type (placeholder)

### Utility Functions

- `package_version(v1, v2)`: Compares package version strings
- `getUni(branch)`: Gets R-universe name for a branch
- `getRuData(pkg, branch)`: Fetches R-universe package data
- `prepareView(df)`: Prepares package VIEW data structure
- `readView(package, path)`: Reads a single package view from JSON
- `readViews(path)`: Reads all package views from directory

### Artifact Functions

- `getCitation(pkg_url, save_path, ext)`: Downloads package citation
- `getLicense(pkg_url, save_path)`: Downloads package license
- `getManual(pkg, pkg_url, save_path, ext)`: Downloads package manual (PDF/HTML)
- `getNews(pkg_url, save_path)`: Downloads package NEWS file
- `getReadme(pkg_url, save_path)`: Downloads package README
- `getVignettes(vignettes, pkg_url, save_path)`: Downloads package vignettes
- `getArtifacts(pkg, branch, view)`: Downloads all artifacts for a package

## Development

This pipeline is designed to run automatically via GitHub Actions. For development and testing, you can run the functions manually in an R session with the required dependencies installed.

## Dependencies

- jsonlite
- yaml
- curl
- git2r
- logger
- biocUniTools
