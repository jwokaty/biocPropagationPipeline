#' Update individual package JSON files
#'
#' @description Fetches R-universe jobs, filters packages meeting propagation criteria,
#' compares SHAs with current Bioconductor VIEWS, and updates individual package JSON files.
#'
#' @param branch Character, either "devel" or "release"
#' @param package_dir Character, directory where individual package JSONs are stored
#' @param dry_run Logical, if TRUE, only report changes without writing files
#'
#' @returns A list containing:
#'   - updated_packages: Character vector of packages that were updated
#'   - new_packages: Character vector of packages that were added
#'   - removed_packages: Character vector of packages that were removed
#'   - package_data: List of package data for all processed packages
#'
#' @examples
#' # Update devel package JSONs
#' result <- updatePackageViews("devel", "devel")
#'
#' @export
updatePackageViews <- function(branch = c("devel", "release"),
                              package_dir = NULL,
                              dry_run = FALSE) {

    branch <- match.arg(branch)
    
    # Set default directory if not provided
    if (is.null(package_dir)) {
        package_dir <- branch
    }

    # Ensure directory exists
    if (!dir.exists(package_dir) && !dry_run) {
        dir.create(package_dir, recursive = TRUE)
    }

    message("Processing ", branch, " branch...")

    # Step 1: Get R-universe jobs
    message("Fetching R-universe jobs...")
    jobs <- tryCatch({
        biocUniTools::get_jobs(getUni(branch))
    }, error = function(e) {
        stop("Failed to fetch R-universe jobs: ", e$message)
    })

    if (nrow(jobs) == 0) {
        message("No jobs found for ", branch)
        return(list(updated_packages = character(0),
                    new_packages = character(0),
                    removed_packages = character(0),
                    package_data = list()))
    }

    # Step 2: Filter packages meeting propagation criteria
    message("Filtering packages by propagation criteria...")
    candidates <- filterPropagationCandidates(jobs, branch)

    if (nrow(candidates) == 0) {
        message("No packages meet propagation criteria for ", branch)
        return(list(updated_packages = character(0),
                    new_packages = character(0),
                    removed_packages = character(0),
                    package_data = list()))
    }

    message("Found ", nrow(candidates), " candidate packages")

    # Step 3: Get current Bioconductor VIEWS for SHA comparison
    message("Fetching current Bioconductor VIEWS...")
    bioc_views <- tryCatch({
        getBiocViews(branch)
    }, error = function(e) {
        warning("Failed to fetch Bioconductor VIEWS: ", e$message)
        NULL
    })

    # Step 4: Compare SHAs and identify changed packages
    message("Comparing SHAs with Bioconductor VIEWS...")
    
    # Extract package names from candidates
    candidate_pkgs <- candidates$Package
    
    # Get current SHA and version from Bioconductor VIEWS
    current_shas <- character(nrow(candidates))
    current_versions <- character(nrow(candidates))
    
    if (!is.null(bioc_views)) {
        for (i in seq_along(candidate_pkgs)) {
            pkg <- candidate_pkgs[i]
            if (pkg %in% rownames(bioc_views)) {
                current_shas[i] <- bioc_views[pkg, "git_last_commit"]
                current_versions[i] <- bioc_views[pkg, "Version"]
            }
        }
    }

    # Identify packages with changed SHA or new version
    changed_mask <- rep(FALSE, nrow(candidates))
    new_mask <- rep(FALSE, nrow(candidates))
    
    for (i in seq_along(candidate_pkgs)) {
        pkg <- candidate_pkgs[i]
        candidate_sha <- candidates$RemoteSha[i]
        candidate_version <- candidates$Version[i]
        
        # Check if package exists in current VIEWS
        if (!(pkg %in% rownames(bioc_views))) {
            # New package
            new_mask[i] <- TRUE
            changed_mask[i] <- TRUE
        } else {
            # Existing package - check SHA and version
            current_sha <- current_shas[i]
            current_version <- current_versions[i]
            
            # SHA changed (7-char comparison)
            sha_changed <- substr(candidate_sha, 1, 7) != substr(current_sha, 1, 7)
            
            # Version changed
            version_changed <- package_version(candidate_version) > package_version(current_version)
            
            changed_mask[i] <- sha_changed || version_changed
        }
    }

    changed_pkgs <- candidate_pkgs[changed_mask]
    new_pkgs <- candidate_pkgs[new_mask]

    message("Packages to update: ", length(changed_pkgs))
    message("New packages: ", length(new_pkgs))

    # Step 5: Process changed packages and update individual JSON files
    package_data <- list()
    updated_packages_list <- character(0)
    
    for (pkg in changed_pkgs) {
        pkg_idx <- which(candidate_pkgs == pkg)
        pkg_job_data <- candidates[pkg_idx, ]
        
        # Get full package data from R-universe
        ru_data <- tryCatch({
            getRuData(pkg, branch)
        }, error = function(e) {
            warning("Failed to get R-universe data for ", pkg, ": ", e$message)
            NULL
        })

        if (!is.null(ru_data)) {
            # Prepare view entry
            view_entry <- prepareView(ru_data)
            
            # Add package type information
            view_entry$PackageType <- determinePackageType(pkg, branch)
            
            # Store the package data
            package_data[[pkg]] <- view_entry
            updated_packages_list <- c(updated_packages_list, pkg)
            
            # Save individual package JSON
            if (!dry_run) {
                pkg_json_path <- file.path(package_dir, paste0(pkg, ".json"))
                jsonlite::write_json(view_entry, pkg_json_path, pretty = TRUE)
                message("Saved: ", pkg_json_path)
            } else {
                message("[DRY RUN] Would save: ", file.path(package_dir, paste0(pkg, ".json")))
            }
        }
    }

    # Step 6: Identify and remove packages that are no longer in candidates
    # Get list of existing JSON files
    existing_json_files <- list.files(package_dir, pattern = "\\.json$", full.names = FALSE)
    existing_pkgs <- gsub("\\.json$", "", existing_json_files)
    
    # Packages to keep: all candidates (both changed and unchanged)
    all_candidate_pkgs <- candidate_pkgs
    
    # Packages that should be removed: existing packages not in candidates
    removed_pkgs <- setdiff(existing_pkgs, all_candidate_pkgs)
    
    for (pkg in removed_pkgs) {
        if (!dry_run) {
            pkg_json_path <- file.path(package_dir, paste0(pkg, ".json"))
            if (file.exists(pkg_json_path)) {
                file.remove(pkg_json_path)
                message("Removed: ", pkg_json_path)
            }
        } else {
            message("[DRY RUN] Would remove: ", file.path(package_dir, paste0(pkg, ".json")))
        }
    }

    # Return results
    list(
        updated_packages = setdiff(changed_pkgs, new_pkgs),
        new_packages = new_pkgs,
        removed_packages = removed_pkgs,
        package_data = package_data
    )
}

#' Update VIEWS.json file
#'
#' @description Creates or updates a VIEWS.json file that aggregates all individual
#' package JSON files with additional metadata fields.
#'
#' @param package_dir Character, directory containing individual package JSON files
#' @param views_path Character, path to the VIEWS.json file
#' @param branch Character, either "devel" or "release"
#' @param dry_run Logical, if TRUE, only report what would be done
#'
#' @returns The aggregated VIEWS data as a list
#'
#' @examples
#' # Update devel VIEWS.json
#' views_data <- updateVIEWS("devel", "devel/VIEWS.json", "devel")
#'
#' @export
updateVIEWS <- function(package_dir, views_path = NULL, branch = c("devel", "release"), dry_run = FALSE) {
    
    branch <- match.arg(branch)
    
    # Set default views path if not provided
    if (is.null(views_path)) {
        views_path <- file.path(package_dir, "VIEWS.json")
    }

    message("Updating VIEWS.json for ", branch, " branch...")

    # Step 1: Read all individual package JSON files
    message("Reading package JSON files from ", package_dir, "...")
    
    json_files <- list.files(package_dir, pattern = "\\.json$", full.names = FALSE)
    
    # Exclude VIEWS.json itself if it exists
    json_files <- json_files[json_files != "VIEWS.json"]
    
    if (length(json_files) == 0) {
        message("No package JSON files found in ", package_dir)
        return(list())
    }

    # Read all package data
    views_data <- list()
    for (json_file in json_files) {
        pkg_name <- gsub("\\.json$", "", json_file)
        tryCatch({
            pkg_data <- jsonlite::read_json(file.path(package_dir, json_file))
            views_data[[pkg_name]] <- pkg_data
        }, error = function(e) {
            warning("Failed to read ", json_file, ": ", e$message)
        })
    }

    if (length(views_data) == 0) {
        message("No valid package data found")
        return(list())
    }

    message("Found ", length(views_data), " package entries")

    # Step 2: Add additional metadata fields to each package entry
    message("Adding additional metadata fields...")
    
    for (pkg_name in names(views_data)) {
        pkg_data <- views_data[[pkg_name]]
        
        # Ensure basic fields exist
        if (is.null(pkg_data$Package)) {
            pkg_data$Package <- pkg_name
        }
        
        # Add branch-specific information
        pkg_data$git_branch <- branch
        
        # Add timestamp for when this entry was last updated
        pkg_data$.last_updated <- Sys.time()
        
        # Add source information
        pkg_data$.source <- "r-universe"
        pkg_data$.source_url <- paste0("https://", getUni(branch), ".r-universe.dev/", pkg_name)
        
        # Add propagation status
        pkg_data$.propagation_status <- "propagated"
        
        # Update the entry
        views_data[[pkg_name]] <- pkg_data
    }

    # Step 3: Sort packages alphabetically
    sorted_pkg_names <- sort(names(views_data))
    views_data <- views_data[sorted_pkg_names]

    # Step 4: Write the aggregated VIEWS.json file
    if (!dry_run) {
        # Ensure directory exists
        views_dir <- dirname(views_path)
        if (!dir.exists(views_dir)) {
            dir.create(views_dir, recursive = TRUE)
        }
        
        jsonlite::write_json(views_data, views_path, pretty = TRUE)
        message("Updated VIEWS.json: ", views_path)
    } else {
        message("[DRY RUN] Would update VIEWS.json: ", views_path)
    }

    # Return the aggregated data
    views_data
}

#' Filter packages meeting propagation criteria
#'
#' @description Applies all propagation criteria to filter R-universe jobs.
#'
#' @param jobs Data frame from biocUniTools::get_jobs()
#' @param branch Character, either "devel" or "release"
#'
#' @returns Filtered data frame of packages meeting all criteria
#'
#' @export
filterPropagationCandidates <- function(jobs, branch) {
    
    # Criteria 1: R CMD check passed (OK/NOTE)
    check_passed <- jobs$`R CMD check` %in% c("OK", "NOTE")
    
    # Criteria 2: Binary builds passed (at least one platform)
    # Assuming jobs has columns for binary build status per platform
    binary_cols <- grep("binary", names(jobs), ignore.case = TRUE, value = TRUE)
    if (length(binary_cols) > 0) {
        binary_passed <- apply(jobs[, binary_cols, drop = FALSE], 1, function(row) {
            any(row %in% c("OK", "NOTE", "TRUE", "true"), na.rm = TRUE)
        })
    } else {
        # Fallback: check for any success indicator
        binary_passed <- jobs$`Binary builds` %in% c("OK", "TRUE", "true")
    }
    
    # Criteria 3: Vignette builds passed
    vignette_passed <- jobs$`Vignette builds` %in% c("OK", "TRUE", "true")
    
    # Criteria 4: Not unsupported
    not_unsupported <- !jobs$Unsupported %in% c("TRUE", "true", TRUE)
    
    # Criteria 5: Version > current Bioconductor version
    bioc_version <- tryCatch({
        getBiocVersion(branch)
    }, error = function(e) {
        warning("Failed to get Bioconductor version: ", e$message)
        "0.0.0"  # Fallback - will likely exclude most packages
    })
    
    version_greater <- tryCatch({
        sapply(jobs$Version, function(v) {
            package_version(v) > package_version(bioc_version)
        })
    }, error = function(e) {
        rep(TRUE, nrow(jobs))  # If version comparison fails, include all
    })
    
    # Apply all criteria
    all_criteria <- check_passed & 
                    binary_passed & 
                    vignette_passed & 
                    not_unsupported & 
                    version_greater
    
    # Return filtered jobs with all columns
    jobs[all_criteria, ]
}

#' Get Bioconductor version for a branch
#'
#' @param branch Character, either "devel" or "release"
#' @returns Character, version string
#'
#' @export
getBiocVersion <- function(branch = c("devel", "release")) {
    branch <- match.arg(branch)
    
    # Try to get from Bioconductor config
    config_url <- "https://bioconductor.org/config.yaml"
    
    tryCatch({
        config <- yaml::read_yaml(config_url)
        if (branch == "devel") {
            return(config$devel_version)
        } else {
            return(config$release_version)
        }
    }, error = function(e) {
        # Fallback: try to get from BiocManager
        tryCatch({
            if (branch == "devel") {
                return(BiocManager::version("devel"))
            } else {
                return(BiocManager::version("release"))
            }
        }, error = function(e2) {
            warning("Failed to get Bioconductor version: ", e2$message)
            # Default fallback versions
            if (branch == "devel") {
                return("3.19")
            } else {
                return("3.18")
            }
        })
    })
}

#' Get Bioconductor VIEWS for a branch
#'
#' @param branch Character, either "devel" or "release"
#' @returns Data frame with package VIEW information
#'
#' @export
getBiocViews <- function(branch = c("devel", "release")) {
    branch <- match.arg(branch)
    
    # URL for Bioconductor VIEWS
    if (branch == "devel") {
        views_url <- "https://raw.githubusercontent.com/Bioconductor/BiocViews/devel/data/VIEWS"
    } else {
        views_url <- "https://raw.githubusercontent.com/Bioconductor/BiocViews/release/data/VIEWS"
    }
    
    tryCatch({
        # Read the VIEWS file (DCF format)
        temp_file <- tempfile(fileext = ".dcf")
        curl::curl_download(views_url, temp_file, quiet = TRUE)
        views <- read.dcf(temp_file)
        file.remove(temp_file)
        return(views)
    }, error = function(e) {
        warning("Failed to fetch Bioconductor VIEWS: ", e$message)
        return(data.frame())
    })
}

#' Determine package type
#'
#' @param pkg Character, package name
#' @param branch Character, either "devel" or "release"
#' @returns Character, package type
#'
#' @export
determinePackageType <- function(pkg, branch) {
    # This is a simplified version - in practice, we'd need to check
    # the Bioconductor manifest files or package metadata
    
    # For now, return "software" as default
    # In a real implementation, we'd check against the manifest files
    "software"
}
