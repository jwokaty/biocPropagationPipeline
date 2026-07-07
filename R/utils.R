.PACKAGE_TYPES <- c("software", "data-annotation", "data-experiment",
                    "workflows", "books")

#' Compare package versions
#'
#' @param v1 Character, first version string
#' @param v2 Character, second version string
#' @returns Logical, TRUE if v1 > v2
#' @export
package_version <- function(v1, v2 = "0.0.0") {
    # Simple version comparison for Bioconductor packages
    # Format: X.Y.Z or X.Y
    
    # Split versions into numeric components
    parse_version <- function(v) {
        # Remove any leading/trailing whitespace
        v <- trimws(v)
        # Split by dots and convert to numeric
        parts <- strsplit(v, "\\.")[[1]]
        # Pad with zeros to ensure same length
        parts <- c(parts, rep("0", 3 - length(parts)))[1:3]
        as.numeric(parts)
    }
    
    v1_parts <- parse_version(v1)
    v2_parts <- parse_version(v2)
    
    # Compare component by component
    for (i in 1:3) {
        if (v1_parts[i] > v2_parts[i]) {
            return(TRUE)
        } else if (v1_parts[i] < v2_parts[i]) {
            return(FALSE)
        }
    }
    # Versions are equal
    FALSE
}

.save_as <- function(df, save_path, ext = c("json", "dcf")) {
    ext <- match.arg(ext)
    if (ext == "dcf") {
        write.dcf(df, save_path)
    } else {
        jsonlite::write_json(df, save_path, pretty = TRUE)
    }
}

getUni <- function(branch) {
    ifelse(branch == "devel", "bioc", "bioc-release")
}

getRuData <- function(pkg, branch) {
    uni <- getUni(branch)
    ru_api <- file.path(paste0("https://", uni, ".r-universe.dev/api/packages"),
                     pkg)
    jsonlite::fromJSON(ru_api)
}

.start_logger <- function(path, level = logger::LOG_INFO) {
    path <- file.path(tempdir(), tempfile(pattern = "log"))
    logger::log_threshold(level)
    logger::log_info("Logging on")
}

readManifest <- function(manifest_repo_url, branch) {
    path <- file.path(tempdir(), 
                      tempfile(pattern = "bioconductor-"),
                      "manifest")
    dir.create(path, recursive = TRUE)
    repo <- git2r::clone(manifest_repo_url, path, branch = branch)
    paths <- paste0(path, paste0(.PACKAGE_TYPES, ".txt"), sep = "/")
    packageTypes <- lapply(paths, read.dcf)
    names(packageTypes) <- .PACKAGE_TYPES
    packageTypes
}
