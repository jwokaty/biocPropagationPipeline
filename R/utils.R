.BRANCHES <- c("devel", "release")

.MANIFEST_URL <- "https://git.bioconductor.org/admin/manifest"

#' oses of interest
.OS <- c("linux", "macosx", "windows")

.PACKAGE_TYPES <- c("software", "data-annotation", "data-experiment",
                    "workflows", "books")

#' Passing R CMD check status
.PASS <- c("OK", "WARNING")

#' Format dependencies
.fmt_deps <- function(deps, role) {
    x <- deps[deps$role == role, ]
    if (!nrow(x)) {
        return(NULL)
    }

    paste(
        ifelse(
            is.na(x$version),
            x$package,
            paste0(x$package, " (", x$version, ")")
        ),
        collapse = ", "
    )
}

#' Format relative paths
.fmt_rel_paths <- function(x, field, pkg) {
    if (length(x) == 0) {
        return(NA_character_)
    }
    paste0(
        paste0(
            "vignettes/",
            pkg,
            "/inst/doc/",
            x[[field]]
        ),
        collapse = ",\n\t"
    )
}

.fmt_titles <- function(x) {
    if (length(x) == 0) {
        return(NA_character_)
    }
    paste0(x$title, collapse = ",\n\t")
}

.get_branch <- function(branch, bioc_version) {
    ifelse(branch == "release",
           paste0("RELEASE_", gsub("\\.", "_", bioc_version)),
           branch)
}

.has <- function(x, a_file) {
    if (length(x) == 0) {
        return(NA_character_)
    }
    any(
        grepl(
            a_file,
            x
        )
    )
}

.save_as <- function(df, save_path, ext = c("json", "dcf")) {
    ext <- match.arg(ext)
    if (ext == "dcf") {
        write.dcf(df, save_path)
    } else {
        jsonlite::write_json(df, paste(save_path, "json", sep = "."), 
                             pretty = TRUE)
    }
}

.start_logger <- function(path, level = logger::LOG_INFO) {
    logger::log_threshold(level)
    logger::log_appender(logger::appender_file(path))
    logger::log_info("Logging on")
}

.valid_branch <- function(branch) {
    if (!branch %in% .BRANCHES)
        stop("Invalid branch name: ", branch)
    branch
}

.valid_increment <- function(version1, version2) {
    package_version(version1) <= package_version(version2)
}

.valid_package_type <- function(package_type = .PACKAGE_TYPES) {
    package_type <- match.arg(package_type)
    ifelse(package_type == "software", "bioc", package_type)
}

.wrap_field <- function(x, width = 78, indent = 8) {
    if (length(x) == 0 || is.na(x)) {
        return(NA_character_)
    }
    paste(
        strwrap(x, width = width, exdent = indent),
        collapse = "\n"
    )
}

read_manifest <- function(manifest_url = .MANIFEST_URL, branch = .BRANCHES,
                          verbose = FALSE) {
    logger::log_info(paste("Reading", manifest_url))
    branch <- match.arg(branch)
    path <- file.path(tempfile(pattern = "bioconductor-"), "manifest")
    dir.create(path, recursive = TRUE)
    repo <- git2r::clone(manifest_url, path, branch = branch)
    paths <- file.path(path, paste0(.PACKAGE_TYPES, ".txt"))
    packageTypes <- lapply(paths, read.dcf)
    names(packageTypes) <- .PACKAGE_TYPES
    packageTypes
}
