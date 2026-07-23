.MAX_TRIES <- 2

.runiverse_url <- function(pkg, branch, subpath = "") {
    if (subpath != "")
        subpath <- paste0(subpath, "/")
    bu <- biocUniTools::uni_for_bioc(branch)
    file.path(paste0("https://", bu$universe, ".r-universe.dev"), subpath, pkg)
}

.get_file <- function(file_url, save_path, max_retries = .MAX_TRIES,
                     quiet = TRUE) {
    for (i in 1:max_retries) {
        tryCatch({
            curl::curl_download(file_url, save_path, quiet = quiet)
            return(save_path)
        }, error = function(e) {
            message("Failed attempt ", i, " to download ", file_url)
            Sys.sleep(30)
        })
    }
    message("Failed to download ", file_url)
    return(NULL)
}

#' @examples
#' pkg_url <- .runiverse_url("BiocCheck", "devel")
#' get_citation(pkg_url)
#' 
#' @export
get_citation <- function(pkg_url, save_path = NULL, ext = "html") {
    if (is.null(save_path))
        save_path <- file.path("citations", basename(pkg_url), "citation.", ext)
    .get_file(file.path(pkg_url, paste0("citation.", ext)), save_path)
}

#' @examples
#' pkg_url <- .runiverse_url("BiocCheck", "devel")
#' get_license(pkg_url)
#' 
#' @export
get_license <- function(pkg_url, save_path = NULL) {
    if (is.null(save_path))
        save_path <- file.path("licenses", basename(pkg_url), "LICENSE")
    .get_file(file.path(pkg_url, "LICENSE"), save_path)
}

#'
#' @details Packages should be placed along the following paths where pkg is
#' the package name:
#' \itemize{
#'  \item{"manuals/pkg/refman/pkg.html"}
#'  \item{"manuals/pkg/man/pkg.pdf"}
#'  \item{"web/packages/pkg/refman/pkg.html"}
#'  \item{"web/packages/pkg/pkg.pdf"}
#' }
#'
#' @examples
#' pkg_url <- .runiverse_url("BiocCheck", "devel")
#' get_manual(pkg, pkg_url, ext = "pdf")
#' 
#' @export
get_manual <- function(pkg, pkg_url, save_path = NULL, ext = c("pdf", "html")) {
    ext <- match.arg(ext)
    stem <- paste0(pkg, ".", ext)
    if (is.null(save_path) && ext == "pdf")
        save_path <- file.path("manuals", pkg, "refman", stem)
    else if (is.null(save_path) && ext == "html")
        save_path <- file.path("manuals", pkg, "man", stem)
    suffix <- ifelse(ext == "pdf", paste0(pkg, ".pdf"), "doc/manual.html")
    .get_file(file.path(pkg_url, suffix), save_path)
}


#' @examples
#' pkg_url <- .runiverse_url("BiocCheck", "devel")
#' get_news(pkg_url)
#' 
#' @export
get_news <- function(pkg_url, save_path = NULL) {
    if (is.null(save_path))
        save_path <- file.path("news", basename(pkg_url), "NEWS")
    .get_file(file.path(pkg_url, "NEWS"), save_path)
}

#' @examples
#' pkg_url <- .runiverse_url("BiocCheck", "devel")
#' get_readme(pkg_url)
#' 
#' @export
get_readme <- function(pkg_url, save_path = NULL) {
    if (is.null(save_path))
        save_path <- file.path("readme", basename(pkg_url), "readme.html")
    .get_file(file.path(pkg_url, "doc/readme.html"), save_path)
}

#' @examples
#' pkg_url <- .runiverse_url("BiocCheck", "devel")
#' df <- get_ruData("BiocCheck", "devel")
#' view <- prepareView(df) 
#' get_vignettes(pkg_url, view$vignettes)
#'
#' @export
get_vignettes <- function(pkg_url, vignettes, save_path = NULL) {
    if (is.null(save_path))
        save_path <- file.path("vignettes", basename(pkg_url), "inst/doc")
    
    # Ensure save_path directory exists
    if (!dir.exists(dirname(save_path))) {
        dir.create(dirname(save_path), recursive = TRUE)
    }
    
    for (vignette in vignettes) {
        vignette_file <- basename(vignette)
        dest_path <- file.path(save_path, vignette_file)
        .get_file(file.path(pkg_url, vignette), dest_path)
    }
}

#' @export
getArtifacts <- function(view, branch) {
    pkg_url <- .runiverse_url(view$Package, branch)
    get_citation(pkg_url)
    get_license(pkg_url)
    get_manual(view$Package, pkg_url, ext = "pdf")
    get_manual(view$Package, pkg_url, ext = "html")
    get_news(pkg_url)
    get_readme(pkg_url)
    get_vignettes(pkg_url, view$assets)
}
