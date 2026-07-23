#' Prepare package VIEW data
#'
#' @description Prepare the VIEW of a package. Does not include
#' source, binary, extra doc, Rfiles, and reverse dependency fields.
#'
#' @param df data.frame R Universe api data
#' @param git_branch character "devel" or "RELEASE_X_Y"
#'
#' @returns a list of named fields for a package VIEW entry in the VIEWS
#' file
#'
#' @examples
#' bu <- biocUniTools::uni_for_bioc("devel")
#' df <- biocUniTools::get_jobs(bu)
#' git_branch <- .get_branch(bu$bioc_branch, bu$bioc_version)
#' view <- package_view(df[1, ], git_branch)
#'
#' @export
package_view <- function(df, git_branch) { 
    fields <- list(
        Package = df$Package,
        Version = df$Version,
        Depends = .fmt_deps(df$`_dependencies`, "Depends"),
        Imports = .fmt_deps(df$`_dependencies`, "Imports"),
        Suggests = .fmt_deps(df$`_dependencies`, "Suggests"),
        LinkingTo = .fmt_deps(df$`_dependencies`, "LinkingTo"),
        License = df$License,
        SystemRequirements = df$SystemRequirements,
        MD5sum = df$MD5sum,
        NeedsCompilation = df$NeedsCompilation,
        Archs = df$arches,
        Title = df$Title,
        Description = .wrap_field(df$Description),
        biocViews = df$biocViews,
        Author = gsub("\n|\\s+", " ", .wrap_field(df$Author)),
        Maintainer = df$Maintainer,
        URL = df$URL,
        VignetteBuilder = df$VignetteBuilder,
        Video = df$Video,
        BugReports = df$BugReports,
        PackageStatus = df$PackageStatus, # deprecated packages removed immediately
        git_url = df$`_upstream`,
        git_branch = git_branch,
        git_last_commit = df$RemoteSha,
        git_last_commit_date = substr(df$`Date/Publication`, 1, 10),
        `Date/Publication` = substr(df$`Date/Publication`, 1, 10),
        `Config/Bioconductor/UnsupportedPlatforms` = df$`UnsupportedPlatforms`,
        vignettes = .fmt_rel_paths(df$`_vignettes`, "filename", df$Package),
        vignetteTitles = .fmt_titles(df$`_vignettes`),
        hasREADME = .has(df$`_assets`, "readme.md"),
        hasNEWS = .has(df$`_assets`, "news.txt"),
        hasLICENSE = .has(df$`_assets`, "LICENSE")
    )

    fields[!vapply(fields, is.null, logical(1))]
}

#' Read a single package view from JSON file
#'
#' @param package character package name
#' @param path character
#'
#' @returns list, the package view data
#'
#' @examples
#' view <- read_view("devel/software", "BiocCheck")
#'
#' @export
read_view <- function(package, path) {
    package_view_path <- file.path("views", path)
    if (!grepl(".json", package_view_path))
        package_view_path <- file.path(package_view_path, paste0(package, ".json"))
    logger::log_info("read_view({package_view_path})")
    jsonlite::read_json(package_view_path)
}

#' Wrap every field of a view as a length-1 list-cell so all views
#' share a consistent type per field, regardless of whether that
#' field happens to be scalar or multi-valued in a given file
#'
#' @param view list, a single package's parsed JSON
#' @returns tibble, one row, every column a list-column
view_to_row <- function(view) {
    tibble::as_tibble(lapply(view, list))
}

#' Simplify list-columns back to atomic vectors where every row's
#' value is actually a length-1 atomic value; leave real list-columns
#' (multi-valued fields) untouched
#'
#' @param df data.frame with list-columns from bind_rows(view_to_row(...))
#' @returns data.frame
simplify_columns <- function(df) {
    df[] <- lapply(df, function(col) {
        col <- lapply(col, function(x) if (length(x) == 0) NA else x)
        is_scalar <- vapply(col, function(x) is.atomic(x) && length(x) == 1, logical(1))
        if (all(is_scalar)) unlist(col) else col
    })
    df
}

#' Read all package views
#'
#' @param branch character "release" or "devel"
#' @param package_type character a value in in .PACKAGE_TYPES
#'
#' @returns data.frame, all package view data, with list-columns for
#'   fields such as Suggests/Depends/Imports/biocViews that have
#'   multiple values in at least one package
#'
#' @examples
#' views <- read_package_views("devel", "software")
#'
#' @export
read_package_views <- function(branch, package_type) {
    file_path <- file.path(branch, package_type)

    views <- list.files(file.path("views", file_path), pattern = "\\.json") |>
        purrr::map(~ read_view(gsub(".json", "", .x), file_path))

    views |>
        purrr::map(view_to_row) |>
        dplyr::bind_rows() |>
        simplify_columns() |>
        dplyr::arrange(Package)
}

#' Filter for packages passing R-Universe R CMD check
#'
#' @param packages vector packages that were updated recently
#' @param universe character corresponding R Universe
#' @param os character (default: linux, win, mac) OSes required to pass
#'
#' @returns data.frame Packages passing on an OS
#'
#' @examples
#' bu <- biocUniTools::uni_for_bioc("release")
#' packages <- c("BiocCheck", "BiocFileCache")
#' passed <- passed_packages(packages, bu$universe)
#'
#' @export
passed_packages <- function(packages, universe, os = c("linux", "win", "mac")) {
    if (length(packages) == 0)
        return(data.frame(Package = character(), OS = character(),
                          Status = character()))

    BiocPropagate::propagate(packages, universe, os)
    # data.frame Package, os TRUE vs False
}

#' Identify new R Universe builds
#'
#' @description
#' Compares each package's `git_last_commit` against R-Universe's `RemoteSha`
#' Packages with no view are treated as changed.
#'
#' @param views data.frame views corresponding to a branch
#' @param universe_df data.frame universe data corresponding to branch
#' must have Package and RemoteSha columns
#'
#' @returns character vector of package names that need updating
#'
#' @examples
#' views <- read_package_views(c("BiocCheck", "a4Reporting"),"devel",
#'     "software")
#' universe_df <- biocUniTools::get_raw_uni_df("bioc")
#' changed <- changed_packages(views, universe_df)
#'
#' @export
changed_packages <- function(views, universe_df) {
    universe_df <- universe_df |>
        dplyr::distinct(Package, .keep_all = TRUE) |>
        dplyr::mutate(ru_sha7 = substr(RemoteSha, 1, 7)) |>
        dplyr::select(Package, ru_sha7)

    if (nrow(views) == 0) {
        logger::log_info("No views available to check against")
        return(c())
    }

    if (!"git_last_commit" %in% names(views))
        stop("git_last_commit not available in views to check against")

    merged <- dplyr::left_join(universe_df, views, by = "Package")

    merged |>
        dplyr::filter(is.na(git_last_commit) | ru_sha7 != git_last_commit) |>
        dplyr::pull(Package) |>
        unique()
}

#' Passed packages VIEWS update
#'
#' @param views data.frame representing VIEWS
#' @param universe character corresponding R Universe
#' @param r_version character R version
#' @param os (default: linux, win, mac) OSes required to pass
#'
#' @returns data.frame, one row per package source or binary
#'
#' @examples
#' bu <- biocUniTools::uni_for_bioc("release")
#' views <- read_package_views(c("BiocCheck", "bedbaser"), "release",
#'     "software")
#' passed <- passed_criteria(views, bu$universe, bu$r_version)
#'
#' @export
passed_criteria <- function(views, universe, r_version,
                            os = c("linux", "win", "mac")) {
    raw_universe_df <- biocUniTools::get_raw_uni_df(universe)
    changed <- changed_packages(views, raw_universe_df)
    passed <- passed_packages(changed, universe, os) |>
        dplyr::filter(Status == TRUE)
    universe_df <- biocUniTools::get_uni_df(raw_universe_df)
    passed_df <- biocUniTools::get_jobs(universe_df, universe, r_version) |>
        dplyr::left_join(passed, by = dplyr::join_by(Package == Package,
                                                     job_os == OS,
                                                     job_arch == Arch))

    missing <- passed_df |> dplyr::filter(is.na(Status))
    if (nrow(missing) > 0)
        logger::log_warn("No propagate() result for {nrow(missing)} job(s)")
    
    passed_df |> dplyr::filter(Status == TRUE)
}

#' Write view
#'
#' @param df data.frame package data
#' @param save_path character path
#' @param ext character json or dcf
#'
#' @examples
#' bu <- biocUniTools::uni_for_bioc("devel")
#' df <- biocUniTools::get_jobs(bu)
#' view <- package_view(df[1, ], bu)
#' write_view(view, tempfile())
#'
#' @export
write_view <- function(df, save_path, ext = c("json", "dcf")) {
    .save_as(df, file.path("views", save_path), ext)
}

#' Write package views
#'
#' @param packages_df data.frame of passed package information
#' @param branch character "release" or "devel"
#' @param bioc_version character Bioconductor version
#' @param package_type character
#' @param verbose logical (default: FALSE)
#'
#' @returns integer number of views updated
#'
#' @examples
#' bu <- biocUniTools::uni_for_bioc("devel")
#' packages_df <- biocUniTools::get_raw_uni_df(bu$universe)
#' write_package_views(packages_df, bu$bioc_branch, bu$bioc_version, "software")
#'
#' @export 
write_package_views <- function(packages_df, branch, bioc_version, package_type,
                                verbose = FALSE) {
    n <- nrow(packages_df)
    if (n == 0)
        return(0L)

    git_branch <- .get_branch(branch, bioc_version)
    updated <- 0L
    for (i in seq_len(n)) {
        pkg_row <- as.list(packages_df[i, ])
        pkg_row <- lapply(pkg_row, function(col) {
            if (is.list(col))
                col[[1]]
            else
                col
        })

        view <- package_view(pkg_row, git_branch)
        branch <- ifelse(branch == "devel", "devel", "release")
        save_path <- file.path(branch, package_type, pkg_row$Package)
        write_view(view, save_path, "json")
        if (verbose)
            logger::log_info("Updated {save_path}")
        updated <- updated + 1L
    }

    if (verbose)
        logger::log_info("Updated", updated)
    updated
}

#' Update the views for passed packages of a package_type
#'
#' @param manifest_url character (default: .MANIFEST_URL) 
#' @param os character or vector of "linux", "mac", "win", etc
#' @param branch character or vector in .BRANCHES
#' @param package_type character or vector in .PACKAGE_TYPES 
#'
#' @returns vector of characters paths to the package_type VIEWS 
#'
#' @examples
#' update_package_type_views()
#'
#' @export 
update_package_type_views <- function(manifest_url = .MANIFEST_URL,
                                      os = .OS, branch = .BRANCHES,
                                      package_type = .PACKAGE_TYPES) {
    paths <- c()
    for (b in branch) {
        packages_by_type <- read_manifest(manifest_url = manifest_url, b)
        bu <- biocUniTools::uni_for_bioc(b)
        logger::log_info("Universe: {bu$universe}")
        raw_universe_df <- biocUniTools::get_raw_uni_df(bu$universe)
        for (pt in package_type) {
            logger::log_info("Package type: {package_type}")
            packages <- packages_by_type[[pt]][, "Package"]
            logger::log_info("Packages: {packages}")
            views <- read_package_views(b, pt)
            raw_universe_df <- biocUniTools::get_raw_uni_df(bu$universe)
            missing <- setdiff(packages, raw_universe_df$Package)
            if (length(missing) > 0)
                logger::log_warn(
                    "{length(missing)} manifest package(s) not yet built on R-Universe: {paste(missing, collapse = ', ')}"
                )
            passed <- passed_criteria(views, bu$universe, bu$r_version, os)
            write_package_views(passed, bu$bioc_branch, bu$bioc_version, pt)

            # TODO calculate additional fields

            for (ext in c("json", "dcf")) {
                save_path <- file.path(b, pt, "VIEWS")
                write_view(views, save_path, ext)
                paths <- c(paths, save_path)
            }
        }
    }
    paths
}
