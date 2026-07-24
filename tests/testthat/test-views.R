make_fake_df_row <- function() {
    list(
        Package = "TestPkg",
        Version = "1.2.3",
        `_dependencies` = data.frame(
            role = c("Depends", "Imports", "Imports"),
            package = c("R", "methods", "stats"),
            version = c("4.0.0", NA, NA),
            stringsAsFactors = FALSE
        ),
        License = "GPL-3",
        SystemRequirements = NA,
        MD5sum = "d41d8cd98f00b204e9800998ecf8427e",
        NeedsCompilation = "no",
        arches = NA,
        Title = "A Test Package",
        Description = "This package exists only to exercise package_view().",
        biocViews = "Software",
        Author = "Jane Doe",
        Maintainer = "Jane Doe <jane@example.com>",
        URL = "https://example.com/TestPkg",
        VignetteBuilder = "knitr",
        Video = NA,
        BugReports = "https://example.com/TestPkg/issues",
        PackageStatus = "ok",
        `_upstream` = "https://github.com/example/TestPkg",
        RemoteSha = "abcdef1234567890",
        `Date/Publication` = "2024-01-15 10:00:00",
        UnsupportedPlatforms = NA,
        `_vignettes` = data.frame(
            filename = "intro.html",
            title = "Introduction",
            stringsAsFactors = FALSE
        ),
        `_assets` = c("readme.md", "news.txt")
    )
}

test_that("package_view assembles the expected fields", {
    view <- package_view(make_fake_df_row(), "devel")

    expect_equal(view$Package, "TestPkg")
    expect_equal(view$Version, "1.2.3")
    expect_equal(view$Depends, "R (4.0.0)")
    expect_equal(view$Imports, "methods, stats")
    expect_equal(view$git_branch, "devel")
    expect_equal(view$git_last_commit, "abcdef1234567890")
    expect_equal(view$git_last_commit_date, "2024-01-15")
    expect_equal(view$`Date/Publication`, "2024-01-15")
    expect_equal(view$vignettes, "vignettes/TestPkg/inst/doc/intro.html")
    expect_equal(view$vignetteTitles, "Introduction")
    expect_true(view$hasREADME)
    expect_true(view$hasNEWS)
})

test_that("package_view drops fields that are NULL/NA-absent from the input", {
    view <- package_view(make_fake_df_row(), "devel")

    # SystemRequirements and arches were NA -> .wrap_field/.has style fields
    # only drop when the source lookup itself is NULL; NA is kept as-is
    expect_false("LinkingTo" %in% names(view))
})

test_that("write_view + read_view round-trip a view to JSON", {
    tmp_dir <- tempfile("views-test-")
    dir.create(file.path(tmp_dir, "views", "devel", "software"), recursive = TRUE)
    old_wd <- setwd(tmp_dir)
    on.exit({ setwd(old_wd); unlink(tmp_dir, recursive = TRUE) }, add = TRUE)

    view <- package_view(make_fake_df_row(), "devel")
    write_view(view, file.path("devel", "software", "TestPkg"), "json")

    # jsonlite::read_json() (simplifyVector = FALSE) returns every field as
    # a length-1 list, not an atomic scalar
    round_tripped <- read_view("TestPkg", file.path("devel", "software"))
    expect_equal(round_tripped$Package[[1]], "TestPkg")
    expect_equal(round_tripped$Version[[1]], "1.2.3")
})

test_that("write_package_views writes a file read_view can find (regression: no double .json)", {
    tmp_dir <- tempfile("views-test-")
    dir.create(file.path(tmp_dir, "views", "devel", "software"), recursive = TRUE)
    old_wd <- setwd(tmp_dir)
    on.exit({ setwd(old_wd); unlink(tmp_dir, recursive = TRUE) }, add = TRUE)

    row <- make_fake_df_row()
    packages_df <- data.frame(
        Package = row$Package, Version = row$Version, License = row$License,
        SystemRequirements = NA, MD5sum = row$MD5sum,
        NeedsCompilation = row$NeedsCompilation, arches = NA, Title = row$Title,
        Description = row$Description, biocViews = row$biocViews,
        Author = row$Author, Maintainer = row$Maintainer, URL = row$URL,
        VignetteBuilder = row$VignetteBuilder, Video = NA,
        BugReports = row$BugReports, PackageStatus = row$PackageStatus,
        RemoteSha = row$RemoteSha, UnsupportedPlatforms = NA,
        stringsAsFactors = FALSE
    )
    packages_df$`_dependencies` <- list(row$`_dependencies`)
    packages_df$`_upstream` <- row$`_upstream`
    packages_df$`Date/Publication` <- row$`Date/Publication`
    packages_df$`_vignettes` <- list(row$`_vignettes`)
    packages_df$`_assets` <- list(row$`_assets`)

    updated <- write_package_views(packages_df, "devel", "3.19", "software")

    expect_equal(updated, 1L)
    expect_true(file.exists(file.path("views", "devel", "software", "TestPkg.json")))
    expect_false(file.exists(file.path("views", "devel", "software", "TestPkg.json.json")))

    round_tripped <- read_view("TestPkg", file.path("devel", "software"))
    expect_equal(round_tripped$Package[[1]], "TestPkg")
    expect_equal(round_tripped$Depends[[1]], "R (4.0.0)")
    expect_equal(round_tripped$Imports[[1]], "methods, stats")
    expect_true(round_tripped$hasREADME[[1]])
    expect_true(round_tripped$hasNEWS[[1]])
})

test_that("view_to_row wraps every field as a length-1 list-column", {
    view <- list(Package = "TestPkg", Depends = c("a", "b"))
    row <- view_to_row(view)

    expect_s3_class(row, "tbl_df")
    expect_equal(nrow(row), 1)
    expect_type(row$Package, "list")
    expect_equal(row$Depends[[1]], c("a", "b"))
})

test_that("simplify_columns collapses scalar list-columns but keeps multi-value ones", {
    df <- data.frame(Package = I(list("A", "B")))
    df$Depends <- list("x", c("y", "z"))

    simplified <- simplify_columns(df)

    expect_type(simplified$Package, "character")
    expect_equal(simplified$Package, c("A", "B"))
    expect_type(simplified$Depends, "list")
})

test_that("simplify_columns replaces zero-length entries with NA", {
    df <- data.frame(Package = I(list("A", "B")))
    df$Missing <- list(character(0), "present")

    simplified <- simplify_columns(df)

    expect_true(is.na(simplified$Missing[1]))
    expect_equal(simplified$Missing[2], "present")
})

test_that("read_package_views reads and combines all JSON views in a directory", {
    tmp_dir <- tempfile("views-test-")
    view_dir <- file.path(tmp_dir, "views", "devel", "software")
    dir.create(view_dir, recursive = TRUE)
    old_wd <- setwd(tmp_dir)
    on.exit({ setwd(old_wd); unlink(tmp_dir, recursive = TRUE) }, add = TRUE)

    for (pkg in c("Beta", "Alpha")) {
        row <- make_fake_df_row()
        row$Package <- pkg
        write_view(package_view(row, "devel"),
                  file.path("devel", "software", pkg), "json")
    }

    views <- read_package_views("devel", "software")
    expect_equal(nrow(views), 2)
    # each Package cell is a length-1 list (see read_view), arranged by Package
    expect_equal(vapply(views$Package, `[[`, character(1), 1), c("Alpha", "Beta"))
})

test_that("read_package_views returns an empty typed data.frame if no views", {
    tmp_dir <- tempfile("views-test-")
    view_dir <- file.path(tmp_dir, "views", "devel", "software")
    dir.create(view_dir, recursive = TRUE)
    old_wd <- setwd(tmp_dir)
    on.exit({ setwd(old_wd); unlink(tmp_dir, recursive = TRUE) }, add = TRUE)

    views <- read_package_views("devel", "software")

    expect_equal(nrow(views), 0)
    expect_true(all(c("Package", "git_last_commit") %in% names(views)))
})

test_that("changed_packages flags packages missing from or stale vs. views", {
    views <- data.frame(
        Package = c("Alpha", "Beta"),
        git_last_commit = c("abcdef1", "1234567"),
        stringsAsFactors = FALSE
    )
    universe_df <- data.frame(
        Package = c("Alpha", "Beta", "Gamma"),
        RemoteSha = c("abcdef1000000000", "differentsha00000", "brandnewsha000000"),
        stringsAsFactors = FALSE
    )

    changed <- changed_packages(views, universe_df)

    expect_setequal(changed, c("Beta", "Gamma"))
    expect_false("Alpha" %in% changed)
})

test_that("changed_packages treats all packages as changed if no views", {
    empty_views <- data.frame(Package = character(),
                              git_last_commit = character(),
                              stringsAsFactors = FALSE)
    universe_df <- data.frame(
        Package = c("Alpha", "Beta"),
        RemoteSha = c("abcdef1000000000", "1234567000000000"),
        stringsAsFactors = FALSE
    )

    changed <- changed_packages(empty_views, universe_df)

    expect_setequal(changed, c("Alpha", "Beta"))
})

test_that("changed_packages errors when views lack git_last_commit", {
    views <- data.frame(Package = "Alpha", stringsAsFactors = FALSE)
    universe_df <- data.frame(Package = "Alpha", RemoteSha = "abcdef1000000000",
                              stringsAsFactors = FALSE)

    expect_error(changed_packages(views, universe_df), "git_last_commit")
})
