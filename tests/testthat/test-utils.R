test_that(".fmt_deps formats package/version pairs and drops empty roles", {
    deps <- data.frame(
        role = c("Depends", "Imports", "Imports", "Suggests"),
        package = c("R", "methods", "stats", "testthat"),
        version = c("4.0.0", NA, NA, NA),
        stringsAsFactors = FALSE
    )

    expect_equal(.fmt_deps(deps, "Depends"), "R (4.0.0)")
    expect_equal(.fmt_deps(deps, "Imports"), "methods, stats")
    expect_equal(.fmt_deps(deps, "Suggests"), "testthat")
    expect_null(.fmt_deps(deps, "LinkingTo"))
})

test_that(".fmt_rel_paths builds vignette paths and handles empty input", {
    vignettes <- data.frame(
        filename = c("intro.html", "advanced.html"),
        stringsAsFactors = FALSE
    )

    result <- .fmt_rel_paths(vignettes, "filename", "MyPkg")
    expect_equal(
        result,
        "vignettes/MyPkg/inst/doc/intro.html,\n\tvignettes/MyPkg/inst/doc/advanced.html"
    )
    expect_identical(.fmt_rel_paths(list(), "filename", "MyPkg"), NA_character_)
})

test_that(".fmt_titles concatenates titles and handles empty input", {
    vignettes <- data.frame(title = c("Introduction", "Advanced Usage"),
                            stringsAsFactors = FALSE)

    expect_equal(.fmt_titles(vignettes), "Introduction,\n\tAdvanced Usage")
    expect_identical(.fmt_titles(list()), NA_character_)
})

test_that(".get_branch maps release versions and passes through devel", {
    expect_equal(.get_branch("release", "3.19"), "RELEASE_3_19")
    expect_equal(.get_branch("devel", "3.20"), "devel")
})

test_that(".has detects file presence case-sensitively and handles empty input", {
    assets <- c("readme.md", "news.txt", "LICENSE")

    expect_true(.has(assets, "readme.md"))
    expect_false(.has(assets, "vignette.html"))
    expect_identical(.has(character(0), "readme.md"), NA_character_)
    # .has() is case-sensitive: an upper-case filename will not match
    expect_false(.has(c("README.md"), "readme.md"))
})

test_that(".save_as writes json and dcf files", {
    tmp <- tempfile()
    on.exit(unlink(c(paste0(tmp, ".json"), tmp), force = TRUE), add = TRUE)

    df <- data.frame(Package = "MyPkg", Version = "1.0.0",
                     stringsAsFactors = FALSE)

    .save_as(df, tmp, "json")
    expect_true(file.exists(paste0(tmp, ".json")))
    written <- jsonlite::read_json(paste0(tmp, ".json"))
    expect_equal(written[[1]]$Package, "MyPkg")

    .save_as(df, tmp, "dcf")
    expect_true(file.exists(tmp))
    written_dcf <- read.dcf(tmp)
    expect_equal(unname(written_dcf[1, "Package"]), "MyPkg")
})

test_that(".valid_branch accepts known branches and rejects others", {
    expect_equal(.valid_branch("devel"), "devel")
    expect_equal(.valid_branch("release"), "release")
    expect_error(.valid_branch("staging"), "Invalid branch name")
})

test_that(".valid_increment compares package versions", {
    expect_true(.valid_increment("1.0.0", "1.0.1"))
    expect_true(.valid_increment("1.0.0", "1.0.0"))
    expect_false(.valid_increment("1.2.0", "1.1.9"))
})

test_that(".valid_package_type maps software to bioc and passes through others", {
    expect_equal(.valid_package_type("software"), "bioc")
    expect_equal(.valid_package_type("workflows"), "workflows")
    expect_error(.valid_package_type("not-a-type"))
})

test_that(".wrap_field wraps long text and handles missing input", {
    expect_identical(.wrap_field(NA), NA_character_)
    expect_identical(.wrap_field(character(0)), NA_character_)

    long_text <- paste(rep("word", 30), collapse = " ")
    wrapped <- .wrap_field(long_text, width = 20, indent = 4)
    expect_true(all(nchar(strsplit(wrapped, "\n")[[1]]) <= 20))
    expect_match(wrapped, "\n")
})
