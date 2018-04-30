replacementList <-
  list("SpaDES4Dummies.html" = "https://htmlpreview.github.io/?https://github.com/CeresBarros/SpaDES4Dummies/blob/master/SpaDES4Dummies.html",
       "GoogleDrive.html" = "https://drive.google.com/open?id=1XnfUTRk59dORiPbdN2sreGDXNmDjcUle",
       "Caching.html" = "https://cran.r-project.org/web/packages/SpaDES/vignettes/iii-cache.html",
       "Plotting.html" = "http://quickplot.predictiveecology.org/articles/iii-plotting.html",
       "Debugging.html" = "https://github.com/PredictiveEcology/SpaDES/wiki/Debugging",
       "Articles <small>version&nbsp;0.1.0</small>" = "Content")

#' Quick and dirty render pkgdown asis
#'
#' This will first run pkgdown::build_site then run rmarkdown::render to
#' build the html pages. This is only relevant if the rmarkdown header
#' contains an html output format that is not supported by pkgdown
#' (e.g., ioslides, slidy). This will not work for pdf.
#' @keywords internal
#' @importFrom reproducible Cache asPath
#' @rdname asis
#' @export
.asis <- function(..., replacements, notOlderThan = NULL, cacheRepo = "docs/cache") {

  build_site(..., cacheRepo = cacheRepo)
  args <- formals(pkgdown::build_site)
  #out <- Cache(pkgdown::init_site, pkg = args$pkg, path = args$path, notOlderThan = notOlderThan, cacheRepo = cacheRepo)
  #out <- pkgdown::init_site(pkg = args$pkg, path = args$path)

  #out <- build_articles(..., cacheRepo = cacheRepo)

  #out <- Cache(do.call, pkgdown::build_home, args[names(args) %in% names(formals(pkgdown::build_home))], notOlderThan = notOlderThan, cacheRepo = cacheRepo)
  #out <- do.call(pkgdown::build_home, args[names(args) %in% names(formals(pkgdown::build_home))])

  #out <- Cache(do.call, pkgdown::build_reference, args[names(args) %in% names(formals(pkgdown::build_reference))], notOlderThan = notOlderThan, cacheRepo = cacheRepo)
  #out <- do.call(pkgdown::build_reference, args[names(args) %in% names(formals(pkgdown::build_reference))])

  a <- dir("vignettes", full.names = TRUE, pattern = "Rmd")
  hasSlides <- unlist(lapply(a, function(x) {
    any(grepl(readLines(x), pattern = "slidy|ioslides"))
  }))
  if (any(hasSlides)) {
    a <- a[hasSlides]

    Cache(renderSlides, a, cacheRepo = cacheRepo)
  }

  # replace modules
  if (missing(replacements )) replacements <- replacementList
  replaceRemoteLinksInArticles(replacements)

}


#' @export
replaceRemoteLinksInArticles <- function(replacements) {
  lapply(names(replacements), function(nam) {
    indexHTML <- file.path("docs", "articles", "index.html")
    cc <- readLines(indexHTML)
    cc1 <- gsub(cc, pattern = nam,
         replacement = replacements[[nam]])
    writeLines(cc1, indexHTML)
  })
}

#' @export
build_site <- function (pkg = ".", path = "docs", examples = TRUE, run_dont_run = FALSE,
          mathjax = TRUE, preview = interactive(), seed = 1014, encoding = "UTF-8", cacheRepo = "cache")
  {
    old <- pkgdown:::set_pkgdown_env("true")
    on.exit(pkgdown:::set_pkgdown_env(old))
    pkg <- pkgdown:::as_pkgdown(pkg)
    path <- pkgdown:::path_abs(path, pkg$src_path)
    pkgdown:::init_site(pkg)
    pkgdown:::build_home(pkg)
    pkgdown:::build_reference(pkg, lazy = FALSE, examples = examples, run_dont_run = run_dont_run,
                    mathjax = mathjax, seed = seed)
    build_articles(pkg, path = file.path(path, "articles"), depth = 1L,
                   encoding = encoding, cacheRepo = cacheRepo)
    pkgdown:::build_news(pkg, path = file.path(path, "news"), depth = 1L)
    if (preview) {
      pkgdown:::preview_site(path)
    }
    invisible(TRUE)
  }

#' build_articles from pkgdown, but with Caching
#'
#' @export
build_articles <- function (pkg = ".", path = "docs/articles", depth = 1L, encoding = "UTF-8",
                                  quiet = TRUE, notOlderThan = NULL, cacheRepo = "cache")
{

  old <- pkgdown:::set_pkgdown_env("true")
  on.exit(pkgdown:::set_pkgdown_env(old))
  pkg <- pkgdown:::as_pkgdown(pkg)
  path <- pkgdown:::path_abs(path, pkg$src_path)
  # if (!pkgdown:::has_vignettes(pkg$src_path)) {
  #   return(invisible())
  # }
  pkgdown:::rule("Building articles")
  fs::dir_create(path)

  # pkgdown:::dir_copy_to(pkg, file.path(pkg$src_path, "vignettes"), path)  ## Ceres: doesn't seem to work
  file.copy(from = file.path(pkg$src_path, "vignettes"), to = path, recursive = TRUE)

  ## Ceres: this is where I stopped - tried to update arguments here and in render_markdown, but no success...
  # articles <- tibble::tibble(input = file.path(path, pkg$vignettes$file_in),
  #                            output_file = pkg$vignettes$file_out, depth = pkg$vignettes$vig_depth +
  #                              depth)

  articles <- tibble::tibble(input = file.path(path, pkg$vignettes$file_in),
                             output_file = pkg$vignettes$file_out, depth = 1L)

  data <- list(pagetitle = "$title$")
  end <- lapply(seq(NROW(articles)), function(r) {
  browser()
      aa <- Cache(pkgdown:::render_rmarkdown, input = asPath(articles$input[r]), output = asPath(articles$output_file[r]),
          quick = TRUE,
          pkg = pkg, quiet = quiet, cacheRepo = cacheRepo,
          notOlderThan = notOlderThan, omitArgs = "pkg")#,
          #sideEffect = dirname(articles$input)[r])
  })
  purrr::walk(articles$input, unlink)
  pkgdown:::build_articles_index(pkg, path = path, depth = depth)
  invisible()

}

renderSlides <- function(a) {
  lapply(a, function(x) rmarkdown::render(x))
  htmlFilenames <- gsub(a, pattern = "Rmd", replacement = "html")
  file.copy(htmlFilenames, to = "docs/articles", overwrite = TRUE)
  unlink(htmlFilenames)
}
