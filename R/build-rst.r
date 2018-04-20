#' Build complete sphinx documentation for a package.
#'
#' Currently, build_mkdocs builds documentation for:
#' \itemize{
#'   \item The package DESCRIPTION
#'   \item Help topics
#'   \item Vignettes
#'   \item README.md files
#' }
#'
#' @param  pkg        path to source version of package.  See
#'                    \link[devtools]{as.package} for details on how paths and package
#'                    names are resolved.
#' @param  doc_path   directory in which to create documentation. If
#'                    \code{NULL} this defaults to \code{pkg}/docs.
#' @param  yaml       if \code{TRUE} generate the \code{mkdocs.yml} file in the parent
#'                    directory of \code{doc_path}.
#' @param  ...        Other additional arguments passed to \code{\link{as.sd_package}}
#'                    used to override package defaults.
#'
#' @import stringr
#' @importFrom devtools load_all
#' @export
build_rst <- function(pkg=".", doc_path=NULL, ...) {
  # Make doc directory
  if (!file.exists(doc_path)) {
    dir.create(doc_path, recursive = TRUE)
  }

  pkg <- as.sd_package(pkg, site_path=doc_path, mathjax=FALSE, ...)
  #load_all(pkg)

  pkg$topics <- build_rst_topics(pkg)
  #pkg$vignettes <- build_md_vignettes(pkg)
  #pkg$news <- build_md_news(pkg)
  #pkg$index <- build_md_index(pkg)

  invisible(TRUE)
}


#' Generate all topic pages for a package.
#'
#' @param pkg path to source version of package.  See
#'   \code{\link[devtools]{as.package}} for details on how paths and package
#'   names are resolved.
#' @param  doc_path   directory in which to create documentation. If
#'                    \code{NULL} this defaults to \code{pkg}/docs.
#' @param ... Other additional arguments passed to \code{\link{as.sd_package}}
#'   used to override package defaults.
#'
#' @export
build_rst_topics <- function(pkg=".", doc_path=NULL, ...) {
  #load_all(pkg)
  pkg <- as.sd_package(pkg, site_path=doc_path, mathjax=FALSE, ...)

  # for each file, find name of one topic
  index <- pkg$rd_index
  index$file_out <- str_replace(index$file_out, "\\.html$", ".rst")
  paths <- file.path(pkg$site_path, "topics", index$file_out)

  ## output path for all topics,
  pkg$topic_path = file.path(pkg$site_path, "topics")
  if(!file.exists(pkg$topic_path)) {
    dir.create(pkg$topic_path, recursive = TRUE)
  }

  # create columns for extra topic info
  index$title <- ""
  index$in_index <- TRUE

  for (i in seq_along(index$name)) {
    message("Generating ", basename(paths[[i]]))

    rd <- pkg$rd[[i]]
    rst <- to_rst.Rd_doc(rd,
                         env = new.env(parent = globalenv()),
                         topic = stringr::str_replace(basename(paths[[i]]), "\\.rst$", ""),
                         pkg = pkg)
    rst$pagetitle <- rst$name

    rst$package <- pkg[c("package", "version")]
    render_page(rst, style="sphinx", format="rst", path=paths[[i]])
    graphics.off()

    if ("internal" %in% rst$keywords) {
      index$in_index[i] <- FALSE
    }
    index$title[i] <- rst$title
  }

  index
}
