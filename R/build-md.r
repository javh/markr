#' Build complete MkDocs site
#'
#' Builds a complete MkDocs site for a package from the package's man pages, vignettes,
#' DESCRIPTION, README.md, NEWS.md and inst/CITATION files.
#'
#' @param  pkg        path to source version of package.  See
#'                    \link[devtools]{as.package} for details on how paths and package
#'                    names are resolved.
#' @param  doc_path   directory in which to create documentation. If
#'                    \code{NULL} this defaults to \code{pkg}/docs.
#' @param  yaml       if \code{TRUE} generate the \code{mkdocs.yml} file in the parent
#'                    directory of \code{doc_path}.
#'
#' @import stringr
#' @importFrom devtools load_all
#' @export
build_mkdocs <- function(pkg=".", doc_path=NULL, yaml=FALSE, style=c("mkdocs", "sphinx")) {
    # Check arguments
    style <- match.arg(style)

    # Make doc directory
    if (!file.exists(doc_path)) {
        dir.create(doc_path, recursive = TRUE)
    }

    pkg <- as.sd_package(pkg, site_path=doc_path, mathjax=FALSE)
    #load_all(pkg)

    pkg$topics <- build_md_topics(pkg, style=style)
    pkg$vignettes <- build_md_vignettes(pkg)
    pkg$news <- build_md_news(pkg, style=style)
    pkg$index <- build_md_index(pkg, style=style)

    if (yaml) { build_yaml(pkg) }

    invisible(TRUE)
}

#' @export
#' @importFrom   stringi    stri_join
#'                          stri_replace_first stri_replace_last stri_replace_all
run_pandoc <- function(doc_path=NULL, format="rst", delete=TRUE) {
    # Make doc directory
    if (!file.exists(doc_path)) {
        dir.create(doc_path, recursive = TRUE)
    }
    md_files <- list.files(doc_path, '.md$', full.names=TRUE, recursive=TRUE)

    # Base pandoc command
    pandoc_cmd <- stri_join("pandoc -f markdown -t", format, sep=" ")

    # Convert each file in the document path
    pandoc_files <- character()
    for (in_file in md_files) {
        cat("Converting", basename(in_file), "to", format, "\n")
        out_file <- stri_replace_last(in_file, 'rst', regex='md$')
        cmd <- stri_join(pandoc_cmd, "-o", out_file, in_file, sep=" ")
        system(cmd)
        pandoc_files <- c(pandoc_files, out_file)

        # Fix links
        in_text  <- readLines(out_file)
        out_text  <- stri_replace_all(in_text, ".html>", fixed=".md>")
        writeLines(out_text, con=out_file)
    }

    if (delete)
    {
        for (f in md_files) { file.remove(f) }
    }

    return(pandoc_files)
}


#' Build complete sphinx (recommonmark) documentation for a package.
#' @export
build_sphinx <- function(pkg=".", doc_path=NULL) {
    # Check arguments
    style <- "sphinx"

    # Make doc directory
    if (!file.exists(doc_path)) {
        dir.create(doc_path, recursive = TRUE)
    }

    pkg <- as.sd_package(pkg, site_path=doc_path, mathjax=FALSE)

    pkg$topics <- build_rst_topics(pkg)
    pkg$vignettes <- build_md_vignettes(pkg)
    pkg$news <- build_md_news(pkg)
    pkg$index <- build_md_index(pkg)

    invisible(TRUE)
}


#' Generate all topic pages for a package.
#'
#' @param pkg path to source version of package.  See
#'   \link[devtools]{as.package} for details on how paths and package
#'   names are resolved.
#' @param  doc_path   directory in which to create documentation. If
#'                    \code{NULL} this defaults to \code{pkg}/docs.
#' @param ... Other additional arguments passed to \link{as.sd_package}
#'   used to override package defaults.
#'
#' @export
build_md_topics <- function(pkg=".", doc_path=NULL, style=c("mkdocs", "sphinx")) {
    # Check arguments
    style <- match.arg(style)

    #load_all(pkg)
    pkg <- as.sd_package(pkg, site_path=doc_path, mathjax=FALSE)

    # for each file, find name of one topic
    index <- pkg$rd_index
    index$file_out <- str_replace(index$file_out, "\\.html$", ".md")
    paths <- file.path(pkg$site_path, "topics", index$file_out)

    ## output path for all topics,
    pkg$topic_path = file.path(pkg$site_path, "topics")
    if(!file.exists(pkg$topic_path)) {
        dir.create(pkg$topic_path, recursive = TRUE)
    }

    # create columns for extra topic info
    index$title <- ""
    index$in_index <- TRUE
    
    pkg_name <- pkg$package

    for (i in seq_along(index$name)) {
        message("Generating ", basename(paths[[i]]))

        rd <- pkg$rd[[i]]
        md <- to_md.Rd_doc(rd,
                           env = new.env(parent = globalenv()),
                           topic = stringr::str_replace(basename(paths[[i]]), "\\.md$", ""),
                           pkg = pkg)
        md$pagetitle <- md$name

        md$package <- pkg[c("package", "version")]
        
        render_page(md, style=style, format="md", path=paths[[i]])
        graphics.off()
        
        if (index$name[i] == pkg_name & style =="mkdocs") {
            text <- readLines(paths[[i]])
            text[1] <- paste0("# ",gsub(".* \\*(.*)\\*","\\1",text[1]))
            text <- paste(text, sep="", collapse = "\n")
            cat( text, file=paths[[i]], append = F)
        }

        if ("internal" %in% md$keywords) {
            index$in_index[i] <- FALSE
        }
        index$title[i] <- md$title
    }

    index
}


#' Knit markdown vignettes
#'
#' @param  pkg path to source version of package.  See
#'   \link[devtools]{as.package} for details on how paths and package
#'   names are resolved.
#' @param  doc_path   directory in which to create documentation. If
#'                    \code{NULL} this defaults to \code{pkg}/docs.
#' @param ... Other additional arguments passed to \link{as.sd_package}
#'   used to override package defaults.
#'
#' @importFrom tools pkgVignettes buildVignettes
#' @importFrom knitr knit opts_knit
#' @importFrom rmarkdown render md_document
#' @importFrom devtools load_all
#' @importFrom tools file_path_sans_ext file_ext
#' @import stringr
#' @export
build_md_vignettes <- function(pkg=".", doc_path=NULL,  strip_yaml=TRUE) {
    #load_all(pkg)
    pkg <- as.sd_package(pkg, site_path=doc_path, mathjax=FALSE)
    vigns <- pkgVignettes(dir = pkg$path)

    # Return if no vignettes
    if (length(vigns$docs) == 0) return()

    # Set and create output directory
    outdir <- file.path(pkg$site_path, "vignettes");
    if(!file.exists(outdir)) { dir.create(outdir) }

    # Knits Rmd to markdown
    .knit_md <- function(d) {
        n <- file_path_sans_ext(basename(d))
        outfile <- str_c(n, ".md")
        if (file_ext(d) %in% c("Rmd", "rmd")) {
            opts_knit$set(unnamed.chunk.label=n)
            #opts_chunk$set(fig.path="figures/")
            #rmarkdown::render(d, output_file=outfile, output_dir=o, intermediates_dir=o,
            #                  md_document(variant="markdown", preserve_yaml=FALSE))
            knit(d, output=outfile, quiet=FALSE, envir=globalenv())
            return(outfile)
        } else {
            return(FALSE)
        }
    }

    # Set working directory for knit
    wd = getwd()
    setwd(outdir)

    # Knit vignettes
    message("Building vignettes")
    files <- sapply(vigns$docs, .knit_md)

    # Reset working directory
    setwd(wd)

    # Extract titles
    .get_title <- function(d) {
        x <- str_c(readLines(d), collapse="\n")
        str_match(x, "\\\\VignetteIndexEntry\\{(.*?)\\}")[2]
    }
    titles <- sapply(vigns$docs, .get_title, USE.NAMES=FALSE)

    # Add title to output
    for (i in 1:length(files)) {
        f <- file.path(outdir, files[i])
        header <- titles[i]
        # Open outfile
        x <- readLines(f)

        # Strip yaml block, because knitr ignores `preserve_yaml=FALSE` for some unknown reason
        if (strip_yaml) {
            y <- which(x == "---")
            if (length(y) == 2) {
                x <- x[-(y[1]:y[2])]
            } else {
                warning("Error finding yaml block. Not removed.")
            }
        }

        # Add header
        x <- c(stri_join('# ', header), x)
        # Write modified file

        writeLines(x, f)
    }

    return(data.frame(title=titles, file_out=files, stringsAsFactors=FALSE))
}


# @importFrom tools pkgVignettes buildVignettes
# build_vignettes <- function(pkg = ".") {
#     pkg <- as.sd_package(pkg)
#     vigns <- pkgVignettes(dir = pkg$path)
#
#     if (length(vigns$docs) == 0) return()
#
#     message("Building vignettes")
#     # Locate source and built versions of vignettes
#     buildVignettes(dir = pkg$path)
#     vigns <- pkgVignettes(dir = pkg$path, output = TRUE)
#
#     message("Copying vignettes")
#     dest <- file.path(pkg$site_path, "vignettes")
#     if (!file.exists(dest)) dir.create(dest)
#     file.copy(vigns$outputs, dest, overwrite = TRUE)
#
#     # Extract titles
#     titles <- vapply(vigns$docs, FUN.VALUE = character(1), function(x) {
#         contents <- str_c(readLines(x), collapse = "\n")
#         str_match(contents, "\\\\VignetteIndexEntry\\{(.*?)\\}")[2]
#     })
#     names <- basename(vigns$outputs)
#
#     list(vignette = unname(Map(list, title = titles, filename = names)))
# }


#' Load README
#'
#' Loads the README.md file for the package into a string vector.
#'
#' @param pkg   path to source version of package.  See
#'              \link[devtools]{as.package} for details on how paths and package
#'              names are resolved.
#' @param ...   Other additional arguments passed to \link{as.sd_package}
#'              used to override package defaults.
#'
#' @export
load_md_readme <- function(pkg = ".", ...) {
    pkg <- as.sd_package(pkg, ...)

    # Use README.md from package root if it exists, fall back to description if not
    f <- file.path(pkg$path, "README.md")
    readme <- if (file.exists(f)) { readLines(f) } else { pkg$description }

    return(readme)
}

#' Load NEWS into a string vector
#'
#' @param pkg path to source version of package.  See
#'   \link[devtools]{as.package} for details on how paths and package
#'   names are resolved.
#' @param ... Other additional arguments passed to \link{as.sd_package}
#'   used to override package defaults.
#'
#' @export
load_md_news <- function(pkg = ".") {
    pkg <- as.sd_package(pkg)

    # Use NEWS.md from package root if it exists, otherwise NULL
    f <- file.path(pkg$path, "NEWS.md")
    news <- if (file.exists(f)) { readLines(f) } else { NULL }

    return(news)
}


#' Build release notes page
#'
#' Must be called after everything else.
#'
#' @param  pkg   path to source version of package.
#'
#' @export
build_md_news <- function(pkg, style=c("mkdocs", "sphinx")) {
    # Check arguments
    style <- match.arg(style)

    outfile <- file.path(pkg$site_path, "news.md")
    message("Generating news.md")

    # Load NEWS
    news <- load_md_news(pkg)

    if (style == "sphinx") {
        news <- c("# Release Notes\n", news)
    } else {
        news <- c("# Release Notes\n", news)
    }

    # Write
    if (!is.null(news)) {
        writeLines(news, outfile)
    } else {
        outfile <- NULL
    }

    invisible(outfile)
}


#' Build home page
#'
#' Must be called after everything else.
#'
#' @param  pkg   path to source version of package.
#'
#' @export
build_md_index <- function(pkg, style=c("mkdocs", "sphinx")) {
    # Check arguments
    style <- match.arg(style)
    if (style == "mkdocs") {
        header_block <- c("## ", "\n")
        outfile <- file.path(pkg$site_path, "index.md")
        message("Generating index.md")
    } else if (style == "sphinx") {
        header_block <- c("# ", "\n")
        outfile <- file.path(pkg$site_path, "about.md")
        message("Generating about.md")
    }

    # Load README
    readme <- load_md_readme(pkg)
    
    if (style == "mkdocs") {
        # Update readme:
        # - add missing h1
        # - put badges in one line
        if (!grepl("^-+$",readme[2])) {
            #readme <- c("#","",readme)
            badges_idx <- grepl("^\\[!\\[\\]", readme)
            readme_0 <- paste(readme[badges_idx], collapse = " ")
            readme_0 <- paste0("# ", readme_0)
            readme <- readme[!badges_idx]
            readme <- c(readme_0,readme)
        }        
    } 
    
    # Make person link; x = eval'd Authors@R
    .make_person <- function(x) {
        s <- NULL
        if (length(x$email))
            s <- "["
        if (length(x$given))
            s <- str_c(s, x$given)
        if (length(x$family))
            s <- str_c(s, x$family, sep=" ")
        if (length(x$email))
            s <- str_c(s, "](mailto:", x$email, ")")
        if (length(x$role))
            s <- str_c(s, " (", paste(x$role, collapse=", "), ")")
        return(s)
    }

    # Make author vector
    if (!is.null(pkg$`authors@r`)) {
        authors <- sapply(eval(parse(text=pkg$`authors@r`)), .make_person)
    } else {
        authors <- NULL
    }

    # Make dependency vector; x = pkg$dependencies list
    .make_depends <- function(x) {
        d <- NULL
        if (length(x$depends)) {
            d <- c(d, str_c("**Depends:**", x$depends, sep=" "))
        }
        if (length(x$imports)) {
            d <- c(d, str_c("**Imports:**", x$imports, sep=" "))
        }
        if (length(x$suggests)) {
            d <- c(d, str_c("**Suggests:**", x$suggests, sep=" "))
        }
        # if (length(x$extends)) {
        #     d <- c(d, str_c("**Extends:**", x$extends, sep=" "))
        # }

        return(d)
    }

    # Define dependencies
    if (!is.null(pkg$dependencies)) {
        depends <- .make_depends(pkg$dependencies)
    } else {
        depends <- NULL
    }

    # Parse CITATION file
    cite_file <- file.path(pkg$path, "inst", "CITATION")
    if (file.exists(cite_file)) {
        x <- capture.output(readCitationFile(cite_file))
        x <- x[!grepl("(To see these entries in BibTeX format)|(bibtex=TRUE)|(citation.bibtex.max)", x)]
        x <- str_c(x, collapse="\n")
        citation <- str_replace_all(x, "<URL:\\s*(http://[^>]+)>", "[\\1](\\1)")
    } else {
        citation <- NULL
    }

    # Make vignette links
    #vignettes <- mapply(function(x, y) { str_c("+ [", x, "](vignettes/", y, ")") },
    #                    pkg$vignettes$title, pkg$vignettes$file_out, USE.NAMES=FALSE)

    # Exclude package docs from topics
    #topic_table <- pkg$topics[pkg$topics$name != pkg$package, ]
    # Make topic links
    #topics <- mapply(function(x, y) { str_c("+ [", x, "](topics/", y, ")") },
    #                 topic_table$name, topic_table$file_out, USE.NAMES=FALSE)

    # Assemble index
    #index <- c(readme,
    #           "\nVignettes\n==========\n",
    #           vignettes,
    #           "\nTopics\n==========\n",
    #           topics)

    if (style == "sphinx") {
        index <- c("# About\n", readme)
    } else {
        index <- readme
    }


    # Add citation section
    if (!is.null(depends))
    {
        index <- c(index, "\n",
                   stri_join(header_block[1], "Dependencies", header_block[2]),
                   str_c(depends, collapse="  \n"))
    }

    # Add citation section
    if (!is.null(authors))
    {
        index <- c(index, "\n",
                   stri_join(header_block[1], "Authors", header_block[2]),
                   str_c(authors, collapse="  \n"))
    }

    # Add citation section
    if (!is.null(citation))
    {
        index <- c(index, "\n",
                   stri_join(header_block[1], "Citing", header_block[2]),
                   str_c(citation, collapse="\n"))
    }

    # Add license section
    if (length(pkg$license)) {
        index <- c(index, "\n",
                   stri_join(header_block[1], "License", header_block[2]),
                   pkg$license)
    }
    
    # Write
    writeLines(index, outfile)

    invisible(outfile)
}

#' @export
build_yaml <- function(pkg) {
    # Spacer
    i <- "  "

    # Default options
    yaml <- c("theme: readthedocs",
              str_c("site_name: ", pkg$package),
              str_c("docs_dir: ", basename(pkg$site_path)),
              "markdown_extensions:",
              str_c(i, "- def_list"),
              str_c(i, "- sane_lists"),
              str_c(i, "- smarty"),
              str_c(i, "- pymdownx.arithmatex"),
              str_c(i, "- toc:"),
              str_c(i, i, i, "permalink: True"),
              "extra_javascript:",
              str_c(i, "- https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML"),
              "pages:",
              str_c(i, "- 'About':"),
              str_c(i, i, "- 'Introduction': index.md"),
              str_c(i, i, "- 'Package Overview': topics/", pkg$package, ".md"))


    # Add NEWS if it exists
    if (!is.null(pkg$news)) {
        yaml <- c(yaml, str_c(i, i, "- 'Release Notes': news.md"))
    }

    # Add vignettes
    vignettes <- mapply(function(x, y) { str_c(i, i, "- '", x, "': ", file.path("vignettes", y)) },
                        pkg$vignettes$title, pkg$vignettes$file_out)

    # Exclude package docs from topics
    topic_table <- pkg$topics[pkg$topics$name != pkg$package, ]
    # Add topics
    topics <- sapply(topic_table$file_out, function(x) { str_c(i, i, "- ", file.path("topics", x)) })

    if (length(vignettes) > 0) {
        yaml <- c(yaml,
                  str_c(i, "- 'Vignettes':"),
                  vignettes)
    }

    yaml <- c(yaml,
              str_c(i, "- 'Help Topics':"),
              topics)


    writeLines(yaml, file.path(pkg$site_path, "..", "mkdocs.yml"))
}
