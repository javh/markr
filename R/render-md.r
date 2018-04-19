#' Render complete page.
#'
#' @param package Path to package to document.
#' @param name Name of the template (e.g. index, demo, topic)
#' @param data Data for the template
#' @param path Location to create file. If \code{""} (the default),
#'   prints to standard out.
#' @export
render_md_page <- function(data, style, path="") {
    # render template components
    pieces <- c("header", "content", "footer")
    components <- lapply(pieces, render_md_template, style=style, data=data)
    names(components) <- pieces

    # render complete layout
    out <- render_md_template("layout", style=style, data=components)
    cat(out, file=path)
}

#' @importFrom whisker whisker.render
render_md_template <- function(section, style, data) {
    template <- readLines(find_md_template(section, style))
    if (length(template) == 0 || (length(template) == 1 && str_trim(template) == "")) {
        return("")
    }

    return(whisker.render(template, data))
}

# Find template file for layout section
find_md_template <- function(section, style) {
    ## DEBUG
    # style="mkdocs"; section="header"
    template_path <- file.path(system.file(package="markr"), "templates")
    template_file <- file.path(template_path, paste0(style, "-", section, ".md"))

    if (!file.exists(template_file)) {
        stop("Template file for style=", style, " and section=", section, "not found")
    }

    return(template_file)
}


