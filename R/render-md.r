#' Render complete page.
#'
#' @param package Path to package to document.
#' @param name Name of the template (e.g. index, demo, topic)
#' @param data Data for the template
#' @param path Location to create file. If \code{""} (the default),
#'   prints to standard out.
#' @export
render_page <- function(data, style="mkdocs", format="md", path="") {
    # render template components
    pieces <- c("header", "content", "footer")
    components <- lapply(pieces, render_template, data=data, style=style, format=format)
    names(components) <- pieces

    # render complete layout
    out <- render_template("layout",  data=components, style=style, format=format)
    cat(out, file=path)
}

#' @importFrom whisker whisker.render
render_template <- function(section, data, style="mkdocs", format="md") {
    template <- readLines(get_template(section, style=style, format=format))
    if (length(template) == 0 || (length(template) == 1 && str_trim(template) == "")) {
        return("")
    }

    return(whisker.render(template, data))
}

# Find template file for layout section
get_template <- function(section, style="mkdocs", format="md") {
    ## DEBUG
    # style="mkdocs"; section="header"
    template_path <- file.path(system.file(package="markr"), "templates")
    template_file <- file.path(template_path, paste0(style, "-", section, ".", format))

    if (!file.exists(template_file)) {
        stop("Template file for section=", section, ", style=", style, " and format=", format, " not found")
    }

    return(template_file)
}


