#' Convert an rdoc to a list of markdown components.
#'
#' @param   x    rd object to convert to html.
#' @param   ...  other arguments passed onto to methods.
#' @return  A list suitable for rendering with \link[whisker]{whisker.render}.
#'
#' @export
to_rst <- function(x, ...) {
  UseMethod("to_rst")
}


# Parse a complete Rd file
#' @export
to_rst.Rd_doc <- function(x, ...) {
  tags <- vapply(x, tag, FUN.VALUE = character(1))
  get_tags <- function(tag) x[tags == tag]
  get_tag <- function(tag) {
    if (tag %in% tags) {
      x[[which(tags == tag)]]
    }
  }

  # Remove line breaks between sections
  line_breaks <- tags == "TEXT"
  x <- x[!line_breaks]
  tags <- tags[!line_breaks]

  out <- list()

  # Capture name, title and aliasess
  out$name <- to_rst(get_tag("name"), ...)
  out$title <- to_rst(get_tag("title"), ...)
  out$description <- to_rst(get_tag("description"), ...)
  out$aliases <- vapply(get_tags("alias"), to_rst, character(1), ...)
  out$keywords <- vapply(get_tags("keyword"), to_rst, character(1), ...)

  out$usage <- to_rst(get_tag("usage"), ...)
  out$arguments <- to_rst(get_tag("arguments"), ...)

  # Work around mustache deficiency
  if (length(out$usage)) { out$has_usage <- TRUE }
  if (length(out$arguments)) { out$has_args <- TRUE }

  out$author <- to_rst(get_tag("author"), ...)
  out$seealso <- to_rst(get_tag("seealso"), ...)
  out$examples <- to_rst(get_tag("examples"), ...)

  # Everything else stays in original order, and becomes a list of sections.
  sections <- x[!(tags %in% c("name", "title", "description", "alias", "keyword",
                              "usage", "author", "seealso", "arguments", "examples"))]
  out$sections <- compact(to_rst(sections, topic = out$name, ...))

  out
}

# A list of elements should stay as a list
#' @export
to_rst.list <- function(x, ...) {
  lapply(x, to_rst, ...)
}

# Elements that don't return anything ----------------------------------------

#' @export
to_rst.NULL <- function(x, ...) character(0)
#' @export
to_rst.COMMENT <- function(x, ...) character(0)
#' @export
to_rst.dontshow <- function(x, ...) character(0)
#' @export
to_rst.testonly <- function(x, ...) character(0)
#' @export
to_rst.concept <- function(x, ...) character(0)

# Various types of text ------------------------------------------------------

# All components inside a text string should be collapsed into a single string
# Also need to do html escaping here and in to_rst.RCODE
#' @export
to_rst.TEXT <- function(x, ...) {
  str_replace_all(str_c(unlist(to_rst.list(x, ...)), collapse = ""), "^ {2,}", "")
}
#' @export
to_rst.RCODE <- to_rst.TEXT
#' @export
to_rst.LIST <- to_rst.TEXT
#' @export
to_rst.VERB <- to_rst.TEXT

# If it's a character vector, we've got to the leaves of the tree
#' @export
to_rst.character <- function(x, ...) x

#' @export
to_rst.name <- function(x, ...) to_rst(x[[1]], ...)
#' @export
to_rst.title <- function(x, ...) to_rst.TEXT(x, ...)
#' @export
to_rst.usage <- function(x, pkg, ...) {
  text <- str_trim(paste(to_rst.TEXT(x, ...), collapse = "\n"))
  # Collapse all hardcoded hanging indents
  text <- stringr::str_replace_all(text, "\n +", " ")

  # It's nice not to wrap in the middle of a simple "arg = default"
  #text <- stringr::str_replace_all(text, " = ", "=")
  # Wrap each individual function in its own div, so that text-indent
  # CSS rules can be used effectively
  text <- stringr::str_replace_all(text, "\n\n", "\n    ")
  text <- paste0("::\n\n    ", text, "\n")

  text
}

#' @export
to_rst.alias <- function(x, ...) unlist(to_rst.list(x, ...))

#' @export
to_rst.keyword <- function(x, ...) unlist(to_rst.list(x, ...))

#' @export
to_rst.seealso <- function(x, ...) {
  #to_rst.TEXT(x, ...)
  parse_section_md(x, "Seealso", ...)[[2]]
}

#' @export
to_rst.author <- function(x, ...) to_rst.TEXT(x, ...)


# Sections get a element called text and an element called content, which
# contains a list of paragraphs.
#' @export
to_rst.details <- function(x, ...) {
  #stringr::str_c("\nDETAILS_START\n", parse_section_md(x, "Details", ...), "\nDETAILS_END\n\n")
  parse_section_md(x, "Details", ...)
}

#' @export
to_rst.description <- function(x, ...) {
  to_rst.TEXT(x, ...)
  #parse_section_md(x, "Description", ...)
}

#' @export
to_rst.value <- function(x, ...) {
  # Note that \value is implicitly a \describe environment
  #class(x) <- c("describe", class(x))
  #text <- to_rst(x, ...)
  #list(title = "Value", contents = text)
  text <- to_rst.TEXT(x, ...)
  list(title = "Value", contents = text)
}

#' @export
to_rst.references <- function(x, ...) { parse_section_md(x, "References", ...) }

#' @export
to_rst.source <- function(x, ...) parse_section_md(x, "Source", ...)

#' @export
to_rst.format <- function(x, ...) parse_section_md(x, "Format", ...)

#' @export
to_rst.note <- function(x, ...) parse_section_md(x, "Note", ...)

#' @export
to_rst.section <- function(x, ...) {
  parse_section_md(x[[2]], to_rst(x[[1]], ...), ...)
}

parse_section_md <- function(x, title, ...) {
  text <- to_rst.TEXT(x, ...)
  #paras <- stringr::str_trim(stringr::str_split(text, "\\n\\s*\\n")[[1]])
  #paras <- stringr::str_replace_all(text, "\\n\\s+", "\n")

  #list(title = title, contents = paras)
  #list(title = stringr::str_c("\n", title), contents = paras)
  #list(title = stringr::str_c("\n", title), contents = text)
  list(title = title, contents = text)

  # Note that sections are implicitly a \describe environment
#   class(x) <- c("describe", class(x))
#
#   text <- to_rst(x, ...)
#   #paras <- stringr::str_trim(stringr::str_split(text, "\\n\\s*\\n")[[1]])
#   paras <- stringr::str_trim(text)
#
#   list(title = title, contents = paras)
}

# Examples ------------------------------------------------------------------

#' @importFrom evaluate evaluate
#' @export
to_rst.examples <- function(x, pkg, topic = "unknown", env = new.env(parent = globalenv()), ...) {
  if (!pkg$examples) return()

  # First element of examples tag is always empty
  text <- str_trim(to_rst.TEXT(x[-1], ...))
  expr <- evaluate(text, env, new_device = TRUE)

  replay_md(expr, pkg = pkg, name = str_c(topic, "-"))
}

# Arguments ------------------------------------------------------------------

#' @export
to_rst.arguments <- function(x, ...) {
  items <- Filter(function(x) tag(x) == "item", x)
  to_rst(items, ...)
}

#' @export
to_rst.item <- function(x, ...) {
  # If no subelements, then is an item from a itemise or enumerate, and
  # is dealt with those methods
  if (length(x) == 0) return()

  list(name = to_rst(x[[1]], ...), description = to_rst.TEXT(x[[2]], ...))
}

# Equations ------------------------------------------------------------------

#' @export
to_rst.eqn <- function(x, pkg, ...) {
  stopifnot(length(x) <= 2)
  ascii_rep <- x[[length(x)]]
  if (pkg$mathjax){
    stringr::str_c("$", to_rst.TEXT(ascii_rep, ...), "$")
  }else{
    stringr::str_c("<code class = 'eq'>", to_rst.TEXT(ascii_rep, ...), "</code>")
  }
}

#' @export
to_rst.deqn <- function(x, pkg, ...) {
  stopifnot(length(x) <= 2)
  if (pkg$mathjax){
    stringr::str_c("$$", to_rst.TEXT(x[[length(x)-1]], ...), "$$")
  }else{
    stringr::str_c("<pre class = 'eq'>", to_rst.TEXT(x[[length(x)]], ...), "</pre>")
  }
}

# Links ----------------------------------------------------------------------
#' @export
to_rst.url <- function(x, ...) {
  stopifnot(length(x) == 1)
  stringr::str_c("[", to_rst.TEXT(x[[1]]), "](", to_rst.TEXT(x[[1]]), ")")
}
#' @export
to_rst.href <- function(x, ...) {
  stopifnot(length(x) == 2)
  stringr::str_c("[", to_rst.TEXT(x[[2]]), "](", to_rst.TEXT(x[[1]]), ")")
}
#' @export
to_rst.email <- function(x, ...) {
  stopifnot(length(x) %in% c(1L, 2L))
  stringr::str_c("[", x[[length(x)]], "](", x[[1]], ")")
}


# If single, need to look up alias to find file name and package
#' @export
to_rst.link <- function(x, pkg, ...) {
  stopifnot(length(x) == 1)

  opt <- attr(x, "Rd_option")

  if (is.null(opt)) {
    topic <- to_rst.TEXT(x[[1]])
    label <- topic
    t_package <- NULL
  } else if (stringr::str_sub(opt, 1, 1) == "=") {
    topic <- stringr::str_sub(opt, 2, -1)
    label <- to_rst.TEXT(x[[1]])
    t_package <- NULL
  } else {
    topic <- to_rst.TEXT(x[[1]])
    label <- topic
    parts <- stringr::str_match(opt, '([^:]+):(.*)')[1,]
    if (is.na(parts[1])) {
      t_package <- opt
    } else {
      topic <- parts[3]
      t_package <- parts[2]
    }
  }

  loc <- find_topic(topic, t_package, pkg$rd_index)
  if (is.null(loc)) {
    message("Can't find help topic ", topic)
    return(topic)
  }

  make_rst_link(loc, label, pkg)
}

make_rst_link <- function(loc, label, pkg = NULL) {
  if (is.null(loc$package)) {
    str_c("`", label, " <", str_replace(loc$file, "\\.html$", "\\.rst"), ">`_")
  } else {
    str_c("`", label, " <http://www.rdocumentation.org/packages/", loc$package,
          "/topics/", loc$topic, ">`_")
  }
}


builtin_packages <- c("base", "boot", "class", "cluster", "codetools", "compiler",
                      "datasets", "foreign", "graphics", "grDevices", "grid", "KernSmooth",
                      "lattice", "MASS", "Matrix", "methods", "mgcv", "nlme", "nnet",
                      "parallel", "rpart", "spatial", "splines", "stats", "stats4",
                      "survival", "tcltk", "tools", "utils")

# Miscellaneous --------------------------------------------------------------

# First element of enc is the encoded version (second is the ascii version)
#' @export
to_rst.enc <- function(x, ...) {
  to_rst(x[[1]], ...)
}

#' @export
to_rst.dontrun <- function(x, ...) {
  if (length(x) == 1) {
    str_c("### Not run: ",
          str_trim(to_rst.TEXT(x)))
  } else {
    str_c("### Not run:\n",
          str_replace_all(str_trim(to_rst.TEXT(x, ...)), "\n", "\n# "))
  }
}

#' @export
to_rst.special <- function(x, ...) {
  txt <- to_rst.TEXT(x, ...)
  # replace '<' and '>' with html markings avoid browser misinterpretation
  txt <- str_replace_all(txt, "<", "&#60;")
  txt <- str_replace_all(txt, ">", "&#62;")
  txt <- str_replace_all(txt, "\\\\dots", "...")

  stupid <- unlist(str_match_all(txt, "\\\\[a-zA-Z]*"))
  for (i in seq_len(length(stupid))) {
    message("Unknown tag (", stupid[i], ") found in 'special' tag")
  }

  str_c("*", txt, "*")
}

#' @export
to_rst.method <- function(x, ...) {
  stringr::str_c('"', to_rst(x[[1]], ...), '"')
}
#' @export
to_rst.S3method <- to_rst.method
#' @export
to_rst.S4method <- to_rst.method

#' @export
to_rst.docType <- function(...) NULL


# Conditionals and Sexprs ----------------------------------------------------

#' @export
#' @importFrom tools parse_Rd
to_rst.Sexpr <- function(x, env, ...) {
  code <- to_rst.TEXT(x)
  expr <- eval(parse(text = code), env)

  con <- textConnection(expr)
  on.exit(close(con))

  rd <- parse_Rd(con, fragment = TRUE)
  rd <- structure(set_classes(rd), class = c("Rd_doc", "Rd"))

  to_rst.TEXT(rd, ...)
}

#' @export
to_rst.if <- function(x, ...) {
  if (x[[1]] != "rst") return()
  x[[2]]
}

#' @export
to_rst.ifelse <- function(x, ...) {
  if (x[[1]] == "rst") x[[2]] else x[[3]]
}

# Tables ---------------------------------------------------------------------

#' @export
to_rst.tabular <- function(x, ...) {
  align_abbr <- stringr::str_split(to_rst(x[[1]], ...), "")[[1]][-1]
  align_abbr <- align_abbr[!(align_abbr %in% c("|", ""))]
  align <- unname(c("r" = "right", "l" = "left", "c" = "center")[align_abbr])

  contents <- x[[2]]
  row_sep <- vapply(contents, function(x) tag(x) == "cr",
                    FUN.VALUE = logical(1))
  col_sep <- vapply(contents, function(x) tag(x) == "tab",
                    FUN.VALUE = logical(1))

  last <- rev(which(row_sep))[1] - 1L
  contents <- contents[seq_len(last)]
  cell_grp <- cumsum(col_sep | row_sep)[seq_len(last)]
  cells <- split(contents, cell_grp)

  cell_contents <- vapply(cells, to_rst.TEXT, ...,
                          FUN.VALUE = character(1), USE.NAMES = FALSE)
  cell_contents <- stringr::str_c("<td>", cell_contents, "</td>\n")
  cell_contents <- matrix(cell_contents, ncol = length(align), byrow = TRUE)

  rows <- apply(cell_contents, 1, str_c, collapse = "")

  str_c("<table>", str_c("<tr>", rows, "</tr>", collapse = ""), "</table>")
}

#' @export
to_rst.tab <- function(x, ...) character(0)
#' @export
to_rst.cr <- function(x, ...) character(0)


# List -----------------------------------------------------------------------

#' @export
to_rst.itemize <- function(x, ...) {
  stringr::str_c("\n", parse_itemize_md(x[-1], ...), "")
}
#' @export
to_rst.enumerate <- function(x, ...) {
  stringr::str_c("\n", parse_enumerate_md(x[-1], ...), "")
}
#' @export
to_rst.describe <- function(x, ...) {
  stringr::str_c("\n", parse_describe_md(x[-1], ...), "")
}

parse_itemize_md <- function(rd, ...) {
  separator <- vapply(rd, function(x) tag(x) == "item",
                      FUN.VALUE = logical(1))
  group <- cumsum(separator)

  # remove empty first group, if present
  rd <- rd[group != 0]
  group <- group[group != 0]

  items <- split(rd, group)

  li <- vapply(items, function(x) {
    stringr::str_c("+ ", to_rst.TEXT(x, ...), "")
  }, FUN.VALUE = character(1))

  stringr::str_c(li, collapse = "")
}

parse_enumerate_md <- function(rd, ...) {
  separator <- vapply(rd, function(x) tag(x) == "item",
                      FUN.VALUE = logical(1))
  group <- cumsum(separator)

  # remove empty first group, if present
  rd <- rd[group != 0]
  group <- group[group != 0]

  items <- split(rd, group)

  li <- vapply(items, function(x) {
    stringr::str_c("1. ", to_rst.TEXT(x, ...), "")
  }, FUN.VALUE = character(1))

  stringr::str_c(li, collapse = "")
}

parse_describe_md <- function(rd, ...) {
  is_item <- vapply(rd, function(x) tag(x) == "item",
                    FUN.VALUE = logical(1))

  li <- character(length(rd))
  for (i in seq_along(rd)) {
    if (is_item[[i]]) {
      li[i] <- stringr::str_c(to_rst.TEXT(rd[[i]][[1]], ...), "\n:   ", to_rst.TEXT(rd[[i]][-1], ...))
    } else {
      li[i] <- to_rst.TEXT(rd[i], ...)
    }
  }

  stringr::str_c(li, collapse = "")
}


# Simple tags that need minimal processing -----------------------------------

#' @export
to_rst.Rd_content <- function(x, ...) {
  tag <- tag(x)

  if (is.null(tag)) {
    to_rst.TEXT(x, ...)
  } else if (!is.null(tag) && tag %in% names(simple_md_tags)) {
    #cat("TAG: ", tag, "\n")
    #print(x)

    # If we can process tag with just prefix & suffix, do so
    md <- simple_rst_tags[[tag]]
    stringr::str_c(md[1], to_rst.TEXT(x, ...), md[2])
  } else {
    # Otherwise we don't know about this tag
    message("Unknown tag: ", tag)
    to_rst.TEXT(x, ...)
  }
}

simple_rst_tags <- list(
  "bold"   = c("**", "**"),
  "code"   = c("``", "``"),
  "emph"   = c("*", "*"),
  "item"   = c("", "\n"),
  "file"   = c("``", "``"),
  "strong" = c("**", "**"),
  "text"   = c("", "\n")
)
