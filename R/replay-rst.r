# Replay a list of evaluated results, just like you'd run them in a R
# terminal, but rendered as md
replay_rst <- function(x, ...) { UseMethod("replay_rst", x) }

#' @importFrom evaluate is.source
#' @export
replay_rst.list <- function(x, ...) {
  # Stitch adjacent source blocks back together
  src <- vapply(x, is.source, logical(1))
  # New group whenever not source, or when src after not-src
  group <- cumsum(!src | c(FALSE, src[-1] != src[-length(src)]))

  parts <- split(x, group)
  parts <- lapply(parts, function(x) {
    if (length(x) == 1) return(x[[1]])
    src <- str_c(vapply(x, "[[", "src", FUN.VALUE = character(1)),
                 collapse = "")
    structure(list(src = src), class = "source")
  })

  # keep only high level plots
  parts <- merge_low_plot(parts)

  pieces <- character(length(parts))
  for (i in seq_along(parts)) {
    pieces[i] <- replay_rst(parts[[i]], obj_id = i, ...)
  }

  str_c(pieces, collapse = "")
}

#' @export
replay_rst.NULL <- function(x, ...) ""

#' @export
replay_rst.character <- function(x, ...) {
  str_c("\n::\n", str_c(x, collapse = ""), "\n\n\n")
}

#' @export
replay_rst.value <- function(x, ...) {
  if (!x$visible) return()

  printed <- str_c(capture.output(print(x$value)), collapse = "\n")
  printed
  #str_c("`", printed, "`")
}

#' @export
replay_rst.source <- function(x, ..., pkg) {
  str_c("\n::\n", x$src, "\n\n\n")
}

#' @export
replay_rst.warning <- function(x, ...) {
  str_c("*Warning*:", x$message)
}

#' @export
replay_rst.message <- function(x, ...) {
  str_c("*", str_replace(x$message, "\n$", ""), "*")
}

#' @export
replay_rst.error <- function(x, ...) {
  if (is.null(x$call)) {
    str_c("**Error**: ", x$message)
  } else {
    call <- deparse(x$call)
    str_c("**Error in ", call, "**: ", x$message)
  }
}

#' @export
replay_rst.recordedplot <- function(x, pkg, name_prefix, obj_id, ...) {
  name <- str_c(name_prefix, obj_id, ".png")
  path <- file.path(pkg$topic_path, name)

  if (!file.exists(path)) {
    png(path, width = 540, height = 400)
    on.exit(dev.off())
    print(x)
  }

  str_c(".. image:: ", name, "\nalt: ", obj_id, "\n")
}
