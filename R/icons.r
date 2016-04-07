# `icons.r` is a regular R file - we parse it, evaluate it all, and then
# extract all elements (with names) that are icons
load_icons <- function(pkg = ".") {
  pkg <- as.sd_package(pkg)

  path <- file.path.ci(pkg$sd_path, "icons.R")
  if (!file.exists(path)) return(list())

  env <- new.env(parent = globalenv())
  eval(parse(path), envir = env)

  all <- mget(ls(envir = env), env)
  Filter(function(x) inherits(x, "sd_icon"), all)
}

#' Define the icon for a function.
#'
#' @param expr,func Either a bare expression or a function with no arguments
#'   that uses grid to create a icon that represents the function.
#' @param inherits Alternatively, use an existing icon specified by a
#'   function name
#' @export
sd_icon <- function(expr = NULL, func = NULL, inherits = NULL) {
  expr <- substitute(expr)

  if ((is.null(func) + is.null(inherits) + is.null(expr)) != 2) {
    stop("Specify one of expr, func or inherits", call. = FALSE)
  }

  if (!is.null(inherits)) {
    structure(list(inherits = inherits), class = "sd_icon")
  } else {
    if (!is.null(expr)) {
      func <- make_function(list(), expr)
    }
    structure(list(func = func), class = "sd_icon")
  }
}


make_function <- function(args, expr, env = globalenv()) {
  args <- as.pairlist(args)

  f <- eval(call("function", args, expr))
  environment(f) <- env
  f
}


render_icons <- function(pkg = ".") {
  pkg <- as.sd_package(pkg)

  icon_path <- file.path(pkg$site_path, "icons")
  if (!file.exists(icon_path)) dir.create(icon_path)

  icons <- pkg$icons
  if (length(icons) == 0) return()

  has_icon <- Filter(function(x) !is.null(x$func), icons)

  for(icon in names(has_icon)) {
    png(file.path(icon_path, icon_name(icon)), width = 40, height = 40)
    try(grid::grid.draw(icons[[icon]]$func()))
    dev.off()
  }
}

icon_path <- function(package, topic) {
  icon <- package$icons[[topic]]
  if (is.null(icon)) return(NULL)

  if (!is.null(icon$func)) {
    return(file.path("icons", icon_name(topic)))
  }
  if (!is.null(icon$inherits)) {
    return(file.path("icons", icon_name(icon$inherits)))
  }

  NULL
}

icon_name <- function(topic) paste(topic, ".png", sep = "")

#' @export
print.sd_icon <- function(x, ...) {
  cat("<sd_icon> ", if (is.null(x$func)) x$inherits else "<fun>", "\n", sep = "")
}
