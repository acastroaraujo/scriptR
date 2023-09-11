

#' Parse Scripts
#'
#' @param path a character vector of source files.
#'
#' @return a ScriptR object
#' @export
#'
parse_script <- function(path) {
  if (!all(get_file_ext(path) == "R")) {
    stop("All scripts must end in `.R`", call. = FALSE)
  }
  ScriptR$new(path)
}

ScriptR <- R6::R6Class(
  classname = "ScriptR",
  public = list(
    path = NULL,
    pkg_search = NULL,
    code = NULL,
    functions = NULL,
    initialize = function(path) {
      self$path <- path
      self$code <- get_code(path)
      self$pkg_search <- get_pkg_search(self$code)
      self$functions <- get_foo(code = self$code, search_list = self$pkg_search)
    },
    print = function(...) {
      cat("Scripts:\n")
      print(self$path)
      invisible(self)
    }
  )
)

#' Get Conflicts
#'
#' @param pkgs a character vector of package names
#'
#' @return a data frame
#' @export
#'
get_conflicts <- function(pkgs) {

  pkgs <- pkgs[pkgs != "datasets"]

  obj_names <- purrr::map(pkgs, getNamespaceExports)
  objs <- purrr::map2(obj_names, pkgs, .f = function(obj, pkg) {
    mget(obj, envir = asNamespace(pkg), inherits = TRUE)
  })

  ok <- purrr::map(objs, \(x) purrr::map_lgl(x, is.function))

  flist <- purrr::map2(obj_names, ok, \(x, i) x[i])
  names(flist) <- pkgs

  package_pairs <- utils::combn(pkgs, 2, simplify = FALSE)
  conflicting_fs <- purrr::map(package_pairs, .f = function(x) {
    intersect(flist[[x[[1]]]], flist[[x[[2]]]])
  }) |>
    unlist() |>
    unique()

  el <- purrr::map2(flist, names(flist), \(x, nm) data.frame(f = x, package = nm)) |>
    dplyr::bind_rows() |>
    dplyr::filter(.data$f %in% conflicting_fs)

  row_names <- unique(el[["f"]])
  col_names <- unique(el[["package"]])
  rows <- length(row_names)
  cols <- length(col_names)

  out <- matrix(0L, rows, cols, dimnames = list(row_names, col_names))
  out[as.matrix(el)] <- 1L

  return(dplyr::as_tibble(out, rownames = "f"))
}

#' Get Unused Objects
#'
#' @param code a data frame as included in the $code of any ScriptR object
#'
#' @return a data frame
#' @export
#'
get_unused_objs <- function(code) {

  ## index for objects created via assignment
  i <- which(stringr::str_detect(code$token, "ASSIGN")) - 1

  objs <- code[i, ] |>
    dplyr::filter(.data$token == "SYMBOL") |>
    dplyr::pull(.data$text) |>
    unique()

  obj_used_once <- code |> ## this includes variables created via $
    dplyr::filter(.data$text %in% objs) |>
    dplyr::count(.data$text) |>
    dplyr::filter(.data$n == 1) |>
    dplyr::pull(.data$text)

  ## index for created objects used once
  i <- which(code[["text"]] %in% obj_used_once)
  ## this index removes objects in data frame created via $<-
  ii <- i[code[["text"]][i - 1] != "$"] # avoid modification of columns via $

  return(code[ii, ])

}
