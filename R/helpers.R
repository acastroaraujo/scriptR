

where_is_it <- function(x, pkgs) {
  pkgs <- pkgs[pkgs != "datasets"]
  export_list <- purrr::map(pkgs, getNamespaceExports)
  names(export_list) <- pkgs
  i <- purrr::map_lgl(export_list, \(exports) x %in% exports)
  if (!any(i)) return(NA_character_) else return(pkgs[i][[1]])
}


get_foo <- function(code, search_list) {
  code |>
    dplyr::filter(.data$token == "SYMBOL_FUNCTION_CALL") |>
    dplyr::select(!.data$token) |>
    dplyr::rename(f = .data$text) |>
    dplyr::mutate(pkg = purrr::map_chr(.data$f, \(x) where_is_it(x, search_list))) |>
    dplyr::relocate(f = .data$f, .data$pkg, dplyr::everything())
}


x <- "https://raw.githubusercontent.com/acastroaraujo/ccc/master/data-raw/04-docterms.R"

get_code <- function(path) {
  purrr::map(path, function(x) {
    out <- utils::getParseData(parse(x, keep.source = TRUE)) |>
      dplyr::as_tibble()

    out$script <- stringr::str_extract(x, "[^/]+$")
    out$dir <- stringr::str_remove(x, "[^/]+$")
    return(out)
  }) |>
    dplyr::bind_rows() |>
    dplyr::filter(.data$token != "COMMENT", .data$text != "") |>
    dplyr::relocate(.data$script, .data$text, .data$token, dplyr::everything())
}

get_pkg_search <- function(d) {

  out <- which(d[["text"]] == "library" & d[["token"]] == "SYMBOL_FUNCTION_CALL") |>
    purrr::map(\(x) x + 0:3) |>
    purrr::map_chr(\(x) paste0(d[["text"]][x], collapse = ""))

  out_file <- paste0(tempdir(), "temp.R")

  write(paste(c("suppressMessages({", out, "})", "search()"), collapse = "\n"), out_file)

  out_search <- system(paste("Rscript", out_file), intern = TRUE)
  return(unlist(stringr::str_extract_all(out_search, "(?<=\"package:).+?(?=\")")))

}

get_file_ext <- function(x) {
  pos <- regexpr("\\.([[:alnum:]]+)$", x)
  ifelse(pos > -1L, substring(x, pos + 1L), "")
}
