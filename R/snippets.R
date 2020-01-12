#' Install js4shiny snippets
#'
#' This function installs a set of R, HTML, JavaScript and CSS snippets that
#' are helpful when developing Shiny apps and doing web development work in
#' RStudio. By default, the snippets are installed where RStudio will find
#' them. If you haven't previously installed snippets to RStudio, these
#' snippets will mask some of the built-in snippets that ship with RStudio.
#'
#' @section Updating Existing Snippets: If you already have snippets installed,
#'   you can  you can have the installed snippets update the existing snippets
#'   in place with `update = TRUE`. Or you can append the new snippets to the
#'   existing snippets files with `update = FALSE`. This option is desirable if
#'   you want to make sure that no snippets are overwritten. The newer snippets
#'   will mask older snippets, but no data will be lost.
#'
#' @examples
#' snip_tmp <- tempfile("snippets")
#' dir.create(snip_tmp)
#' snippets_install(snip_tmp)
#'
#' @param install_path Where should the snippets be installed? If `NULL`, the
#'   snippets will install to a default path based on the current version of
#'   RStudio.
#' @param update Should existing snippets be updated in place if there are any
#'   conflicts? Default is yes (`TRUE`). Otherwise, new snippets are appended to
#'   the end of the existing file, ensuring that you can recover your previous
#'   snippets by editing the snippets file.
#' @aliases snippets
#' @export
snippets_install <- function(install_path = NULL, update = TRUE) {
  new <- snippets_list()
  old <- snippets_list("system")
  dir <- install_path %||% snippets_dir("system")
  fs::dir_create(dir)

  for (snippet in names(new)) {
    has_mask <- snippets_warn_mask(new[snippet], old[snippet], warn = !update)
    install_to <- fs::path(dir, snippet)
    if (!fs::file_exists(install_to)) {
      fs::file_copy(new[snippet], install_to)
      message(glue("Installed {snippet}"))
    } else {
      snippets <- if (update) {
        snippets_merge(new[snippet], old[snippet])
      } else {
        c("", read_lines(new[snippet]))
      }
      cat(snippets, file = install_to, sep = "\n", append = !update)
      added <- !update || !has_mask
      message(glue(
        "{if (added) 'Added' else 'Updated'} snippets ",
        "{if (added) 'to' else 'in'} {snippet}",
        added = added
      ))
    }
  }
}

snippets_dir <- function(which = c("js4shiny", "system"), .intern = NULL) {
  switch(
    match.arg(which),
    js4shiny = js4shiny_file("snippets"),
    system = if (rstudio_gt_1.3()) {
      fs::path_home_r(".config", "rstudio", "snippets")
    } else {
      fs::path_home_r(".R", "snippets")
    }
  )
}

snippets_list <- function(which = c("js4shiny", "system")) {
  dir <- snippets_dir(which)
  if (!fs::dir_exists(dir)) return(character(0))
  x <- fs::dir_ls(dir, regexp = "snippets$")
  names(x) <- fs::path_file(x)
  x
}

snippets_warn_mask <- function(new, old = NULL, warn = FALSE) {
  # returns TRUE if snippets are masked, else FALSE
  file_new <- fs::path_file(new)
  file_old <- fs::path_file(old)
  if (is.null(old) || is.na(old)) {
    message(glue("The new {file_new} may mask the default RStudio snippets"))
    return(FALSE)
  }

  if (!identical(file_new, file_old)) {
    stop("Not a good idea to compare different snippets")
  }

  snp <-
    list(new = new, old = old) %>%
    purrr::map(snippets_read_names) %>%
    purrr::reduce(intersect)

  if (length(snp)) {
    if (warn) {
      warning(glue(
        "New snippets in '{file_new}' mask older versions:",
        "{collapse(snp, sep_c = ', ')}",
        .sep = " "
      ), call. = FALSE)
    }
    TRUE
  } else FALSE
}

snippets_read_names <- function(path) {
  snp <- if (length(path) == 1) read_lines(path) else path
  snp <- grep("^snippet", snp, value = TRUE)
  gsub("^snippet ([^ ]+).*", "\\1", snp)
}

snippets_merge <- function(new, old) {
  if (is.null(old) || is.na(old) || !fs::file_exists(old)) {
    return(read_lines(new))
  }

  snps <-
    list(new = new, old = old) %>%
    purrr::map(read_lines) %>%
    purrr::map(collapse) %>%
    purrr::map(trimws) %>%
    purrr::map(strsplit, split = "(^|\n)snippet ") %>%
    purrr::map_depth(2, ~ .x[.x != ""]) %>%
    purrr::map_depth(2, ~ {
      nm <- gsub("^([^\n\t ]+).*", "\\1", .x)
      names(.x) <- nm
      .x
    }) %>%
    purrr::flatten()

  new_snps <- setdiff(names(snps$new), names(snps$old))
  merged <- c()
  str2snippet <- function(str) {
    str <- ifelse(substr(str, 1, 1) != "#", paste("snippet", str), str)
    strsplit(collapse(str), "\n")[[1]]
  }
  for (snippet in unique(names(snps$old))) {
    if (snippet %in% names(snps$new)) {
      # choose new snippet
      merged <- c(merged, str2snippet(snps$new[[snippet]]))
    } else {
      # choose last old snippet
      old_snippet <- snps$old[which(snippet == names(snps$old))]
      old_snippet <- old_snippet[[length(old_snippet)]]
      merged <- c(merged, str2snippet(old_snippet))
    }
  }
  if (length(new_snps)) {
    for (snippet in new_snps) {
      merged <- c(merged, str2snippet(snps$new[[snippet]]))
    }
  }
  merged
}
