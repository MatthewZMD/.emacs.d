
.ess_keep <- function(.x, .f, ...) {
  is_true <- vapply(.x, .f, logical(1), ...)
  .x[is_true]
}

.ess_devtools_functions <- function() {
  if (!requireNamespace("devtools")) {
    .ess_mpi_error("devtools is not installed")
    stop("internal error")
  }
  devtools_env <- asNamespace("devtools")
  exports <- getNamespaceExports("devtools")
  funs_exported <- as.list(devtools_env)[exports]

  is_first_arg <- function(f, arg) {
    args <- names(formals(f))
    length(args) && args[[1]] == arg
  }

  funs_pkg <- .ess_keep(funs_exported, is.function)
  funs_pkg <- .ess_keep(funs_pkg, is_first_arg, "pkg")
  funs_names <- sort(names(funs_pkg))

  funs_names
}
