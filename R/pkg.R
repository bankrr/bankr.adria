#' Package utility functions
#'
#' Helper functions for working with R packages, including retrieving package
#' metadata and locating package files.
#'
#' @param env Environment to search for package name. Default is the parent frame
#' @param pkg Character string; package name. Defaults to the current package
#' @param ... Character vectors specifying subdirectories and file names within
#'   the package installation directory
#'
#' @return
#' - `pkg_name()`: Character string with the package name
#' - `pkg_version()`: Package version as a `package_version` object
#' - `pkg_file()`: Character string with the full path to the requested file
#' - `is_pkg_avail()`: Logical; `TRUE` if the package is available, `FALSE` otherwise
#'
#' @examples
#' # Get current package name
#' pkg_name()
#'
#' # Get package version
#' pkg_version("base")
#'
#' # Check if a package is available
#' is_pkg_avail("stats")
#'
#' @name pkg-utils
#' @noRd
#' @keywords internal
pkg_name <- function(env = parent.frame()) {
  utils::packageName(env)
}

#' @rdname pkg-utils
#' @noRd
#' @keywords internal
pkg_version <- function(pkg = pkg_name()) {
  utils::packageVersion(pkg)
}

#' @rdname pkg-utils
#' @noRd
#' @keywords internal
pkg_file <- function(...) {
  system.file(..., package = pkg_name())
}

#' @rdname pkg-utils
#' @noRd
#' @keywords internal
is_pkg_avail <- function(pkg) {
  requireNamespace(pkg, quietly = TRUE)
}
