# Check if GPG is installed and can be run
.onLoad <- function(libname, pkgname) {
  # Error if gpg is not installed
  if(!system2("gpg", "--version", stdout = FALSE) == 0){
    stop("Please install GPG first or check if GPG can be run from the command line. The rcrypt package is just an interface to GPG. See https://gnupg.org/ for installation guidelines.")
  }

  # Set gpg version for proper usage
  version <- system2("gpg", "--version", stdout = TRUE, stderr = TRUE)
  version <- version[1]
  ## looks for number-dot-number-dot-number and returns position of first
  ## number and how long it is
  version_loc <- regexpr("(?:\\d\\.)(?:\\d*\\.)(?:\\d*)", version, perl=TRUE)
  ## Saves just the version number
  version_num <- substr(
    version,
    version_loc[1],
    version_loc[1] + attributes(version_loc)$match.length - 1
  )
  ## Saves first number in version
  version_num <- as.integer(substring(version_num, 1, 1))

  ## This isn't good, but it would probably work
  if (version_num != 1 && version_num != 2 || is.na(version_num)) {
    version_num <- 2
  }

  # Store gpg version in options
  op <- options()
  op.rcrypt <- list(
    rcrypt.gpg.version = version_num
  )
  toset <- !(names(op.rcrypt) %in% names(op))
  if(any(toset)) options(op.rcrypt[toset])
  invisible()
}
