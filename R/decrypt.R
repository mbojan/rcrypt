#' Decrypt a File Using GPG
#'
#' Decrypt a symmetrically encrypted file using GPG.
#'
#' @param input A character string of the file name you wish to decrypt.
#' @param output A character string of the file name that will be created. The default is to create a file with the same name (stripped of the .gpg or .asc file extension) in the same folder.
#' @param passphrase A character string of the passphrase used to decrypt the encrypted file. WARNING: use this to bypass the more secure option of GPG's passphrase popup box. WARNING: the passphrase may be saved in the script as cleartext, saved in the terminal history in cleartext, and/or available in the list of processes in cleartext. The default value is \code{NULL} (Insert the passphrase using GPG's secure pop-up box).
#' @return A decrypted file.
#' @examples
#' \dontrun{
#' decrypt("path/to/your/file.csv.gpg")
#' decrypt("path/to/your/file.csv.gpg", output = "path/to/your/file.csv")
#' # WARNING: only use the passphrase argument if you understand why it's
#' # not secure.
#' decrypt("path/to/your/file.csv.gpg", passphrase = "your-passphrase")
#' }
#' @export
decrypt <- function(input, output = NULL, passphrase = NULL) {
  #-----------------------------------------------------------------------------
  # Check the arguments
  #-----------------------------------------------------------------------------
  if (missing(input)) {
    stop("Check the input argument, it seems to be missing. There's nothing to decrypt.")
  }
  if (!file.exists(input)) {
    stop("Check the input argument, the file name doesn't exist. There's nothing to decrypt.")
  }
  # Use output location if specified. Otherwise use input location and name.
  if (is.null(output)) {
    # If you try to encrypt to an existing file name.
    if (file.exists(gsub(".gpg|.asc","", input))) {
      stop("Check the output argument, the file name is already in use! The decrypted file may already exist, or you need to specify a new output file name.")
    }
    output <- paste("--output", gsub(".gpg|.asc","", input))
  } else{
    # If you try to encrypt to an existing file name.
    if (file.exists(output)) {
      stop("Check the output argument, the file name is already in use! The decrypted file may already exist, or you need to specify a new output file name.")
    }
    output <- paste("--output", output)
  }
  # Unix type OS need '--no-tty' for terminal passphrase insertion.
  if (.Platform$OS.type == "unix") {
    tty <- "--no-tty"
  } else{
    tty <- NULL
  }

  #-----------------------------------------------------------------------------
  # Check GPG version
  #-----------------------------------------------------------------------------
  version <- options()[["rcrypt.gpg.version"]]

  #-----------------------------------------------------------------------------
  # Decrypt file
  #-----------------------------------------------------------------------------
  if (is.null(passphrase)) {
    # Decrypt with GUI passphrase.
    system2(
      "gpg",
      args = c(
        output,
        "--decrypt",
        input
      )
    )
  } else{
    # Decrypt with terminal passphrase insertion.
    if (version == 1) {
      system2(
        "echo",
        args = c(
          passphrase,
          "|",
          "gpg",
          "--passphrase-fd 0",
          tty,
          output,
          "--decrypt",
          input
        )
      )
    }
    if (version == 2) {
      system2(
        "gpg",
        args = c(
          "--passphrase",
          passphrase,
          "--batch",
          tty,
          output,
          "--decrypt",
          input
        )
      )
    }
  }
}
