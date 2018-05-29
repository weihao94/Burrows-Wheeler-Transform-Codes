##################################
### Burrows-Wheeler Transform  ###
### Created by: Wei Hao Khoong ###
### Date: 29/5/2018            ###
##################################

## Documentation file 'Burrows_Wheeler_Transform.Rd' is available at
## https://github.com/weihao94/Burrows-Wheeler-Transform-Codes

## Notations: End-Of-File (eof)

bwt <- function(x, eof = "!") {
  if (grepl(eof, "[[:cntrl:]]")) stop("eof can't be a RegEx control character")
  if (grepl(eof, x)) stop("x cannot contain an eof character")
  x <- paste0(x, eof)
  n <- nchar(x)

  # Append the first character to the end
  rotate <- function(x) paste0(substring(x, 2), substring(x, 1, 1))

  # A table of all possible rotations
  tbl <- c(x, vector("character", n - 1))
  for(i in 2:n) tbl[i] <- rotate(tbl[i - 1])

  # Sort rows alphabetically
  tbl <- sort(tbl)

  # Retreive last column of the table
  out <- sapply(tbl, substring, first = n, USE.NAMES = FALSE)
  paste(out, collapse = "")
}

ibwt <- function(x, eof = "!") {
  if (!grepl(eof, x)) stop("x does not contain an eof character")
  tbl <- x <- strsplit(x, "")[[1]]

  while (nchar(tbl[1]) < length(x)) {
    sorted <- sort(tbl)
    tbl <- apply(cbind(x, sorted), 1, paste, collapse = "")
  }
  sub(eof, "", tbl[grepl(paste0(eof, "$"), tbl)])
}
