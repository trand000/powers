#' None Exportable Functions
#'
#' @noRd
dumtest <- function() {
  "hello"
}


#' Title of the Function
#'
#' @description A short description of what the function does OK.
#'
#' @param x Description of the first argument XX.
#'
#' @return Description of the return value ZZ.
#' @examples
#' square(3)
#'
#' @export
square <- function(x) {
  x^2
}




#' Title of the Function
#'
#' @description A short description of what the function does OK.
#'
#' @param x Description of the first argument XX.
#'
#' @return Description of the return value ZZ.
#' @examples
#' df1 <- data.frame(
#'  a = c("hello", "  NA", "   "),
#'  b = c("x", NA , "y"),
#'  c = c(1,2,NA),
#'  stringsAsFactors = FALSE
#'  )
#'
#' clean_characters(df1)
#'
#' df2 <- df1 |>
#'  dplyr::mutate(aa = clean_characters(a))
#' df2
#'
#' a = c("hello", "hi", NA)
#' b = c(1,2)
#'
#' clean_characters(a)
#' clean_characters(b)
#'
#' clean_characters(df1[1:3])
#' clean_characters(df1$a)
#'
#' @export
clean_characters <- function(
    df
    ,convert_real_na = TRUE
    ,trim = TRUE
    ,extra_bad = NULL          # user-supplied extra patterns
) {


  if (!(is.character(df) || is.data.frame(df))) {
    stop("Input must be either a character vector or a data frame.")
  }

  # Base set of "fake NA" values (case-insensitive)
  base_bad <- c("na", "n/a", "null", "none", "unk", "unknown", "missing", "")

  df[] <- lapply(df, function(col) {
    if (is.character(col)) {

      # Trim whitespace if requested
      if (trim) {
        col <- trimws(col)
      }

      # Normalize case for comparison
      col_lower <- tolower(col)

      # Combine all bad values
      bad_values <- unique(c(base_bad, tolower(extra_bad)))

      # Replace exact matches
      col[col_lower %in% bad_values] <- ""

      # Convert real NA if requested
      if (convert_real_na) {
        col[is.na(col)] <- ""
      }
    }
    col
  })

  if (is.data.frame(df) == FALSE){
    df <- as.character(df)
  }

  return(df)
}

