#' Insert decimals to the ICD codes
#'
#' This is a preprocessing step to standardize the icd codes to have decimals.
#'
#' @param df The dataframe to be converted.
#' @param icd_cols A character vector specifying the names of the columns containing ICD codes.
#' @param places An numeric value specifying the number of decimal places. Default is 3 decimal places.
#' @return A dataframe in wide format where each row represents a unique identifier (ID),
#' and each column contains a variable associated with that ID.
#' @examples
#' df <- data.frame(
#'   id = c(1, 2, 3),
#'   icd_1 = c("C509", "D633", "I210"),
#'   icd_2 = c("D509", "E788", "N183")
#' )
#' add_decimal(df, icd_cols = c("icd_1", "icd_2"), places = 3)
#' @export
add_decimal <- function(df, icd_cols, places = 3) {
	# Iterate over each specified column
	for (col in icd_cols) {
		# Convert column to character to ensure decimal can be inserted
		df[[col]] <- as.character(df[[col]])
		# Insert decimal 3 characters from the left for non-NA values
		df[[col]][!is.na(df[[col]])] <- gsub("^(.{3})", "\\1.", df[[col]][!is.na(df[[col]])], perl = TRUE)
	}

	return(df)
}
