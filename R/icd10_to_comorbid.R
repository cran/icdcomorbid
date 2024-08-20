#' Find Comorbidities from ICD_10 Codes
#'
#' This function maps ICD_10 codes to comorbidities based on a provided mapping in order to
#' indicate whether each comorbidity is present for each ID.
#'
#' @param df The dataframe containing the data.
#' @param idx The name of the column representing the patient identifiers.
#' @param icd_cols A character vector of the columns containing ICD codes.
#' @param mapping The mapping of comorbidities to ICD codes (e.g., quan_elixhauser10, charlson10, custom list).
#' @param batch_size An optional integer specifying the number of rows to process per batch. Default is 1000.
#'
#' @return A dataframe with comorbidities as columns and IDs as rows, with True or False values indicating
#'   whether each comorbidity is present for each ID.
#'
#' @details This function assumes that the input dataframe is in wide format, where each row represents
#'   a unique identifier (ID), and each column contains a variable associated with that ID. The function
#'   maps the ICD_10 codes in the specified columns to comorbidities based on the provided mapping.
#'
#'  The mapping can be one of the following:
#'
#'	- Pre-defined mappings such as "quan_elixhauser10" or "charlson10", which are based on established comorbidity indices.
#'
#'  - Custom mappings (list), where each key represents a comorbidity and its value is a vector of ICD-9 codes associated with that comorbidity.
#'   The custom mapping codes may include up to 2 decimal places.
#'
#' @references
#' 1. Quan, H., Sundararajan, V., Halfon, P., Fong, A., Burnand, B., Luthi, J. C., ... & Ghali, W. A. (2005).
#'   Coding algorithms for defining comorbidities in ICD-9-CM and ICD-10 administrative data.
#'   Medical care, 43(11), 1130-1139.
#' 2. ICD: Python library for working with International Classification of Diseases (ICD) codes.
#'    Available online: https://github.com/mark-hoffmann/icd
#' @examples
#' df <- data.frame(ID = c(1, 2, 3),
#'                  icd_1 = c("I21.0", "I50.3", "J45.1"),
#'                  icd_2 = c("I63.38", "I10.2", "I25.2"))
#' # Using pre-existing mapping (e.g., charlson10 or quan_elixhauser10)
#' mapping <- "charlson10"
#' icd10_to_comorbid(df, "ID", c("icd_1", "icd_2"), mapping)
#'
#' # Using custom mapping
#' custom_mapping <- list("Myocardial Infarction" = c("I21.x", "I22.x", "I25.2"),
#'                        "Congestive Heart Failure" = c("I43.x", "I50.x", "I09.9"))
#' icd10_to_comorbid(df, "ID", c("icd_1", "icd_2"), custom_mapping, batch_size = 2)
#'
#' @export
icd10_to_comorbid <- function(df, idx, icd_cols, mapping, batch_size = 1000) {
	# Define valid mappings
	validMappings <- c("quan_elixhauser10", "charlson10")

	# Load mapping from JSON file
	if (is.character(mapping)) {
		mapping_file <- system.file("comorbidity_mappings", paste0(mapping, ".json"), package = "icdcomorbid")
		if (!file.exists(mapping_file)) {
			stop("Mapping file not found.")
		}
		mapping <- jsonlite::fromJSON(mapping_file)
	} else if (!is.list(mapping)) {
		stop("Bad mapping format. Please look at help document.")
	}

	# Create a list to store results from each batch
	result_list <- list()

	# Function to process a batch of rows
	process_batch <- function(batch_df) {
		# Create an empty dataframe to store the results for this batch
		result_df <- data.frame(matrix(NA, nrow = nrow(batch_df), ncol = length(mapping)))
		colnames(result_df) <- names(mapping)

		# Iterate over each comorbidity and retrieve the ICD codes associated with the comorbidity
		for (i in seq_along(mapping)) {
			comorbidity <- names(mapping)[i]
			icd_list <- mapping[[comorbidity]]

			# Apply function for each row to check if any ICD code matches
			result_df[[comorbidity]] <- apply(batch_df[icd_cols], 1, function(row) {
				row_codes <- as.character(row)  # Ensure the row codes are treated as characters
				any(sapply(icd_list, function(icd) {
					if (grepl("\\.x$", icd)) {  # Codes like "I21.x"
						# Match the code before the ".x"
						icd_prefix <- sub("\\.x$", "", icd)
						any(grepl(paste0("^", icd_prefix), row_codes))
					} else if (grepl("\\.$", icd)) {  # Codes like "I43."
						# Match the code before the period
						icd_prefix <- sub("\\.$", "", icd)
						any(grepl(paste0("^", icd_prefix), row_codes))
					} else if (grepl("\\.\\d$", icd)) {  # Codes like "E13.4"
						# Match the first 5 characters
						icd_prefix <- substr(icd, 1, 5)
						any(grepl(paste0("^", icd_prefix), substr(row_codes, 1, 5)))  # Match first 5 characters
					} else {
						# Exact match including potential decimal places
						any(grepl(paste0("^", icd), row_codes))
					}
				}))
			})
		}

		# Add the ID column to the result dataframe
		result_df[[idx]] <- batch_df[[idx]]
		result_df <- result_df[c(idx, setdiff(names(result_df), idx))]

		return(result_df)
	}

	# Process the dataframe in batches
	num_batches <- ceiling(nrow(df) / batch_size)
	for (batch in 1:num_batches) {
		start_row <- (batch - 1) * batch_size + 1
		end_row <- min(batch * batch_size, nrow(df))
		batch_df <- df[start_row:end_row, ]
		batch_results <- process_batch(batch_df)
		result_list[[batch]] <- batch_results
	}

	# Combine all batch results into a single dataframe
	final_result_df <- do.call(rbind, result_list)

	return(final_result_df)
}
