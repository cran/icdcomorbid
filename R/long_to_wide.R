#' Reshape Long Format Data to Wide Format
#'
#' This is a preprocessing step to transform a dataframe from long to wide format
#' to use with icd_to_comorbid function.
#'
#' @param df The dataframe to be converted.
#' @param idx The name of the column containing the unique identifier (ID).
#' @param icd_cols A character vector specifying the names of the columns containing ICD codes.
#' @param batch_size An optional integer specifying the number of rows to process per batch. Default is 1000.
#'
#' @return A dataframe in wide format where each row represents a unique identifier (ID),
#' and each column contains a variable associated with that ID.
#' @references
#' ICD: Python library for working with International Classification of Diseases (ICD) codes.
#'    Available online: https://github.com/mark-hoffmann/icd
#' @examples
#' df <- data.frame( ID = c(1, 1, 2, 2, 3, 3, 3),
#'	                 icd_1 = c("I10.2", "E03.9", "E11.9", "N18.9", "A04.7", NA, NA),
#'	                 icd_2 = c("I11.9", "E78.5", "E78.2", "E14.9","A04.7", "E11.9", NA))
#' long_to_wide(df, "ID", c("icd_1", "icd_2"), batch_size = 1000)
#' @export
long_to_wide <- function(df, idx, icd_cols, batch_size = 1000) {
	if (!idx %in% names(df)) {
		stop("The specified index column does not exist in the dataframe.")
	}

	df[df == ""] <- NA

	# Determine the maximum number of ICD columns needed
	max_icd_count <- max(colSums(!is.na(df[icd_cols])))

	# Initialize variables
	info_list <- list()
	current_id <- NULL
	current_icd_set <- NULL

	# Function to process a batch of rows
	process_batch <- function(batch_df) {
		batch_info_list <- list()

		# Initialize variables for the current batch
		current_id <- NULL
		current_icd_set <- rep(NA, max_icd_count)  # Initialize with logical NA values

		# Loop through each row of the batch
		for (i in 1:nrow(batch_df)) {
			# Extract the ID for the current row
			id_val <- batch_df[i, idx]

			# If the ID changes, save the information for the previous ID (if any) and reset the current ICD set
			if (!is.null(current_id) && id_val != current_id) {
				batch_info_list[[length(batch_info_list) + 1]] <- c(current_id, current_icd_set)
				current_icd_set <- rep(NA, max_icd_count)  # Reset with logical NA values
			}

			# Loop through each ICD code column
			for (col in icd_cols) {
				icd_val <- as.character(batch_df[i, col])
				if (!is.na(icd_val)) {
					# Place the ICD value in the first available slot in the ICD set
					na_idx <- which(is.na(current_icd_set))[1]
					current_icd_set[na_idx] <- icd_val
				}
			}

			current_id <- id_val
		}

		# Save the information for the last ID in the batch
		if (!is.null(current_icd_set)) {
			batch_info_list[[length(batch_info_list) + 1]] <- c(current_id, current_icd_set)
		}

		return(batch_info_list)
	}

	# Process the dataframe in batches
	num_batches <- ceiling(nrow(df) / batch_size)
	for (batch in 1:num_batches) {
		start_row <- (batch - 1) * batch_size + 1
		end_row <- min(batch * batch_size, nrow(df))
		batch_df <- df[start_row:end_row, ]
		batch_results <- process_batch(batch_df)
		info_list <- c(info_list, batch_results)
	}

	# Convert the list of information into a dataframe
	transformed_df <- as.data.frame(do.call(rbind, info_list), stringsAsFactors = FALSE)

	# Rename the columns
	names(transformed_df)[1] <- idx
	names(transformed_df)[-1] <- paste0("icd_", 1:max_icd_count)

	# Convert columns to character and then to logical NA
	for (i in 2:ncol(transformed_df)) {
		transformed_df[[i]] <- as.character(transformed_df[[i]])
		# Convert empty strings to logical NA
		transformed_df[[i]] <- ifelse(transformed_df[[i]] == "", NA, transformed_df[[i]])
	}

	return(transformed_df)
}
