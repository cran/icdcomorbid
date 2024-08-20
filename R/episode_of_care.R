#' Identify Episodes of Care
#'
#' This function identifies episodes of care for patients based on their visit and discharge dates
#' from two different data sources (DAD and NACRS).
#'
#' @param dad_df DataFrame containing DAD data.
#' @param nacrs_df DataFrame containing NACRS data.
#' @param patient_id_col String representing the column name for patient ID.
#' @param dad_visit_date_col String representing the column name for visit date in DAD data.
#' @param dad_exit_date_col String representing the column name for exit date in DAD data.
#' @param nacrs_visit_date_col String representing the column name for visit date in NACRS data.
#' @param nacrs_exit_date_col String representing the column name for exit date in NACRS data.
#'
#' @return A DataFrame with episodes of care identified and a unique record ID for each row.
#' @export
#' @examples
#' # Sample DAD data
#' dad_df <- data.frame(
#'   patient_id = c("A001", "A001", "A002", "A002", "A003"),
#'   admit_dt = c("01Dec2023:10:00:00", "03Jan2024:12:00:00",
#'   "05Jan2024:09:00:00", "07Jan2024:14:00:00", "12Jan2024:12:00:00"),
#'   discharge_dt = c("02Dec2023:10:00:00", "04Jan2024:10:00:00",
#'   "06Jan2024:10:00:00", "08Jan2024:10:00:00", "15Jan2024:08:00:00")
#' )
#'
#' # Sample NACRS data
#' nacrs_df <- data.frame(
#'   patient_id = c("A001", "A002", "A003", "A003", "A004"),
#'   visit_dt = c("03Jan2024:09:00:00", "07Feb2024:15:00:00",
#'   "10Jan2024:09:00:00", "11Jan2024:10:00:00", "12Jan2024:11:00:00"),
#'   disp_dt = c("03Jan2024:11:00:00", "07Feb2024:17:00:00",
#'   "10Jan2024:10:00:00", "12Jan2024:12:00:00", "13Jan2024:13:00:00")
#' )
#'
#' episode_of_care(dad_df, nacrs_df, "patient_id", "admit_dt", "discharge_dt", "visit_dt", "disp_dt")
#'
episode_of_care <- function(dad_df, nacrs_df, patient_id_col, dad_visit_date_col, dad_exit_date_col, nacrs_visit_date_col, nacrs_exit_date_col) {

	# Ensure the date columns are in POSIXct format for accurate date-time operations
	dad_df[[dad_visit_date_col]] <- as.POSIXct(dad_df[[dad_visit_date_col]], format="%d%b%Y:%H:%M:%S", tz="UTC")
	dad_df[[dad_exit_date_col]] <- as.POSIXct(dad_df[[dad_exit_date_col]], format="%d%b%Y:%H:%M:%S", tz="UTC")
	# Check and convert NACRS date columns to POSIXct
	if (is.numeric(nacrs_df[[nacrs_visit_date_col]])) {
		nacrs_df[[nacrs_visit_date_col]] <- as.POSIXct(nacrs_df[[nacrs_visit_date_col]], origin="1970-01-01", tz="UTC")
	} else {
		nacrs_df[[nacrs_visit_date_col]] <- as.POSIXct(nacrs_df[[nacrs_visit_date_col]], format="%d%b%Y:%H:%M:%S", tz="UTC")
	}

	if (is.numeric(nacrs_df[[nacrs_exit_date_col]])) {
		nacrs_df[[nacrs_exit_date_col]] <- as.POSIXct(nacrs_df[[nacrs_exit_date_col]], origin="1970-01-01", tz="UTC")
	} else {
		nacrs_df[[nacrs_exit_date_col]] <- as.POSIXct(nacrs_df[[nacrs_exit_date_col]], format="%d%b%Y:%H:%M:%S", tz="UTC")
	}

	# Specify the column names to keep from dad_df and nacrs_df
	dad_cols <- c(patient_id_col, dad_visit_date_col, dad_exit_date_col)
	nacrs_cols <- c(patient_id_col, nacrs_visit_date_col, nacrs_exit_date_col)

	# Select the specified columns from dad_df and nacrs_df
	dad_subset <- dad_df[, dad_cols]
	nacrs_subset <- nacrs_df[, nacrs_cols]

	# Rename columns in dad_df and nacrs_df
	colnames(dad_subset) <- c(patient_id_col, "dad_admit", "dad_dis")
	colnames(nacrs_subset) <- c(patient_id_col, "nacrs_admit", "nacrs_dis")

	# Add a source column to each dataframe to differentiate records from DAD and NACRS
	dad_subset$source <- 'DAD'
	nacrs_subset$source <- 'NACRS'

	# Create a dataframe for dad visits
	dad_visits <- data.frame(
		patient_id = dad_subset[[patient_id_col]],
		dad_admit = dad_subset$dad_admit,
		dad_dis = dad_subset$dad_dis,
		nacrs_admit = as.POSIXct(NA, format="%d%b%Y:%H:%M:%S", tz="UTC"),
		nacrs_dis = as.POSIXct(NA, format="%d%b%Y:%H:%M:%S", tz="UTC"),
		source = dad_subset$source
	)

	# Create a dataframe for nacrs visits
	nacrs_visits <- data.frame(
		patient_id = nacrs_subset[[patient_id_col]],
		dad_admit = NA,
		dad_dis = NA,
		nacrs_admit = nacrs_subset$nacrs_admit,
		nacrs_dis = nacrs_subset$nacrs_dis,
		source = nacrs_subset$source
	)

	# Combine the dataframes
	combined_df <- rbind(dad_visits, nacrs_visits)

	# Sort the combined dataframe by patient_id and the earliest non-NA date
	combined_df <- combined_df[order(combined_df$patient_id,
																	 ifelse(is.na(combined_df$dad_admit), combined_df$nacrs_admit, combined_df$dad_admit)), ]

	# Initialize episode_of_care column
	combined_df$episode_of_care <- 1

	# Iterate through each patient's records to determine episodes of care
	for (i in 2:nrow(combined_df)) {
		# Check if the current record and the previous record belong to the same patient
		if (combined_df$patient_id[i] == combined_df$patient_id[i - 1]) {
			# Determine the previous discharge date
			if (!is.na(combined_df$dad_dis[i - 1])) {
				previous_discharge <- combined_df$dad_dis[i - 1]
			} else {
				previous_discharge <- combined_df$nacrs_dis[i - 1]
			}

			# Determine the current admission date
			if (!is.na(combined_df$dad_admit[i])) {
				current_admit <- combined_df$dad_admit[i]
			} else {
				current_admit <- combined_df$nacrs_admit[i]
			}

			# Calculate the time difference between the current admission and the previous discharge
			time_diff <- difftime(current_admit, previous_discharge, units = "hours")

			# Handle cases where previous_discharge is NA and source is NACRS
			if (is.na(previous_discharge) && combined_df$source[i - 1] == "NACRS") {
				# Compare the current admission with the previous admission date
				previous_admit <- ifelse(!is.na(combined_df$dad_admit[i - 1]), combined_df$dad_admit[i - 1], combined_df$nacrs_admit[i - 1])
				time_diff_admit <- difftime(current_admit, previous_admit, units = "hours")
				if (!is.na(time_diff_admit) && time_diff_admit >= 24) {
					combined_df$episode_of_care[i] <- combined_df$episode_of_care[i - 1] + 1
				} else {
					combined_df$episode_of_care[i] <- combined_df$episode_of_care[i - 1]
				}
			} else if (!is.na(time_diff) && time_diff >= 24) {
				combined_df$episode_of_care[i] <- combined_df$episode_of_care[i - 1] + 1
			} else {
				combined_df$episode_of_care[i] <- combined_df$episode_of_care[i - 1]
			}
		}
	}

	# Assign a unique record_id to each row
	combined_df$record_id <- seq_len(nrow(combined_df))
	rownames(combined_df) <- combined_df$record_id

	# Return the combined dataframe with specified columns
	combined_df <- combined_df[, c("record_id", "patient_id", "dad_admit", "dad_dis", "nacrs_admit", "nacrs_dis", "source", "episode_of_care")]
	return(combined_df)
}
