# tests/testthat/test_episode_of_care.R
library(testthat)
library(icdcomorbid)

test_that("episode_of_care works correctly", {
	# Create sample data for DAD and NACRS
	dad_data <- data.frame(
		patient_id = c(1, 1, 2),
		dad_admit = as.POSIXct(c("2023-01-01 10:00:00", "2023-02-01 09:00:00",
														 "2023-01-15 08:00:00"), tz="UTC"),
		dad_dis = as.POSIXct(c("2023-01-10 15:00:00", "2023-02-10 14:00:00",
													 "2023-01-20 12:00:00"), tz="UTC")
	)

	nacrs_data <- data.frame(
		patient_id = c(1, 2, 2),
		nacrs_admit = as.POSIXct(c("2023-01-15 10:00:00", "2023-01-25 09:00:00",
															 "2023-03-01 08:00:00"), tz="UTC"),
		nacrs_dis = as.POSIXct(c("2023-01-20 15:00:00", "2023-01-30 14:00:00",
														 "2023-03-05 12:00:00"), tz="UTC")
	)

	# Call the function
	result <- episode_of_care(
		dad_data,
		nacrs_data,
		patient_id_col = "patient_id",
		dad_visit_date_col = "dad_admit",
		dad_exit_date_col = "dad_dis",
		nacrs_visit_date_col = "nacrs_admit",
		nacrs_exit_date_col = "nacrs_dis"
	)

	# Expected output
	expected <- data.frame(
		record_id = 1:6,
		patient_id = c(1, 1, 1, 2, 2, 2),
		dad_admit = as.POSIXct(c("2023-01-01 10:00:00", NA, "2023-02-01 09:00:00",
														 "2023-01-15 08:00:00", NA, NA), tz = "UTC"),
		dad_dis = as.POSIXct(c("2023-01-10 15:00:00", NA, "2023-02-10 14:00:00",
													 "2023-01-20 12:00:00", NA, NA), tz = "UTC"),
		nacrs_admit = as.POSIXct(c(NA, "2023-01-15 10:00:00", NA, NA,
															 "2023-01-25 09:00:00", "2023-03-01 08:00:00"),
														 tz = "UTC"),
		nacrs_dis = as.POSIXct(c(NA, "2023-01-20 15:00:00", NA, NA,
														 "2023-01-30 14:00:00", "2023-03-05 12:00:00"),
													 tz = "UTC"),
		source = c("DAD", "NACRS", "DAD", "DAD", "NACRS", "NACRS"),
		episode_of_care = c(1, 2, 3, 1, 2, 3),
		stringsAsFactors = FALSE
	)

  # Check if the result matches the expected output
  expect_equal(result, expected)
})
