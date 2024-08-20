# tests/testthat/test_long_to_wide.R
library(testthat)
library(icdcomorbid)

test_that("long_to_wide works correctly", {
	# Create a sample data frame in long format
	long_data <- data.frame(
		patient_id = c(1, 1, 2, 2, 3),
		icd_1 = c("A01", "A02", "B01", "B02", "C01"),
		icd_2 = c("D01", "E02", "F01", "G02", "H01"),
		stringsAsFactors = FALSE
	)

	# Call the function
	result <- long_to_wide(df = long_data, idx = "patient_id",
												 icd_cols = c("icd_1", "icd_2"))

	# Expected output in wide format
	expected <- data.frame(
		patient_id = c("1", "2", "3"),
		icd_1 = c("A01", "B01", "C01"),
		icd_2 = c("D01", "F01", "H01"),
		icd_3 = c("A02", "B02", NA),
		icd_4 = c("E02", "G02", NA),
		icd_5 = c(NA, NA, NA),
		stringsAsFactors = FALSE
	)
	# Check if the result matches the expected output
	expect_equal(result, expected)
})
