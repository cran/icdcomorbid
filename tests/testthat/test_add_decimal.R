# tests/testthat/test_add_decimal.R
library(testthat)
library(icdcomorbid)

test_that("add_decimal works correctly", {
	# Create a sample data frame
	sample_data <- data.frame(
		patient_id = c(1, 2),
		icd_code = c("4100", "4280")
	)

	# Call the function
	result <- add_decimal(df = sample_data, icd_cols = "icd_code")

	# Expected output
	expected <- data.frame(
		patient_id = c(1, 2),
		icd_code = c("410.0", "428.0"),
		stringsAsFactors = FALSE
	)

	# Check if the result matches the expected output
	expect_equal(result, expected)
})
