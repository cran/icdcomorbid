# tests/testthat/test-icd9_to_comorbid.R
library(testthat)
library(icdcomorbid)

test_that("icd9_to_comorbid works correctly", {
	# Create a sample data frame
	icd9_data <- data.frame(
		patient_id = c(1, 2),
		icd9_code = c("410.0", "428.0")
	)

	# Call the function
	result <- icd9_to_comorbid(df = icd9_data, idx = "patient_id",
														 icd_cols = "icd9_code", mapping = "charlson9")

	# Expected output
	expected <- data.frame(
		patient_id = c(1, 2),
		myocardial_infarction = c(TRUE, FALSE),
		congestive_heart_failure = c(FALSE, TRUE),
		peripheral_vascular_disease = c(FALSE, FALSE),
		cerebrovascular_disease = c(FALSE, FALSE),
		dementia = c(FALSE, FALSE),
		chronic_pulmonary_disease = c(FALSE, FALSE),
		connective_tissue_disease_rheumatic_disease = c(FALSE, FALSE),
		mild_liver_disease = c(FALSE, FALSE),
		diabetes_wo_complications = c(FALSE, FALSE),
		diabetes_w_complications = c(FALSE, FALSE),
		paraplegia_and_hemiplegia = c(FALSE, FALSE),
		renal_disease = c(FALSE, FALSE),
		cancer = c(FALSE, FALSE),
		moderate_or_severe_liver_disease = c(FALSE, FALSE),
		metastatic_carcinoma = c(FALSE, FALSE),
		aids_hiv = c(FALSE, FALSE),
		stringsAsFactors = FALSE
	)

	# Check if the result matches the expected output
	expect_equal(result, expected)
})
