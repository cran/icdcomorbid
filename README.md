<!-- README.md is generated from README.Rmd. Please edit that file -->

# icdcomorbid

## Introduction

The `icdcomorbid` R package provides a comprehensive toolkit for mapping
International Classification of Diseases (ICD) codes to comorbidities.
It offers versatile functionality to categorize diagnoses into standard
comorbidity groups established in the medical literature. The package is
inspired by the need for robust comorbidity interpretations in
healthcare research and practice. Leveraging established methodologies
such as those outlined by Charlson et al. (1987), Quan and Deyo (2005),
and Elixhauser et al. (1998), this package simplifies the process of
identifying and quantifying comorbidities in patient populations. With
an emphasis on efficiency and accuracy, `icdcomorbid` empowers
researchers, clinicians, and healthcare professionals to extract
meaningful insights from medical data, driving advancements in patient
care and clinical decision-making.

## Installation

You can install the `icdcomorbid` package from CRAN using the following
command:

    install.packages("icdcomorbid")

## Key Functions

### `add_decimal`

Formats ICD codes by adding decimal points to match standard formats.

### `long_to_wide`

Reshapes data from long format (multiple rows per patient) to wide
format (one row per patient with multiple columns for diagnoses). This
transformation is essential before applying comorbidity calculation
functions.

### `icd9_to_comorbid`

Maps ICD-9 codes to standard comorbidity indices using either the
Charlson or Quan-Elixhauser index.

### `icd10_to_comorbid`

Maps ICD-10 codes to standard comorbidity indices using either the
Charlson or Quan-Elixhauser index.

### `episode_of_care`

Assigns patients to episodes of care, useful for analyzing patient
treatment over time.

## Contact

For any questions or feedback, please feel free to reach out at:

**April Nguyen**  
Email: <april.nguyen1@ucalgary.ca>

## License

This package is licensed under the MIT License. See the LICENSE file for
more details.
