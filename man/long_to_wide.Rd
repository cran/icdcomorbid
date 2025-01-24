% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/long_to_wide.R
\name{long_to_wide}
\alias{long_to_wide}
\title{Reshape Long Format Data to Wide Format}
\usage{
long_to_wide(df, idx, icd_cols, batch_size = 1000)
}
\arguments{
\item{df}{The dataframe to be converted.}

\item{idx}{The name of the column containing the unique identifier (ID).}

\item{icd_cols}{A character vector specifying the names of the columns containing ICD codes.}

\item{batch_size}{An optional integer specifying the number of rows to process per batch. Default is 1000.}
}
\value{
A dataframe in wide format where each row represents a unique identifier (ID),
and each column contains a variable associated with that ID.
}
\description{
This is a preprocessing step to transform a dataframe from long to wide format
to use with icd_to_comorbid function.
}
\examples{
df <- data.frame( ID = c(1, 1, 2, 2, 3, 3, 3),
                 icd_1 = c("I10.2", "E03.9", "E11.9", "N18.9", "A04.7", NA, NA),
                 icd_2 = c("I11.9", "E78.5", "E78.2", "E14.9","A04.7", "E11.9", NA))
long_to_wide(df, "ID", c("icd_1", "icd_2"), batch_size = 1000)
}
\references{
ICD: Python library for working with International Classification of Diseases (ICD) codes.
   Available online: https://github.com/mark-hoffmann/icd
}
