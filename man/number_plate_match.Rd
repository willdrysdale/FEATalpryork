% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/func_numberplate_verification.R
\name{number_plate_match}
\alias{number_plate_match}
\title{Number Plate Match}
\usage{
number_plate_match(results, img_dir)
}
\arguments{
\item{results}{output of of cloudaplr code}

\item{img_dir}{directory where image files are stored}
}
\description{
Sorts through numberplate recognition results file, and asks user to confirm if the algorithm correctly analysed the plate
}
\author{
W. S. Drysdale
}
