% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/genomicrange_helpers.R
\name{grl2bedpe}
\alias{grl2bedpe}
\title{GenomicRangesList to bedpe}
\usage{
grl2bedpe(grl, add_breakend_mcol = FALSE, flip = FALSE,
  as.data.table = TRUE, zerobased = TRUE)
}
\description{
converting grl to bedpe-like table
also shifts coordinates to half closed 0 based
}

