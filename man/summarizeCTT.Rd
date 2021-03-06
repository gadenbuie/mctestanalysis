% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classicTestTheory.R
\name{summarizeCTT}
\alias{summarizeCTT}
\title{Summarize Classic Test Theory Results}
\usage{
summarizeCTT(mctd, summarize_by = "whole",
  digits.round = getOption("digits"))
}
\arguments{
\item{mctd}{Existing mcTestAnalysis data object}

\item{summarize_by}{One of \code{"whole"}, \code{"concept"} or \code{"item"}}

\item{digits.round}{Round output to specified number of digits, defaults to
\code{digits} option (see \code{getOption("digits")})}
}
\description{
Generates Classic Test Theory summary table for CTT results.
}
