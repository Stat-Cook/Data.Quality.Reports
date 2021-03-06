% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/feature.fit.R
\name{feature.fit}
\alias{feature.fit}
\title{Analyze if the missingess of `target` can be predicted from `feature`.
`Feature` can be a numeric or categorical vector, and the model is LDA with
a binary decision tree as back up (rpart).}
\usage{
feature.fit(target, feature)
}
\arguments{
\item{target}{A vector with some values missing}

\item{feature}{A vector we wish to predict missingess from.}
}
\description{
Analyze if the missingess of `target` can be predicted from `feature`.
`Feature` can be a numeric or categorical vector, and the model is LDA with
a binary decision tree as back up (rpart).
}
