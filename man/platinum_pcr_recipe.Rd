% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pcr.R
\name{platinum_pcr_recipe}
\alias{platinum_pcr_recipe}
\title{Creates table for PCR recipie}
\usage{
platinum_pcr_recipe(rxn_count, rxn_volume, template_volume, safety = 1.1,
  checklist = TRUE)
}
\arguments{
\item{rxn_count}{The number of PCR reactions.}

\item{rxn_volume}{The total volume of each reaction}

\item{template_volume}{How much template to add}

\item{safety}{How much extra master mix to make to account for errors. For example 1.1 would be \%10 extra.}

\item{checklist}{If TRUE, print check list}
}
\description{
Creates a markdown table for a PCR master mix recipe.
}
