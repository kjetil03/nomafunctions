% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_list.R
\name{create_list}
\alias{create_list}
\title{Create series list input for noma_ts_plot}
\usage{
create_list(name, plot_type, legendname = name, color = NULL)
}
\arguments{
\item{name}{navn på serie}

\item{plot_type}{hva slags plot-type man vil lage. Kan ta verdiene "dot", "line", "dashed line", "bar", "stacked bar"}

\item{legendname}{hva skal legendname være}

\item{color}{valg av farge}
}
\description{
Create series list input for noma_ts_plot
}
\examples{
\dontrun{series_list = list(create_list("last_3m", "line", "NOK 3m terminpunkter",
                            create_list("last_2m", "bar", "NOK 2m terminpunkter"),
                            create_list("last_1m", "dashed line", "NOK 1m terminpunkter"))}
}
