% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_wrappers.R
\name{get_player_match_history}
\alias{get_player_match_history}
\title{Get match data}
\usage{
get_player_match_history(
  gamertag,
  count = 25L,
  offset = 0L,
  mode = c("matchmade", "custom"),
  one = FALSE
)
}
\arguments{
\item{gamertag}{Character value for the gamertag to view.}

\item{one}{Boolean, if TRUE returns only the first page. Useful for testing.}
}
\value{
A list of two elements, a data frame with medals, and a data frame with statistics.
}
\description{
Queries the match history for the supplied gamertag.
}
\details{
Each response is returned as JSON, which is formatted as a data.frame for the
user. The response contains a field for the next page number, which is used
until it returns \code{NULL}.

Statistics and medals are available for each match.
}
