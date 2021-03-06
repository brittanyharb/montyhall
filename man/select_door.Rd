% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-problem.R
\name{select_door}
\alias{select_door}
\title{\preformatted{Select Door.
}}
\usage{
select_door()
}
\arguments{
\item{x}{Numeric vector}
}
\value{
\preformatted{an integer that represents the number of the door that the participant selects.
}
}
\description{
\preformatted{`select_door()` A door is randomly selected as the contestant choice. 
}
}
\details{
This function assignes a random door as the contestant's first choice
of door before anything is revealed. The contestant can only choose between door 1, 2, or 3.
}
\examples{
	select_door()

}
