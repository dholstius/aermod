\name{read.unformatted}
\alias{read.unformatted}
\title{read.unformatted}
\usage{read.unformatted(filename, n.receptors, endian=.Platform$endian)}
\description{Read an "unformatted" (binary) AERMOD output file.}
\value{a list of records, one per modelled interval. Each record contains a date (to the hour); 
the length of the averaging period; the group ID; and a list of concentrations, one per receptor.}
\references{EPA (2004). User's Guide for the AMS/EPA Regulatory Model AERMOD. EPA-454/B-03-001.}
\arguments{\item{filename}{file to read}
\item{n.receptors}{number of receptors in the model}
\item{endian}{(optional)}}
