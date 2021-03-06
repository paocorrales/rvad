% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/regrid.R
\name{vad_regrid}
\alias{vad_regrid}
\title{Wind profile from VAD}
\usage{
vad_regrid(vad, layer_width, resolution = layer_width, ht_out = NULL,
  min_n = 5)
}
\arguments{
\item{vad}{an \code{rvad_vad} object returned by \code{\link[=vad_fit]{vad_fit()}}.}

\item{layer_width}{width of the layers in meters (see Details).}

\item{resolution}{vertical resolution in meters.}

\item{ht_out}{vector of heights where to evaluate. Overrides \code{resolution}.}

\item{min_n}{minimum number of points in each layer.}
}
\value{
A data frame with class \code{rvad_vad} that has a \code{\link[=plot]{plot()}} method and contains
7 variables:
\describe{
\item{height}{height above the radar in meters.}
\item{u}{zonal wind in m/s.}
\item{v}{meridional wind in m/s.}
\item{u_std.error}{standar error of u in m/s.}
\item{v_std.error}{standar error of v in m/s.}
}
}
\description{
Aggregates the result of \code{\link[=vad_fit]{vad_fit()}} using a modified loess
smooth of degree 1 to get a wind profile on a regular (or other
user-supplied) grid.
}
\details{
The method approximates wind components in a regular grid using weighted local
regression at each point in the grid. Unlike \code{\link[stats:loess]{stats::loess()}}, the layer_width
is specified in physical units instead of in ammount of points and thus the
value at each gridpoint represents the wind at a layer of thickness
\code{layer_width}. This means that, while the \code{resolution} parameter determines
how many points are used to define the wind profile, the effective resolution
is controlled by \code{layer_width}. Increasing \code{layer_width} results in more precise
estimates (because it's basedon more data points) but reduces the effective
resolution.
}
\examples{
VAD <- with(radial_wind, vad_fit(radial_wind, azimuth, range, elevation))

# Wind profile with effective resolution of 100
plot(vad_regrid(VAD, layer_width = 100, resolution = 100))
# The same effective resoution, but sampled at 50m
plot(vad_regrid(VAD, layer_width = 100, resolution = 50))

# Using too thin layers can cause problems and too many
# mising values
plot(fine_resolution <- vad_regrid(VAD, layer_width = 10))
mean(is.na(fine_resolution$u))

}
