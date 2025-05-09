% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tdm_generate_subdaily_models.R
\name{gen.subdaily.models}
\alias{gen.subdaily.models}
\title{Generate Subdaily Models}
\usage{
gen.subdaily.models(
  outfolder,
  path.train,
  yrs.train,
  direction.filter = "forward",
  in.prefix,
  n.beta,
  day.window,
  seed = Sys.time(),
  resids = FALSE,
  parallel = FALSE,
  n.cores = NULL,
  overwrite = TRUE,
  verbose = FALSE,
  print.progress = FALSE
)
}
\arguments{
\item{outfolder}{- directory where models will be stored *** storage required varies by size of training dataset, but prepare for >10 GB}

\item{path.train}{- path to CF/PEcAn style training data where each year is in a separate file.}

\item{yrs.train}{- which years of the training data should be used for to generate the model for 
the subdaily cycle.  If NULL, will default to all years}

\item{direction.filter}{- Whether the model will be filtered backward or forward in time. options = c("backward", "forward")
(PalEON will go backward, anybody interested in the future will go forward)}

\item{in.prefix}{not used}

\item{n.beta}{- number of betas to save from linear regression model}

\item{day.window}{- integer specifying number of days around the day being modeled you want to use data from for that 
specific hours coefficients. Must be integer because we want statistics from the same time of day
for each day surrounding the model day}

\item{seed}{- seed for randomization to allow for reproducible results}

\item{resids}{- logical stating whether to pass on residual data or not (this increases both memory & storage requirements)}

\item{parallel}{- logical stating whether to run temporal_downscale_functions.R in parallel}

\item{n.cores}{- deals with parallelization}

\item{overwrite}{logical: replace output file if it already exists?}

\item{verbose}{logical, currently ignored}

\item{print.progress}{- print progress bar? (gets passed through)}
}
\description{
Create statistical models to predict subdaily meteorology
This is the 2nd function in the tdm workflow that takes the dat.train_file that is created from the
             nc2dat.train function and generates "lag.days" and "next.days". These variables pass along information
             of the previous time step and provides a preview of the next time step. After these variables are created,
             the models are generated by calling the tdm_temporal_downscale_functions.R scripts and these models
             and betas are saved separately. Please note that these models and betas require a significant
             amount of space. The storage required varies by the size of the training dataset, but prepare for
             >100 GB. These will be called later in tdm_predict_subdaily_met to perform the linear regression
             analysis.
}
\seealso{
Other tdm - Temporally Downscale Meteorology: 
\code{\link{lm_ensemble_sims}()},
\code{\link{model.train}()},
\code{\link{nc.merge}()},
\code{\link{predict_subdaily_met}()},
\code{\link{save.betas}()},
\code{\link{save.model}()},
\code{\link{subdaily_pred}()},
\code{\link{temporal.downscale.functions}()}
}
\author{
Christy Rollinson, James Simkins
}
\concept{tdm - Temporally Downscale Meteorology}
