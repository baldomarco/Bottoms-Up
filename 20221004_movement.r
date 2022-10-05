#' # Resource selection and step selection functions
#' BjÃ¶rn Reineking, 2022-09-30
#' 
#' Model the relative density of animals (also called range distribution or 
#' utilisation distribution) as a function of environmental predictors. 
#' 
#' We will use the buffalo data set. The reference for this data set is Getz et al. (2007) PLoS ONE 2(2): e207.
#' doi:10.1371/journal.pone.0000207 and Cross et al. (2016) Movebank Data Repository. doi:10.5441/001/1.j900f88t
#' 

# use this packages for the working of the code and install Rtools!!

devtools::install_github("biomodhub/biomod2", dependencies = TRUE)
remotes::install_github("annescharf/animove")

installed.packages("Rtools")
install.packages("animove")
install.packages("ctmm")
install.packages("sf")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("amt")
install.packages("move")
install.packages("raster")

#'Loading packages
#+  results='hide', message=FALSE, warning=FALSE
library(animove)
library(ctmm)
library(sf)
library(lubridate)
library(ggplot2)
library(amt)
library(move)
library(raster)

#' ## Load buffalo data
#' 

data(buffalo_utm)
# buffalo_utm <- readRDS("Data/buffalo_utm.rds")
#' ## Environmental data: topography, waterways, and NDVI
data(buffalo_env)
raster::plot(buffalo_env)

#' ## Animals and elevation
raster::plot(raster(buffalo_env, 1))
points(buffalo_utm)

#' To speed up analyses, we will only work with one individual, Cilla
cilla <- buffalo_utm[["Cilla"]]
raster::plot(raster(buffalo_env, 1), ext = extent(cilla) * 2)
lines(cilla)

#' The first day of Cilla shows some unrealistic movements. We remove those observations
cilla <- cilla[timestamps(cilla) > min(timestamps(cilla)) + days(1), ]
raster::plot(raster(buffalo_env, 1), ext = extent(cilla) * 2)
lines(cilla)

#' # Minimal example of rsf.fit
#' - Estimate a continuous time movement model
#' - Estimate an akde (autocorrelated kernel density estimate)
#' - Fit RSF
#' - Summarise model (parameter estimates and confidence intervals) 
#' - Predict suitability map (only habitat selection, no homeranging)
#' - Predict range distribution (includes habitat selection and homeranging behaviour)

#' Create telemetry object
cilla_telemetry <- as.telemetry(cilla)
#' Fit ctmm model
#' 
#' rsf.fit assumes that the model is isotropic, i.e. that the home range attractor is equally strong in all directions.
#' 
cilla_guess <- ctmm.guess(cilla_telemetry, CTMM=ctmm(isotropic = TRUE), interactive = FALSE)
cilla_select <- ctmm.select(cilla_telemetry, cilla_guess)
summary(cilla_select)

#' Fit akde
cilla_akde <- akde(cilla_telemetry, cilla_select)
plot(cilla_akde)

#' Create named list of rasters - this is needed by rsf.fit
#' The advantage is that rsf.fit can deal with rasters that differ in projection
be <- list("elev" = raster(buffalo_env, "elev"),
           "slope" = raster(buffalo_env, "slope"),
           "mean_NDVI" = raster(buffalo_env, "mean_NDVI"))

#' The integrator = "Riemann" option is still experimental, we use it here because it is much faster.
#' 
cilla_rsf <- rsf.fit(cilla_telemetry, cilla_akde, R = be, integrator = "Riemann")
summary(cilla_rsf)


#' Range distribution (includes the ranging behaviour)
agde_map <- agde(cilla_rsf, be)
plot(agde_map)


#' A suitability map
elev_crop <- crop(be[[1]], extent(cilla) * 2)
suitability_map <- suitability(cilla_rsf, be, aggregate(elev_crop, 2))
raster::plot(suitability_map)

#' Selection-informed akde (includes habitat selection and kernel density estimation)
# cilla_rsf_akde <- akde(cilla_telemetry, cilla_rsf, R = be, grid = elev_crop)
# plot(cilla_rsf_akde) # plot function does not currently work

#' # Step selection functions
#'
#' ## Convert MoveStack to amt::track object
#' Currently, there is no conversion function from move::moveStack to amt::track implemented, so we do it by hand
buffalo_tracks <- as.data.frame(buffalo_utm) %>% 
  select(id = individual.local.identifier, x = coords.x1, y = coords.x2, ts = timestamp) %>% 
  make_track(x, y, ts, id, crs = projection(buffalo_utm))

#' To speed up analyses, we will only work with one individual, Cilla
cilla_track <- filter(buffalo_tracks, id == "Cilla")
cilla_track <- filter(cilla_track, t_ > min(t_) + days(1))

#' ## Thin movement data and split to bursts
#' - We reduce the data set to observations that are within a certain time step range. The SSF assumes Brownian motion, so we should thin sufficiently, so that the velocities of successive steps are uncorrelated. See presentation by Chris on Monday. Here we go for 3 hours. 
#' - There is some tolerance around the target time interval of 3 hours. When two observations are separated by less than the threshold, the second observation is removed
#' - When two observations are separated by more than the upper threshold, the observations are assigned to different bursts.
#' 
#' It is a good idea to perform the analysis at several temporal scales, i.e. different step durations.
#' 
#' The initial sampling rate of cilla is about 1 hour:
#' 
summarize_sampling_rate(cilla_track)

#' ## Prior analysis of spatio-temporal autocorrelation
#' SSF assumes that the velocities are not autocorrelated. So we cannot simply do the analysis
#' at the highest temporal resolution of the data.
#' We could try to use the downweighting trick that we use for the RSF, but for the SSF, 
#' the time between successive steps will also affect the point estimates of the parameters,
#' so the problem is a bit more tricky.
#' As a rule-of-thumb, I would downsample the data such that the autocorrelation in velocities
#' has decayed to something between 1% and 2%.
#' Say you had a sampling of 1 hour, and the SSF should be done at 3 hours given this
#' rule of thumb, then you could still use all data, by constructing 3 step data sets, each starting one hour apart, and
#' give each a weight of 1/3 in the likelihood. 
#' 
#' 

plot(variogram(cilla_telemetry), xlim = c(0, 10 * 3600))
abline(v = cilla_select$tau["velocity"]  * -log(0.01) / 3600, col = "blue")
abline(v = cilla_select$tau["velocity"]  * -log(0.02) / 3600, col = "red")
legend("bottomright", lty = 1, col = c("blue", "red"), legend = c("1%", "2%"), title = "Velocity\nautocorrelation", 
       bty = "n")

#' Now we resample to 3 hour intervals, with a tolerance of 15 minutes
step_duration <- 3
cilla_track <- track_resample(cilla_track, hours(step_duration), tolerance = minutes(15))

#' Look at the new sampling rate
summarize_sampling_rate(cilla_track)

#' So there is at least two observations that are more than 3 hours 15 minutes apart, so there should be at least two bursts: 
table(cilla_track$burst_)

#' If there are bursts, we may want to filter bursts with very few locations. For example, to calculate a turning angle, we need at least three locations. So we often will want to filter out bursts with at least 3 observations:
cilla_track <- filter_min_n_burst(cilla_track, 3)

#' Convert locations to steps. We will have fewer rows in the step data frame than in the track data frame because the final position is not a complete step.
ssf_cilla <- steps_by_burst(cilla_track)

#' We still have steps without a turning angle (the first step in a burst)
which(is.na(ssf_cilla$ta_))
ssf_cilla <- filter(ssf_cilla, !is.na(ta_))

#' ## Empirical distances and turning angles
par(mfrow = c(1, 2))
hist(ssf_cilla$sl_, breaks = 20, main = "", 
     xlab = "Distance (m)")
hist(ssf_cilla$ta_,  main="",breaks = seq(-pi, pi, len=11),
     xlab="Relative angle (radians)")

#' Create random steps. We typically get a warning that "Step-lengths or turning angles contained NA, which were removed", because of the missing turning angles at the start of a burst.
set.seed(2)
ssf_cilla <- steps_by_burst(cilla_track)
ssf_cilla <- random_steps(ssf_cilla, n_control = 200)

#' ## Sanity check: plot the choice set for a given step
my_step_id <- 3
ggplot(data = filter(ssf_cilla, step_id_ == my_step_id | (step_id_ %in% c(my_step_id - 1, my_step_id - 2) & case_ == 1)),
       aes(x = x2_, y = y2_)) + geom_point(aes(color = factor(step_id_))) + geom_point(data = filter(ssf_cilla, step_id_ %in% c(my_step_id, my_step_id - 1, my_step_id - 2) & case_ == 1), aes(x = x2_, y = y2_, color = factor(step_id_), size = 2))

#' ## Extract environmental covariates
#' I recommend to always use the option "both", which provides the environmental conditions at the start and the end of the step.
#' The condition at the end are what we use for selection, and the conditions at the start can be used
#' to modify e.g. turning angles and step length.
ssf_cilla <- extract_covariates(ssf_cilla, buffalo_env, where = "both")

#' Remove NA's
ssf_cilla <- ssf_cilla[complete.cases(ssf_cilla),]

#' ## A first model
m_1 <- fit_clogit(ssf_cilla, case_ ~ cos(ta_) + sl_ + log(sl_) + slope_end +
                    elev_end + mean_NDVI_end + strata(step_id_))
summary(m_1)

#' ## Model selection
#' Model selection is a vast topic. I recommend using only few models with ecological justification, rather than 
#' searching for the "best" model in a huge model space.
#' Here we just use stepwise backward selection based on AIC

m_2 <- fit_clogit(ssf_cilla, case_ ~ cos(ta_) + sl_ + log(sl_) + 
                    elev_end + mean_NDVI_end + strata(step_id_))
summary(m_2)

AIC(m_1$model)
AIC(m_2$model)
#' So we pick m2, because it has the lower AIC
m_cilla <- m_2

#' ## Interpretation
#'  - Map preference
#' - Response functions

#'## Map habitat preference ("Habitat map")
#' Caveat: Note that this habitat preference in general will not match the utilisation distribution of the animal (i.e. how much time it spends where). See below for more information.
#' The raster prediction function assumes that all environmental layers are
#' represented in one raster stack
#'
#' We need to exclude the parameters that are not related to the environment, i.e. the first three parameters related to turning angles and step length
coef(m_cilla)
remove_end <- function(x) {
  names(x) <- gsub("_end", "", names(x))
  x
}
habitat_map <- habitat_kernel(remove_end(coef(m_cilla)[-(1:3)]), buffalo_env, exp = FALSE)
raster::plot(habitat_map)
lines(ssf_cilla$x1_, ssf_cilla$y1_)

#' Now zoom in
raster::plot(crop(habitat_map, extent(cilla) * 2))
lines(ssf_cilla$x1_, ssf_cilla$y1_)

#' ## Simulating with the model
set.seed(2)
k1 <- amt:::redistribution_kernel(m_cilla, map = buffalo_env, 
                                  start = amt:::make_start.steps_xyt(ssf_cilla[1, ]),
                                  stochastic = TRUE, tolerance.outside = 0.2, as.raster = FALSE, 
                                  n.control = 1e3)
s1 <- amt:::simulate_path.redistribution_kernel(k1, n.steps = 500)

extent_tracks <- function(x, y) {
  df <- data.frame(na.omit(rbind(x[,c("x_", "y_")], y[,c("x_", "y_")])))
  raster::extent(c(range(df$x_), range(df$y_)))
}

elev_crop <- crop(buffalo_env[["elev"]], extent_tracks(s1, cilla_track) + 2000)
raster::plot(elev_crop)
lines(cilla)
lines(s1$x_, s1$y_, col = "red")


#' # Home ranging behaviour (Thanks, Chris!)
m_hr <- fit_clogit(ssf_cilla, case_ ~ cos(ta_) + sl_ + log(sl_) + 
                     elev_end + water_dist_end + x2_ + y2_ + I(x2_^2 + y2_^2) + strata(step_id_))
summary(m_hr)

k2 <- amt:::redistribution_kernel(m_hr, map = buffalo_env, start = amt:::make_start.steps_xyt(ssf_cilla[1, ]),
                                  stochastic = TRUE, tolerance.outside = 0.2, 
                                  as.raster = FALSE, n.control = 1e3)
set.seed(2)
s2 <- amt:::simulate_path.redistribution_kernel(k2, n.steps = 1000)

raster::plot(crop(buffalo_env[["elev"]], extent_tracks(s2, cilla_track) + 2000))
lines(cilla)
lines(s2$x_, s2$y_, col = "red")
