# ==============================================================================
# 03_bandwidths.R : IK bandwidths for every RD outcome
#
# Bandwidths use Imbens-Kalyanaraman (2009) optimal bandwidth, as used in 
# Meyersson (2014) paper via author's Stata rdob.ado
#
# IKBandwidth is taken from the rdd (v0.57) package by Drew Dimmery available on
# Github. 
#  Link: github.com/cran/rdd/blob/master/R/IKbandwidth.R
#  
# rdd was archived on CRAN, 2025, so we include the function here, instead of 
# installing the package. 
# 
# Changes from the original to match rdob.ado:
#   1. Kernel constant C_K = 3.4375 (as used in rdob.ado), instead of 3.4375
#   2. Observations at X = c are assigned to the right side in the pilot
#      window and the medians 
# 
# Change 1 shifts every bandwidth and moves one town out of the men's window
# to match the paper (N=1341).
#
# rdd drops rows with missing Y; but rdob.ado does not. No outcome is missing in
# our analysis sample, so the two agree.
#
#
# h_stored holds full-precision output of this file
# ==============================================================================

# 1. Samples -------------------------------------------------------------------

df   <- readRDS(file.path(path_derived, "analysis.rds"))
df11 <- readRDS(file.path(path_derived, "analysis_2011.rds"))

outcomes_2000 <- c("hischshr1520f", "hischshr1520m", "studshr1530f", 
                   "studshr1530m")

outcomes_2011 <- c("ahischshr15_19f", "ahischshr15_19m", "ahischshr15_29f",
                   "ahischshr15_29m", "nevm15_19f", "nevm15_19m")


# 2. Imported IKBandwidth() ----------------------------------------------------

# Imported from rdd v0.57, and modified. 
# Author: Drew Dimmmy
# Based on Imbens, Guido and Karthik Kalyanaraman. (2009):
#   "Optimal Bandwidth Choice for the regression discontinuity estimator"

IKbandwidth <-function (X,Y,cutpoint=NULL,verbose=FALSE,kernel="triangular") {
  #Implementation of Imbens-Kalyanaraman optimal bandwidth
  # for regression discontinuity
  sub<-complete.cases(X)&complete.cases(Y)
  X <- X[sub]
  Y <- Y[sub]
  Nx<-length(X)
  Ny<-length(Y)
  if(Nx!=Ny)
    stop("Running and outcome variable must be of equal length")
  if(is.null(cutpoint)) {
    cutpoint<-0
    if(verbose) cat("Using default cutpoint of zero.\n")
  } else {
    if(! (typeof(cutpoint) %in% c("integer","double")))
      stop("Cutpoint must be of a numeric type")
  }
  
  #Now we should be ready to start
  #Pilot bandwidth
  h1<-1.84*sd(X)*Nx^(-1/5)
  if (verbose) cat("Initial Pilot Bandwidth: ", sprintf("%.3f", h1), "\n")
  left<-X>=(cutpoint-h1) & X<cutpoint
  right<-X>=cutpoint & X<=(cutpoint+h1) # changed
  Nl<-sum(left)
  Nr<-sum(right)
  Ybarl<-mean(Y[left])
  Ybarr<-mean(Y[right])
  fbarx<-(Nl+Nr)/(2*Nx*h1)
  varY<-(sum((Y[left]-Ybarl)^2)+sum((Y[right]-Ybarr)^2))/(Nl+Nr)
  medXl<-median(X[X<cutpoint]) # changed
  medXr<-median(X[X>=cutpoint])
  Nl<-sum(X<cutpoint)
  Nr<-sum(X>=cutpoint)
  cX<-X-cutpoint
  if(sum(X[left]>medXl)==0 | sum(X[right]<medXr)==0)
    stop("Insufficient data in vicinity of the cutpoint to calculate bandwidth."
         )
  #Model a cubic within the pilot bandwidth
  mod<-lm(Y~I(X>=cutpoint)+poly(cX,3,raw=T),subset=(X>=medXl&X<=medXr))
  m3<-6*coef(mod)[5]
  #New bandwidth estimate
  h2l<-3.56*(Nl^(-1/7))*(varY/(fbarx*max(m3^2,0.01)))^(1/7)
  h2r<-3.56*(Nr^(-1/7))*(varY/(fbarx*max(m3^2,0.01)))^(1/7)
  if (verbose) {
    cat("Second Stage Bandwidth Estimate - Left: ", sprintf("%.3f", h2l), "\n")
    cat("Second Stage Bandwidth Estimate - Right: ", sprintf("%.3f", h2r), "\n")
  }
  left<-(X>=(cutpoint-h2l)) & (X<cutpoint)
  right<-(X>=cutpoint) & (X<= (cutpoint+h2r))
  Nl<-sum(left)
  Nr<-sum(right)
  if(Nl==0 | Nr==0)
    stop("Insufficient data in vicinity of the cutpoint to calculate bandwidth."
         )
  #Estimate quadratics for curvature estimation
  mod<-lm(Y~poly(cX,2,raw=T),subset=right)
  m2r<-2*coef(mod)[3]
  mod<-lm(Y~poly(cX,2,raw=T),subset=left)
  m2l<-2*coef(mod)[3]
  rl<-720*varY/(Nl*(h2l^4))
  rr<-720*varY/(Nr*(h2r^4))
  #Which kernel are we using?
  # Method for finding these available in I--K p. 6
  if(kernel=="triangular") {
    ck<-3.4375 # changed
  } else if (kernel=="rectangular") {
    ck<-5.40384
  } else if(kernel=="epanechnikov") {
    ck<-3.1999
  } else if(kernel=="quartic" | kernel=="biweight") {
    ck<-3.65362
  } else if(kernel=="triweight") {
    ck<-4.06065
  } else if(kernel=="tricube") {
    ck<-3.68765
  } else if(kernel=="gaussian") {
    ck<-1.25864
  } else if(kernel=="cosine") {
    ck<-3.25869
  } else {
    stop("Unrecognized kernel.") 
  }
  #And there's our optimal bandwidth
  optbw<-ck*(2*varY/(fbarx*((m2r-m2l)^2+rr+rl)))^(1/5)*(Nx^(-1/5))
  left<-(X>=(cutpoint-optbw)) & (X<cutpoint)
  right<-(X>=cutpoint) & (X<= (cutpoint+optbw))
  if(sum(left)==0 | sum(right)==0)
    stop("Insufficient data in the calculated bandwidth.")
  names(optbw)<-NULL
  if(verbose) cat("Imbens-Kalyanamaran Optimal Bandwidth: ",
                  sprintf("%.3f",optbw),"\n")
  return(optbw)
}


# 3. Compute IK Bandwidths, ----------------------------------------------------


ik <- function(y, data) IKbandwidth(data$iwm94, data[[y]], cutpoint = 0)

h_ik <- c(
  sapply(outcomes_2000, ik, data = df),
  sapply(outcomes_2011, ik, data = df11)
)

# 4. Stored Values to check against --------------------------------------------

# Full precision outputs of function above

h_stored <- c(
  hischshr1520f   = 0.2397583971,
  hischshr1520m   = 0.3229937172,
  studshr1530f    = 0.2047881527,
  studshr1530m    = 0.2297030193,
  ahischshr15_19f = 0.2385141899,
  ahischshr15_19m = 0.1993020441,
  ahischshr15_29f = 0.2625644998,
  ahischshr15_29m = 0.2270204918,
  nevm15_19f      = 0.2801012688,
  nevm15_19m      = 0.2710378901
)

# 5. Consistency Check ---------------------------------------------------------

stopifnot(isTRUE(all.equal(h_ik, h_stored, tolerance = 1e-8)))
message("IK Bandwidths stored")

