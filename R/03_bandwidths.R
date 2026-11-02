# ==============================================================================
# 03_bandwidths.R : IK bandwidths for every RD outcome
#
# Bandwidths follow the paper: Imbens-Kalyanaraman, as implemented in
# rdd::IKbandwidth(). rdd was archived on CRAN in July 2025; so this file tries
# install it from the CRAN archive.
#
# If rdd is unavailable, we fall back to stored values. These were computed with
# rdd::IKbandwidth() and stored at full precision, so the fallback
# gives identical results. The paper reports them rounded to three decimals,
# which is not precise enough for reproduction.
# ==============================================================================

# 1. Samples -------------------------------------------------------------------

df   <- readRDS(file.path(path_derived, "analysis.rds"))
df11 <- readRDS(file.path(path_derived, "analysis_2011.rds"))

outcomes_2000 <- c("hischshr1520f", "hischshr1520m", "studshr1530f", 
                   "studshr1530m")

outcomes_2011 <- c("ahischshr15_19f", "ahischshr15_19m", "ahischshr15_29f",
                   "ahischshr15_29m", "nevm15_19f", "nevm15_19m")


# 2. Stored fallback values (full precision, from rdd::IKbandwidth) ------------

h_stored <- c(
  hischshr1520f   = 0.2397611870,
  hischshr1520m   = 0.3229974756,
  studshr1530f    = 0.2047905357,
  studshr1530m    = 0.2297056922,
  ahischshr15_19f = 0.2385169654,
  ahischshr15_19m = 0.1993043632,
  ahischshr15_29f = 0.2625675551,
  ahischshr15_29m = 0.2270231335,
  nevm15_19f      = 0.2801045282,
  nevm15_19m      = 0.2710410439
)

# 3. Try to install rdd (archived on CRAN), then compute or fall back ----------

if (!requireNamespace("rdd", quietly = TRUE)) {
  tryCatch({
    install.packages(c("AER", "sandwich", "lmtest", "Formula"),
                     repos = "https://cloud.r-project.org")
    install.packages(
      "https://cran.r-project.org/src/contrib/Archive/rdd/rdd_0.57.tar.gz",
      repos = NULL, type = "source"
    )
  }, error = function(e) {
    message("03_bandwidths: could not install rdd (", conditionMessage(e), ")")
  })
}


if (requireNamespace("rdd", quietly = TRUE)) {
  
  ik <- function(y, data) rdd::IKbandwidth(data$iwm94, data[[y]], cutpoint = 0)
  
  h_ik <- c(
    sapply(outcomes_2000, ik, data = df),
    sapply(outcomes_2011, ik, data = df11)
  )
  message("03_bandwidths: computed with rdd::IKbandwidth()")
  
} else {
  
  h_ik <- h_stored
  message("03_bandwidths: rdd not available, using stored bandwidths")
}

# 4. Consistency Check ---------------------------------------------------------

# Check if rdd is producing a different set of bandwidths
stopifnot(isTRUE(all.equal(h_ik, h_stored, tolerance = 1e-8)))
print(round(h_ik, 3))

