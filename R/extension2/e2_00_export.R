# ==============================================================================
# e2_00_export.R : Export analysis sample for index construction
#===============================================================================

# 1. Load config and data ------------------------------------------------------

# Read shared config
cfg <- jsonlite::read_json(here::here("config", "extension2.json"),
                           simplifyVector = TRUE)

# Bring in analysis sample
df <- readRDS(file.path(path_derived, "analysis.rds"))


# 2. Add permanent town_id -----------------------------------------------------


# We join files on this id 
df <- df %>% mutate(mun_id = row_number())


# 3. Select columns ------------------------------------------------------------

design <- c(cfg$id, cfg$treatment, cfg$running, cfg$fe, cfg$cluster)

targets <- sapply(cfg$targets, function(t) t$var)

vars <- unique(c(design,
                 unlist(cfg$outcomes),
                 cfg$rd_controls,
                 cfg$features_strict,
                 cfg$features_2000_demographics,
                 targets))

# Check if all in data
stopifnot(all(vars %in% names(df)))

out <- df %>% select(all_of(vars))

# 4. Checks --------------------------------------------------------------------

# Verify dataset
stopifnot(nrow(out) == 2629,
          sum(out[[cfg$treatment]]) == 315,
          !anyDuplicated(out[[cfg$id]]))

# How many missing values
na_counts <- colSums(is.na(out))
print(na_counts[na_counts > 0])


# 5. Write ---------------------------------------------------------------------

# Save as .csv so python can use it

write.csv(out, here::here(cfg$paths$input), row.names = FALSE)

message("e2_00_export done: ", nrow(out), " towns, ", ncol(out),
        " columns written to ", cfg$paths$input)