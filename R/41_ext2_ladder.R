# ==============================================================================

# 41_ext2_ladder.R: We build the ladder model and test it.

# 2 sets of RD estimates using Table 2 specification. (local linear, Meyersson
# controls, province FE, province clustering, one bandwidth per gender)


# Levels / Decomposition

# ==============================================================================

# 1. Load ----------------------------------------------------------------------

df_ext2 <- readRDS(file.path(path_derived, "ext2.rds"))
h_ext2  <- readRDS(file.path(path_derived, "ext2_h.rds"))$h2000

mults <- c(1, 0.5, 2) # Multiples of bandwidth

# Var names
levels_vars <- c("pi1", "pi2", "pi3", "piv2", "piv3")
log_vars    <- c("lp1", "lp2", "lp3", "lpi3")

# 2. Estimation Grid -----------------------------------------------------------

# One row per outcome x gender x bandwidth multiple.
# Log outcomes run on the chain sample for that gender.

grid <- bind_rows(
  expand_grid(stub = levels_vars, block = "levels"),
  expand_grid(stub = log_vars, block ="decomposition")
) %>%
  expand_grid(g = c("f", "m"), mult = mults) %>%
  mutate(
    y = paste0(stub, g),
    h = mult * h_ext3[g]
  )

run_one <- function(stub, block, g, mult, y, h) {
  d <- if (block == "decomposition") {
    filter(df_ext3, .data[[paste0("chain_ok", g)]])
  } else {
    df_ext3
  }
  # 
  rd_est(d, y = y, h = h, p = 1, controls = controls, fe = TRUE) %>%
    mutate(stub = stub, block = block, g = g, mult = mult)
}

ladder <- grid %>%
  pmap_dfr(run_one) %>%
  relocate(block, stub, g, mult)

# 3. Addding-up Check ----------------------------------------------------------

# tau[lp1] + tau[lp2] + tau[lp3] must equal tau[lpi3] for every gender and h


adding_up <- ladder %>%
  filter(block == "decomposition") %>%
  select(g, mult, stub, coef) %>%
  pivot_wider(names_from = stub, values_from = coef) %>%
  mutate(sum_steps = lp1 + lp2 + lp3,
         gap       = sum_steps - lpi3)

print(adding_up)
stopifnot(all(abs(adding_up$gap) < 1e-10))

shares <- adding_up %>%
  filter(mult == 1) %>%
  transmute(g,
            share_lp1 = lp1 / lpi3,
            share_lp2 = lp2 / lpi3,
            share_lp3 = lp3 / lpi3)

print(shares)

# 4. Tests ---------------------------------------------------------------------

chain <- function(g) filter(df_ext3, .data[[paste0("chain_ok", g)]])

# H1: one-sided p-value for tau[lp2] > 0 (main bandwidth)
h1 <- ladder %>%
  filter(block == "decomposition", stub == "lp2", mult == 1) %>%
  transmute(test = "H1: tau[lp2] > 0", g,
            diff = coef, se_diff = se,
            p    = pnorm(coef / se, lower.tail = FALSE))

# Step comparison within gender: tau[lp2] = tau[lp3]

step_cmp <- map_dfr(c("f", "m"), function(g) {
  sur_test(chain(g),
           yf = paste0("lp2", g), ym = paste0("lp3", g),
           hf = h_ext3[[g]],      hm = h_ext3[[g]],
           p = 1, controls = controls, fe = TRUE) %>%
    transmute(test = "tau[lp2] = tau[lp3]", g,
              diff, se_diff, p = p_sur)
})


# Women vs men at each step 
both_ok <- filter(df_ext3, chain_okf, chain_okm)

gender_cmp <- map_dfr(c("lp1", "lp2", "lp3", "lpi3"), function(s) {
  sur_test(both_ok,
           yf = paste0(s, "f"), ym = paste0(s, "m"),
           hf = h_ext3[["f"]],  hm = h_ext3[["m"]],
           p = 1, controls = controls, fe = TRUE) %>%
    transmute(test = paste0("women = men: ", s), g = "f-m",
              diff, se_diff, p = p_sur)
})

tests <- bind_rows(h1, step_cmp, gender_cmp)
print(tests)


# 7. Save ----------------------------------------------------------------------


write_csv(ladder, file.path(path_extension2, "ext2_ladder_raw.csv"))
write_csv(tests,  file.path(path_extension2, "ext2_ladder_tests_raw.csv"))

message("41_ext2_ladder done: ", nrow(ladder), " RD estimates, ",
        nrow(tests), " tests. Adding-up holds to 1e-10.")

# 8. Word save -----------------------------------------------------------------

# Columns: Women, Men, Women - Men (main bandwidth h only)
# Panel A: stage completion (levels), full window
# Panel B: decomposition of ln(pi3), chain_ok towns
# Panel C: tests

f3    <- function(x) sprintf("%.3f", x)
paren <- function(x) paste0("(", f3(x), ")")
stars <- function(p) case_when(p < 0.01 ~ "***", p < 0.05 ~ "**",
                               p < 0.10 ~ "*",   TRUE     ~ "")

pi_ <- "π"                                  
sub <- c("1" = "₁", "2" = "₂", "3" = "₃")

lab <- c(
  pi1  = paste0("Primary (", pi_, sub["1"], ")"),
  pi2  = paste0("Middle, general (", pi_, sub["2"], ")"),
  pi3  = paste0("High, general (", pi_, sub["3"], ")"),
  piv2 = "Middle, vocational",
  piv3 = "High, vocational",
  lp1  = paste0("ln ", pi_, sub["1"], " (primary)"),
  lp2  = paste0("ln p", sub["2"], " (primary to middle)"),
  lp3  = paste0("ln p", sub["3"], " (middle to high)"),
  lpi3 = paste0("ln ", pi_, sub["3"], " (total)")
)

main <- ladder %>% filter(mult == 1)

est_rows <- function(s, diff_tbl = NULL, show_mean = FALSE) {
  w <- filter(main, stub == s, g == "f")
  m <- filter(main, stub == s, g == "m")
  d <- if (!is.null(diff_tbl)) filter(diff_tbl, test == paste0("women = men: ", s))
  has_d <- length(d) && nrow(d)
  out <- rbind(
    c(paste0(f3(w$coef), stars(w$pval)),
      paste0(f3(m$coef), stars(m$pval)),
      if (has_d) paste0(f3(d$diff), stars(d$p)) else ""),
    c(paren(w$se), paren(m$se),
      if (has_d) paren(d$se_diff) else "")
  )
  rn <- c(lab[[s]], " ")
  if (show_mean) {
    out <- rbind(out, c(sprintf("[%.3f]", w$mean_y), sprintf("[%.3f]", m$mean_y), ""))
    rn  <- c(rn, "  ")
  }
  rownames(out) <- rn
  out
}

n_row <- function(block_name, s) {
  w <- filter(main, block == block_name, stub == s, g == "f")$n
  m <- filter(main, block == block_name, stub == s, g == "m")$n
  rbind(Observations = c(as.character(w), as.character(m), ""))
}

title_row <- function(txt) matrix("", 1, 3, dimnames = list(txt, NULL))

fp     <- function(p) if_else(p < 0.001, "<0.001", f3(p))
p_test <- function(t, gg) fp(filter(tests, test == t, g == gg)$p)

header <- rbind(
  Bandwidth  = c(f3(h_ext3[["f"]]), f3(h_ext3[["m"]]), "")
)

share_row <- rbind(
  `Share of total from step 2` = c(sprintf("%.2f", shares$share_lp2[shares$g == "f"]),
                                   "n.a.", "")
)

tab_mat <- rbind(
  header,
  title_row("Panel A: Stage completion by 2000 (levels)"),
  do.call(rbind, lapply(c("pi1", "pi2", "pi3", "piv2", "piv3"), est_rows,
                        show_mean = TRUE)),
  n_row("levels", "pi3"),
  title_row(paste0("Panel B: Decomposition of ln ", pi_, sub["3"])),
  do.call(rbind, lapply(c("lp1", "lp2", "lp3", "lpi3"), est_rows,
                        diff_tbl = tests)),
  share_row,
  n_row("decomposition", "lpi3"),
  title_row("Panel C: Tests (p-values)"),
  rbind(
    `H1: step 2 effect > 0 (one-sided)` =
      c(p_test("H1: tau[lp2] > 0", "f"), p_test("H1: tau[lp2] > 0", "m"), ""),
    `Step 2 = step 3` =
      c(p_test("tau[lp2] = tau[lp3]", "f"), p_test("tau[lp2] = tau[lp3]", "m"), "")
  )
)

colnames(tab_mat) <- c("Women", "Men", "Women - Men")

ext3_tab <- as_tibble(tab_mat) %>%
  mutate(Variable = trimws(rownames(tab_mat)), .before = 1)

save_docx(path = path_extension2, ext3_tab, "ext3_ladder.docx",
          bold_rows = which(startsWith(ext3_tab$Variable, "Panel")))


