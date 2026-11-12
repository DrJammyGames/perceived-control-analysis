# =============================================================================
#  Perceived Control, Cultural Values and Mood
#  Mediation and moderation analyses
#
#  Analyst:  Dr. Yemaya Halbrook (drjammy.com)
#  Study:    Senan, S., Msetfi, R. M., El Keshky, M., & Halbrook, Y. (2019).
#            The relationship between perception of control and mood: The
#            intervening effect of cultural values in a Saudi Arabian sample.
#            PLOS ONE, 14(8), e0220509.
#            https://doi.org/10.1371/journal.pone.0220509
#
#  What this script does:
#    1. Loads the raw survey export
#    2. Scores every scale (with reverse scoring)
#    3. Screens the data: missing values, out-of-range scores, duplicate
#       submissions and careless (straight-lined) responding
#    4. Checks reliability
#    5. Produces descriptive statistics and correlations
#    6. Tests H1: cultural values as parallel mediators (6 models)
#    7. Tests H2: cultural values as moderators (6 models), with follow-up
#    8. Runs a sensitivity analysis without duplicate submissions
#    9. Saves every results table to /output as CSV
#
#  The mediation models reproduce PROCESS Model 4 (Hayes): ordinary least
#  squares regressions with percentile bootstrap confidence intervals. All
#  coefficients are unstandardised, matching the published paper.
#
#  Requirements: R 4.x and the readxl package (everything else is base R).
# =============================================================================


# ---- 0. Setup ---------------------------------------------------------------

library(readxl)

DATA_FILE  <- "data/Perception_of_control_study.xlsx"
OUTPUT_DIR <- "output"
N_BOOT     <- 5000   # bootstrap resamples, as in the original analysis
SEED       <- 2019   # fixed seed so bootstrap intervals are reproducible

dir.create(OUTPUT_DIR, showWarnings = FALSE)


# ---- 1. Load data -----------------------------------------------------------

raw <- read_excel(DATA_FILE, .name_repair = "minimal")

# Some column names in the export have trailing spaces (e.g. "BDI-4 "),
# so trim them before selecting columns by name.
names(raw) <- trimws(names(raw))

cat("Participants loaded:", nrow(raw), "\n")   # expected: 614


# ---- 2. Scoring -------------------------------------------------------------

# 2a. Sense of Control Measure (Lachman & Weaver, 1998) -----------------------
# Responses were exported as text labels, so convert them to 1-7.
# "Don't know" is the scale midpoint (4). It is matched with grepl() because
# the export uses a curly apostrophe that is easy to mistype.
code_agreement <- function(x) {
  x <- trimws(x)
  key <- c("Strongly disagree" = 1, "Somewhat disagree" = 2,
           "Disagree a little" = 3, "Agree a little"    = 5,
           "Somewhat agree"    = 6, "Strongly agree"    = 7)
  out <- unname(key[x])
  out[grepl("know", x, ignore.case = TRUE)] <- 4
  out
}

scm <- sapply(raw[, paste0("SCM-Q", 1:12)], code_agreement)
stopifnot(!anyNA(scm))   # stop if any label was not recognised

# Item key: mastery = items 3, 6, 8, 12; the other 8 items are constraints.
MASTERY_ITEMS    <- c(3, 6, 8, 12)
CONSTRAINT_ITEMS <- setdiff(1:12, MASTERY_ITEMS)

# Constraint items are reverse scored (8 - x on a 1-7 scale), so that higher
# scores mean MORE perceived control on every control variable.
mastery_items    <- scm[, MASTERY_ITEMS]
constraint_items <- 8 - scm[, CONSTRAINT_ITEMS]

# 2b. Individualism and Collectivism Scale, 16-item (Triandis & Gelfand, 1998)
# Already numeric (1-7). Four items per subscale, in questionnaire order.
ic <- as.data.frame(raw[, paste0("IC-", 1:16)])
IC_KEY <- list(HI = 1:4, VI = 5:8, HC = 9:12, VC = 13:16)

# 2c. Beck Depression Inventory --------------------------------------------
# 21 items scored 0-3. Item 19 has a follow-up yes/no question (19-B: "trying
# to lose weight on purpose") which is not part of the total score.
bdi_items <- raw[, c(paste0("BDI-", 1:18), "BDI-19-A", "BDI-20", "BDI-21")]

# 2d. Mood Disorder Questionnaire --------------------------------------------
# Only the first 13 yes/no symptom items are scored. The export repeats the
# names "MDQ-2" and "MDQ-3" for the MDQ's follow-up questions, so select the
# first 13 MDQ columns by position rather than by name.
mdq_cols  <- which(startsWith(names(raw), "MDQ-"))[1:13]
mdq_items <- as.data.frame(raw[, mdq_cols] == "Yes") * 1

# 2e. Build the analysis dataset ---------------------------------------------
dat <- data.frame(
  mastery     = rowSums(mastery_items),
  constraints = rowSums(constraint_items),
  HI  = rowSums(ic[, IC_KEY$HI]),
  VI  = rowSums(ic[, IC_KEY$VI]),
  HC  = rowSums(ic[, IC_KEY$HC]),
  VC  = rowSums(ic[, IC_KEY$VC]),
  BDI = rowSums(bdi_items),
  MDQ = rowSums(mdq_items)
)
dat$control <- dat$mastery + dat$constraints   # overall perceived control


# ---- 3. Data screening ------------------------------------------------------

# All cases are retained for the main analyses, to stay consistent with the
# published study. The flags created here are used in the sensitivity
# analysis (section 9) to check whether any of them change the results.

# 3a. Missing values ------------------------------------------------------------
scale_items <- cbind(scm, ic, bdi_items, mdq_items)
cat("Missing values across all scale items:", sum(is.na(scale_items)), "\n")

# 3b. Out-of-range scores ----------------------------------------------------
# Every scale score should fall inside its possible range.
possible_range <- list(mastery = c(4, 28), constraints = c(8, 56),
                       HI = c(4, 28), VI = c(4, 28), HC = c(4, 28), VC = c(4, 28),
                       BDI = c(0, 63), MDQ = c(0, 13))
out_of_range <- sapply(names(possible_range), function(v)
  sum(dat[[v]] < possible_range[[v]][1] | dat[[v]] > possible_range[[v]][2]))
print(out_of_range)   # expected: all 0

# 3c. Duplicate submissions --------------------------------------------------
# A row identical to an earlier row on every variable (including
# demographics) is almost certainly the same person submitting twice.
# duplicated() flags the second and later copies, keeping the first.
is_duplicate <- duplicated(raw)
cat("Exact duplicate submissions:", sum(is_duplicate), "\n")

# 3d. Careless responding (straight-lining) ----------------------------------
# Giving the identical answer to every item of a scale. On the Sense of
# Control Measure this is a strong warning sign: half the items are reverse
# scored, so identical answers contradict each other.
straight_lined <- function(items) apply(items, 1, function(r) length(unique(r)) == 1)
flat_scm <- straight_lined(raw[, paste0("SCM-Q", 1:12)])
flat_ic  <- straight_lined(ic)
cat("Straight-lined Sense of Control:", sum(flat_scm),
    "| Individualism-Collectivism:", sum(flat_ic),
    "| both:", sum(flat_scm & flat_ic), "\n")

screening <- data.frame(
  check = c("Missing values", "Out-of-range scores", "Exact duplicate submissions",
            "Straight-lined Sense of Control", "Straight-lined Individualism-Collectivism"),
  n = c(sum(is.na(scale_items)), sum(out_of_range), sum(is_duplicate),
        sum(flat_scm), sum(flat_ic))
)
write.csv(screening, file.path(OUTPUT_DIR, "screening.csv"), row.names = FALSE)


# ---- 4. Reliability ---------------------------------------------------------

# Cronbach's alpha, written out so the calculation is transparent:
# alpha = k / (k - 1) * (1 - sum of item variances / variance of the total)
cronbach_alpha <- function(items) {
  items <- as.matrix(items)
  k <- ncol(items)
  k / (k - 1) * (1 - sum(apply(items, 2, var)) / var(rowSums(items)))
}

reliability <- data.frame(
  scale = c("Perceived control", "Mastery", "Constraints",
            "HI", "VI", "HC", "VC", "BDI", "MDQ"),
  alpha = c(cronbach_alpha(cbind(mastery_items, constraint_items)),
            cronbach_alpha(mastery_items),
            cronbach_alpha(constraint_items),
            sapply(IC_KEY, function(i) cronbach_alpha(ic[, i])),
            cronbach_alpha(bdi_items),
            cronbach_alpha(mdq_items))
)
reliability$alpha <- round(reliability$alpha, 3)
print(reliability, row.names = FALSE)


# ---- 5. Descriptive statistics and correlations -----------------------------

vars <- c("control", "mastery", "constraints", "HI", "VI", "HC", "VC", "BDI", "MDQ")

descriptives <- data.frame(
  variable = vars,
  M   = round(sapply(dat[vars], mean), 2),
  SD  = round(sapply(dat[vars], sd), 2),
  min = sapply(dat[vars], min),
  max = sapply(dat[vars], max)
)
print(descriptives, row.names = FALSE)

# Clinical cut-offs used in the write-up
cat("BDI 16+ (moderate depression):", round(100 * mean(dat$BDI >= 16), 1), "%\n")
cat("MDQ 7+ (positive screen):     ", round(100 * mean(dat$MDQ >= 7), 1), "%\n")

correlations <- round(cor(dat[vars]), 2)
print(correlations)

write.csv(descriptives, file.path(OUTPUT_DIR, "descriptives.csv"), row.names = FALSE)
write.csv(reliability,  file.path(OUTPUT_DIR, "reliability.csv"),  row.names = FALSE)
write.csv(correlations, file.path(OUTPUT_DIR, "correlations.csv"))


# ---- 5b. Assumption checks --------------------------------------------------

# Checked on the outcome models (Y ~ X + all four cultural values), which are
# the regressions that carry the direct and b-path estimates.
# All helpers are written in base R so the calculations are visible.

# Skewness and excess kurtosis of each variable (|values| < 2 are usually
# treated as acceptable for regression with a sample this size).
skewness <- function(v) { z <- (v - mean(v)) / sd(v); mean(z^3) }
kurtosis <- function(v) { z <- (v - mean(v)) / sd(v); mean(z^4) - 3 }
distribution <- data.frame(variable = vars,
                           skew     = round(sapply(dat[vars], skewness), 2),
                           kurtosis = round(sapply(dat[vars], kurtosis), 2))
print(distribution, row.names = FALSE)

# Variance inflation factor: regress each predictor on the others;
# VIF = 1 / (1 - R squared). Values under 5 indicate no multicollinearity.
vif <- function(df, predictors) {
  sapply(predictors, function(p) {
    r2 <- summary(lm(reformulate(setdiff(predictors, p), response = p), data = df))$r.squared
    1 / (1 - r2)
  })
}

# Breusch-Pagan test for heteroscedasticity: regress the squared residuals on
# the predictors; n * R squared follows a chi-square with k degrees of freedom.
breusch_pagan <- function(fit) {
  df_bp <- data.frame(e2 = residuals(fit)^2, model.frame(fit)[-1])
  r2 <- summary(lm(e2 ~ ., data = df_bp))$r.squared
  k  <- ncol(df_bp) - 1
  stat <- nrow(df_bp) * r2
  c(BP = stat, df = k, p = pchisq(stat, k, lower.tail = FALSE))
}

pdf(file.path(OUTPUT_DIR, "diagnostic_plots.pdf"), width = 9, height = 4.5)
assumptions <- do.call(rbind, lapply(c("BDI", "MDQ"), function(y)
  do.call(rbind, lapply(c("control", "mastery", "constraints"), function(x) {
    preds <- c(x, "HI", "VI", "HC", "VC")
    fit <- lm(reformulate(preds, response = y), data = dat)
    std_res <- rstandard(fit)
    bp <- breusch_pagan(fit)
    
    # Residuals vs fitted (linearity, constant variance) and Q-Q (normality)
    par(mfrow = c(1, 2))
    plot(fitted(fit), residuals(fit), main = paste(x, "->", y), xlab = "Fitted", ylab = "Residual")
    abline(h = 0, lty = 2)
    qqnorm(std_res, main = "Normal Q-Q"); qqline(std_res)
    
    data.frame(
      model = paste(x, "->", y),
      shapiro_W = round(shapiro.test(residuals(fit))$statistic, 3),
      shapiro_p = signif(shapiro.test(residuals(fit))$p.value, 3),
      std_resid_over_3 = sum(abs(std_res) > 3),
      max_cooks_d = round(max(cooks.distance(fit)), 3),
      BP = round(bp["BP"], 2), BP_p = signif(bp["p"], 3),
      max_VIF = round(max(vif(dat, preds)), 2),
      row.names = NULL)
  }))))
dev.off()
print(assumptions, row.names = FALSE)

# VIFs for the moderation models (centred predictors + interaction terms).
# Interaction terms are stored as columns so vif() can treat them as predictors.
mod_vif <- do.call(rbind, lapply(c("control", "mastery", "constraints"), function(x) {
  df <- as.data.frame(lapply(dat, function(v) v - mean(v)))   # mean-centre
  for (m in c("HI", "VI", "HC", "VC")) df[[paste0(x, "_x_", m)]] <- df[[x]] * df[[m]]
  preds <- c(x, "HI", "VI", "HC", "VC", paste0(x, "_x_", c("HI", "VI", "HC", "VC")))
  v <- vif(df, preds)
  data.frame(predictor = x, max_VIF = round(max(v), 2), min_tolerance = round(min(1 / v), 2))
}))
print(mod_vif, row.names = FALSE)

write.csv(distribution, file.path(OUTPUT_DIR, "distribution.csv"), row.names = FALSE)
write.csv(assumptions,  file.path(OUTPUT_DIR, "assumptions.csv"),  row.names = FALSE)
write.csv(mod_vif,      file.path(OUTPUT_DIR, "moderation_vif.csv"), row.names = FALSE)


# ---- 6. H1: Parallel mediation (equivalent to PROCESS Model 4) --------------

# For predictor X, mediators M1..Mk and outcome Y:
#   a paths:  each Mi ~ X
#   b paths:  Y ~ X + M1 + ... + Mk    (the X coefficient is the direct effect)
#   indirect effect through Mi = a_i * b_i
# Confidence intervals come from a percentile bootstrap: resample participants
# with replacement, refit every model, and take the 2.5th and 97.5th
# percentiles of each indirect effect across resamples.

MEDIATORS <- c("HI", "VI", "HC", "VC")

# Fits all the paths once and returns the effects as a named vector.
# .lm.fit() is the fast core of lm(), which matters with 5,000 resamples.
mediation_effects <- function(df, x, y, mediators) {
  X1 <- cbind(1, df[[x]])
  a  <- sapply(mediators, function(m) .lm.fit(X1, df[[m]])$coefficients[2])
  X2 <- cbind(1, df[[x]], as.matrix(df[mediators]))
  b_fit <- .lm.fit(X2, df[[y]])$coefficients
  b  <- b_fit[3:(2 + length(mediators))]
  c(direct = b_fit[2], setNames(a * b, mediators))
}

run_mediation <- function(df, x, y, mediators = MEDIATORS, n_boot = N_BOOT, seed = SEED) {
  estimate <- mediation_effects(df, x, y, mediators)
  
  set.seed(seed)
  boots <- replicate(n_boot, {
    rows <- sample(nrow(df), replace = TRUE)
    mediation_effects(df[rows, ], x, y, mediators)
  })
  
  ci <- apply(boots, 1, quantile, probs = c(0.025, 0.975))
  data.frame(
    predictor = x, outcome = y,
    effect    = c("direct", paste("indirect via", mediators)),
    b         = round(estimate, 4),
    ci_lower  = round(ci[1, ], 4),
    ci_upper  = round(ci[2, ], 4),
    # An indirect effect is significant when its CI does not include zero.
    significant = ci[1, ] > 0 | ci[2, ] < 0,
    row.names = NULL
  )
}

mediation_results <- do.call(rbind, lapply(c("BDI", "MDQ"), function(y)
  do.call(rbind, lapply(c("control", "mastery", "constraints"), function(x)
    run_mediation(dat, x, y)))))

print(mediation_results, row.names = FALSE)
write.csv(mediation_results, file.path(OUTPUT_DIR, "mediation.csv"), row.names = FALSE)


# ---- 6b. Path diagrams ------------------------------------------------------

# Draws one parallel mediation model: predictor on the left, the four
# mediators stacked in the middle, outcome on the right. Arrows are solid
# when the path is significant (p < .05) and dashed when it is not.
# Labels show unstandardised coefficients (* p < .05, ** p < .01,
# *** p < .001); the bottom arrow shows the direct
# effect c' with the total effect c in brackets.
LABELS <- c(control = "Perceived\ncontrol", mastery = "Perceived\nmastery",
            constraints = "Perceived\nconstraints", BDI = "Depression\n(BDI)",
            MDQ = "Bipolar\nsymptoms (MDQ)", HI = "HI", VI = "VI", HC = "HC", VC = "VC")
COL_SIG <- "#2A6590"; COL_NS <- "#8C82A6"; COL_BOX <- "#E6F1F9"; COL_TEXT <- "#1A1528"

fmt_b <- function(est, p) {
  stars <- if (p < .001) "***" else if (p < .01) "**" else if (p < .05) "*" else ""
  num <- sprintf("%.2f", est)   # APA: b can exceed 1, so keep the leading zero
  paste0(sub("^-", "\u2212", num), stars)                    # true minus sign, not a hyphen
}

draw_mediation <- function(df, x, y, panel_label, mediators = MEDIATORS) {
  coef_p <- function(f, term) summary(f)$coefficients[term, c(1, 4)]
  a_fits <- lapply(mediators, function(m) lm(reformulate(x, m), df))
  b_fit  <- lm(reformulate(c(x, mediators), y), df)
  c_tot  <- coef_p(lm(reformulate(x, y), df), x)
  c_dir  <- coef_p(b_fit, x)
  
  plot.new(); plot.window(xlim = c(0, 10), ylim = c(-1.6, 4.6))
  box_at <- function(cx, cy, label, w = 1.1, h = 0.42) {
    rect(cx - w, cy - h, cx + w, cy + h, col = COL_BOX, border = COL_SIG, lwd = 1.2)
    text(cx, cy, label, cex = 0.8, col = COL_TEXT)
  }
  arrow_lab <- function(x0, y0, x1, y1, est, p, pos) {
    sig <- p < .05
    arrows(x0, y0, x1, y1, length = 0.08, lwd = 1.3,
           lty = if (sig) 1 else 2, col = if (sig) COL_SIG else COL_NS)
    # White box behind the label so the arrow line never runs through it
    lab <- fmt_b(est, p); lx <- x0 + (x1 - x0) * pos; ly <- y0 + (y1 - y0) * pos
    w <- strwidth(lab, cex = 0.72) / 2 + 0.06; h <- strheight(lab, cex = 0.72) / 2 + 0.06
    rect(lx - w, ly - h, lx + w, ly + h, col = "white", border = NA)
    text(lx, ly, lab, cex = 0.72, col = if (sig) COL_SIG else COL_NS)
  }
  
  X <- c(1.2, 1.5); Y <- c(8.8, 1.5)
  my <- c(4, 2.65, 1.3, -0.05)   # mediator heights, top to bottom
  for (i in seq_along(mediators)) {
    a <- coef_p(a_fits[[i]], x); b <- coef_p(b_fit, mediators[i])
    arrow_lab(X[1] + 1.1, X[2], 5 - 0.65, my[i], a[1], a[2], 0.55)
    arrow_lab(5 + 0.65, my[i], Y[1] - 1.1, Y[2], b[1], b[2], 0.45)
    box_at(5, my[i], LABELS[mediators[i]], w = 0.65, h = 0.32)
  }
  # Direct effect c' (total effect c in brackets) along the bottom
  arrows(X[1], X[2] - 0.42, X[1], -1.25, length = 0, lwd = 1.3, col = COL_SIG)
  arrows(X[1], -1.25, Y[1], -1.25, length = 0, lwd = 1.3, col = COL_SIG)
  arrows(Y[1], -1.25, Y[1], Y[2] - 0.42, length = 0.08, lwd = 1.3, col = COL_SIG)
  text(5, -1.05, paste0("c' = ", fmt_b(c_dir[1], c_dir[2]),
                        "  (c = ", fmt_b(c_tot[1], c_tot[2]), ")"), cex = 0.75, col = COL_SIG)
  box_at(X[1], X[2], LABELS[x]); box_at(Y[1], Y[2], LABELS[y])
  text(0, 4.5, panel_label, font = 2, cex = 1, adj = 0, col = COL_TEXT)
}

# One figure per outcome, three panels each, saved at print resolution.
for (y in c("BDI", "MDQ")) {
  png(file.path(OUTPUT_DIR, paste0("figure_paths_", y, ".png")),
      width = 6.5, height = 9, units = "in", res = 300)
  par(mfrow = c(3, 1), mar = c(0.5, 0.5, 0.5, 0.5))
  draw_mediation(dat, "control",     y, "A")
  draw_mediation(dat, "mastery",     y, "B")
  draw_mediation(dat, "constraints", y, "C")
  dev.off()
}


# ---- 7. H2: Moderation (block-wise regression) ------------------------------

# Predictors and moderators are mean-centred before the interaction terms are
# built. This keeps the interaction terms from being highly correlated with
# their components (the multicollinearity issue noted in the paper).
centre <- function(v) v - mean(v)
dat_c  <- as.data.frame(lapply(dat, centre))

run_moderation <- function(df, x, y, moderators = MEDIATORS) {
  block1 <- reformulate(c(x, moderators), response = y)
  block2 <- reformulate(c(x, moderators, paste0(x, ":", moderators)), response = y)
  m1 <- lm(block1, data = df)
  m2 <- lm(block2, data = df)
  
  change <- anova(m1, m2)   # does adding the interactions improve the model?
  terms  <- paste0(x, ":", moderators)
  ci     <- confint(m2)[terms, ]
  
  data.frame(
    predictor = x, outcome = y,
    interaction = terms,
    b        = round(coef(m2)[terms], 4),
    ci_lower = round(ci[, 1], 4),
    ci_upper = round(ci[, 2], 4),
    p        = round(summary(m2)$coefficients[terms, 4], 3),
    block2_delta_R2 = round(summary(m2)$r.squared - summary(m1)$r.squared, 3),
    block2_F_p      = round(change$`Pr(>F)`[2], 3),
    row.names = NULL
  )
}

moderation_results <- do.call(rbind, lapply(c("BDI", "MDQ"), function(y)
  do.call(rbind, lapply(c("control", "mastery", "constraints"), function(x)
    run_moderation(dat_c, x, y)))))

print(moderation_results, row.names = FALSE)
write.csv(moderation_results, file.path(OUTPUT_DIR, "moderation.csv"), row.names = FALSE)


# ---- 8. Follow-up: constraints x HI on MDQ ----------------------------------

# Simple slopes: the effect of perceived constraints on MDQ at low, middle
# and high levels of horizontal individualism. Following Hayes (2022), these
# are the 16th, 50th and 84th percentiles of HI rather than -1/+1 SD: HI is
# heavily skewed (35% of participants score the maximum of 28), so +1 SD
# (29.0) would lie outside the possible range of the scale.
# Because the variables are centred, shifting HI so that the chosen value
# sits at zero makes the constraints coefficient equal to the slope there.
hi_points <- quantile(dat$HI, c(.16, .50, .84))
slopes <- do.call(rbind, lapply(seq_along(hi_points), function(i) {
  df <- dat_c
  df$HI <- df$HI - (hi_points[i] - mean(dat$HI))
  fit <- lm(MDQ ~ constraints * HI + VI + HC + VC, data = df)
  est <- summary(fit)$coefficients["constraints", ]
  ci  <- confint(fit)["constraints", ]
  data.frame(HI_percentile = names(hi_points)[i], HI_value = hi_points[i],
             slope = round(est[1], 4), SE = round(est[2], 4),
             t = round(est[3], 2), p = round(est[4], 3),
             ci_lower = round(ci[1], 4), ci_upper = round(ci[2], 4),
             row.names = NULL)
}))
print(slopes, row.names = FALSE)

# The published follow-up instead correlated constraints with MDQ within HI
# score bands. The lowest band (HI = 4-5) contains very few participants,
# so report its size alongside the correlation.
low_hi <- dat[dat$HI <= 5, ]
cat("Low HI (4-5): n =", nrow(low_hi),
    ", r =", round(cor(low_hi$constraints, low_hi$MDQ), 2), "\n")

write.csv(slopes, file.path(OUTPUT_DIR, "simple_slopes.csv"), row.names = FALSE)


# ---- 9. Sensitivity analysis: without duplicate submissions ---------------

# Re-runs the mediation models and the key moderation model with the
# duplicate submissions removed, and compares them with the main results.
# If the conclusions are the same, the duplicates do not affect the findings.
dat_unique <- dat[!is_duplicate, ]
cat("Sensitivity sample: N =", nrow(dat_unique), "\n")

mediation_unique <- do.call(rbind, lapply(c("BDI", "MDQ"), function(y)
  do.call(rbind, lapply(c("control", "mastery", "constraints"), function(x)
    run_mediation(dat_unique, x, y)))))

# Side-by-side comparison. Effects are listed in the same order in both
# tables, so the rows line up directly.
sensitivity <- data.frame(
  mediation_results[, c("predictor", "outcome", "effect")],
  b_all         = mediation_results$b,
  sig_all       = mediation_results$significant,
  b_unique      = mediation_unique$b,
  ci_lower_uniq = mediation_unique$ci_lower,
  ci_upper_uniq = mediation_unique$ci_upper,
  sig_unique    = mediation_unique$significant
)
sensitivity$changed <- sensitivity$sig_all != sensitivity$sig_unique
print(sensitivity, row.names = FALSE)
cat("Effects that changed significance:", sum(sensitivity$changed), "\n")

# Key moderation finding without duplicates
moderation_unique <- run_moderation(as.data.frame(lapply(dat_unique, centre)),
                                    "constraints", "MDQ")
print(moderation_unique[moderation_unique$interaction == "constraints:HI",
                        c("interaction", "b", "ci_lower", "ci_upper", "p")],
      row.names = FALSE)

write.csv(sensitivity, file.path(OUTPUT_DIR, "sensitivity_mediation.csv"), row.names = FALSE)
write.csv(moderation_unique, file.path(OUTPUT_DIR, "sensitivity_moderation.csv"), row.names = FALSE)


# ---- 10. Session info --------------------------------------------------------

# Recorded so anyone re-running the analysis knows the exact R version.
writeLines(capture.output(sessionInfo()), file.path(OUTPUT_DIR, "session_info.txt"))