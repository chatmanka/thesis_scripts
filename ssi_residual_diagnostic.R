# =============================================================================
# ssi_residual_diagnostic.R
# =============================================================================
# Thesis      : Closing the Governance Gap — Shellfish Mariculture in South Carolina
# Author      : Kate Chatman | MPA/EVSS Dual Degree
#               College of Charleston / S.C. Sea Grant Consortium
# Component   : Validation
# Used in     : Ch. 5 Validation / Limitations
# Status      : FINAL
#
# PURPOSE
#   Tests two questions arising from La Peyre et al. (2009): whether a Dermo
#   (Perkinsus marinus) disease signal causes the SSI to systematically under-
#   predict mortality at high-salinity sites, and how alternative outcome
#   variables compare.
#
# INPUTS      : Validation panel; SSI results; mortality records
# OUTPUTS     : Residual diagnostic tables and figures
# RUN AFTER   : ssi_biological_validation.R
# =============================================================================
# NOTE: This is the archived thesis version. Logic is unchanged from the script
# that produced the reported results; only this header has been added.
# =============================================================================

# =============================================================================
# SSI RESIDUAL DIAGNOSTIC — DISEASE-SIGNAL TEST & OUTCOME-VARIABLE COMPARISON
# Kate Chatman | MPA/EVSS | College of Charleston / SC Sea Grant Consortium
# Date: July 2026
#
# PURPOSE: Two questions arising from La Peyre et al. (2009).
#
#   Q1 (disease signal): Perkinsus marinus (Dermo) is suppressed at low salinity
#       and favored at high salinity, running OPPOSITE in sign to the acute
#       freshwater stress the SSI scores. If high-SSI sites carry elevated
#       chronic disease burden, the SSI should systematically UNDER-predict
#       mortality there — i.e. positive residuals concentrated at high SSI.
#       Tested via residual means by class/tercile and a curvature (quadratic) term.
#
#   Q2 (outcome variable): The SSI is designed to capture extreme variability
#       and catastrophic loss, not typical-year mortality. Compare model fit
#       across four outcome variables: median (typical year), mean, max (worst
#       year), and SD (volatility) of site mortality.
#
# NOTE ON Q1: OLS residuals are orthogonal to the predictor by construction, so
#       a simple resid~ssi correlation is a null test by design. Systematic
#       under-prediction at one end of the range must be detected as CURVATURE
#       or as a difference in residual means BETWEEN GROUPS. Both are used here.
#
# INPUT:
#   - ssi_mortality_validation_results.csv   (output of ssi_biological_validation.R)
#
# OUTPUTS:
#   - ch5_residual_diagnostic.png
#   - ch5_outcome_variable_comparison.png
#   - ssi_residual_diagnostic_report.txt
# =============================================================================

library(tidyverse)
library(broom)

# --- SET PATHS ----------------------------------------------------------------
BASE_DIR <- "C:/Users/chatm/OneDrive - College of Charleston/Desktop/thesis/ssiworkthesis"
DATA_DIR <- file.path(BASE_DIR, "data")
FIG_DIR  <- file.path(BASE_DIR, "figures")

dat <- read_csv(file.path(DATA_DIR, "ssi_mortality_validation_results.csv"),
                show_col_types = FALSE)

sink(file.path(DATA_DIR, "ssi_residual_diagnostic_report.txt"))
cat("SSI RESIDUAL DIAGNOSTIC REPORT\n")
cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
cat("n sites =", nrow(dat), "\n\n")

cat("SSI class distribution:\n"); print(table(dat$ssi_class))
cat("\nSSI range:", min(dat$ssi), "-", max(dat$ssi), "\n\n")

# =============================================================================
# SECTION 1: PRIMARY MODEL (reproduces published validation result)
# =============================================================================
m_mean <- lm(mean_mortality ~ ssi, data = dat)
cat("=== PRIMARY MODEL: mean_mortality ~ ssi ===\n")
print(glance(m_mean)[c("r.squared","p.value")]); print(tidy(m_mean))

dat <- dat %>% mutate(resid_mean = resid(m_mean))

# =============================================================================
# SECTION 2: Q1 — DISEASE SIGNAL TEST
# =============================================================================
cat("\n=== Q1: DISEASE-SIGNAL TEST ===\n")
cat("(positive residual = MORE mortality than the SSI predicts)\n\n")

cat("-- Mean residual by SSI class --\n")
print(dat %>% group_by(ssi_class) %>%
        summarise(mean_resid = mean(resid_mean), n = n()) %>% arrange(desc(mean_resid)))

dat <- dat %>% mutate(tercile = ntile(ssi, 3),
                      tercile = factor(tercile, labels = c("Low SSI","Mid SSI","High SSI")))
cat("\n-- Mean residual by SSI tercile --\n")
print(dat %>% group_by(tercile) %>%
        summarise(mean_resid = mean(resid_mean), pos_resid = sum(resid_mean > 0), n = n()))

tt <- t.test(resid_mean ~ tercile,
             data = dat %>% filter(tercile %in% c("Low SSI","High SSI")))
cat("\n-- Welch t-test, High vs Low tercile residuals --\n"); print(tt)

m_quad <- lm(mean_mortality ~ ssi + I(ssi^2), data = dat)
cat("\n-- Curvature test (quadratic term) --\n"); print(tidy(m_quad))
cat("R2 linear:", summary(m_mean)$r.squared,
    "-> quadratic:", summary(m_quad)$r.squared, "\n")

# =============================================================================
# SECTION 3: Q2 — OUTCOME VARIABLE COMPARISON
# =============================================================================
cat("\n=== Q2: OUTCOME VARIABLE COMPARISON ===\n")
outcomes <- c("median_mortality","mean_mortality","max_mortality","sd_mortality")
res <- map_dfr(outcomes, function(v) {
  f <- lm(reformulate("ssi", v), data = dat)
  tibble(outcome = v,
         slope   = coef(f)[2],
         r2      = summary(f)$r.squared,
         p       = summary(f)$coefficients[2,4],
         spearman_rho = cor(dat$ssi, dat[[v]], method = "spearman"),
         spearman_p   = cor.test(dat$ssi, dat[[v]], method = "spearman")$p.value)
})
print(res)

# =============================================================================
# SECTION 4: ROBUSTNESS — INFLUENCE, JACKKNIFE, WITHIN-CLASS
# =============================================================================
cat("\n=== ROBUSTNESS ===\n")
dat <- dat %>% mutate(cooks = cooks.distance(m_mean),
                      leverage = hatvalues(m_mean))
cat("-- Most influential sites --\n")
print(dat %>% arrange(desc(cooks)) %>%
        select(Site_code, ssi, ssi_class, mean_mortality, max_mortality, cooks, leverage) %>%
        head(6))
cat("Cook's D threshold (4/n):", 4/nrow(dat), "\n")

cat("\n-- Jackknife: leave-one-out refits --\n")
jack <- map_dfr(dat$Site_code, function(s) {
  d <- dat %>% filter(Site_code != s); f <- lm(mean_mortality ~ ssi, data = d)
  tibble(dropped = s, r2 = summary(f)$r.squared, p = summary(f)$coefficients[2,4])
})
cat("p range:", min(jack$p), "-", max(jack$p),
    "| significant in", sum(jack$p < .05), "of", nrow(jack), "fits\n")
print(jack %>% arrange(desc(p)) %>% head(4))

cat("\n-- Within Excellent class only --\n")
exc <- dat %>% filter(str_starts(ssi_class, "Excellent"))
m_exc <- lm(mean_mortality ~ ssi, data = exc); print(tidy(m_exc))
cat("n =", nrow(exc), "| R2 =", summary(m_exc)$r.squared, "\n")

sink()

# =============================================================================
# SECTION 5: FIGURES
# =============================================================================
p1 <- ggplot(dat, aes(ssi, resid_mean)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_point(aes(colour = ssi_class), size = 3, alpha = .85) +
  geom_smooth(method = "loess", se = TRUE, colour = "black", linewidth = .6) +
  labs(title = "SSI residual diagnostic: no systematic under-prediction at high SSI",
       subtitle = "Positive residual = more mortality than the SSI predicts. A disease signal would push high-SSI points upward.",
       x = "SSI score", y = "Residual (observed - predicted mean mortality, %)",
       colour = "SSI class") +
  theme_minimal(base_size = 12)
ggsave(file.path(FIG_DIR, "ch5_residual_diagnostic.png"), p1,
       width = 9, height = 6, dpi = 300)

p2 <- res %>%
  mutate(outcome = factor(outcome, levels = outcomes,
                          labels = c("Median\n(typical year)","Mean","Max\n(worst year)","SD\n(volatility)"))) %>%
  ggplot(aes(outcome, r2)) +
  geom_col(fill = "steelblue", alpha = .85) +
  geom_text(aes(label = sprintf("R2 = %.3f\np = %.4f", r2, p)), vjust = -.3, size = 3.4) +
  ylim(0, .8) +
  labs(title = "The SSI predicts mortality volatility and worst-case loss, not typical-year mortality",
       subtitle = "OLS fit of each mortality outcome on SSI score (n = 29 sentinel sites)",
       x = NULL, y = expression(R^2)) +
  theme_minimal(base_size = 12)
ggsave(file.path(FIG_DIR, "ch5_outcome_variable_comparison.png"), p2,
       width = 9, height = 6, dpi = 300)

message("Done. Report and two figures written.")
