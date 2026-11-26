#!/usr/bin/env Rscript
# SRN proximity exposure, descriptive statistics, and the suppression model.
# Inputs : data/derived/lsoa_base.csv, data/cache/lsoa_pwc.gpkg,
#          data/cache/srn_links.gpkg
# Outputs: data/derived/lsoa_analysis.csv  (per-LSOA distances + bands)
#          data/derived/band_summary.csv   (descriptives by distance band)
#          data/derived/model_ols.csv, model_wls.csv (regression results)
#          data/derived/headline.json      (key numbers for the paper/page)
#          figures-srn/srn-band-descriptives.png

library(sf)
library(data.table)
library(dplyr)
library(ggplot2)

derived_dir <- "data/derived"
fig_dir <- "figures-srn"
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

band_labels <- c("0-500 m", "500-1,000 m", "1-2 km", "2-5 km", ">5 km")
band_breaks <- c(0, 500, 1000, 2000, 5000, Inf)

# ---------------------------------------------------------------- load inputs
base <- fread(file.path(derived_dir, "lsoa_base.csv"))
pwc <- st_read(file.path("data", "cache", "lsoa_pwc.gpkg"), quiet = TRUE)
srn <- st_read(file.path("data", "cache", "srn_links.gpkg"), quiet = TRUE)
message("LSOAs: ", nrow(pwc), " | SRN links: ", nrow(srn))

# ---------------------------------------------- distance to nearest SRN link
# Only distances up to 5,100 m matter for the bands; points beyond that fall
# in the >5 km reference band, so a spatial-index prefilter suffices.
analysis_file <- file.path(derived_dir, "lsoa_analysis.csv")
if (!file.exists(analysis_file)) {
  matches <- st_is_within_distance(pwc, srn, dist = 5100)
  idx_near <- which(lengths(matches) > 0)
  message("LSOAs within 5,100 m of an SRN link: ", length(idx_near), " of ",
          nrow(pwc))

  # elementwise exact distances for matched (point, link) pairs
  pt_idx <- unlist(lapply(seq_along(matches), function(i) rep(i, length(matches[[i]]))))
  ln_idx <- unlist(matches)
  message("Candidate (point, link) pairs: ", length(pt_idx))
  dist_m <- geos::geos_distance(
    geos::as_geos_geometry(st_geometry(pwc)[pt_idx]),
    geos::as_geos_geometry(st_geometry(srn)[ln_idx])
  )
  min_by_pt <- tapply(as.numeric(dist_m), pt_idx, min)
  dist_srn <- rep(NA_real_, nrow(pwc))
  dist_srn[as.integer(names(min_by_pt))] <- as.numeric(min_by_pt)

  out <- copy(base)
  out[, dist_srn_m := dist_srn]
  out[, band := cut(dist_srn_m, breaks = band_breaks, labels = band_labels,
                    include.lowest = TRUE, right = FALSE)]
  out[is.na(band), band := band_labels[5]] # no SRN within 5.1 km
  out[, band := factor(band, levels = band_labels)]
  fwrite(out, analysis_file)
} else {
  out <- fread(analysis_file)
  out[, band := factor(band, levels = band_labels)]
}
message("Band counts: ", paste(names(table(out$band)), table(out$band),
                               collapse = ", "))

# ------------------------------------------------------------- descriptives
band_summary <- out[, {
  w <- commuters
  .(n_lsoas = .N,
    commuters = sum(w),
    residents = sum(residents_lsoa),
    mean_share = round(mean(share_active), 2),
    weighted_share = round(sum(share_active * w) / sum(w), 2),
    se_share = round(sd(share_active) / sqrt(.N), 2),
    median_dist_m = round(median(dist_srn_m, na.rm = TRUE)),
    mean_dens_km2 = round(mean(dens_km2)),
    mean_imd_rank = round(mean(imd_rank, na.rm = TRUE)))
}, by = band][order(band)]
fwrite(band_summary, file.path(derived_dir, "band_summary.csv"))
print(band_summary)

p <- ggplot(band_summary, aes(x = band, y = mean_share, group = 1)) +
  geom_line(colour = "grey55", linewidth = 0.4) +
  geom_point(size = 2.2, colour = "darkgreen") +
  geom_errorbar(aes(ymin = mean_share - 1.96 * se_share,
                    ymax = mean_share + 1.96 * se_share),
                width = 0.06, colour = "darkgreen") +
  geom_text(aes(label = sprintf("n=%s", scales::comma(n_lsoas))),
            vjust = -1.2, size = 3, colour = "grey30") +
  scale_y_continuous(limits = c(0, max(band_summary$mean_share) * 1.18)) +
  labs(
    x = "Distance from LSOA population-weighted centroid to nearest SRN link",
    y = "Walking or cycling to work (%)",
    title = "Active travel to work falls with proximity to the Strategic Road Network",
    subtitle = "Census 2021, England LSOAs. Bars show 95% CI on the unweighted mean.",
    caption = "Sources: ONS Census 2021 TS061; National Highways Network Model (Public), OGL v3."
  ) +
  theme_minimal(base_size = 12)
ggsave(file.path(fig_dir, "srn-band-descriptives.png"), p,
       width = 8, height = 5, dpi = 200)

# ------------------------------------------------------------------ models
d <- out[!is.na(imd_rank) & is.na(rural) == FALSE & commuters > 0]
d[, log_dens := log10(pmax(dens_km2, 1))]
d[, band5 := relevel(band, ref = band_labels[5])] # >5 km as reference

fit_ols <- lm(share_active ~ band5 + log_dens + imd_rank + rural, data = d)
fit_wls <- lm(share_active ~ band5 + log_dens + imd_rank + rural, data = d,
              weights = commuters)

write_model <- function(fit, type, file) {
  hc <- sandwich::vcovHC(fit, type = "HC1")
  co <- lmtest::coeftest(fit, vcov = hc)
  dt <- as.data.table(unclass(co), keep.rownames = "term") # unclass: as.data.table on "coeftest" objects recurses (C stack)
  setnames(dt, c("Estimate", "Std. Error", "Pr(>|t|)"),
           c("estimate", "se", "p_value"))
  # robust 95% CIs, normal approximation (lmtest::coefci recurses here)
  dt[, ci_low := estimate - 1.96 * se]
  dt[, ci_high := estimate + 1.96 * se]
  dt[, model := type]
  dt <- dt[, .(model, term, estimate = round(estimate, 3),
               se = round(se, 3), ci_low = round(ci_low, 3),
               ci_high = round(ci_high, 3), p_value = signif(p_value, 3))]
  fwrite(dt, file)
  dt
}
m_ols <- write_model(fit_ols, "OLS (HC1)",
                     file.path(derived_dir, "model_ols.csv"))
m_wls <- write_model(fit_wls, "OLS weighted by commuters",
                     file.path(derived_dir, "model_wls.csv"))

# ------------------------------------------------------------------ headline
b1 <- m_ols[term == "band50-500 m"] # relevel renames levels with band5 prefix
b1 <- m_ols[grepl("^band5", term) & grepl("0-500", term)]
adj_gap_pp <- -b1$estimate # suppression in percentage points
commuters_near <- d[band == band_labels[1], sum(commuters)]
counterfactual_people <- round(adj_gap_pp / 100 * commuters_near)
raw_gap_pp <- band_summary[band == band_labels[1], weighted_share] -
  band_summary[band == band_labels[5], weighted_share]

headline <- list(
  n_lsoas = nrow(out),
  n_lsoas_model = nrow(d),
  commuters_total = out[, sum(commuters)],
  active_share_overall_weighted = round(
    out[, sum(share_active * commuters) / sum(commuters)], 2
  ),
  share_by_band_weighted = setNames(
    as.list(band_summary$weighted_share), as.character(band_summary$band)
  ),
  raw_gap_pp = round(raw_gap_pp, 2),
  adjusted_gap_pp = round(adj_gap_pp, 2),
  adjusted_gap_ci = c(round(b1$ci_low, 2), round(b1$ci_high, 2)),
  commuters_within_500m = commuters_near,
  counterfactual_people = counterfactual_people,
  srn_km = round(sum(as.numeric(st_length(srn))) / 1000)
)
jsonlite::write_json(headline, file.path(derived_dir, "headline.json"),
                     pretty = TRUE, auto_unbox = TRUE)
message("Headline: adjusted suppression ", round(adj_gap_pp, 2),
        " pp (95% CI ", paste(headline$adjusted_gap_ci, collapse = " to "),
        "); counterfactual ", counterfactual_people, " commuters")
