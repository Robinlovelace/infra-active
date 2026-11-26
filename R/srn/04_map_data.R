#!/usr/bin/env Rscript
# Prepare map inputs: simplified LSOA polygons with analysis attributes, and
# simplified SRN centre-lines, as GeoJSON for tippecanoe -> PMTiles.
# Inputs : data/derived/lsoa_analysis.csv (from 03), LSOA BGC boundaries,
#          data/cache/srn_links.gpkg, Census TS061 (for LSOA names)
# Outputs: docs/map/data/lsoa.geojson, docs/map/data/srn.geojson

library(sf)
library(data.table)

derived_dir <- "data/derived"
out_dir <- "docs/map/data"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
lake_ons <- "/data/bronze/ons"

analysis <- fread(file.path(derived_dir, "lsoa_analysis.csv"))
band_labels <- c("0-500 m", "500-1,000 m", "1-2 km", "2-5 km", ">5 km")
analysis[, band_int := match(band, band_labels)]
stopifnot(!any(is.na(analysis$band_int)))

# LSOA names straight from the census table
ts061_csv <- unzip(
  file.path(lake_ons, "census-2021-ts061", "census2021-ts061.zip"),
  "census2021-ts061-lsoa.csv", exdir = tempdir()
)
names_dt <- fread(ts061_csv, select = c("geography code", "geography"))
setnames(names_dt, c("lsoa21cd", "name"))
analysis <- merge(analysis, names_dt, by = "lsoa21cd")

# ------------------------------------------------- LSOA polygons with attributes
message("Reading + simplifying LSOA (2021) BGC boundaries")
lsoa <- st_read(
  file.path(lake_ons, "lsoa-2021-boundaries", "LSOA_2021_EW_BGC_V5.geojson"),
  quiet = TRUE
)
lsoa <- lsoa[grepl("^E", lsoa$LSOA21CD), ]
lsoa <- st_transform(lsoa, 27700) |>
  st_simplify(preserveTopology = TRUE, dTolerance = 50) |>
  st_transform(4326)

keep <- c(
  "lsoa21cd", "name", "band_int", "share_active", "commuters",
  "residents_lsoa", "dens_km2", "imd_decile", "rural"
)
m <- analysis[, ..keep]
setnames(m, "residents_lsoa", "residents")
lsoa <- merge(lsoa, m, by.x = "LSOA21CD", by.y = "lsoa21cd")
keep_final <- c("lsoa21cd", "name", "band_int", "share_active", "commuters",
                "residents", "dens_km2", "imd_decile", "rural")
lsoa <- lsoa[, c("LSOA21CD", setdiff(keep_final, "lsoa21cd"))]
setnames(lsoa, "LSOA21CD", "lsoa21cd")
lsoa <- lsoa[, keep_final]

st_write(
  lsoa, file.path(out_dir, "lsoa.geojson"),
  delete_dsn = TRUE, driver = "GeoJSON", quiet = TRUE
)
message("Wrote ", out_dir, "/lsoa.geojson (", nrow(lsoa), " LSOAs)")

# ------------------------------------------------------------- SRN centrelines
message("Reading + simplifying SRN links")
srn <- st_read(file.path("data", "cache", "srn_links.gpkg"), quiet = TRUE)
srn <- st_transform(srn, 27700) |>
  st_simplify(preserveTopology = TRUE, dTolerance = 10) |>
  st_transform(4326)
srn <- srn[, c("roadname", "linkref")]
st_write(
  srn, file.path(out_dir, "srn.geojson"),
  delete_dsn = TRUE, driver = "GeoJSON", quiet = TRUE
)
message("Wrote ", out_dir, "/srn.geojson (", nrow(srn), " links)")
