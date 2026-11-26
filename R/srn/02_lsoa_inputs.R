#!/usr/bin/env Rscript
# Build the LSOA (2021) base table for England:
#  - Census 2021 TS061 (method of travel to work) outcome shares
#  - Census 2021 TS001 (usual residents, OA level) for population density and
#    for reconstructing LSOA population-weighted centroids (PWCs) from ONS
#    OA (2021) PWCs, per ONS centroid guidance
#  - LSOA land area from ONS LSOA (2021) BGC boundaries
#  - IMD 2019 ranks (LSOA 2011 codes) mapped to 2021 LSOAs via the ONS
#    best-fit lookup
#  - Rural-urban classification (2021) for LSOAs from ONS OGC
# Outputs: data/derived/lsoa_base.csv, data/cache/lsoa_pwc.gpkg (27700 points)

library(sf)
library(data.table)
library(dplyr)
library(jsonlite)

cache_dir <- "data/cache"
derived_dir <- "data/derived"
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(derived_dir, showWarnings = FALSE, recursive = TRUE)
lake_ons <- "/data/bronze/ons"

# ------------------------------------------------- helpers: ONS geoportal APIs
fetch_json <- function(url) {
  txt <- paste(readLines(url, warn = FALSE), collapse = "")
  fromJSON(txt, simplifyVector = FALSE)
}

query_attrs <- function(service_url, chunk = 2000, max_rows = 200000) {
  # Paginated attribute-only query of an ArcGIS FeatureServer layer/table.
  service_url <- sub("/$", "", service_url)
  info <- fetch_json(paste0(service_url, "?f=json"))
  fields <- vapply(info$fields, function(f) f$name, "")
  code_field <- grep("LSOA", fields, value = TRUE)[1]
  stopifnot(!is.na(code_field))
  srv_chunk <- info$maxRecordCount
  if (!is.null(srv_chunk) && srv_chunk > 0) chunk <- min(chunk, srv_chunk)
  count <- fetch_json(paste0(
    service_url, "/query?where=1%3D1&returnCountOnly=true&f=json"
  ))$count
  message("Querying ", count, " rows from ", basename(service_url))
  out <- vector("list", ceiling(count / chunk))
  for (i in seq_along(out)) {
    offset <- (i - 1) * chunk
    url <- sprintf(
      paste0(service_url, "/query?where=1%%3D1&outFields=*&returnGeometry=false",
             "&f=json&resultOffset=%d&resultRecordCount=%d"), offset, chunk
    )
    d <- fetch_json(url)
    rows <- lapply(d$features, function(f) f$attributes)
    rows <- rbindlist(lapply(rows, function(r) as.data.table(r)), fill = TRUE)
    if (nrow(rows) == 0) { # transient AGOL failure: retry the page up to 3x
      for (retry in 1:3) {
        Sys.sleep(2)
        d <- fetch_json(url)
        rows <- lapply(d$features, function(f) f$attributes)
        rows <- rbindlist(lapply(rows, function(r) as.data.table(r)), fill = TRUE)
        if (nrow(rows) > 0) break
      }
    }
    out[[i]] <- rows
    message(sprintf("  page %d: %d rows", i, nrow(rows)))
    if (offset + chunk >= count) break
  }
  result <- rbindlist(out, fill = TRUE)
  stopifnot(nrow(result) == count)
  result
}

service_from_item <- function(item_id) {
  d <- fetch_json(paste0(
    "https://geoportal.statistics.gov.uk/api/search/v1/collections/dataset/items/",
    item_id
  ))
  url <- d$properties$url
  stopifnot(!is.null(url))
  sub("FeatureServer.*$", "FeatureServer/0", url)
}

download_once <- function(url, dest) {
  if (!file.exists(dest)) {
    message("Downloading ", basename(dest))
    download.file(url, dest, mode = "wb", quiet = TRUE)
  }
  dest
}

# ------------------------------------------------------- Census TS061 outcome
message("Reading Census 2021 TS061 (method of travel to work), LSOA level")
ts061_csv <- unzip(
  file.path(lake_ons, "census-2021-ts061", "census2021-ts061.zip"),
  "census2021-ts061-lsoa.csv", exdir = tempdir()
)
ttw <- fread(ts061_csv, encoding = "UTF-8")
setnames(ttw, "geography code", "lsoa21cd")
ttw <- ttw[grepl("^E", lsoa21cd), .(
  lsoa21cd,
  workers_total = `Method of travel to workplace: Total: All usual residents aged 16 years and over in employment the week before the census`,
  home = `Method of travel to workplace: Work mainly at or from home`,
  bicycle = `Method of travel to workplace: Bicycle`,
  foot = `Method of travel to workplace: On foot`
)]
ttw[, commuters := workers_total - home]
ttw[, active := bicycle + foot]
ttw[, share_active := 100 * active / commuters]
message("TS061: ", nrow(ttw), " England LSOAs; population-weighted mean active share computed later")

# ------------------------------- OA residents + PWC -> LSOA PWC reconstruction
message("Reading Census 2021 TS001 (usual residents), OA level")
ts001_csv <- unzip(
  file.path(lake_ons, "census-2021-ts001", "census2021-ts001.zip"),
  "census2021-ts001-oa.csv", exdir = tempdir()
)
oa_pop <- fread(
  ts001_csv,
  select = c("geography code", "Residence type: Total; measures: Value")
)
setnames(oa_pop, c("oa21cd", "residents"))

message("Reading OA (2021) population-weighted centroids and OA->LSOA lookup")
oa_pwc <- fread(file.path(lake_ons, "oa21-pwc", "oa21_pwc.csv"))
setnames(oa_pwc, "OA21CD", "oa21cd")
oa_lsoa <- fread(file.path(lake_ons, "oa21-pwc", "oa21_msoa21_lad22.csv"),
                 select = c("OA21CD", "LSOA21CD"))
setnames(oa_lsoa, "OA21CD", "oa21cd")

oa <- merge(oa_pwc, oa_pop, by = "oa21cd")
oa <- merge(oa, oa_lsoa, by = "oa21cd")
oa <- oa[grepl("^E", LSOA21CD)]
stopifnot(nrow(oa) > 150000)

lsoa_pwc <- oa[, .(
  x = sum(residents * lon) / sum(residents),
  y = sum(residents * lat) / sum(residents),
  residents_lsoa = sum(residents)
), by = LSOA21CD]

pwc_sf <- st_as_sf(lsoa_pwc, coords = c("x", "y"), crs = 4326) |>
  st_transform(27700)
st_write(pwc_sf, file.path(cache_dir, "lsoa_pwc.gpkg"), delete_dsn = TRUE)
message("Reconstructed PWCs for ", nrow(pwc_sf), " England LSOAs")

# ------------------------------------------------------- LSOA areas from BGC
bgc_file <- file.path(lake_ons, "lsoa-2021-boundaries", "LSOA_2021_EW_BGC_V5.geojson")
message("Reading LSOA (2021) BGC boundaries for areas")
lsoa <- st_read(bgc_file, quiet = TRUE)
lsoa <- lsoa[grepl("^E", lsoa$LSOA21CD), "LSOA21CD"]
st_geometry(lsoa) <- st_geometry(lsoa)
areas <- st_transform(lsoa, 27700) |>
  st_area() |>
  as.numeric()
lsoa_dt <- data.table(lsoa21cd = lsoa$LSOA21CD, area_km2 = areas / 1e6)

# --------------------------------------------------------------------- IMD 2019
imd_url <- "https://assets.publishing.service.gov.uk/media/5dc407b440f0b6379a7acc8d/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_3.csv"
imd_file <- download_once(imd_url, file.path(cache_dir, "imd2019_file7.csv"))
imd <- fread(imd_file, select = c(
  "LSOA code (2011)",
  "Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)",
  "Index of Multiple Deprivation (IMD) Decile (where 1 is most deprived 10% of LSOAs)",
  "Total population: mid 2015 (excluding prisoners)"
))
setnames(imd, c("lsoa11cd", "imd_rank", "imd_decile", "pop_2015"))

message("Fetching ONS LSOA (2011) to LSOA (2021) best-fit lookup")
lookup_item <- "b684a0dbf786473f9563ec0616da2f8b" # LSOA (2011) to LSOA (2021) to LAD (2022) Best Fit Lookup for EW (V2)
lookup_url <- service_from_item(lookup_item)
lookup <- query_attrs(lookup_url)
l11 <- grep("LSOA11CD|LSOA11", names(lookup), value = TRUE)[1]
l21 <- grep("LSOA21CD|LSOA21", names(lookup), value = TRUE)[1]
message("Lookup columns: ", l11, " -> ", l21, " (", nrow(lookup), " rows)")
if (grepl("11", l21)) { # keyed by 2021 codes with a best-fit 2011 code
  setnames(lookup, c(l21, l11), c("lsoa21cd", "lsoa11cd"))
  lsoa_imd <- merge(
    lookup[, .(lsoa21cd, lsoa11cd)],
    imd, by = "lsoa11cd"
  )
  lsoa_imd <- lsoa_imd[, .(
    imd_rank = stats::weighted.mean(imd_rank, pop_2015),
    imd_decile = round(stats::weighted.mean(imd_decile, pop_2015))
  ), by = lsoa21cd]
} else { # keyed by 2011 codes: population-weighted aggregation into 2021 zones
  setnames(lookup, c(l11, l21), c("lsoa11cd", "lsoa21cd"))
  lsoa_imd <- merge(lookup, imd, by = "lsoa11cd")
  lsoa_imd <- lsoa_imd[, .(
    imd_rank = stats::weighted.mean(imd_rank, pop_2015),
    imd_decile = round(stats::weighted.mean(imd_decile, pop_2015))
  ), by = lsoa21cd]
}

# ----------------------------------------------------- rural-urban classification
message("Fetching ONS rural-urban classification (2021) for LSOAs")
ruc_item <- "b976e08d5c894df3901963469bd4f84f" # LSOA (Dec 2021) BSC (V4) and Rural Urban Classification
ruc_url <- service_from_item(ruc_item)
ruc <- query_attrs(ruc_url)
ruc_code <- grep("LSOA21CD|LSOA21", names(ruc), value = TRUE)[1]
ruc_col <- grep("RUC|rural", names(ruc), ignore.case = TRUE, value = TRUE)[1]
message("RUC columns: ", ruc_code, ", ", ruc_col)
ruc <- ruc[, .(lsoa21cd = get(ruc_code),
               ruc_raw = as.character(get(ruc_col)))]
# RUC 2021 codes: 1-4 urban, 5-8 rural (town/fringe/village/hamlet incl. sparse)
numeric_code <- suppressWarnings(as.integer(ruc$ruc_raw))
if (all(is.na(numeric_code))) {
  ruc[, rural := as.integer(grepl("Rural", ruc_raw, ignore.case = TRUE))]
} else {
  ruc[, rural := as.integer(numeric_code >= 5)]
}
ruc[, ruc_raw := NULL]

# ------------------------------------------------------------------- assemble
base <- merge(ttw, lsoa_dt, by = "lsoa21cd")
base <- merge(base, lsoa_imd, by = "lsoa21cd", all.x = TRUE)
base <- merge(base, ruc, by = "lsoa21cd", all.x = TRUE)
base <- merge(base, as.data.table(pwc_sf)[, .(lsoa21cd = LSOA21CD,
                                              residents_lsoa)], by = "lsoa21cd")
base[, dens_km2 := residents_lsoa / area_km2]

stopifnot(nrow(base) == nrow(pwc_sf))
fwrite(base, file.path(derived_dir, "lsoa_base.csv"))
message("Wrote ", derived_dir, "/lsoa_base.csv with ", nrow(base), " LSOAs and ",
        ncol(base), " columns")
message("Rows with missing IMD: ", sum(is.na(base$imd_rank)))
