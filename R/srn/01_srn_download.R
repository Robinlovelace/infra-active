#!/usr/bin/env Rscript
# Download the Strategic Road Network (SRN) centre-lines from the National
# Highways "Network Model (Public)" feature service (Open Government Licence).
# Output: data/cache/srn_links.gpkg  (gitignored, re-derivable)

library(sf)
library(dplyr)

fs_url <- "https://services-eu1.arcgis.com/mZXeBXkkZpekxjXT/arcgis/rest/services/Network_Model_Public_view2/FeatureServer"
layer_id <- 1 # "Link": SRN centre-line links
page_size <- 2000
out_dir <- "data/cache"
out_file <- file.path(out_dir, "srn_links.gpkg")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

if (file.exists(out_file)) {
  message("Cached SRN links found at ", out_file, " - skipping download")
  srn <- st_read(out_file, quiet = TRUE)
  message("Loaded ", nrow(srn), " cached SRN links")
  quit(save = "no", status = 0)
}

query_page <- function(offset) {
  url <- sprintf(
    "%s/%s/query?where=1%%3D1&outFields=%s&outSR=27700&f=json&resultOffset=%d&resultRecordCount=%d",
    fs_url, layer_id,
    paste(
      "objectid", "linkid", "linkref", "roadname", "linkcategory",
      "linkform", "directionality", "numberoflanes", sep = ","
    ),
    offset, page_size
  )
  sf::st_read(url, quiet = TRUE)
}

# total count
count_url <- sprintf("%s/%s/query?where=1%%3D1&returnCountOnly=true&f=json", fs_url, layer_id)
count_txt <- paste(readLines(count_url, warn = FALSE), collapse = "")
total <- jsonlite::fromJSON(count_txt)$count
message("SRN Link feature count reported by service: ", total)

pages <- ceiling(total / page_size)
srn_list <- vector("list", pages)
for (i in seq_len(pages)) {
  offset <- (i - 1) * page_size
  srn_list[[i]] <- query_page(offset)
  message(sprintf("Fetched page %d/%d (offset %d)", i, pages, offset))
}

srn <- do.call(rbind, srn_list)
names(srn)[1] <- "objectid" # ESRIJSON driver names first col FID?; normalise
stopifnot(nrow(srn) == total)

total_km <- sum(st_length(srn)) / 1000
message(sprintf("Downloaded %d SRN links, %.0f km total", nrow(srn), as.numeric(total_km)))

st_write(srn, out_file, delete_dsn = TRUE)
message("Wrote ", out_file)
