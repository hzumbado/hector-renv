# Demographic data downloaded from Eurostat
# https://ec.europa.eu/eurostat
url <- paste0("https://ec.europa.eu/eurostat/",
              "estat-navtree-portlet-prod/BulkDownloadListing",
              "?sort=1&downfile=data%2Fdemo_pjanbroad.tsv.gz")

# Save raw data
download.file(url,
              destfile = "data/raw-data.tsv.gz")
