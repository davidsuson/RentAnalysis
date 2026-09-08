handle <- curl::new_handle(http_version = 1)
curl::curl_download(
  "https://www.dffh.vic.gov.au/quarterly-median-rents-local-government-area-september-quarter-2025-excel",
  destfile = "C:/Users/David/Desktop/MicroEcon - Rent/RentAnalysis/Data/Raw Data/vic/VIC all quarters.xlsx",
  mode = "wb"
)
