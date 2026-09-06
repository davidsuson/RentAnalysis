# 1. Define the complete URL
file_url <- "https://nsw.gov.au"

# 2. Define the name you want to give the file on your computer
dest_file <- "C:/Users/David/Desktop/MicroEcon - Code/MicroEcon-Code/Data/Raw Data/issue-124-sales-tables-march-2018.xlsx"

years <- 2018:2025
quarters <- c("march", "june", "september", "december")

all_file_combinations

# 3. Download the file (using "wb" mode for Excel files)
download.file(url = "https://dcj.nsw.gov.au/content/dam/dcj/dcj-website/documents/about-us/families-and-communities-statistics/housing-and-rent-sales/previous-rent-and-sales-reports/issue-124-sales-tables-march-2018.xlsx",
              destfile = dest_file,
              mode = "wb")

