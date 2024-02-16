## code to prepare `cbbga_2023` dataset goes here

# Read the CSV file containing the dataset
cbbga_2023 <- read.csv("https://raw.githubusercontent.com/PriscillaWen2023/cbbga/main/data-raw/cbbga_2023.csv")
usethis::use_data(cbbga_2023, overwrite = TRUE)


