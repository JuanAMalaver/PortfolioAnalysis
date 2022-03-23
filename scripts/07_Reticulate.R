library(reticulate)

# conda_create("r-reticulate")

# py_install("pandas")
 
# py_install("pandas-datareader")

# py_install("scikit-learn")

source_python("scripts/01_Data_Import.py")

df <- get_stock_data(ticker = c("NVDA", "AMD"), start = "2021-03-02", end = "2022-03-02")

df









