# Create a sample dataframe
df <- data.frame(
  date = c("", "1999 Q1", "1999 Q2", "1999 Q3"),
  household = c("australia", 1, 4, 7),
  "...3" = c("austria", 2, 5, 8),
  "...4" = c("belgium", 3, 6, 9),
  financial = c("australia", 10, 11, 12),
  "...6" = c("austria", 13, 14, 15),
  "...7" = c("belgium", 13, 14, 15)
)

# Find the index of the first column name
first_col_index <- match("household", colnames(df))

# Find the index of the column name "financial"
financial_col_index <- match("financial", colnames(df))

# Get the first column name
first_col_name <- colnames(df)[first_col_index]

# Set the column names from the first column until "financial"
colnames(df)[(first_col_index+1):(financial_col_index -1)] <- paste(first_col_name, colnames(df)[(first_col_index+1):(financial_col_index -1)], sep = "_")

# Carry "financial" label to all columns to the right
colnames(df)[(financial_col_index + 1):ncol(df)] <- paste("financial", colnames(df)[(financial_col_index + 1):ncol(df)], sep = "_")


#Add country from first row
colnames(df)[(first_col_index):ncol(df)] <- paste(df[1,-1], colnames(df)[(first_col_index):ncol(df)], sep = "_")

colnames(df) <- sub("_\\.\\.\\.\\d+", "", colnames(df))




# Create a sample dataframe
df <- data.frame(
  Australia...2 = 1:5,
  Austria...3 = 6:10,
  Belgium...4 = 11:15,
  Croatia...5 = 16:20,
  `...33` = 21:25
)

# Find the column without a country prefix
column_without_prefix <- grep("^\\.\\.\\..*$", colnames(df), value = TRUE)

# Print the column name without a country prefix
print(column_without_prefix)





############################################



# Assuming you have a dataframe named 'adv'

# Define the column indices and corresponding names
column_indices <- c(hh_col_index, nfc_col_index, gov_col_index, fs_col_index, agg_pct_col_index, agg_usd_col_index)
column_names <- c("hh", "nfc", "gov", "fs", "agg_pct", "agg_tril")

# Create a vector to store the new indexes
new_indexes <- vector("numeric", length(column_indices))

# Loop over the column indices and perform renaming
for (i in 1:(length(column_indices) - 1)) {
  col_start <- column_indices[i] + 1
  col_end <- column_indices[i + 1] - 1
  col_name <- column_names[i]
  
  colnames(adv)[col_start:col_end] <- paste(col_name, colnames(adv)[col_start:col_end], sep = "_")
  
  # Store the new index in the vector
  new_indexes[i] <- col_start
}

# Rename the last set of columns
col_start <- column_indices[length(column_indices)] + 1
col_end <- ncol(adv)
col_name <- column_names[length(column_indices)]

colnames(adv)[col_start:col_end] <- paste(col_name, colnames(adv)[col_start:col_end], sep = "_")

# Store the new index for the last set of columns
new_indexes[length(column_indices)] <- col_start

# Print the modified dataframe
print(adv)

# Print the new indexes
for (i in 1:length(column_indices)) {
  print(new_indexes[i])
}


##################################

# Create a sample dataframe
df <- data.frame(
  var1 = 1:10,
  var2 = 11:20,
  var3 = 21:30,
  var4 = 31:40,
  var5 = 41:50
)

# Define the positions of columns to be reordered
desired_positions <- c(1, 4, 5)

# Reorder columns based on desired positions
df <- df[, c(desired_positions, setdiff(1:ncol(df), desired_positions))]

# Print the reordered dataframe
print(df)



new_sheet_names <- setNames(
  list(
    "global" = paste("1_global debt figures", qtr, sep = " "),
    "adv_1" = paste("2a_mature markets", qtr, sep = " "),
    "adv_2" = paste("2b_mature markets", qtr, "USD", sep = " "),
    "em_1" = paste("3a_emerging markets", qtr, sep = " "),
    "em_2" = paste("3b_emerging markets", qtr, "USD", sep = " "),
    "for_owner_gov_sec" = "4_foreign owner. of govt. sec.",
    "nfc_curr_gdp" = "5a_NFC debt curr breakdown",
    "nfc_curr_usd" = "5b_NFC debt curr breakdown USD",
    "fs_curr_gdp" = "6a_FS debt curr breakdown",
    "fs_curr_usd" = "6b_FS debt curr breakdown USD",
    "gov_curr_gdp" = "7a_Gov debt curr breakdown ",
    "gov_curr_usd" = "7b_Gov debt curr breakdown USD",
    "hh_curr_gdp" = "8a_HH debt curr breakdown" ,
    "hh_curr_usd" = "8b_HH debt curr breakdown USD",
    "em_bonds" = "9_EM bond redemption profile ",
    "em_loans" = "10_EM loan redemption profile",
    "global_bond" = "11_Global bond markets",
    "soe" = "12_SOE"
  ),
  c("global", "adv_1", "adv_2", "em_1", "em_2", "for_owner_gov_sec", 
    "nfc_curr_gdp", "nfc_curr_usd", "fs_curr_gdp", "fs_curr_usd", 
    "gov_curr_gdp", "gov_curr_usd", "hh_curr_gdp", "hh_curr_usd", 
    "em_bonds", "em_loans", "global_bond", "soe")
)



# Specify the sheet names to be dropped
#sheets_to_drop <- c("Table of contents", "A. General Information")

# Exclude the sheets to be dropped from the list
#filtered_sheet_names <- setdiff(sheet_names, sheets_to_drop)
