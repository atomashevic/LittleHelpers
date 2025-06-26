library(officer)
library(flextable)
source("model_tables.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  stop("Please provide the Mplus output file as a CLI argument.")
}

# Use the first argument as the input file
single_file <- args[1]

if (!file.exists(single_file)) {
  stop(sprintf("File %s not found!", single_file))
}

# Set output file name based on the input file name
base_name <- tools::file_path_sans_ext(basename(single_file))
output_file <- paste0(base_name, "_results.docx")

# Extract tables from the Mplus output file
tables <- extract_mplus_tables(single_file)

# Create a new Word document
doc <- read_docx()

# Add each extracted table to the document
for (table_name in names(tables)) {
  # Add section header
  doc <- body_add_par(doc, table_name, style = "heading 1")
  
  # Convert table to flextable
  ft <- flextable(tables[[table_name]])
  ft <- autofit(ft)
  ft <- bold(ft, bold = TRUE, part = "header")
  ft <- colformat_double(ft, digits = 3, na_str = "")
  
  # Add table to the document
  doc <- body_add_flextable(doc, ft)
  doc <- body_add_par(doc, "")  # Empty paragraph for spacing
}

# Create indirect effects table (if available)
ind_table <- create_ind_table(single_file)
if (!is.null(ind_table) && nrow(ind_table) > 0) {
  doc <- body_add_par(doc, "Indirect Effects (unstandardized)", style = "heading 1")
  
  ft <- flextable(ind_table)
  ft <- bold(ft, bold = TRUE, part = "header")
  ft <- colformat_double(ft, digits = 3, na_str = "")
  ft <- autofit(ft)
  
  doc <- body_add_flextable(doc, ft)
  doc <- body_add_par(doc, "")  # Empty paragraph for spacing
}

# Save the document to the specified output file
print(doc, target = output_file)
cat(sprintf("Processed %s -> %s\n", single_file, output_file))