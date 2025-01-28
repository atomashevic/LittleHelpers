# This script goes through all mplus output files in the models/ directory
# and uses model_tables.R to extact model results and save them to docx file in the results/ directory

library(officer)
library(flextable)
source("model_tables.R")

# Create results directory if it doesn't exist
if (!dir.exists("results")) {
  dir.create("results")
}

# Get all .out files from models directory
out_files <- list.files(path = "models", pattern = "\\.out$", full.names = TRUE)

if (length(out_files) == 0) {
  stop("No .out files found in models/ directory")
}

# Process each file
for (file_path in out_files) {
  # Extract base name for output file
  base_name <- tools::file_path_sans_ext(basename(file_path))
  output_file <- file.path("results", paste0(base_name, "_results.docx"))
  
  # Extract tables from Mplus output
  tables <- extract_mplus_tables(file_path)
  
  # Create a new Word document
  doc <- read_docx()
  
  # Add each table to the document
  for (table_name in names(tables)) {
    # Add section header
    doc <- body_add_par(doc, table_name, style = "heading 1")
    
    # Convert table to flextable
    ft <- flextable(tables[[table_name]])
    ft <- autofit(ft)
    ft <- bold(ft, bold = TRUE, part = "header")
    ft <- colformat_double(ft, digits = 3, na_str = "")
    
    # Add table to document
    doc <- body_add_flextable(doc, ft)
    doc <- body_add_par(doc, "") # Add empty paragraph for spacing
  }
  
  # Create indirect effects table
  ind_table <- create_ind_table(file_path)
  
  if (!is.null(ind_table) && nrow(ind_table) > 0) {
    # Add indirect effects header
    doc <- body_add_par(doc, "Indirect Effects (unstandardized)", style = "heading 1")
    
    # Format and add table
    ft <- flextable(ind_table)
    ft <- bold(ft, bold = TRUE, part = "header")
    ft <- colformat_double(ft, digits = 3, na_str = "")
    ft <- autofit(ft)
    
    doc <- body_add_flextable(doc, ft)
    doc <- body_add_par(doc, "") # Add empty paragraph for spacing
  }
  
  # Save the document
  print(doc, target = output_file)
  cat(sprintf("Processed %s -> %s\n", file_path, output_file))
}

cat("All files processed successfully!\n")

