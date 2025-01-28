# Goes through all the Mplus files in the results folder and creates a word document with all the indirect effects tables. It should be clear for which model the indirect effects are.

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

# Create a new Word document
doc <- read_docx()

# Process each file
for (file_path in out_files) {
  # Extract base name for model identification
  base_name <- tools::file_path_sans_ext(basename(file_path))
  
  # Create indirect effects table
  ind_table <- create_ind_table(file_path)
  
  if (!is.null(ind_table) && nrow(ind_table) > 0) {
    # Add model identifier as header
    doc <- body_add_par(doc, paste("Model:", base_name), style = "heading 1")
    
    # Format and add table
    ft <- flextable(ind_table)
    ft <- bold(ft, bold = TRUE, part = "header")
    ft <- colformat_double(ft, digits = 3, na_str = "")
    ft <- autofit(ft)
    
    doc <- body_add_flextable(doc, ft)
    doc <- body_add_par(doc, "") # Add empty paragraph for spacing
  }
}

# Save the document
output_file <- file.path("results", "all_indirect_effects.docx")
print(doc, target = output_file)
cat(sprintf("All indirect effects saved to %s\n", output_file))