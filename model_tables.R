extract_mplus_tables <- function(file_path) {
  # Read the file
  lines <- readLines(file_path)
  
  # Define known table sections
  sections <- c(
    "MODEL RESULTS",
    "STANDARDIZED MODEL RESULTS",
    "STDYX Standardization",
    "R-SQUARE",
    "CONFIDENCE INTERVALS OF MODEL RESULTS"
  )
  
  # Initialize results list
  tables <- list()
  
  # Process each section
  for(section in sections) {
    # Find the section start
    section_start <- which(grepl(paste0("^", section, "$"), lines))
    
    if(length(section_start) > 0) {
      # Find where the results table starts based on section type
      if(section %in% c("MODEL RESULTS", "STANDARDIZED MODEL RESULTS", "STDYX Standardization")) {
        header_pattern <- "^\\s*Estimate\\s+S\\.E\\.\\s+Est\\./S\\.E\\.\\s+P-Value"
      } else if(section == "CONFIDENCE INTERVALS OF MODEL RESULTS") {
        header_pattern <- "^\\s*Lower .5%\\s+Lower 2.5%\\s+Lower 5%\\s+Estimate\\s+Upper 5%\\s+Upper 2.5%\\s+Upper .5%"
      } else if(section == "R-SQUARE") {
        header_pattern <- "^\\s*Observed\\s+Two-Tailed"
      } else {
        next
      }
      
      # Find table header
      header_idx <- which(grepl(header_pattern, lines[section_start:length(lines)]))
      if(length(header_idx) == 0) next
      table_start <- section_start + header_idx[1]
      
      # Find where the table ends (at next major section or blank line followed by capital letter)
      remaining_lines <- lines[(table_start+1):length(lines)]
      end_markers <- which(grepl("^\\s*$", remaining_lines))
      for(i in end_markers) {
        if(i < length(remaining_lines) && grepl("^[A-Z]", remaining_lines[i+1])) {
          end_idx <- i - 1
          break
        }
      }
      if(!exists("end_idx")) end_idx <- which(grepl("^\\s*$", remaining_lines))[1] - 1
      if(is.na(end_idx)) end_idx <- length(remaining_lines)
      
      # Extract the table rows
      table_lines <- remaining_lines[1:end_idx]
      
      # Remove empty lines and header separators
      table_lines <- table_lines[!grepl("^\\s*$", table_lines)]
      table_lines <- table_lines[!grepl("^\\s*_", table_lines)]
      
      # Parse into a data frame based on section type
      if(section %in% c("MODEL RESULTS", "STANDARDIZED MODEL RESULTS", "STDYX Standardization")) {
        # Split into different parts
        means_idx <- which(grepl("^\\s*Means$", table_lines))
        intercepts_idx <- which(grepl("^\\s*Intercepts$", table_lines))
        variances_idx <- which(grepl("^\\s*Variances$", table_lines))
        residual_idx <- which(grepl("^\\s*Residual Variances$", table_lines))
        
        # Function to extract specific part
        extract_part <- function(start_idx, end_idx) {
          if(length(start_idx) == 0) return(NULL)
          end_idx <- if(length(end_idx) > 0) min(end_idx) - 1 else length(table_lines)
          part_lines <- table_lines[(start_idx + 1):end_idx]
          tryCatch({
            data.frame(
              Parameter = trimws(gsub("\\s+[-0-9\\.]+.*$", "", part_lines)),
              Estimate = as.numeric(gsub(".*?\\s+([-0-9\\.]+)\\s+.*", "\\1", part_lines)),
              SE = as.numeric(gsub(".*?\\s+[-0-9\\.]+\\s+([-0-9\\.]+)\\s+.*", "\\1", part_lines)),
              `Est./S.E.` = as.numeric(gsub(".*?\\s+[-0-9\\.]+\\s+[-0-9\\.]+\\s+([-0-9\\.]+)\\s+.*", "\\1", part_lines)),
              P = as.numeric(gsub(".*?\\s+[-0-9\\.]+\\s+[-0-9\\.]+\\s+[-0-9\\.]+\\s+([-0-9\\.]+).*", "\\1", part_lines))
            )
          }, error = function(e) NULL)
        }
        
        # Extract each part
        results <- extract_part(1, c(means_idx, intercepts_idx, variances_idx, residual_idx))
        if(!is.null(results)) tables[[section]] <- results
        
        means_results <- extract_part(means_idx, c(intercepts_idx, variances_idx, residual_idx))
        if(!is.null(means_results)) tables[[paste(section, "Means")]] <- means_results
        
        intercepts_results <- extract_part(intercepts_idx, c(variances_idx, residual_idx))
        if(!is.null(intercepts_results)) tables[[paste(section, "Intercepts")]] <- intercepts_results
        
        variances_results <- extract_part(variances_idx, residual_idx)
        if(!is.null(variances_results)) tables[[paste(section, "Variances")]] <- variances_results
        
        residual_results <- extract_part(residual_idx, length(table_lines) + 1)
        if(!is.null(residual_results)) tables[[paste(section, "Residual Variances")]] <- residual_results
        
      } else if(section == "CONFIDENCE INTERVALS OF MODEL RESULTS") {
        results <- tryCatch({
          data.frame(
            Parameter = trimws(gsub("\\s+[-0-9\\.]+.*$", "", table_lines)),
            Lower_5 = as.numeric(gsub(".*?\\s+[-0-9\\.]+\\s+[-0-9\\.]+\\s+([-0-9\\.]+)\\s+.*", "\\1", table_lines)),
            Estimate = as.numeric(gsub(".*?\\s+[-0-9\\.]+\\s+[-0-9\\.]+\\s+[-0-9\\.]+\\s+([-0-9\\.]+)\\s+.*", "\\1", table_lines)),
            Upper_5 = as.numeric(gsub(".*?\\s+[-0-9\\.]+\\s+[-0-9\\.]+\\s+[-0-9\\.]+\\s+[-0-9\\.]+\\s+([-0-9\\.]+)\\s+.*", "\\1", table_lines))
          )
        }, error = function(e) NULL)
        
        if(!is.null(results)) tables[[section]] <- results
        
      } else if(section == "R-SQUARE") {
        # Only process latent variables
        latent_idx <- which(grepl("^\\s*Latent", table_lines))
        if(length(latent_idx) > 0) {
          latent_vars <- table_lines[(latent_idx + 1):length(table_lines)]
          latent_vars <- latent_vars[!grepl("Observed|Two-Tailed", latent_vars)]
          
          results <- tryCatch({
            data.frame(
              Variable = trimws(gsub("\\s+[-0-9\\.]+.*$", "", latent_vars)),
              Estimate = as.numeric(gsub(".*?\\s+([-0-9\\.]+)\\s+.*", "\\1", latent_vars)),
              SE = as.numeric(gsub(".*?\\s+[-0-9\\.]+\\s+([-0-9\\.]+)\\s+.*", "\\1", latent_vars)),
              `Est./S.E.` = as.numeric(gsub(".*?\\s+[-0-9\\.]+\\s+[-0-9\\.]+\\s+([-0-9\\.]+)\\s+.*", "\\1", latent_vars)),
              P = as.numeric(gsub(".*?\\s+[-0-9\\.]+\\s+[-0-9\\.]+\\s+[-0-9\\.]+\\s+([-0-9\\.]+).*", "\\1", latent_vars))
            )
          }, error = function(e) NULL)
          
          if(!is.null(results)) tables[[section]] <- results
        }
      }
    }
  }
  
  return(tables)
}

format_mplus_table <- function(table, title = NULL) {
  # Check if table is empty
  if(is.null(table) || nrow(table) == 0) {
    return(NULL)
  }
  
  # Create a new data frame matching the input table structure
  col_names <- names(table)
  formatted_table <- as.data.frame(matrix(nrow = 0, ncol = length(col_names)))
  names(formatted_table) <- col_names
  
  # Process each row
  current_header <- ""
  for(i in 1:nrow(table)) {
    row <- table[i,]
    
    # Check if this is a header row
    if(length(row$Parameter) > 0 && 
       grepl("BY$|ON$|WITH$|Means$|Intercepts$|Variances$", row$Parameter)) {
      current_header <- trimws(row$Parameter)
      # Add header row
      new_row <- row
      new_row$Parameter <- current_header
      new_row[2:ncol(new_row)] <- NA
      formatted_table <- rbind(formatted_table, new_row)
    } else {
      # Add indented parameter row
      new_row <- row
      if(length(new_row$Parameter) > 0) {
        new_row$Parameter <- sprintf("    %s", trimws(row$Parameter))
      }
      formatted_table <- rbind(formatted_table, new_row)
    }
  }
  
  # Replace NAs with empty strings in character columns
  char_cols <- sapply(formatted_table, is.character)
  formatted_table[char_cols][is.na(formatted_table[char_cols])] <- ""
  
  # Round numeric columns to 3 decimal places and replace NAs
  numeric_cols <- sapply(formatted_table, is.numeric)
  formatted_table[numeric_cols] <- lapply(formatted_table[numeric_cols], function(x) {
    x[is.na(x)] <- ""
    if(is.numeric(x)) round(x, 3) else x
  })
  
  return(formatted_table)
}

# Helper function to write tables to Word
write_mplus_tables_to_docx <- function(tables, output_file) {
  if(!requireNamespace("officer", quietly = TRUE)) {
    stop("Package 'officer' is required. Please install it first.")
  }
  if(!requireNamespace("flextable", quietly = TRUE)) {
    stop("Package 'flextable' is required. Please install it first.")
  }
  
  # Create a new Word document
  doc <- officer::read_docx()
  
  # Add each table
  for(name in names(tables)) {
    # Format the table
    formatted <- format_mplus_table(tables[[name]], title = name)
    
    # Skip if table is NULL or empty
    if(is.null(formatted) || nrow(formatted) == 0) {
      next
    }
    
    # Add title
    doc <- officer::body_add_par(doc, name, style = "heading 2")
    
    # Convert to flextable
    ft <- flextable::flextable(formatted)
    
    # Set header style
    ft <- flextable::bold(ft, bold = TRUE, part = "header")
    
    # Auto-adjust column widths
    ft <- flextable::autofit(ft)
    
    # Add to document
    doc <- flextable::body_add_flextable(doc, ft)
    
    # Add space after table
    doc <- officer::body_add_par(doc, "")
  }
  
  # Save the document
  print(doc, target = output_file)
}

