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
      
      # Find where the table ends
      remaining_lines <- lines[(table_start+1):length(lines)]
      end_markers <- which(grepl("^\\s*$", remaining_lines))
      end_idx <- NULL
      for(i in end_markers) {
        if(i < length(remaining_lines) && grepl("^[A-Z]", remaining_lines[i+1])) {
          end_idx <- i - 1
          break
        }
      }
      if(is.null(end_idx)) end_idx <- which(grepl("^\\s*$", remaining_lines))[1] - 1
      if(is.na(end_idx)) end_idx <- length(remaining_lines)
      
      # Extract the table rows
      table_lines <- remaining_lines[1:end_idx]
      table_lines <- table_lines[!grepl("^\\s*$", table_lines)]
      table_lines <- table_lines[!grepl("^\\s*_", table_lines)]
      
      # Parse into a data frame based on section type
      if(section %in% c("MODEL RESULTS", "STANDARDIZED MODEL RESULTS", "STDYX Standardization")) {
        # Split into different parts
        means_idx <- which(grepl("^\\s*Means$", table_lines))
        intercepts_idx <- which(grepl("^\\s*Intercepts$", table_lines))
        variances_idx <- which(grepl("^\\s*Variances$", table_lines))
        residual_idx <- which(grepl("^\\s*Residual Variances$", table_lines))
        newadd_idx <- which(grepl("^\\s*New/Additional Parameters$", table_lines))
        
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
        results <- extract_part(1, c(means_idx, intercepts_idx, variances_idx, residual_idx, newadd_idx))
        if(!is.null(results)) tables[[section]] <- results
        
        means_results <- extract_part(means_idx, c(intercepts_idx, variances_idx, residual_idx, newadd_idx))
        if(!is.null(means_results)) tables[[paste(section, "Means")]] <- means_results
        
        intercepts_results <- extract_part(intercepts_idx, c(variances_idx, residual_idx, newadd_idx))
        if(!is.null(intercepts_results)) tables[[paste(section, "Intercepts")]] <- intercepts_results
        
        variances_results <- extract_part(variances_idx, c(residual_idx, newadd_idx))
        if(!is.null(variances_results)) tables[[paste(section, "Variances")]] <- variances_results
        
        residual_results <- extract_part(residual_idx, newadd_idx)
        if(!is.null(residual_results)) tables[[paste(section, "Residual Variances")]] <- residual_results
        
        newadd_results <- extract_part(newadd_idx, length(table_lines) + 1)
        if(!is.null(newadd_results)) tables[[paste(section, "New/Additional Parameters")]] <- newadd_results
        
      } else if(section == "CONFIDENCE INTERVALS OF MODEL RESULTS") {
        results <- tryCatch({
          data.frame(
            Parameter = trimws(gsub("\\s+[-0-9\\.]+.*$", "", table_lines)),
            Lower_.5 = as.numeric(gsub("^\\s*(\\S+).*", "\\1", table_lines)),
            Lower_2.5 = as.numeric(gsub("^\\s*\\S+\\s+(\\S+).*", "\\1", table_lines)),
            Lower_5 = as.numeric(gsub("^\\s*(?:\\S+\\s+){2}(\\S+).*", "\\1", table_lines)),
            Estimate = as.numeric(gsub("^\\s*(?:\\S+\\s+){3}(\\S+).*", "\\1", table_lines)),
            Upper_5 = as.numeric(gsub("^\\s*(?:\\S+\\s+){4}(\\S+).*", "\\1", table_lines)),
            Upper_2.5 = as.numeric(gsub("^\\s*(?:\\S+\\s+){5}(\\S+).*", "\\1", table_lines)),
            Upper_.5 = as.numeric(gsub("^\\s*(?:\\S+\\s+){6}(\\S+).*", "\\1", table_lines))
          )
        }, error = function(e) NULL)
        
        if(!is.null(results)) tables[[section]] <- results
        
      } else if(section == "R-SQUARE") {
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
  if(is.null(table) || nrow(table) == 0) return(NULL)
  
  col_names <- names(table)
  formatted_table <- as.data.frame(matrix(nrow = 0, ncol = length(col_names)))
  names(formatted_table) <- col_names
  
  current_header <- ""
  for(i in 1:nrow(table)) {
    row <- table[i,]
    
    if(length(row$Parameter) > 0 && 
       grepl("BY$|ON$|WITH$|Means$|Intercepts$|Variances$", row$Parameter)) {
      current_header <- trimws(row$Parameter)
      new_row <- row
      new_row$Parameter <- current_header
      new_row[2:ncol(new_row)] <- NA
      formatted_table <- rbind(formatted_table, new_row)
    } else {
      new_row <- row
      if(length(new_row$Parameter) > 0) {
        new_row$Parameter <- sprintf("    %s", trimws(row$Parameter))
      }
      formatted_table <- rbind(formatted_table, new_row)
    }
  }
  
  char_cols <- sapply(formatted_table, is.character)
  formatted_table[char_cols][is.na(formatted_table[char_cols])] <- ""
  
  numeric_cols <- sapply(formatted_table, is.numeric)
  formatted_table[numeric_cols] <- lapply(formatted_table[numeric_cols], function(x) {
    x[is.na(x)] <- ""
    if(is.numeric(x)) round(x, 3) else x
  })
  
  return(formatted_table)
}

extract_indirect_effects <- function(tables) {
  unst <- tables[["MODEL RESULTS New/Additional Parameters"]]
  if(is.null(unst) || nrow(unst) == 0) return(NULL)
  
  ind_unst <- unst[grepl("^\\s*IND\\s*$", unst$Parameter), ]
  if(nrow(ind_unst) == 0) return(NULL)
  
  ci <- tables[["CONFIDENCE INTERVALS OF MODEL RESULTS"]]
  ind_ci <- if(!is.null(ci)) ci[grepl("^\\s*IND\\s*$", ci$Parameter), ] else NULL
  
  data.frame(
    Effect = "Indirect Effect (IND)",
    Estimate = ind_unst$Estimate,
    SE = ind_unst$SE,
    `Est./S.E.` = ind_unst$`Est./S.E.`,
    `p-value` = ind_unst$P,
    `95% CI Lower` = if(!is.null(ind_ci)) ind_ci$Lower_2.5 else NA,
    `95% CI Upper` = if(!is.null(ind_ci)) ind_ci$Upper_2.5 else NA
  )
}

arse_mplus_block <- function(lines, section_title,
                              all_section_titles = c(
                                "MODEL RESULTS",
                                "STANDARDIZED MODEL RESULTS",
                                "STDYX Standardization",
                                "R-SQUARE",
                                "CONFIDENCE INTERVALS OF MODEL RESULTS",
                                "New/Additional Parameters"
                              )) {
  # Locate the start line for this section (exact match with optional trailing punctuation/spaces)
  start_idx <- grep(paste0("^", section_title, "\\s*[:]*\\s*$"), lines)
  if (length(start_idx) == 0) {
    # If not found exactly, optionally do a fallback substring search.
    # start_idx <- grep(section_title, lines, ignore.case=TRUE)
    # if (length(start_idx)==0) return(NULL)
    return(NULL)
  }
  
  start_idx <- start_idx[1]  # if multiple, take the first
  
  # Find next heading among known sections, after start_idx
  all_starts <- unlist(lapply(all_section_titles, function(tt)
    grep(paste0("^", tt, "\\s*[:]*\\s*$"), lines)
  ))
  all_starts <- all_starts[all_starts > start_idx]
  
  end_idx <- if (length(all_starts) == 0) length(lines) else (min(all_starts) - 1)
  if (start_idx >= end_idx) {
    return(NULL)
  }
  
  # Extract lines from just after the heading line up to end_idx
  block <- lines[(start_idx + 1):end_idx]
  # Remove blank lines
  block <- block[!grepl("^\\s*$", block)]
  if (!length(block)) {
    return(NULL)
  }
  
  # Parse with whitespace-based splitting
  df <- tryCatch(
    read.table(
      text = block,
      header = FALSE,
      fill = TRUE,             # ensures lines with fewer columns don't break
      strip.white = TRUE,
      blank.lines.skip = TRUE,
      stringsAsFactors = FALSE
    ),
    error = function(e) NULL
  )
  
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }
  
  # Give columns a consistent set of names
  colnames(df) <- paste0("Col", seq_len(ncol(df)))
  df
}


rab_all_IND_from_mplus <- function(file_path) {
  lines <- readLines(file_path)
  
  # Parse New/Additional Parameters block
  new_params_df <- parse_mplus_block(lines, "New/Additional Parameters")
  
  # Parse Confidence Intervals block
  ci_df <- parse_mplus_block(lines, "CONFIDENCE INTERVALS OF MODEL RESULTS")
  
  # If both are NULL, nothing to do
  blocks <- list(new_params_df, ci_df)
  blocks <- Filter(Negate(is.null), blocks)  # remove NULL entries
  if (!length(blocks)) {
    message("No relevant blocks found in the file.")
    return(NULL)
  }
  
  # Ensure all blocks have same number of columns
  max_cols <- max(sapply(blocks, ncol))
  blocks_padded <- lapply(blocks, function(df) {
    current_cols <- ncol(df)
    if (current_cols < max_cols) {
      # pad extra columns with NA
      needed <- max_cols - current_cols
      df <- cbind(df, matrix(NA, nrow = nrow(df), ncol = needed))
    }
    # rename columns to unify
    colnames(df) <- paste0("Col", seq_len(max_cols))
    df
  })
  
  # Combine them
  combined <- do.call(rbind, blocks_padded)
  
  # Filter for rows with 'IND' in the first column (trim whitespace first)
  combined$Col1 <- trimws(combined$Col1)
  ind_rows <- combined[combined$Col1 == "IND", , drop = FALSE]
  
  if (!nrow(ind_rows)) {
    message("No 'IND' row found in either block.")
    return(NULL)
  }
  
  # Name the first 5 columns to typical Mplus pattern: 
  #   (Parameter, Estimate, SE, Est.S.E., pval),
  # remaining columns become Extra6, Extra7, etc.
  n_cols <- ncol(ind_rows)
  base_names <- c("Parameter", "Estimate", "SE", "Est.S.E.", "pval",
                  paste0("Extra", 6:50))  # enough spares
  colnames(ind_rows) <- base_names[seq_len(n_cols)]
  
  # Return the rows for IND
  ind_rows
}


grab_IND_from_mplus <- function(file_path) {
  # Read the file
  lines <- readLines(file_path)
  
  # Find ALL "New/Additional Parameters" lines
  param_lines <- which(grepl("^New/Additional Parameters", lines))
  if (length(param_lines) == 0) return(NULL)
  
  # Get the first occurrence (regular estimates)
  ind_line <- lines[param_lines[1] + 1]
  
  # Split the values and create the data frame
  values <- strsplit(trimws(ind_line), "\\s+")[[1]]
  data.frame(
    Parameter = values[1],
    Estimate = as.numeric(values[2]),
    SE = as.numeric(values[3]),
    Est.SE = as.numeric(values[4]),
    pval = as.numeric(values[5])
  )
}

grab_IND_CI_from_mplus <- function(file_path) {
  # Read the file
  lines <- readLines(file_path)
  
  # Find ALL "New/Additional Parameters" lines
  param_lines <- which(grepl("^New/Additional Parameters", lines))
  if (length(param_lines) < 2) return(NULL)  # Need at least 2 occurrences
  
  # Get the second occurrence (confidence intervals)
  ind_line <- lines[param_lines[2] + 1]
  
  # Split the values and create the data frame
  values <- strsplit(trimws(ind_line), "\\s+")[[1]]
  data.frame(
    Parameter = values[1],
    CI_lower_0.5 = as.numeric(values[2]),     # -0.026
    CI_lower_2.5 = as.numeric(values[3]),     # -0.016
    CI_lower_5 = as.numeric(values[4]),       # -0.012
    Estimate = as.numeric(values[5]),         # -0.002
    CI_upper_5 = as.numeric(values[6]),       #  0.006
    CI_upper_2.5 = as.numeric(values[7]),     #  0.009
    CI_upper_0.5 = as.numeric(values[8])      #  0.016
  )
}

grab_all_IND_from_mplus <- function(file_path) {
  list(
    estimates = grab_IND_from_mplus(file_path),
    confidence_intervals = grab_IND_CI_from_mplus(file_path)
  )
}

create_ind_table <- function(file_path) {
  ind_data <- grab_all_IND_from_mplus(file_path)
  if(is.null(ind_data)) return(NULL)
  
  data.frame(
    Effect = "Indirect Effect (IND)",
    Estimate = ind_data$estimates$Estimate,
    SE = ind_data$estimates$SE,
    `Est./S.E.` = ind_data$estimates$Est.SE,
    `p-value` = ind_data$estimates$pval,
    `95% CI Lower` = ind_data$confidence_intervals$CI_lower_2.5,
    `95% CI Upper` = ind_data$confidence_intervals$CI_upper_2.5
  )
}

write_mplus_tables_to_docx <- function(tables, output_file, mplus_file = NULL) {
  if(!requireNamespace("officer", quietly = TRUE) ||
     !requireNamespace("flextable", quietly = TRUE)) {
    stop("Required packages 'officer' and 'flextable' must be installed")
  }
  
  doc <- officer::read_docx()
  
  # Create indirect effects table
  ind_table <- create_ind_table(mplus_file)
  
  # Add indirect effects table if present
  if(!is.null(ind_table) && nrow(ind_table) > 0) {
    doc <- officer::body_add_par(doc, "Indirect Effects (unstandardized)", style = "heading 1")
    ft <- flextable::flextable(ind_table)
    ft <- flextable::bold(ft, bold = TRUE, part = "header")
    ft <- flextable::colformat_double(ft,
                                    digits = 3,
                                    na_str = "")
    ft <- flextable::autofit(ft)
    doc <- flextable::body_add_flextable(doc, ft)
    doc <- officer::body_add_par(doc, "")  # Add blank line
  }
  
  # Add each remaining table to the document
  for(table_name in names(tables)) {
    # Skip New/Additional Parameters since we've handled IND separately
    if(!grepl("New/Additional Parameters$", table_name)) {
      formatted <- format_mplus_table(tables[[table_name]], title = table_name)
      if(is.null(formatted) || nrow(formatted) == 0) next
      
      # Add section title
      doc <- officer::body_add_par(doc, table_name, style = "heading 1")
      
      # Create and add table
      ft <- flextable::flextable(formatted)
      ft <- flextable::bold(ft, bold = TRUE, part = "header")
      ft <- flextable::autofit(ft)
      doc <- flextable::body_add_flextable(doc, ft)
      doc <- officer::body_add_par(doc, "")  # Add blank line
    }
  }
  
  # Save the document
  print(doc, target = output_file)
}

# Example usage:
# tables <- extract_mplus_tables("1.out")
# write_mplus_tables_to_docx(tables, "results.docx", mplus_file = "1.out")

