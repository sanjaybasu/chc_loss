# Required Libraries
library(tidyverse)
library(tidycensus)
library(foreign)
library(grf)
library(boot)
library(kableExtra)
library(readxl)
library(censusapi)
library(zipcodeR)
library(tidygeocoder)
library(tidyr)
library(purrr)
library(dplyr)
library(lmtest)
library(sandwich)
library(MatchIt)
library(cobalt)
library(ggplot2)
library(broom)
library(tableone)
library(writexl)
library(urbnmapr)
library(viridis)

# Set your Census API key
census_api_key("[INSERT YOUR PERSONAL KEY HERE", install = TRUE, overwrite=T)
readRenviron("~/.Renviron")

get_county_data <- function(year) {
  if (year == 2011) {
    variables <- c(
      education_total = "B99151_001",
      education_bachelors_male = "B15002_015",
      education_bachelors_female = "B15002_032",
      education_masters_male = "B15002_016",
      education_masters_female = "B15002_033",
      education_professional_male = "B15002_017",
      education_professional_female = "B15002_034",
      education_doctorate_male = "B15002_018",
      education_doctorate_female = "B15002_035",
      median_income = "B19013_001",
      children = "DP05_0005P",
      elderly = "DP05_0024P",
      female = "DP05_0002P",
      total_population = "B01003_001"
    )
  } else {
    variables <- c(
      education_total = "B15003_001",
      education_bachelors = "B15003_022",
      education_masters = "B15003_023",
      education_professional = "B15003_024",
      education_doctorate = "B15003_025",
      median_income = "B19013_001",
      children = "DP05_0005P",
      elderly = "DP05_0024P",
      female = "DP05_0002P",
      total_population = "B01003_001"
    )
  }
  
  acs_data <- get_acs(
    geography = "county",
    variables = variables,
    year = year,
    survey = "acs5"
  )
  
  if (year == 2011) {
    acs_data <- acs_data %>%
      pivot_wider(
        id_cols = c(GEOID, NAME),
        names_from = variable,
        values_from = estimate,
        values_fn = list(estimate = sum)
      ) %>%
      mutate(
        education_bachelors = education_bachelors_male + education_bachelors_female,
        education_masters = education_masters_male + education_masters_female,
        education_professional = education_professional_male + education_professional_female,
        education_doctorate = education_doctorate_male + education_doctorate_female,
        education_bachelors_plus = education_bachelors + education_masters + education_professional + education_doctorate,
        education_percent = (education_bachelors_plus / education_total) * 100,
        year = as.integer(year)
      ) %>%
      select(GEOID, NAME, year, education_percent, median_income, children, elderly, female, total_population)
  } else {
    acs_data <- acs_data %>%
      pivot_wider(
        id_cols = c(GEOID, NAME),
        names_from = variable,
        values_from = estimate,
        values_fn = list(estimate = sum)
      ) %>%
      mutate(
        education_bachelors_plus = education_bachelors + education_masters + education_professional + education_doctorate,
        education_percent = (education_bachelors_plus / education_total) * 100,
        year = as.integer(year)
      ) %>%
      select(GEOID, NAME, year, education_percent, median_income, children, elderly, female, total_population)
  }
  
  return(acs_data)
}

# Get ACS data for 2011-2019
years <- 2011:2019
acs <- map_dfr(years, get_county_data)


# Load required libraries
library(tidyverse)
library(readxl)

# Function to download and read CHC data
download_and_read_chc_data <- function(year) {
  # Create the URL for the file
  url <- paste0("https://www.hrsa.gov/sites/default/files/hrsa/foia/h80-", year, ".xlsx")
  
  # Create a temporary file path
  temp_file <- tempfile(fileext = ".xlsx")
  
  # Download the file
  download.file(url, destfile = temp_file, mode = "wb")
  
  # Determine the correct sheet name based on the year
  sheet_name <- if (year == 2011) "tblGranteeSiteInfo" else "HealthCenterSiteInfo"
  
  # Read the Excel file
  chc_data <- read_excel(temp_file, sheet = sheet_name)
  
  # Check if the "SiteZIPCode" column exists, otherwise use "Site ZIP Code"
  zip_code_col <- if ("SiteZIPCode" %in% names(chc_data)) {
    "SiteZIPCode"
  } else if ("Site ZIP Code" %in% names(chc_data)) {
    "Site ZIP Code"
  } else {
    # If neither column is found, print column names and stop execution
    print(names(chc_data))
    stop("ZIP code column not found in the data")
  }
  
  chc_data <- chc_data %>%
    select(all_of(zip_code_col)) %>%
    mutate(
      site = 1,
      Year = year,
      SiteZIPCode = str_sub(!!sym(zip_code_col), 1, 5)  # Extract first 5 characters of ZIP code
    )
  
  # Return the CHC data
  return(chc_data)
}


# Load CHC data
chc_df <- map_dfr(years, download_and_read_chc_data)

# Combine the two ZIP code columns into a single "SiteZIPCode" column
chc_df <- chc_df %>%
  mutate(SiteZIPCode = coalesce(SiteZIPCode, `Site ZIP Code`)) %>%
  select(-`Site ZIP Code`)


test = chc_df %>%
  group_by(Year) %>%
  summarise(sites = sum(site, na.rm=T))
test

# calculate the delta between each row and the row before it in 'sites' in the test df
test$delta = c(0, diff(test$sites))

# Load ZIP to County FIPS crosswalk
# You can download this file from: https://www.huduser.gov/portal/datasets/usps_crosswalk.html
zip_to_county <- read_csv("~/Downloads/ZIP_COUNTY_032023.csv") %>%
  select(ZIP, COUNTY) %>%
  rename(SiteZIPCode = ZIP, county_fips = COUNTY)

# Join CHC data with ZIP to County crosswalk
chc_df <- chc_df %>%
  left_join(zip_to_county, by = "SiteZIPCode")

# Aggregate CHC sites by county and year
chc_county <- chc_df %>%
  group_by(county_fips, Year) %>%
  summarise(sites = sum(site, na.rm = TRUE), .groups = "drop")



# Load and process mortality data for multiple types (county level)
load_mortality_data <- function(years, disease_code) {
  m_df <- bind_rows(
    lapply(years, function(year) {
      file_path <- paste0("~/Downloads/IHME_USA_COD_COUNTY_RACE_ETHN_2000_2019_MX_", disease_code, "_BOTH/IHME_USA_COD_COUNTY_RACE_ETHN_2000_2019_MX_", year, "_", disease_code, "_BOTH_Y2023M06D12.CSV")
      
      read.csv(file_path) %>%
        filter(!is.na(fips),
               race_name %in% c("Total", "Latino", "Black", "White", "AIAN", "API"),
               age_name == "Age-standardized",
               year == year) %>%
        select(fips, year, val, race_name)
    })
  ) %>%
    filter(!is.na(val)) %>%
    mutate(fips = str_pad(fips, 5, pad = "0", side = "left"))
  
  return(m_df)
}

# Load mortality data for all types
disease_codes <- c("ALL", "SUBS", "RESP", "NEO", "MATER_NEONAT", "INFECT", "DIAB_CKD", "CVD")
disease_names <- c("Total", "Substance Use", "Respiratory", "Cancer", "Maternal/Neonatal", "Infectious", "Diabetes/CKD", "Cardiovascular")

mortality_data <- map2_dfr(disease_codes, disease_names, function(code, name) {
  data <- load_mortality_data(years, code)
  data$mortality_type <- name
  return(data)
})


# Load County Health Rankings data
chr_data <- read.csv("~/Downloads/chr_trends_csv_2022.csv") %>%
  mutate(
    year = as.numeric(substr(yearspan, 1, 4)),
    fips = paste0(statecode, countycode)
  ) %>%
  select(year, measurename, fips, rawvalue) %>%
  filter(measurename %in% c('Uninsured', 'Adult obesity', 'Unemployment rate', 'Children in poverty', 'Violent crime rate', 'Air pollution - particulate matter', 'Primary care physicians', 'School funding', 'Sexually transmitted infections')) %>%
  mutate(fips = str_pad(fips, 5, pad = "0", side = "left"))

# Function to safely convert to numeric
safe_as_numeric <- function(x) {
  as.numeric(ifelse(x %in% c("", "NA", "N/A"), NA, x))
}

# Process CHR data
chr_data_processed <- chr_data %>%
  mutate(rawvalue = safe_as_numeric(rawvalue)) %>%
  group_by(fips, year, measurename) %>%
  summarise(rawvalue = mean(rawvalue, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = measurename, values_from = rawvalue)

# Impute missing values
chr_data_imputed <- chr_data_processed %>%
  group_by(fips) %>%
  fill(everything(), .direction = "downup") %>%
  ungroup() %>%
  mutate(across(-c(fips, year), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Print summary to check for NAs
print(summary(chr_data_imputed))

# If there are still NAs, you might want to investigate which columns/rows are affected
na_counts <- colSums(is.na(chr_data_imputed))
print(na_counts)

# If necessary, remove rows with any remaining NAs
chr_data_final <- chr_data_imputed %>%
  na.omit()

# Print final summary
print(summary(chr_data_final))


# limit the data to years 2011 through 2019
chr_data_final <- chr_data_final %>%
  filter(year %in% years)


# Merge all datasets
merged_data <- acs %>%
  left_join(chc_county, by = c("GEOID" = "county_fips", "year" = "Year")) %>%
  left_join(mortality_data, by = c("GEOID" = "fips", "year")) %>%
  left_join(chr_data_final, by = c("GEOID" = "fips", "year")) %>%
  mutate(sites = replace_na(sites, 0)) %>%
  na.omit()
# save the merged data as a csv
write.csv(merged_data, "~/Downloads/merged_data.csv", row.names = FALSE)



# main analysis

# clear the environment
rm(list = ls())
merged_data = read.csv("~/Downloads/merged_data.csv") %>%
  mutate(fips = as.factor(GEOID))



# Step 1: Prepare the data
prepare_data <- function(data) {
  # Rename variables
  data <- data %>%
    rename(
      air_pollution = `Air.pollution...particulate.matter`,
      child_poverty = `Children.in.poverty`,
      unemployment = `Unemployment.rate`,
      violent_crime = `Violent.crime.rate`,
      school_funding = `School.funding`
    )
  
  # Calculate CHC change between 2013 and 2014
  chc_change <- data %>%
    filter(year %in% c(2013, 2014)) %>%
    select(GEOID, year, sites) %>%
    group_by(GEOID) %>%
    summarise(
      sites_2013 = mean(as.numeric(sites[year == 2013]), na.rm = TRUE),
      sites_2014 = mean(as.numeric(sites[year == 2014]), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      chc_change_2013_2014 = sites_2014 - sites_2013,
      chc_loss = as.integer(chc_change_2013_2014 < 0)
    )
  
  # Prepare the main dataset
  prepared_data <- data %>%
    group_by(GEOID) %>%
    mutate(
      across(c(education_percent, median_income, children, elderly, female, 
               total_population, air_pollution, child_poverty, 
               unemployment, violent_crime, Uninsured, school_funding),
             list(
               baseline = ~first(as.numeric(.x[year == 2012])),
               change = ~last(as.numeric(.x[year == 2019])) - first(as.numeric(.x[year == 2012]))
             )),
      baseline_sites = first(as.numeric(sites[year == 2012])),
      baseline_mortality = first(as.numeric(val[year == 2012])),
      val = as.numeric(val) * 100000  # Convert mortality rate to per 100,000 people
    ) %>%
    ungroup() %>%
    # Join with chc_change data
    left_join(chc_change, by = "GEOID") %>%
    # Remove rows with NA in important columns
    filter(!is.na(chc_loss), !is.na(val)) %>%
    # Convert character columns to factors with explicit levels
    mutate(
      mortality_type = factor(mortality_type, levels = c("Cancer", "Cardiovascular", "Diabetes/CKD", "Infectious", "Maternal/Neonatal", "Respiratory", "Substance Use", "Total")),
      race_name = factor(race_name, levels = c("AIAN", "API", "Black", "Latino", "Total", "White"))
    )
  
  return(prepared_data)
}

# Step 2: Perform Propensity Score Matching
perform_psm_matching <- function(data, covariates) {
  ps_formula <- as.formula(paste("chc_loss ~", paste(paste0(covariates, "_baseline"), collapse = " + ")))
  
  match_obj <- matchit(ps_formula,
                       data = data %>% filter(year == 2013),  # Match based on 2013 data
                       method = "nearest",
                       distance = "glm",
                       ratio = 3,  # One-to-three matching
                       caliper = 0.2)
  
  return(match_obj)
}

# Step 3: Analyze matched data
analyze_matched_data <- function(data, match_obj, outcome_var) {
  matched_data <- match.data(match_obj)
  
  # Merge matched data with all years
  full_matched_data <- inner_join(data, matched_data %>% select(GEOID, weights, subclass), by = "GEOID")
  
  # Check if we have enough matched units
  if(nrow(full_matched_data) < 100) {  # Increased threshold due to multiple years
    warning("Insufficient matched units. Skipping analysis.")
    return(NULL)
  }
  
  # Regression model including baseline sites and mortality, with year as a factor
  reg_model <- lm(paste(outcome_var, "~ chc_loss * factor(year) + baseline_mortality + baseline_sites"), 
                  data = full_matched_data, weights = weights)
  robust_se <- coeftest(reg_model, vcov = vcovHC(reg_model, type = "HC3"))
  
  return(tidy(robust_se))
}

# Main analysis function
chc_impact_analysis <- function(data, covariates, outcome_var) {
  # Perform PSM matching
  match_result <- perform_psm_matching(data, covariates)
  
  # Analyze matched data
  analysis_result <- analyze_matched_data(data, match_result, outcome_var)
  
  return(list(match_result = match_result, analysis_result = analysis_result))
}

# Create Table 1
create_table_one <- function(data, covariates) {
  # Prepare data for Table 1
  table_data <- data %>%
    filter(year == 2013) %>%  # Use only 2013 data for baseline
    select(GEOID, chc_loss, ends_with("_baseline"), ends_with("_change"))
  
  # Define variables for the table
  baseline_vars <- paste0(covariates, "_baseline")
  change_vars <- paste0(covariates, "_change")
  all_vars <- c(baseline_vars, change_vars)
  
  # Create Table 1
  table1 <- CreateTableOne(vars = all_vars, 
                           strata = "chc_loss", 
                           data = table_data, 
                           test = TRUE,
                           testNormal = oneway.test,
                           testNonNormal = wilcox.test,
                           testFactor = chisq.test,
                           smd = TRUE)
  
  # Modify the table
  table1_modified <- print(table1, 
                           printToggle = FALSE, 
                           showAllLevels = TRUE,
                           nonnormal = change_vars,  # Treat change variables as non-normal
                           catDigits = 1, 
                           contDigits = 2, 
                           pDigits = 3,
                           smdDigits = 2)
  
  # Add row names as a column
  table1_modified <- cbind(Variable = rownames(table1_modified), table1_modified)
  rownames(table1_modified) <- NULL
  
  # Add total N (unique counties) to the table
  total_n <- data %>% 
    filter(year == 2013) %>% 
    summarise(n = n_distinct(GEOID)) %>% 
    pull(n)
  
  n_by_group <- data %>%
    filter(year == 2013) %>%
    group_by(chc_loss) %>%
    summarise(n = n_distinct(GEOID)) %>%
    pivot_wider(names_from = chc_loss, values_from = n, names_prefix = "n_")
  
  n_row <- c("N (unique counties)", total_n, n_by_group$n_0, n_by_group$n_1, "", "")
  table1_modified <- rbind(n_row, table1_modified)
  
  return(table1_modified)
}

# Create summary table
create_summary_table <- function(results_list) {
  summary_data <- lapply(names(results_list), function(name) {
    x <- results_list[[name]]
    if (is.null(x) || is.null(x$analysis_result)) return(NULL)
    result <- x$analysis_result %>% filter(grepl("chc_loss", term))
    data.frame(
      Analysis = name,
      Term = result$term,
      Estimate = result$estimate,
      CI_Lower = result$estimate - 1.96 * result$std.error,
      CI_Upper = result$estimate + 1.96 * result$std.error,
      P_Value = result$p.value
    )
  })
  
  summary_table <- do.call(rbind, summary_data)
  rownames(summary_table) <- NULL
  return(summary_table)
}

# Create map of CHC site changes
create_chc_change_map <- function(data) {
  changes_data <- data %>%
    filter(year %in% c(2013, 2014)) %>%
    select(GEOID, year, sites) %>%
    distinct() %>% 
    pivot_wider(names_from = year, values_from = sites) %>%
    mutate(change_sites = `2014` - `2013`,
           chc_change_category = case_when(
             change_sites > 0 ~ "Increase",
             change_sites < 0 ~ "Decrease",
             TRUE ~ "No Change"
           ))
  
  map_data <- left_join(counties, changes_data, by = c("county_fips" = "GEOID"))
  map_data$change_sites[is.na(map_data$change_sites)] <- 0
  
  ggplot(map_data, aes(long, lat, group = group, fill = change_sites)) +
    geom_polygon(color = "#ffffff", size = 0.05) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    scale_fill_viridis(option = "magma", name = "Change in CHC Sites",
                       limits = c(min(map_data$change_sites, na.rm = TRUE),
                                  max(map_data$change_sites, na.rm = TRUE)),
                       oob = scales::squish) +
    labs(title = "Change in CHC Sites by County (2013-2014)",
         subtitle = "Losses shown in darker colors, gains in lighter colors") +
    theme_minimal() +
    theme(legend.position = "bottom",
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank())
}

# Print balance statistics
print_balance_stats <- function(match_result) {
  print(summary(match_result, standardize = TRUE))
}

# Improved results presentation
present_results <- function(result) {
  if (!is.null(result) && !is.null(result$analysis_result)) {
    cat("Analysis Results:\n")
    print(result$analysis_result)
    
    cat("\nBalance Statistics:\n")
    print_balance_stats(result$match_result)
    
    cat("\nCreating balance plot...\n")
    love.plot(result$match_result, threshold = 0.1)
  } else {
    cat("Analysis could not be completed due to errors or insufficient matches.\n")
  }
}

# Function to run subgroup analyses
run_subgroup_analysis <- function(data, subgroup_vars) {
  subgroup_combinations <- expand.grid(lapply(subgroup_vars, function(var) unique(data[[var]])))
  
  results <- lapply(1:nrow(subgroup_combinations), function(i) {
    subgroup <- subgroup_combinations[i,]
    subgroup_data <- data
    for (var in names(subgroup)) {
      subgroup_data <- subgroup_data %>% filter(!!sym(var) == subgroup[[var]])
    }
    result <- tryCatch({
      chc_impact_analysis(subgroup_data, covariates, "val")
    }, error = function(e) {
      cat("Error in subgroup analysis for", paste(names(subgroup), subgroup, collapse = ", "), ":", conditionMessage(e), "\n")
      return(NULL)
    })
    setNames(list(result), paste(subgroup, collapse = "_"))
  })
  
  results[!sapply(results, is.null)]
}

covariates <- c("education_percent", "median_income", "children", "elderly", "female", 
                "total_population", "air_pollution", "child_poverty", 
                "unemployment", "violent_crime", "Uninsured", "school_funding")

# Prepare data
prepared_data <- prepare_data(merged_data)

# Print summary to check the prepared data
summary(prepared_data)
print(table(prepared_data$chc_loss))

# Create Table 1
create_table_one <- function(data, covariates) {
  # Prepare data for Table 1
  table_data <- data %>%
    filter(year == 2013) %>%  # Use only 2013 data for baseline
    select(GEOID, chc_loss, ends_with("_baseline"), ends_with("_change"))
  
  # Define variables for the table
  baseline_vars <- paste0(covariates, "_baseline")
  change_vars <- paste0(covariates, "_change")
  all_vars <- c(baseline_vars, change_vars)
  
  # Create Table 1
  table1 <- CreateTableOne(vars = all_vars, 
                           strata = "chc_loss", 
                           data = table_data, 
                           test = TRUE,
                           testNormal = oneway.test,
                           testNonNormal = wilcox.test,
                           smd = TRUE)
  
  # Modify the table
  table1_modified <- print(table1, 
                           printToggle = FALSE, 
                           showAllLevels = TRUE,
                           nonnormal = change_vars,  # Treat change variables as non-normal
                           catDigits = 1, 
                           contDigits = 2, 
                           pDigits = 3,
                           smdDigits = 2)
  
  # Add row names as a column
  table1_modified <- cbind(Variable = rownames(table1_modified), table1_modified)
  rownames(table1_modified) <- NULL
  
  # Add total N (unique counties) to the table
  total_n <- data %>% 
    filter(year == 2013) %>% 
    summarise(n = n_distinct(GEOID)) %>% 
    pull(n)
  
  n_by_group <- data %>%
    filter(year == 2013) %>%
    group_by(chc_loss) %>%
    summarise(n = n_distinct(GEOID)) %>%
    pivot_wider(names_from = chc_loss, values_from = n, names_prefix = "n_")
  
  n_row <- c("N (unique counties)", total_n, n_by_group$n_0, n_by_group$n_1, "", "")
  table1_modified <- rbind(n_row, table1_modified)
  
  return(table1_modified)
}

# Usage in main script
# Prepare data
prepared_data <- prepare_data(merged_data)

# Create Table 1
table_one <- create_table_one(prepared_data, covariates)

# Save Table 1 as CSV (easily importable to Google Sheets)
write.csv(table_one, "table_one_comprehensive.csv", row.names = FALSE)

# If you want to create an HTML version (which can be copied into Google Docs)
library(kableExtra)
table_one_html <- kable(table_one, format = "html", caption = "Table 1: Baseline Characteristics and Changes by CHC Loss Status") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
save_kable(table_one_html, file = "table_one_comprehensive.html")

cat("\nTable 1 has been saved as 'table_one_comprehensive.csv' and 'table_one_comprehensive.html'.\n")

# Create and save CHC change map
chc_change_map <- create_chc_change_map(merged_data)
ggsave("chc_sites_change_map_2013_2015.png", chc_change_map, width = 12, height = 8, dpi = 300)

# For overall mortality
overall_result <- tryCatch({
  chc_impact_analysis(prepared_data %>% filter(mortality_type == "Total" & race_name == "Total"), covariates, "val")
}, error = function(e) {
  cat("Error in overall analysis:", conditionMessage(e), "\n")
  return(NULL)
})

# Present overall results
cat("\nOverall Analysis:\n")
present_results(overall_result)

# Function to run subgroup analyses
run_subgroup_analysis <- function(data, subgroup_vars) {
  # Get unique combinations of subgroup variables
  subgroup_combinations <- data %>%
    select(all_of(subgroup_vars)) %>%
    distinct()
  
  results <- list()
  
  for (i in 1:nrow(subgroup_combinations)) {
    subgroup <- subgroup_combinations[i, ]
    subgroup_data <- data
    
    for (var in subgroup_vars) {
      subgroup_data <- subgroup_data %>% 
        filter(!!sym(var) == subgroup[[var]])
    }
    
    subgroup_name <- paste(subgroup_vars, subgroup, sep = "_", collapse = "_")
    
    result <- tryCatch({
      chc_impact_analysis(subgroup_data, covariates, "val")
    }, error = function(e) {
      cat("Error in subgroup analysis for", subgroup_name, ":", conditionMessage(e), "\n")
      return(NULL)
    })
    
    if (!is.null(result)) {
      results[[subgroup_name]] <- result
    }
  }
  
  return(results)
}

# Usage in main script
# Run subgroup analyses
subgroup_results <- run_subgroup_analysis(prepared_data, c("mortality_type", "race_name"))

# Present subgroup results
for (name in names(subgroup_results)) {
  cat("\nSubgroup Analysis:", name, "\n")
  present_results(subgroup_results[[name]])
}

# Combine all results
all_results <- c(list(Overall = overall_result), subgroup_results)

# Create and save summary table
summary_table <- create_summary_table(all_results)
write_xlsx(summary_table, "chc_loss_effect_summary_longitudinal.xlsx")

# Save overall balance plot
png("balance_plot_overall.png", width = 1200, height = 800, res = 100)
love.plot(overall_result$match_result, threshold = 0.1)
dev.off()

cat("\nAnalysis complete. Check the following files:\n")
cat("1. table_one_comprehensive.csv - Descriptive statistics for 2013 and changes\n")
cat("2. table_one_comprehensive.html - HTML version of Table 1\n")
cat("3. chc_sites_change_map_2013_2015.png - Map of CHC site changes\n")
cat("4. chc_loss_effect_summary_longitudinal.xlsx - Summary of all analyses\n")
cat("5. balance_plot_overall.png - Balance plot for overall analysis\n")






# Load required libraries
library(tidyverse)
library(readxl)
library(ggplot2)
library(patchwork)

# Read the data
data <- read_excel("chc_loss_effect_summary_longitudinal.xlsx")

# Define correct orders
race_order <- c("AIAN", "API", "Black", "Latino", "Total", "White")
disease_order <- c("Cancer", "Cardiovascular", "Diabetes/CKD", "Infectious", "Maternal/Neonatal", "Respiratory", "Substance Use", "Total")

# Function to create a basic forest plot
create_forest_plot <- function(data, title, y_label = "") {
  ggplot(data, aes(y = Category, x = Estimate, xmin = CI_Lower, xmax = CI_Upper)) +
    geom_point() +
    geom_errorbarh(height = 0.2) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(title = title,
         x = "Change in Mortality Rate per 100,000 Population",
         y = y_label) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_text(size = 10))
}

# Prepare data for main text figure (total and disease subtypes)
main_text_data <- data %>%
  filter(grepl("mortality_type_", Analysis), grepl("race_name_5", Analysis), Term == "chc_loss") %>%
  mutate(Category = case_when(
    grepl("mortality_type_1", Analysis) ~ "Cancer",
    grepl("mortality_type_2", Analysis) ~ "Cardiovascular",
    grepl("mortality_type_3", Analysis) ~ "Diabetes/CKD",
    grepl("mortality_type_4", Analysis) ~ "Infectious",
    grepl("mortality_type_5", Analysis) ~ "Maternal/Neonatal",
    grepl("mortality_type_6", Analysis) ~ "Respiratory",
    grepl("mortality_type_7", Analysis) ~ "Substance Use",
    grepl("mortality_type_8", Analysis) ~ "Total"
  )) %>%
  filter(!is.na(Category)) %>%
  mutate(Category = factor(Category, levels = disease_order))

# Create main text figure
main_text_plot <- create_forest_plot(main_text_data, "Association Between CHC Site Loss and Mortality Rates by Cause of Death", "Cause of Death")

# Prepare data for appendix figure (total and race subtypes)
appendix_data <- data %>%
  filter(grepl("race_name_", Analysis), Term == "chc_loss") %>%
  mutate(Category = case_when(
    Analysis == "mortality_type_8_race_name_1" ~ "AIAN",
    Analysis == "mortality_type_8_race_name_2" ~ "API",
    Analysis == "mortality_type_8_race_name_3" ~ "Black",
    Analysis == "mortality_type_8_race_name_4" ~ "Latino",
    Analysis == "mortality_type_8_race_name_5" ~ "Total",
    Analysis == "mortality_type_8_race_name_6" ~ "White"
  )) %>%
  filter(!is.na(Category)) %>%
  mutate(Category = factor(Category, levels = race_order))

# Create appendix figure
appendix_plot <- create_forest_plot(appendix_data, "Association Between CHC Site Loss and Mortality Rates by Race/Ethnicity", "Race/Ethnicity")

# Save the main text figure
ggsave("main_text_figure.png", main_text_plot, width = 10, height = 6)

# Save the appendix figure
ggsave("appendix_figure.png", appendix_plot, width = 10, height = 6)

# Display the figures
print(main_text_plot)
print(appendix_plot)



# Load required libraries
library(tidyverse)
library(readxl)
library(ggplot2)

# Read the data
data <- read_csv("~/Downloads/merged_data.csv")

# Calculate CHC changes by county and year
chc_changes <- data %>%
  group_by(GEOID) %>%
  arrange(GEOID, year) %>%
  mutate(chc_change = sites - lag(sites),
         chc_loss = if_else(chc_change < 0, 1, 0),
         chc_gain = if_else(chc_change > 0, 1, 0)) %>%
  ungroup()

# Summarize changes by year
yearly_summary <- chc_changes %>%
  group_by(year) %>%
  summarise(
    counties_lost = sum(chc_loss, na.rm = TRUE),
    counties_gained = sum(chc_gain, na.rm = TRUE),
    net_change = sum(chc_change, na.rm = TRUE)
  ) %>%
  filter(year > min(year))  # Remove the first year as it won't have changes

# Plot CHC losses and gains by year
ggplot(yearly_summary, aes(x = year)) +
  geom_bar(aes(y = counties_lost, fill = "Lost"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = counties_gained, fill = "Gained"), stat = "identity", position = "dodge") +
  geom_line(aes(y = net_change, group = 1, color = "Net Change")) +
  geom_point(aes(y = net_change, color = "Net Change")) +
  scale_fill_manual(values = c("Lost" = "purple", "Gained" = "orange")) +
  scale_color_manual(values = c("Net Change" = "black")) +
  labs(title = "CHC Losses and Gains by Year",
       x = "Year",
       y = "Number of Counties",
       fill = "Change Type",
       color = "Net Change") +
  theme_minimal()

# Save the plot
ggsave("chc_changes_by_year.png", width = 10, height = 6)

# Create cohorts of counties that lost at least one CHC each year
cohorts <- chc_changes %>%
  filter(chc_loss == 1) %>%
  group_by(year) %>%
  summarise(counties_lost_chc = n_distinct(GEOID))

# Print cohort sizes
print(cohorts)


# Format the yearly_summary data
table_data <- yearly_summary %>%
  mutate(
    year = as.integer(year),
    net_change = ifelse(net_change > 0, paste0("+", net_change), as.character(net_change))
  ) %>%
  rename(
    Year = year,
    "Counties Lost CHCs" = counties_lost,
    "Counties Gained CHCs" = counties_gained,
    "Net Change in CHCs" = net_change
  )

# Create the table
table_output <- table_data %>%
  kbl(caption = "Yearly Summary of Community Health Center (CHC) Changes",
      align = c("c", "c", "c", "c"),
      format = "html") %>%
  kable_classic(full_width = FALSE, html_font = "Arial") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                font_size = 14) %>%
  add_header_above(c(" " = 1, "Number of Counties" = 2, " " = 1)) %>%
  footnote(general = "Net Change represents the overall increase or decrease in CHC sites across all counties.")

# Print the table in the R console
print(table_output)

# Save the table as an HTML file
save_kable(table_output, file = "yearly_chc_changes_summary.html")


# Load required libraries
library(tidyverse)
library(lmtest)
library(sandwich)
library(ggplot2)

match_result = overall_result$match_result
# Function to check parallel trends
check_parallel_trends <- function(data, match_obj, outcome_var, treatment_var, time_var, pre_treatment_years) {
  
  # Get matched data
  matched_data <- match.data(match_obj)
  
  # Merge matched data with all years
  full_matched_data <- inner_join(data, matched_data %>% select(GEOID, weights, subclass), by = "GEOID")
  
  # Filter data to pre-treatment period and total population
  pre_data <- full_matched_data %>%
    filter(year %in% pre_treatment_years,
           race_name == "Total",
           mortality_type == "Total")
  
  # Visualize trends
  trend_plot <- ggplot(pre_data, aes(x = year, y = !!sym(outcome_var), color = factor(!!sym(treatment_var)))) +
    stat_summary(fun = mean, geom = "line") +
    stat_summary(fun = mean, geom = "point") +
    labs(title = "Pre-treatment Trends in Mortality Rates",
         x = "Year",
         y = "Mortality Rate per 100,000",
         color = "CHC Loss Status") +
    theme_minimal()
  
  print(trend_plot)
  
  # Statistical test for parallel trends
  model <- lm(paste(outcome_var, "~", treatment_var, "*factor(", time_var, ")"), data = pre_data, weights = weights)
  robust_se <- coeftest(model, vcov = vcovHC(model, type = "HC3"))
  
  # Extract interaction terms
  interaction_terms <- robust_se[grep(paste0(treatment_var, ":factor\\(", time_var, "\\)"), rownames(robust_se)),]
  
  return(list(plot = trend_plot, test_results = interaction_terms))
}

# Define pre-treatment years
pre_treatment_years <- 2011:2014

# Run the check
parallel_trends_check <- check_parallel_trends(prepared_data, match_result, "val", "chc_loss", "year", pre_treatment_years)

# Print statistical test results
print(parallel_trends_check$test_results)

# Save the plot
ggsave("parallel_trends_plot.png", parallel_trends_check$plot, width = 10, height = 6)






# Load required additional libraries
library(gt)
library(dplyr)
library(tidyr)
library(stringr)

# Function to format results
format_results <- function(results_list) {
  formatted_data <- lapply(names(results_list), function(name) {
    x <- results_list[[name]]
    if (is.null(x) || is.null(x$analysis_result)) return(NULL)
    
    result <- x$analysis_result %>% 
      filter(term == "chc_loss") %>%  # Only include the overall effect
      mutate(
        Analysis = name,
        Year = "Overall",
        Estimate = sprintf("%.2f", estimate),
        CI = sprintf("(%.2f, %.2f)", estimate - 1.96 * std.error, estimate + 1.96 * std.error),
        P_Value = sprintf("%.3f", p.value)
      ) %>%
      select(Analysis, Year, Estimate, CI, P_Value)
  })
  
  do.call(rbind, formatted_data) %>%
    separate(Analysis, c("Mortality_Type", "Race_Name"), sep = "_race_name_", fill = "right") %>%
    mutate(
      Mortality_Type = case_when(
        Mortality_Type == "mortality_type_1" ~ "Cancer",
        Mortality_Type == "mortality_type_2" ~ "CVD",
        Mortality_Type == "mortality_type_3" ~ "Diabetes/CKD",
        Mortality_Type == "mortality_type_4" ~ "Infectious",
        Mortality_Type == "mortality_type_5" ~ "Maternal/Neonatal",
        Mortality_Type == "mortality_type_6" ~ "Respiratory",
        Mortality_Type == "mortality_type_7" ~ "Substance Use",
        Mortality_Type == "mortality_type_8" ~ "Total",
        TRUE ~ Mortality_Type
      ),
      Race_Name = case_when(
        Race_Name == "1" ~ "American Indian/Alaska Native",
        Race_Name == "2" ~ "Asian/Pacific Islander",
        Race_Name == "3" ~ "Black",
        Race_Name == "4" ~ "Latino",
        Race_Name == "5" ~ "Total",
        Race_Name == "6" ~ "White",
        is.na(Race_Name) ~ "Total",  # Assign 'Total' to NA values
        TRUE ~ Race_Name
      )
    ) %>%
    select(-Year)  # Remove Year column as it's no longer needed
}

# Create summary table
create_summary_table <- function(results_list) {
  formatted_data <- format_results(results_list)
  
  # Create a table for each Mortality_Type
  tables <- lapply(unique(formatted_data$Mortality_Type), function(mort_type) {
    data_subset <- formatted_data %>%
      filter(Mortality_Type == mort_type) %>%
      select(-Mortality_Type) %>%
      arrange(Race_Name)
    
    gt(data_subset) %>%
      tab_header(
        title = paste("Impact of CHC Loss on", mort_type, "Mortality"),
        subtitle = "Overall Effect Sizes and 95% Confidence Intervals"
      ) %>%
      cols_label(
        Race_Name = "Race/Ethnicity",
        Estimate = "Effect Size",
        CI = "95% CI",
        P_Value = "P-Value"
      ) %>%
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_column_labels()
      ) %>%
      tab_options(
        table.border.top.color = "black",
        table.border.bottom.color = "black",
        column_labels.border.bottom.color = "black",
        table.font.size = px(12)
      )
  })
  
  return(list(tables = tables, formatted_data = formatted_data))
}

# Usage in main script
# Assuming 'all_results' is your list of analysis results
summary_results <- create_summary_table(all_results)
summary_tables <- summary_results$tables
formatted_data <- summary_results$formatted_data

# Function to sanitize file names
sanitize_filename <- function(filename) {
  filename %>%
    str_to_lower() %>%
    str_replace_all("[/\\?%*:|\"<>\\s]", "_") %>%
    str_replace_all("__+", "_") %>%
    str_trim()
}

# Save tables as HTML files
for (i in seq_along(summary_tables)) {
  mortality_type <- unique(formatted_data$Mortality_Type)[i]
  safe_filename <- sanitize_filename(mortality_type)
  filename <- paste0("chc_loss_effect_", safe_filename, ".html")
  
  tryCatch({
    summary_tables[[i]] %>%
      gtsave(filename)
    cat("Table saved as", filename, "\n")
  }, error = function(e) {
    cat("Error saving table for", mortality_type, ":", conditionMessage(e), "\n")
  })
}

# If you want to combine all tables into a single HTML file
combined_tables <- summary_tables %>%
  lapply(as_raw_html) %>%
  paste(collapse = "<br><br>")

writeLines(combined_tables, "chc_loss_effect_all_mortality_types.html")
cat("Combined table saved as chc_loss_effect_all_mortality_types.html\n")









##### mediation analysis on patient losses #####

# Required Libraries
library(tidyverse)
library(readxl)
library(lmtest)
library(sandwich)
library(ggplot2)
library(gt)
library(scales)

library(tidyverse)
library(readxl)
library(httr)

library(tidyverse)
library(readxl)
library(httr)
library(stringr)

library(dplyr)
library(tidyr)
library(readxl)
library(httr)
library(stringr)


# clear the environment
rm(list = ls())
merged_data = read.csv("~/Downloads/merged_data.csv") %>%
  mutate(fips = as.factor(GEOID))



load_uds_data <- function(year) {
  tryCatch({
    # Create the URL for the file
    url <- paste0("https://www.hrsa.gov/sites/default/files/hrsa/foia/h80-", year, ".xlsx")
    
    # Create a temporary file path
    temp_file <- tempfile(fileext = ".xlsx")
    
    # Download the file
    GET(url, write_disk(temp_file, overwrite = TRUE))
    
    # Determine the correct sheet names based on the year
    site_info_sheet <- if (year == 2011) "tblGranteeSiteInfo" else "HealthCenterSiteInfo"
    patient_data_sheet <- "Table3A"
    
    # Read the site info sheet
    site_info <- read_excel(temp_file, sheet = site_info_sheet)
    
    # Determine the ZIP code column name
    zip_code_col <- names(site_info)[grep("ZIP|Zip|zip", names(site_info))]
    if (length(zip_code_col) == 0) {
      stop("ZIP code column not found in the data")
    }
    
    # Process site info
    site_info <- site_info %>%
      dplyr::select(1, all_of(zip_code_col)) %>%
      rename(BHCMISID = 1) %>%
      mutate(across(all_of(zip_code_col), as.character)) %>%
      separate(col = zip_code_col[1], into = c("SiteZIPCode1", "SiteZIPCode2", "SiteZIPCode3"), sep = ",", fill = "right") %>%
      mutate(
        SiteZIPCode = coalesce(SiteZIPCode2, SiteZIPCode1),
        SiteZIPCode = stringr::str_extract(SiteZIPCode, "\\d{5}")
      ) %>%
      select(BHCMISID, SiteZIPCode)
    
    # Read the patient data sheet
    patient_data <- read_excel(temp_file, sheet = patient_data_sheet)
    
    # Print column names for debugging
    cat(paste("Year:", year, "\n"))
    cat("Patient Data Column Names:\n")
    print(names(patient_data))
    
    # Determine the correct column names based on the year
    if (year >= 2014) {
      id_col <- "BHCMIS ID"
      total_male_col <- "Total Patients - Male Patients"
      total_female_col <- "Total Patients - Female Patients"
    } else if (year == 2011) {
      id_col <- "BHCMISID"
      total_male_col <- "Total Patients-Male Patients"
      total_female_col <- "Total Patients-Female Patients"
    } else {
      id_col <- "BHCMISID"
      total_male_col <- "T3a_L39_Ca"
      total_female_col <- "T3a_L39_Cb"
    }
    
    # Process patient data
    patient_data <- patient_data %>%
      dplyr::select(all_of(id_col), all_of(total_male_col), all_of(total_female_col)) %>%
      rename(BHCMISID = all_of(id_col), total_male = all_of(total_male_col), total_female = all_of(total_female_col)) %>%
      mutate(
        total_male = as.numeric(total_male),
        total_female = as.numeric(total_female),
        total_patients = total_male + total_female
      ) %>%
      dplyr::select(BHCMISID, total_patients)
    
    # Combine the data
    combined_data <- site_info %>%
      left_join(patient_data, by = "BHCMISID") %>%
      mutate(Year = year) %>%
      dplyr::select(BHCMISID, SiteZIPCode, total_patients, Year)
    
    # Remove the temporary file
    file.remove(temp_file)
    
    return(combined_data)
  }, error = function(e) {
    cat(paste("Error processing year", year, ":", e$message, "\n"))
    return(NULL)
  })
}




# Load data for all years
years <- 2011:2019
uds_data_list <- lapply(years, load_uds_data)

uds_data <- bind_rows(uds_data_list[!sapply(uds_data_list, is.null)])


zip_to_county <- read_csv("~/Downloads/ZIP_COUNTY_032023.csv") %>%
  select(ZIP, COUNTY) %>%
  rename(SiteZIPCode = ZIP, county_fips = COUNTY)

# Step 2: Process and aggregate patient data to county level
uds_county_data <- uds_data %>%
  left_join(zip_to_county, by = c("SiteZIPCode" = "SiteZIPCode")) %>%
  group_by(county_fips, Year, BHCMISID) %>%
  summarise(total_patients = first(total_patients), .groups = "drop") %>%
  group_by(county_fips, Year) %>%
  summarise(total_patients = sum(total_patients), .groups = "drop") %>%
  mutate(county_fips = as.character(county_fips))  # Ensure county_fips is character



# Ensure GEOID in merged_data is character
merged_data <- merged_data %>%
  mutate(GEOID = as.character(GEOID))
        
# Now perform the join
merged_data <- merged_data %>%
  left_join(uds_county_data, by = c("GEOID" = "county_fips", "year" = "Year"))



# Step 3: Compare patient growth in counties with and without CHC site losses
chc_loss_counties <- merged_data %>%
  filter(year %in% c(2013,2014)) %>%
  group_by(GEOID) %>%
  arrange(GEOID, year) %>%
  mutate(site_change = sites - lag(sites)) %>%
  summarise(
    chc_loss = if_else(any(site_change < 0 & year %in% c(2014)), 
                       "Site Loss", "No Site Loss"), 
    .groups = "drop"
  )

# Calculate patient growth relative 
patient_growth <- merged_data %>%
  filter(year >= 2013) %>%  
  group_by(GEOID) %>%
  arrange(GEOID, year) %>%
  mutate(
    baseline_patients = first(total_patients),
    patient_growth = (total_patients - baseline_patients) / baseline_patients
  ) %>%
  ungroup() %>%
  left_join(chc_loss_counties, by = "GEOID") %>%
  group_by(year, chc_loss) %>%
  summarise(
    mean_growth = mean(patient_growth, na.rm = TRUE),
    se_growth = sd(patient_growth, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Ensure chc_loss is a factor with specified levels
patient_growth$chc_loss <- factor(patient_growth$chc_loss, 
                                  levels = c("Site Loss", "No Site Loss"))

# Visualize patient growth
ggplot(patient_growth, aes(x = year, y = mean_growth, color = chc_loss, group = chc_loss)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean_growth - 1.96*se_growth, 
                    ymax = mean_growth + 1.96*se_growth), 
                width = 0.2) +
  labs(title = "Mean Patient Growth by CHC Loss Status",
       x = "Year", 
       y = "Mean Patient Growth Rate",
       color = "CHC Status") +
  scale_y_continuous(labels = scales::percent) +
  scale_color_discrete(name = "CHC Status", 
                       labels = c("Site Loss", "No Site Loss")) +
  theme_minimal() +
  scale_x_continuous(breaks = 2013:2019)  # Ensure all years are shown on x-axis

# Print the data
print(patient_growth)

# Save the plot
ggsave("patient_growth_relative_to_2014.png", width = 10, height = 6, dpi = 300)



# Step 4: Perform mediation analysis


merged_data = merged_data %>%
  filter(year >= 2013) %>%  
  group_by(GEOID) %>%
  arrange(GEOID, year) %>%
  mutate(
    baseline_patients = first(total_patients),
    patient_growth = (total_patients - baseline_patients) / baseline_patients
  ) %>%
  ungroup() %>%
  left_join(chc_loss_counties, by = "GEOID") 

  data <- merged_data %>%
    rename(
      air_pollution = `Air.pollution...particulate.matter`,
      child_poverty = `Children.in.poverty`,
      unemployment = `Unemployment.rate`,
      violent_crime = `Violent.crime.rate`,
      school_funding = `School.funding`
    )
  
  # Calculate CHC change between 2013 and 2014
  chc_change <- data %>%
    filter(year %in% c(2013, 2014)) %>%
    dplyr::select(GEOID, year, sites) %>%
    group_by(GEOID) %>%
    summarise(
      sites_2013 = mean(as.numeric(sites[year == 2013]), na.rm = TRUE),
      sites_2014 = mean(as.numeric(sites[year == 2014]), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      chc_change_2013_2014 = sites_2014 - sites_2013,
      chc_loss = as.integer(chc_change_2013_2014 < 0)
    )
  
  # Prepare the main dataset
  prepared_data <- data %>%
    group_by(GEOID) %>%
    mutate(
      across(c(education_percent, median_income, children, elderly, female, 
               total_population, air_pollution, child_poverty, 
               unemployment, violent_crime, Uninsured, school_funding),
             list(
               baseline = ~first(as.numeric(.x[year == 2012])),
               change = ~last(as.numeric(.x[year == 2019])) - first(as.numeric(.x[year == 2012]))
             )),
      baseline_sites = first(as.numeric(sites[year == 2012])),
      baseline_mortality = first(as.numeric(val[year == 2012])),
      val = as.numeric(val) * 100000  # Convert mortality rate to per 100,000 people
    ) %>%
    ungroup() %>%
    # Join with chc_change data
    left_join(chc_change, by = "GEOID") %>%
    # Convert character columns to factors with explicit levels
    mutate(
      mortality_type = factor(mortality_type, levels = c("Cancer", "Cardiovascular", "Diabetes/CKD", "Infectious", "Maternal/Neonatal", "Respiratory", "Substance Use", "Total")),
      race_name = factor(race_name, levels = c("AIAN", "API", "Black", "Latino", "Total", "White"))
    ) %>%
    mutate(chc_loss = as.numeric(chc_loss.y=="Site Loss")) %>%
    filter(mortality_type == "Total",
           race_name == "White")
  

  
  covariates <- c("education_percent", "median_income", "children", "elderly", "female", 
                  "total_population", "air_pollution", "child_poverty", 
                  "unemployment", "violent_crime", "Uninsured", "school_funding")
  
  ps_formula <- as.formula(paste("chc_loss ~", paste(paste0(covariates), collapse = " + ")))
  
  match_obj <- matchit(ps_formula,
                       data = prepared_data %>% filter(year == 2013),  # Match based on 2013 data
                       method = "nearest",
                       distance = "glm",
                       ratio = 3,  # One-to-three matching
                       caliper = 0.2)
  
 
  matched_data <- match.data(match_obj)
  
  # Merge matched data with all years
  full_matched_data <- inner_join(data, matched_data %>% select(GEOID, weights, subclass), by = "GEOID")
  
# mediation regs
  # Direct effect of CHC loss on mortality rate:
  
  summary(lm(val~ chc_loss * factor(year) + education_percent + median_income + children + elderly + female + total_population + sites + air_pollution + child_poverty + unemployment + violent_crime + Uninsured + school_funding, 
     data = full_matched_data, weights = weights))$coefficients[2]
  
  # Effect of CHC loss on patient growth (mediator):
  
  summary(lm(patient_growth~ chc_loss * factor(year) + education_percent + median_income + children + elderly + female + total_population + sites + air_pollution + child_poverty + unemployment + violent_crime + Uninsured + school_funding, 
   data = full_matched_data, weights = weights))$coefficients[2]
  
  # Effect of CHC loss on mortality rate, controlling for patient growth:
  
  summary(lm(val~ chc_loss * factor(year) + patient_growth + education_percent + median_income + children + elderly + female + total_population + sites + air_pollution + child_poverty + unemployment + violent_crime + Uninsured + school_funding, 
   data = full_matched_data, weights = weights))$coefficients[2]

  # Direct effect of CHC loss on mortality rate:
  
  summary(lm(val~ chc_loss * factor(year) + education_percent + median_income + children + elderly + female + total_population + sites + air_pollution + child_poverty + unemployment + violent_crime + Uninsured + school_funding, 
             data = full_matched_data, weights = weights))$coefficients[2]
  
  # Effect of CHC loss on PCP density (mediator):
  
  summary(lm(Primary.care.physicians ~ chc_loss * factor(year) + education_percent + median_income + children + elderly + female + total_population + sites + air_pollution + child_poverty + unemployment + violent_crime + Uninsured + school_funding, 
             data = full_matched_data, weights = weights))$coefficients[2]
  
  # Effect of CHC loss on mortality rate, controlling for patient growth:
  
  summary(lm(val~ chc_loss * factor(year) + Primary.care.physicians + education_percent + median_income + children + elderly + female + total_population + sites + air_pollution + child_poverty + unemployment + violent_crime + Uninsured + school_funding, 
             data = full_matched_data, weights = weights))$coefficients[2]
  
  
  
  
  
  