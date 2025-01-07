# Scratch Code: 
  ## Continuous Vars
  ### GEE Linear regression function 
  run_linear_regression_gee <- function(data, predictor, responses, cluster_var) {
    results <- map_dfr(responses, function(response) {
      # Fit GEE model
      model <- gee(as.formula(paste(response, "~", predictor)), id = cluster_var, data = data, family = gaussian, corstr = "exchangeable")
      
      # Extract coefficients and p-values
      coef <- summary(model)$coefficients[2, 1]
      p_value <- summary(model)$coefficients[2, 4]
      conf_int <- confint(model)[2, ]
      
      # Create a data frame for the result
      result_df <- data.frame(
        Response = response,
        Coefficient = coef,
        P_Value = p_value,
        CI_Lower = conf_int[1],
        CI_Upper = conf_int[2]
      )
      
      return(result_df)
    })
    
    return(results)
  }
  
  
  ### List of continuous outcome vars- need to add distance variables (hh to water main and hh to water source)
  predictor <- "fakearm"
  responses <- c("use_respond")
  cluster_var <- "final_match_strata_3"
  
  ### Run Regression 
  results <- run_linear_regression_gee(all_data, predictor, responses, cluster_var)
  results <- t(results) # transposing for aesthetics 
  
  ### Create rmarkdown table 
  results_table_continuous <- kable(results, align = "lccc") %>%
    kable_styling(full_width = FALSE) %>%
    column_spec(1, bold = TRUE) %>%
    column_spec(2:5, width = "2cm") %>%
    row_spec(0, bold = TRUE, color = "white", background = "#0073e6")
  
  results_table_continuous
  
  ### Convert the table to a flextable object to output into Word 
  table3a <- flextable(as.data.frame(results))
  print(table3a, preview = "docx")
  
  
  results <- run_poisson_regression_with_rr(all_data, predictor, responses)
  
  results_table_cat <- kable(results, align = "lccc", format = "html") %>%
    kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) %>%
    column_spec(1, bold = TRUE) %>%
    column_spec(2:5, width = "2cm") %>%
    row_spec(0, bold = TRUE, color = "black", background = "white") %>%
    row_spec(1:nrow(results), background = "white")  # Alternate row colors
  
  results_table_cat
  
  ### Create output table to Word 
  table3b <- flextable(as.data.frame(results))
  print(table3b, preview = "docx")
  
  ## GEE for continuous variables 
  # Continuous outcome vars 
  ## Write model for gee linear regression 
  model_formula <- use_respond ~ fakearm
  model_lin <- gee(model_formula, id = all_data$code, data = all_data, family = gaussian(link= "identity"), corst = "exchangeable")
  model_sum <- summary(model_lin)
  
  ## Extract coefficients and standard errors
  coefficients <- model_sum$coefficients[2, 1]
  p_value <- 2 * (1-pnorm(abs(model_sum$coefficients[2, 5])))
  
  ### Extract robust standard errors
  robust_se <- abs(model_sum$coefficients[2, 4])
  
  ### Calculate confidence intervals using robust standard errors
  ci_lower <- exp(coefficients - 1.96 * robust_se)
  ci_upper <- exp(coefficients + 1.96 * robust_se)
  risk_ratio <- exp(coefficients)
  
  ## Combine coefficients and confidence intervals into a dataframe
  results <- data.frame(Risk_Ratio = risk_ratio, P_Value = p_value, CI_Lower = ci_lower, CI_Upper = ci_upper)
  results <- results[-1,]
  
  
  ## Run GEE linear regression for continuous outcome variable (use_meter)
  model_formula <- use_respond ~ hh_water
  model_lin <- gee(model_formula, id = all_data$code, data = all_data, family = gaussian(link = "identity"), corst = "exchangeable")
  model_sum <- summary(model_lin)
  
  ### Extract coefficients and standard errors
  coefficients <- model_sum$coefficients[2, 1]
  p_value <- 2 * (1-pnorm(abs(model_sum$coefficients[2, 5])))
  
  ### Extract robust standard errors
  robust_se <- abs(model_sum$coefficients[2, 4])
  
  ### Calculate confidence intervals using robust standard errors
  ci_lower <- exp(coefficients - 1.96 * robust_se)
  ci_upper <- exp(coefficients + 1.96 * robust_se)
  risk_ratio <- exp(coefficients)
  
  ### Combine coefficients and confidence intervals into a dataframe
  results <- data.frame(Risk_Ratio = risk_ratio, P_Value = p_value, CI_Lower = ci_lower, CI_Upper = ci_upper)
  results <- results[-1,]
  
  # Concatenate dataframes 
  table_3b <- rbind(result_df, results)
  
  ## Convert row names to a column
  table_3b$variable <- row.names(table_3b)
  row.names(table_3b) <- NULL 
  
  ## Move 'variable' column to the first position
  table_3b <- table_3b %>%
    select(variable, everything())
  
  ## Rename use_respond observation
  table_3b$variable[table_3b$variable == "hh_water"] <- "use_respond"
  
  
  model_formula <- use_respond ~ fakearm + SES_score + san_basic_obs + secondary_complete
  model_lin <- gee(model_formula, id = all_data$code, data = all_data, family = gaussian(link = "identity"), corst = "exchangeable")
  
  ### Extract model summary
  model_sum <- summary(model_lin)
  
  ### Extract coefficients and standard errors
  coefficients <- model_sum$coefficients[2, 1]
  p_value <- 2 * (1-pnorm(abs(model_sum$coefficients[2, 5])))
  
  ### Extract robust standard errors
  robust_se <- abs(model_sum$coefficients[2, 4])
  
  ### Calculate confidence intervals using robust standard errors
  ci_lower <- exp(coefficients - 1.96 * robust_se)
  ci_upper <- exp(coefficients + 1.96 * robust_se)
  risk_ratio <- exp(coefficients)
  
  ### Combine coefficients and confidence intervals into a dataframe
  results <- data.frame(Risk_Ratio = risk_ratio, P_Value = p_value, CI_Lower = ci_lower, CI_Upper = ci_upper, Robust_se = robust_se)
  
  ### Convert row names to a column
  results$Outcome_Variable <- row.names(results)
  row.names(results) <- NULL 
  
  ### Move 'variable' column to the first position
  result_use <- results %>%
    select(Outcome_Variable, everything())
  
  ## Rename use_respond observation
  result_use$Outcome_Variable[result_use$Outcome_Variable == "1"] <- "Water usage (L/day) - respondent"
  
  # Univariate Tables - does not need to be included 
  ## Table 3. Univariate association between intervention and water quality and access
  
  ### Table 3a - primary variables + Network effect

  ## Categorical Vars - log binomial regression 
  
  ### Sort data by cluster variable 
  all_data <- all_data %>%
    arrange(code)
  
  ### Write function for running log binomial regression
  run_gee_regression <- function(data, outcome_vars, predictor_var, cluster_var) {
    results <- list()
    for (outcome_var in outcome_vars) {
      # Define the model formula
      model_formula <- as.formula(paste(outcome_var, "~", predictor_var))
      
      # Fit GEE logistic regression model
      model <- gee(model_formula, id = code, data = data, family = binomial(link = "log"), corstr = "exchangeable")
      
      # Get model summary
      model_summary <- summary(model)
      
      # Store model summary in results list
      results[[outcome_var]] <- model_summary
    }
    return(results)
  }
  
  # Run logistic regression for all outcome variables (add in all binary vars)
  result_summary_binary_outcomes <- run_gee_regression(all_data, outcome_vars = c("ecoli_bin_source", "ecoli_bin_stored", "onpremises", "HWISE_insecure", "always_sat_service", "always_sat_afford", "always_sat_avail", "always_sat_pressure", "always_sat_appear", "always_sat_taste", "use_respond_bin"), predictor_var = "fakearm")
  
  # Function to extract risk ratio, p-value, and confidence intervals from GEE model summary
  extract_summary <- function(model_summary) {
    coef <- model_summary$coefficients[2, 1]
    p_value <- 2 * (1-pnorm(abs(model_summary$coefficients[2, 5]))) # Calculated based on robust z
    robust_se <- abs(model_summary$coefficients[2, 4]) # Standard error from robust z
    ci_lower <- exp(coef - 1.96 * robust_se)
    ci_upper <- exp(coef + 1.96 * robust_se)
    risk_ratio <- exp(coef)
    return(data.frame(Risk_Ratio = risk_ratio, P_Value = p_value, CI_Lower = ci_lower, CI_Upper = ci_upper))
  }
  
  # Function to process GEE model summaries for multiple outcome variables
  process_gee_summaries <- function(result_summary) {
    output <- list()
    for (outcome_var in names(result_summary)) {
      # Extract summary for a specific outcome variable
      summary_for_outcome <- result_summary[[outcome_var]]
      # Extract coefficient, p-value, and confidence intervals
      extracted_summary <- extract_summary(summary_for_outcome)
      # Append results to the output list
      output[[outcome_var]] <- extracted_summary
    }
    # Combine all results into a single data frame
    output_df <- do.call(rbind, output)
    return(output_df)
  }
  
  # Process GEE model summaries
  result_df <- process_gee_summaries(result_summary_binary_outcomes)
  
  ## Concatenate dataframes
  table_3a <- result_df
  
  ## Convert row names to a column
  table_3a$variable <- row.names(table_3a)
  row.names(table_3a) <- NULL 
  
  ## Move 'variable' column to the first position
  table_3a <- table_3a %>%
    select(variable, everything())
  
  ## Save table
  write.csv(table_3a, "data-out/Table 3a. Univariate association between network effect and water quality and access.csv")
  
  
  ### Table 3b - primary variables + HH effect

  # Data Analysis 
  # Predictor variable will be combination of C_1 and C_2 - called here hh_water for coding purposes 
  
  ## Run GEE logistic regression for all outcome variables
  result_summary_binary_outcomes <- run_gee_regression(all_data, outcome_vars = c("ecoli_bin_source", "ecoli_bin_stored", "HWISE_insecure", "always_sat_service", "always_sat_afford", "always_sat_avail", "always_sat_pressure", "always_sat_appear", "always_sat_taste", "use_respond_bin"), predictor_var = "hh_water")
  
  ### Extract results from each model and store in data frame
  result_df <- process_gee_summaries(result_summary_binary_outcomes)
  
  ## Concatenate dataframes 
  table_3b <- result_df
  
  ## Convert row names to a column
  table_3b$variable <- row.names(table_3b)
  row.names(table_3b) <- NULL 
  
  ## Move 'variable' column to the first position
  table_3b <- table_3b %>%
    select(variable, everything())
  
  table_3b
  
  ## Save table
  write.csv(table_3b, "data-out/Table 3b. Univariate association of household access to FIPAG water connection between water quality and access.csv")
  
  # Cleanup
  # rm(insert removable objects here)
  
  ## Table 5 - Univariate analysis between intervention status and E. coli in source water at visit 5 (combine network and HH effect)

  
  # Sort data by cluster variable 
  data_visit5 <- data_visit5 %>%
    arrange(final_match_strata_3)
  
  ## Prev. E. coli in source water by fakearm
  model_formula <- ecoli_bin_source ~ fakearm 
  extract_prevecoli_network <- extract_summary(model_formula, "Fakearm", data_visit5)
  model_summary_prevecoli_network <- extract_prevecoli_network$model_summary
  result_prevecoli_network <- extract_prevecoli_network$result_df
  
  
  ## Prev. E. coli in source water by hh_water
  model_formula <- ecoli_bin_source ~ hh_water 
  extract_prevecoli_hh <- extract_summary(model_formula, "Household connection", data_visit5)
  model_summary_prevecoli_hh <- extract_prevecoli_hh$model_summary
  result_prevecoli_hh <- extract_prevecoli_hh$result_df
  
  # Combine tables to create table 5
  table_5 <- rbind(result_prevecoli_network, result_prevecoli_hh)
  
  table_5
  
  # Write table output to CSV 
  write.csv(table_5, "data-out/Table 5. Univariate association between intervention and prevalence of E. coli in source water at visit 5.csv")

  
  # Factor vars 
  data_visit1$san_basic_obs_bl <- 
    factor(data_visit1$san_basic_obs_bl, levels=c(1,0),
           labels=c("Yes", 
                    "No"))
  
  data_visit1$fixed_emp_pri <- 
    factor(data_visit1$fixed_emp_pri, levels=c(1,0),
           labels=c("Yes", 
                    "No"))
  
  data_visit1$secondary_complete_bl <- 
    factor(data_visit1$secondary_complete_bl, levels=c(1,0),
           labels=c("Yes", 
                    "No"))
  
  data_visit1$human_feces <- 
    factor(data_visit1$human_feces, levels=c(1,0),
           labels=c("Yes", 
                    "No"))
  
  data_visit1$animal_feces <- 
    factor(data_visit1$animal_feces, levels=c(1,0),
           labels=c("Yes", 
                    "No"))
  
  data_visit1$HW_dwell <- 
    factor(data_visit1$HW_dwell, levels=c(1,0),
           labels=c("Yes", 
                    "No"))
  
  data_visit1$season_rainy <- 
    factor(data_visit1$season_rainy, levels=c(1,0),
           labels=c("Yes", 
                    "No"))
  
  data_visit1$flooding_HH_yard <- 
    factor(data_visit1$flooding_HH_yard, levels=c(1,0),
           labels=c("Yes", 
                    "No"))
  # Analysis of secondary variables - based on output from primary variables above 
  
  #Secondary variables (not in pre-analysis plan): 
   # **Binary outcomes**: Prevalence of any bacterial or protozoan pathogen in the water 
  # **Count outcomes**: Pathogen count of any non-viral pathogen 
  # **Continuous outcomes**: free chlorine, total chlorine, distance of HH to water main, distance of HH to drinking water source, time to collect water, piped water flow rate, availability (hrs/day), availability (days/week), water usage (meter), HWISE_score, E. coli in water source, E. coli in stored water, total coliforms in water source, total coliforms in stored water, aggregated satisfaction score (see rapid survey paper)
  # **Ordinal outcomes**: FIB Catgeory 
  
  
  # Scratch code for model validation - used and no longer needed in the final output 
  #### Model Fit (Table 4a)
  # Fit all the models with independent correlation and extract standard errors 
  extract_summary_independent <- function(model_formula, outcome_var_name, data) {
    library(gee)
    
    # Fit GEE model
    model <- gee(model_formula, id = code, data = data, family = binomial(link = "log"), corst = "independence")
    
    # Extract model summary
    model_summary <- summary(model)
    
    # Get the index of the 'arm' predictor in the model formula
    arm_index <- which(names(model$coefficients) == "arm")
    
    # Extract robust standard error for 'arm'
    robust_se_arm <- sqrt(model$robust.variance[arm_index, arm_index])
    
    # Extract coefficients and compute risk ratio, p-value, and confidence interval
    coef <- model_summary$coefficients[2, 1]
    p_value <- 2 * (1-pnorm(abs(model_summary$coefficients[2, 5]))) # Calculated based on robust z
    robust_se <- abs(model_summary$coefficients[2, 4]) # Standard error from robust z
    ci_lower <- exp(coef - 1.96 * robust_se)
    ci_upper <- exp(coef + 1.96 * robust_se)
    risk_ratio <- exp(coef)
    
    # Create a dataframe for the result
    result_df <- data.frame(
      Outcome_Variable = outcome_var_name,
      independent_se = robust_se
    )
    
    # Return a list containing both the result dataframe and the model summary object
    return(list(result_df = result_df, model_summary = model_summary))
  }
  
  extract_summary_independent_pois <- function(model_formula, outcome_var_name, data) {
    library(gee)
    
    # Fit GEE model
    model <- gee(model_formula, id = code, data = data, family = poisson(link = "log"), corst = "independence")
    
    # Extract model summary
    model_summary <- summary(model)
    
    # Get the index of the 'arm' predictor in the model formula
    arm_index <- which(names(model$coefficients) == "arm")
    
    # Extract robust standard error for 'arm'
    robust_se_arm <- sqrt(model$robust.variance[arm_index, arm_index])
    
    # Extract coefficients and compute risk ratio, p-value, and confidence interval
    coef <- model_summary$coefficients[2, 1]
    p_value <- 2 * (1-pnorm(abs(model_summary$coefficients[2, 5]))) # Calculated based on robust z
    robust_se <- abs(model_summary$coefficients[2, 4]) # Standard error from robust z
    ci_lower <- exp(coef - 1.96 * robust_se)
    ci_upper <- exp(coef + 1.96 * robust_se)
    risk_ratio <- exp(coef)
    
    # Create a dataframe for the result
    result_df <- data.frame(
      Outcome_Variable = outcome_var_name,
      independent_se = robust_se
    )
    
    # Return a list containing both the result dataframe and the model summary object
    return(list(result_df = result_df, model_summary = model_summary))
  }
  
  # Run models using function above 
  ## Prev. E. coli in source water 
  model_formula <- ecoli_bin_source ~ arm + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh
  extract_prevecoli <- extract_summary_independent(model_formula, "Prev. E. coli in source water", all_data)
  model_summary_prevecoli <- extract_prevecoli$model_summary
  result_prevecoli <- extract_prevecoli$result_df
  
  ## Prev. E. coli in stored water 
  model_formula <- ecoli_bin_stored ~ arm + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh
  extract_prevecoli_stored <- extract_summary_independent(model_formula, "Prev. E. coli in stored water", all_data)
  model_summary_prevecoli_stored <- extract_prevecoli_stored$model_summary
  result_prevecoli_stored <- extract_prevecoli_stored$result_df
  
  ## Prev. of access to at least basic water source - did not converge
  #model_formula <- basic_water ~ arm + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh
  #extract_basicwater <- extract_summary_independent(model_formula, "Prev. of access to at least basic water source", all_data)
  #model_summary_basicwater <- extract_basicwater$model_summary
  #result_basicwater <- extract_basicwater$result_df
  
  ## Prev. of access to basic water (modified Poisson)
  model_formula <- basic_water ~ arm + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh
  extract_basicwater <- extract_summary_independent_pois(model_formula, "Prev. of access to at least basic water source", all_data)
  model_summary_basicwater <- extract_basicwater$model_summary
  result_basicwater <- extract_basicwater$result_df
  
  ## Prev. of access to at water on premises
  #model_formula <- onpremises ~ arm + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh
  #extract_onpremises <- extract_summary_independent(model_formula, "Prev. of access to water on the premises", all_data)
  #model_summary_onpremises <- extract_onpremises$model_summary
  #result_onpremises <- extract_onpremises$result_df
  
  ## Prev. of access to water on premises (modified Poisson)
  model_formula <- onpremises ~ arm + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh
  extract_onpremises <- extract_summary_independent_pois(model_formula, "Prev. of access to water on the premises", all_data)
  model_summary_onpremises <- extract_onpremises$model_summary
  result_onpremises <- extract_onpremises$result_df
  
  ## Prev. of HH with sufficient quantities of water when needed (did not converge)
  #model_formula <- sufficient ~ arm + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh
  #extract_sufficient <- extract_summary_independent(model_formula, "Prev. of HH with sufficient quantities of water when needed", all_data)
  #model_summary_sufficient <- extract_sufficient$model_summary
  #result_sufficient <- extract_sufficient$result_df
  
  ## Prev. of HH with sufficient quantities of water when needed (modified Poisson)
  model_formula <- sufficient ~ arm + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh
  extract_sufficient <- extract_summary_independent_pois(model_formula, "Prev. of HH with sufficient quantities of water when needed", all_data)
  model_summary_sufficient <- extract_sufficient$model_summary
  result_sufficient <- extract_sufficient$result_df
  
  ## Prev. of HH with HWISE score >=12
  model_formula <- HWISE_insecure ~ arm + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh
  extract_hwise <- extract_summary_independent(model_formula, "Prev. of HH with water insecurity", all_data)
  model_summary_hwise <- extract_hwise$model_summary
  result_hwise <- extract_hwise$result_df
  
  ## Prev. of HH always being satisfied with water service 
  model_formula <- always_sat_service ~ arm + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh
  extract_sat_service <- extract_summary_independent(model_formula, "Prev. of HH always being satisfied with water service", all_data)
  model_summary_sat_service <- extract_sat_service$model_summary
  result_sat_service <- extract_sat_service$result_df
  
  ## Prev. of HH always being satisfied with water affordability (did not converge)
  #model_formula <- always_sat_afford ~ arm + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh
  #extract_sat_afford <- extract_summary_independent(model_formula, "Prev. of HH always being satisfied with water affordability", all_data)
  #model_summary_sat_afford <- extract_sat_afford$model_summary
  #result_sat_afford <- extract_sat_afford$result_df
  
  ## Prev. of HH always being satisfied with water affordability (modified Poisson)
  model_formula <- always_sat_afford ~ arm + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh
  extract_sat_afford <- extract_summary_independent_pois(model_formula, "Prev. of HH always being satisfied with water affordability", all_data)
  model_summary_sat_afford <- extract_sat_afford$model_summary
  result_sat_afford <- extract_sat_afford$result_df
  
  ## Prev. of HH always being satisfied with water availability
  model_formula <- always_sat_avail ~ arm + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh
  extract_sat_avail <- extract_summary_independent(model_formula, "Prev. of HH always being satisfied with water availability", all_data)
  model_summary_sat_avail <- extract_sat_avail$model_summary
  result_sat_avail <- extract_sat_avail$result_df
  
  ## Prev. of HH always being satisfied with water pressure
  model_formula <- always_sat_pressure ~ arm + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh
  extract_sat_pressure <- extract_summary_independent(model_formula, "Prev. of HH always being satisfied with water pressure", all_data)
  model_summary_sat_pressure <- extract_sat_pressure$model_summary
  result_sat_pressure <- extract_sat_pressure$result_df
  
  ## Prev. of HH always being satisfied with water color and appearance
  model_formula <- always_sat_appear ~ arm + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh
  extract_sat_appear <- extract_summary_independent(model_formula, "Prev. of HH always being satisfied with water color and appearance", all_data)
  model_summary_sat_appear <- extract_sat_appear$model_summary
  result_sat_appear <- extract_sat_appear$result_df
  
  ## Prev. of HH always being satisfied with water taste and smell
  model_formula <- always_sat_taste ~ arm + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh
  extract_sat_taste <- extract_summary_independent(model_formula, "Prev. of HH always being satisfied with water taste and smell", all_data)
  model_summary_sat_taste <- extract_sat_taste$model_summary
  result_sat_taste <- extract_sat_taste$result_df
  
  ## Binary water use (respondent)  
  model_formula <- use_respond_bin ~ arm + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh
  extract_use_respond <- extract_summary_independent_pois(model_formula, "> 80 L/day of water use (respondent)", all_data)
  model_summary_use_respond <- extract_use_respond$model_summary
  result_use_respond <- extract_use_respond$result_df
  
  # Concatenate all results into one dataframe
  table_4a_independence <- rbind(result_prevecoli, result_prevecoli_stored, result_sufficient, result_hwise, result_sat_service, result_sat_afford, result_sat_avail, result_sat_pressure, result_sat_appear, result_sat_taste, result_use_respond, result_basicwater, result_onpremises)
  
  # Merge with results from exchangeable correlation structure above 
  merged_table4a <- merge(table_4a, table_4a_independence, by = "Outcome_Variable")
  
  # Divide SE from exchangeable correlation by independent correlation to assess model fit
  merged_table4a$se_ratio <- merged_table4a$Robust_se / merged_table4a$independent_se
  
  
# Model fit for hh direct connection effect
  # Compare ratios of standard errors with GEE fitted with independent correlation to GEE with exchangeable correlation
  ## Prev. E. coli in source water (write model)
  model_formula <- ecoli_bin_source ~ hh_water + high_poverty + san_basic_obs_bl + secondary_complete_bl + months_in_hh + flooding_HH_yard + arm + final_match_strata_3
  extract_prevecoli <- extract_summary_independent(model_formula, "Prev. E. coli in source water", all_data)
  result_prevecoli <- extract_prevecoli$result_df
  
  ## Prev. E. coli in stored water 
  model_formula <- ecoli_bin_stored ~ hh_water + high_poverty + san_basic_obs_bl + secondary_complete_bl + months_in_hh + flooding_HH_yard + arm + final_match_strata_3
  extract_prevecoli_stored <- extract_summary_independent(model_formula, "Prev. E. coli in stored water", all_data)
  result_prevecoli_stored <- extract_prevecoli_stored$result_df
  
  ## Prev. of access to at least basic water source - did not converge
  #model_formula <- basic_water ~ hh_water + high_poverty + san_basic_obs_bl + secondary_complete_bl + months_in_hh + flooding_HH_yard + arm + final_match_strata_3
  #extract_basicwater <- extract_summary_independent(model_formula, "Prev. of access to at least basic water source", all_data)
  #result_basicwater <- extract_basicwater$result_df
  
  ## Prev. of access to basic water (modified Poisson)
  model_formula <- basic_water ~ hh_water + high_poverty + san_basic_obs_bl + secondary_complete_bl + months_in_hh + flooding_HH_yard + arm + final_match_strata_3
  extract_basicwater <- extract_summary_independent_pois(model_formula, "Prev. of access to at least basic water source", all_data)
  result_basicwater <- extract_basicwater$result_df
  
  ## Prev. of access to at water on premises- did not converge
  #model_formula <- onpremises ~ hh_water + high_poverty + san_basic_obs_bl + secondary_complete_bl + months_in_hh + flooding_HH_yard + arm + final_match_strata_3
  #extract_onpremises <- extract_summary_independent(model_formula, "Prev. of access to water on the premises", all_data)
  #result_onpremises <- extract_onpremises$result_df
  
  ## Prev. of access to water on premises (modified Poisson)
  model_formula <- onpremises ~ hh_water + high_poverty + san_basic_obs_bl + secondary_complete_bl + months_in_hh + flooding_HH_yard + arm + final_match_strata_3
  extract_onpremises <- extract_summary_independent_pois(model_formula, "Prev. of access to water on the premises", all_data)
  result_onpremises <- extract_onpremises$result_df
  
  ## Prev. of HH with sufficient quantities of water when needed - did not converge
  #model_formula <- sufficient ~ hh_water + high_poverty + san_basic_obs_bl + secondary_complete_bl + months_in_hh + flooding_HH_yard + arm + final_match_strata_3
  #extract_sufficient <- extract_summary_independent(model_formula, "Prev. of HH with sufficient quantities of water when needed", all_data)
  #result_sufficient <- extract_sufficient$result_df
  
  ## Prev. of HH with sufficient quantities of water when needed - modified Poisson
  model_formula <- sufficient ~ hh_water + high_poverty + san_basic_obs_bl + secondary_complete_bl + months_in_hh + flooding_HH_yard + arm + final_match_strata_3
  extract_sufficient <- extract_summary_independent_pois(model_formula, "Prev. of HH with sufficient quantities of water when needed", all_data)
  result_sufficient <- extract_sufficient$result_df
  
  ## Prev. of HH with HWISE score >=12- modified Poisson
  all_data$HWISE_insecure <- as.numeric(all_data$HWISE_insecure)
  model_formula <- HWISE_insecure ~ hh_water + high_poverty + san_basic_obs_bl + secondary_complete_bl + months_in_hh + flooding_HH_yard + arm + final_match_strata_3
  extract_hwise <- extract_summary_independent_pois(model_formula, "Prev. of HH with water insecurity", all_data)
  result_hwise <- extract_hwise$result_df
  
  ## Prev. of HH always being satisfied with water service - did not converge
  #model_formula <- always_sat_service ~ hh_water + high_poverty + san_basic_obs_bl + secondary_complete_bl + months_in_hh + flooding_HH_yard + arm + final_match_strata_3
  #extract_sat_service <- extract_summary_independent(model_formula, "Prev. of HH always being satisfied with water service", all_data)
  #result_sat_service <- extract_sat_service$result_df
  
  ## Prev. of HH always being satisfied with water service - modified Poisson
  model_formula <- always_sat_service ~ hh_water + high_poverty + san_basic_obs_bl + secondary_complete_bl + months_in_hh + flooding_HH_yard + arm + final_match_strata_3
  extract_sat_service <- extract_summary_independent_pois(model_formula, "Prev. of HH always being satisfied with water service", all_data)
  result_sat_service <- extract_sat_service$result_df
  
  ## Prev. of HH always being satisfied with water affordability - did not converge
  #model_formula <- always_sat_afford ~ hh_water + high_poverty + san_basic_obs_bl + secondary_complete_bl + months_in_hh + flooding_HH_yard + arm + final_match_strata_3
  #extract_sat_afford <- extract_summary_independent(model_formula, "Prev. of HH always being satisfied with water affordability", all_data)
  #result_sat_afford <- extract_sat_afford$result_df
  
  # Satisfaction with affordability (modified Poisson)
  model_formula <- always_sat_afford ~ hh_water + high_poverty + san_basic_obs_bl + secondary_complete_bl + months_in_hh + flooding_HH_yard + arm + final_match_strata_3
  extract_sat_afford <- extract_summary_independent_pois(model_formula, "Prev. of HH always being satisfied with water affordability", all_data)
  result_sat_afford <- extract_sat_afford$result_df
  
  ## Prev. of HH always being satisfied with water availability - did not converge
  #model_formula <- always_sat_avail ~ hh_water + high_poverty + san_basic_obs_bl + secondary_complete_bl + months_in_hh + flooding_HH_yard + arm + final_match_strata_3
  #extract_sat_avail <- extract_summary_independent(model_formula, "Prev. of HH always being satisfied with water availability", all_data)
  #result_sat_avail <- extract_sat_avail$result_df
  
  # Satisfaction with availability (modified Poisson)
  model_formula <- always_sat_avail ~ hh_water + high_poverty + san_basic_obs_bl + secondary_complete_bl + months_in_hh + flooding_HH_yard + arm + final_match_strata_3
  extract_sat_avail <- extract_summary_independent_pois(model_formula, "Prev. of HH always being satisfied with water availability", all_data)
  result_sat_avail <- extract_sat_avail$result_df
  
  ## Prev. of HH always being satisfied with water pressure- did not converge
  #model_formula <- always_sat_pressure ~ hh_water + high_poverty + san_basic_obs_bl + secondary_complete_bl + months_in_hh + flooding_HH_yard + arm + final_match_strata_3
  #extract_sat_pressure <- extract_summary_independent(model_formula, "Prev. of HH always being satisfied with water pressure", all_data)
  #result_sat_pressure <- extract_sat_pressure$result_df
  
  # Satisfaction with pressure (modified Poisson)
  model_formula <- always_sat_pressure ~ hh_water + high_poverty + san_basic_obs_bl + secondary_complete_bl + months_in_hh + flooding_HH_yard + arm + final_match_strata_3
  extract_sat_pressure <- extract_summary_independent_pois(model_formula, "Prev. of HH always being satisfied with water pressure", all_data)
  result_sat_pressure <- extract_sat_pressure$result_df   
  
  ## Prev. of HH always being satisfied with water color and appearance
  model_formula <- always_sat_appear ~ hh_water + high_poverty + san_basic_obs_bl + secondary_complete_bl + months_in_hh + flooding_HH_yard + arm + final_match_strata_3
  extract_sat_appear <- extract_summary_independent(model_formula, "Prev. of HH always being satisfied with water color and appearance", all_data)
  result_sat_appear <- extract_sat_appear$result_df
  
  ## Prev. of HH always being satisfied with water taste and smell
  model_formula <- always_sat_taste ~ hh_water + high_poverty + san_basic_obs_bl + secondary_complete_bl + months_in_hh + flooding_HH_yard + arm + final_match_strata_3
  extract_sat_taste <- extract_summary_independent_pois(model_formula, "Prev. of HH always being satisfied with water taste and smell", all_data)
  result_sat_taste <- extract_sat_taste$result_df
  
  ## Binary water use (respondent)  - did not converge
  #model_formula <- use_respond_bin ~ hh_water + high_poverty + san_basic_obs_bl + secondary_complete_bl + months_in_hh + flooding_HH_yard + arm + final_match_strata_3
  #extract_use_respond <- extract_summary(model_formula, "> 80 L/day of water use (respondent)", all_data)
  #model_summary_use_respond <- extract_use_respond$model_summary
  #result_use_respond <- extract_use_respond$result_df
  
  # Binary water use (modified Poisson)
  model_formula <- use_respond_bin ~ hh_water + high_poverty + san_basic_obs_bl + secondary_complete_bl + months_in_hh + flooding_HH_yard + arm + final_match_strata_3
  extract_use_respond <- extract_summary_independent_pois(model_formula, "> 80 L/day of water use (respondent)", all_data)
  result_use_respond <- extract_use_respond$result_df   
  
  # Concatenate all results into one dataframe
  table_4b_independent <- rbind(result_prevecoli, result_prevecoli_stored, result_hwise, result_sat_appear, result_sat_taste, result_basicwater, result_onpremises, result_sufficient, result_sat_service, result_sat_afford, result_sat_avail, result_sat_pressure, result_use_respond)
  
  # Merge with results from exchangeable correlation structure above 
  merged_table4b <- merge(table_4b, table_4b_independent, by = "Outcome_Variable")
  
  # Divide SE from exchangeable correlation by independent correlation to assess model fit
  merged_table4b$se_ratio <- merged_table4b$Robust_se / merged_table4b$independent_se

# Primary outcome model fit evaluation
  # Model fit evaluation 
  # Prev. E. coli in source water - arm 
  model_formula <- ecoli_bin_source ~ arm + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh
  extract_prevecoli_network_multi <- extract_summary_independent(model_formula, "Neighborhood Intervention- RR", data_visit5)
  result_prevecoli_network_multi <- extract_prevecoli_network_multi$result_df
  
  # Prev. E. coli in source water - hh_water 
  model_formula <- ecoli_bin_source ~ hh_water + high_poverty + san_basic_obs_bl + secondary_complete_bl + months_in_hh + flooding_HH_yard + arm + final_match_strata_3
  extract_prevecoli_hh_multi <- extract_summary_independent(model_formula, "HH Water- RR", data_visit5)
  result_prevecoli_hh_multi <- extract_prevecoli_hh_multi$result_df
  
  table_5_independent <- rbind(result_prevecoli_network_multi, result_prevecoli_hh_multi)
  
  # Merge with results from exchangeable correlation structure above 
  merged_table5 <- merge(table_5a_RR, table_5_independent, by = "Outcome_Variable")
  
  # Divide SE from exchangeable correlation by independent correlation to assess model fit
  merged_table5$se_ratio <- merged_table5$Robust_se / merged_table5$independent_se
  
  rm(model_formula, extract_prevecoli_network_multi, result_prevecoli_network_multi, extract_prevecoli_hh_multi, result_prevecoli_hh_multi, table_5_independent)
  
  
# Previous code for E. coli levels at 12-month timepoint only 
  # filter dataset to visit 5 
  data_visit5 <- all_data %>%
    filter(visit == 5)
  
  data_visit5_network <- data_visit5 %>%
    filter(crossover == 0)
  
  table(data_visit5_network$arm, data_visit5_network$ecoli_bin_source)
  table(data_visit5$hh_water)
  
  # Prev. E. coli in source water - arm 
  ## Unadjusted
  model_formula <- ecoli_bin_source ~ arm 
  extract_prevecoli_network_multi <- extract_summary(model_formula, "Neighborhood intervention - RR", data_visit5_network)
  result_prevecoli_network_multi_unadjusted <- extract_prevecoli_network_multi$result_df
  
  ## Adjusted
  model_formula <- ecoli_bin_source ~ arm + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh
  extract_prevecoli_network_multi <- extract_summary(model_formula, "Neighborhood intervention - RR", data_visit5_network)
  result_prevecoli_network_multi <- extract_prevecoli_network_multi$result_df
  
  ## Risk difference
  #extract_prevecoli_network_rd <- gee_riskdiff(model_formula, "Neighborhood intervention- RD", data_visit5)
  #result_prevecoli_network_rd <- extract_prevecoli_network_rd$result_df
  
  # Prev. E. coli in source water - hh_water 
  ## Unadjusted
  model_formula <- ecoli_bin_source ~ hh_water
  extract_prevecoli_hh_multi <- extract_summary(model_formula, "HH Water- RR", data_visit5)
  result_prevecoli_hh_multi_unadjusted <- extract_prevecoli_hh_multi$result_df
  
  ## Adjusted
  model_formula <- ecoli_bin_source ~ hh_water + high_poverty + san_basic_obs_bl + secondary_complete_bl + months_in_hh + flooding_HH_yard + arm + final_match_strata_3
  extract_prevecoli_hh_multi <- extract_summary(model_formula, "HH Water- RR", data_visit5)
  result_prevecoli_hh_multi <- extract_prevecoli_hh_multi$result_df
  
  ## Risk difference
  #extract_prevecoli_hh_rd <- gee_riskdiff_pois(model_formula, "HH Water- RD", data_visit5) # log bin would not run
  #result_prevecoli_hh_rd <- extract_prevecoli_hh_rd$result_df
  
  table_5a_RR <- rbind(result_prevecoli_network_multi, result_prevecoli_hh_multi)
  table_5a_unadjusted <- rbind(result_prevecoli_hh_multi_unadjusted, result_prevecoli_network_multi_unadjusted)
  
  #table_5b_RD <- rbind(result_prevecoli_network_rd, result_prevecoli_hh_rd)
  
  # Write table output to CSV 
  write.csv(table_5a_RR, "data-out/Table 5a. Multivariate association between intervention and prevalence of E. coli in source water at visit 5 (Risk Ratio).csv")
  write.csv(table_5a_unadjusted, "data-out/Table 5a. Unadjusted association between intervention and prevalence of E. coli in source water at visit 5 (Risk Ratio).csv")
  
  
  #write.csv(table_5b_RD, "data-out/Table 5b. Multivariate association between intervention and prevalence of E. coli in source water at visit 5 (Risk Difference).csv")
  
  # Cleanup 
  rm(result_prevecoli_hh_multi, result_prevecoli_hh_rd, result_prevecoli_network_multi, result_prevecoli_network_rd, extract_prevecoli_hh_multi, extract_prevecoli_hh_rd, extract_prevecoli_network_multi, extract_prevecoli_network_rd, factor_vars, model_formula)

  # Old version of frequency table 
  network_table <- table1( ~ ecoli_bin_source + ecoli_bin_stored + basic_water + onpremises + sufficient + HWISE_insecure_new + always_sat_service + always_sat_afford + always_sat_avail + always_sat_pressure + always_sat_appear + always_sat_taste + use_respond_bin | arm,
                           data = all_data_network, 
                           overall=F,
                           render.missing = NULL) 
  
  network_table
  
  ## Function for adding a p-value column to table1 (deleted this because I could not get it to run for factor variables)
  pvalue <- function(x, ...) {
    # Construct vectors of data y, and groups (strata) g
    y <- unlist(x)
    g <- rep(1:length(x), times = sapply(x, length))
    if (is.numeric(y)) {
      # For numeric variables, perform simple linear regression
      model <- lm(y ~ as.factor(g))
      coef <- summary(model)$coefficients[2, 1]  # Extract coefficient (slope) from regression summary
      rr <- exp(coef)  # Calculate risk ratio
      p <- summary(model)$coefficients[2, 4]  # Extract p-value from regression summary
    } else {
      # For categorical variables, perform log-binomial regression
      model <- glm(y ~ as.factor(g), family = "binomial", control = list(maxit = 50))
      coef <- coef(summary(model))[2, 1]  # Extract coefficient from regression summary
      rr <- exp(coef)  # Calculate risk ratio
      p <- summary(model)$coefficients[2, 4]  # Extract p-value from regression summary
    }
    # Format the coefficient (risk ratio) and p-value into one cell
    c(paste(sprintf("%.2f", rr), " (", sprintf("%.3f", p), ")"))
  }
  
  # Data Analysis 
  # Remove crossover for analysis of network effect
  data_visit1_network <- data_visit1 %>%
    filter(crossover == 0)
  
  ## Create Table 1
  table_1 <- table1(
    ~ num_lt5 + num_HH + months_in_hh + HFIAS_score + high_poverty + san_basic_obs_bl + fixed_emp_pri + secondary_complete_bl +  human_feces + animal_feces  + HW_dwell + season_rainy + flooding_HH_yard | arm, 
    data = data_visit1_network, 
    overall=FALSE, 
    caption = caption, 
    footnote = footnote,
    render.continuous=my.render.cont,
    render.categorical=my.render.cat, 
    # extra.col = list(`Risk Ratio* (p-value)` = pvalue), # commenting out because we don't need for table
  ) 
  
  print(table_1)
  
  ## Save table to CSV output
  ### Write table1 object as dataframe
  table_1_df <- as.data.frame(table_1)
  write.csv(table_1_df, file = "data-out/Table 1. Baseline differences in covariates of interest (Network effect).csv")
  
  # Cleanup
  rm(caption, footnote, table_1, table_1_df)
  
  # Factorial analysis (categorical variable for intervention)
  # Prev. E. coli in source water 
  ## Unadjusted 
  model_formula <- ecoli_bin_source ~ combined_intervention
  extract_prevecoli <- extract_summary(model_formula, "E. coli in source water", all_data_network)
  model_summary_prevecoli <- extract_prevecoli$model_summary
  result_prevecoli_unadjusted <- extract_prevecoli$result_df
  
  ## Adjusted
  model_formula <- ecoli_bin_source ~ arm + hh_water + arm*hh_water + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh + fixed_emp_pri + num_lt5 + num_HH + flooding_HH_yard
  extract_prevecoli <- extract_summary(model_formula, "E. coli in source water", all_data_network)
  model_summary_prevecoli <- extract_prevecoli$model_summary
  result_prevecoli <- extract_prevecoli$result_df
  
  print(model_summary_prevecoli)
  
  print(model_summary_prevecoli$coefficients)
  
  # Prev. E. coli in stored water 
  ## Unadjusted 
  all_data_network$ecoli_bin_stored <- as.numeric(all_data_network$ecoli_bin_stored)
  model_formula <- ecoli_bin_stored ~ combined_intervention
  extract_prevecoli_stored <- extract_summary_pois(model_formula, "E. coli in stored water", all_data_network)
  model_summary_prevecoli_stored <- extract_prevecoli_stored$model_summary
  result_prevecoli_stored_unadjusted <- extract_prevecoli_stored$result_df
  
  ## Adjusted
  model_formula <- ecoli_bin_stored ~ combined_intervention + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh + fixed_emp_pri + num_lt5 + num_HH + flooding_HH_yard
  extract_prevecoli_stored <- extract_summary_pois(model_formula, "E. coli in stored water", all_data_network)
  model_summary_prevecoli_stored <- extract_prevecoli_stored$model_summary
  result_prevecoli_stored <- extract_prevecoli_stored$result_df
  
  # Prev. of access to at least basic water source - MODEL DID NOT CONVERGE with log binomial
  ## Unadjusted
  all_data_network$basic_water <- as.numeric(all_data_network$basic_water) # convert back to numeric for modified poisson
  model_formula <- basic_water ~ combined_intervention
  extract_basicwater <- extract_summary_pois(model_formula, "Access to at least basic water source", all_data_network)
  model_summary_basicwater <- extract_basicwater$model_summary
  result_basicwater_unadjusted <- extract_basicwater$result_df
  
  ## Adjusted
  model_formula <- basic_water ~ combined_intervention + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh + fixed_emp_pri + num_lt5 + num_HH + flooding_HH_yard
  extract_basicwater <- extract_summary_pois(model_formula, "Access to at least basic water source", all_data_network)
  extract_basicwater
  model_summary_basicwater <- extract_basicwater$model_summary
  result_basicwater <- extract_basicwater$result_df
  
  # Prev. of access to at water on premises - did not converge with log binomial
  ## Unadjusted
  all_data_network$onpremises <- as.numeric(all_data_network$onpremises) # convert back to numeric for modified poisson
  model_formula <- onpremises ~ combined_intervention 
  extract_onpremises <- extract_summary_pois(model_formula, "Access to water on the premises", all_data_network)
  model_summary_onpremises <- extract_onpremises$model_summary
  result_onpremises_unadjusted <- extract_onpremises$result_df
  
  ## Adjusted
  model_formula <- onpremises ~ combined_intervention + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh + fixed_emp_pri + num_lt5 + num_HH + flooding_HH_yard
  extract_onpremises <- extract_summary_pois(model_formula, "Access to water on the premises", all_data_network)
  model_summary_onpremises <- extract_onpremises$model_summary
  result_onpremises <- extract_onpremises$result_df
  
  # Prev. of HH with sufficient quantities of water when needed - did not run with log binomial
  ## Unadjusted
  all_data_network$sufficient <- as.numeric(all_data_network$sufficient) # convert back to numeric for modified poisson
  model_formula <- sufficient ~ combined_intervention
  extract_sufficient <- extract_summary_pois(model_formula, "Sufficient quantities of water when needed", all_data_network)
  model_summary_sufficient <- extract_sufficient$model_summary
  result_sufficient_unadjusted <- extract_sufficient$result_df
  
  ## Adjusted
  model_formula <- sufficient ~ combined_intervention + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh + fixed_emp_pri + num_lt5 + num_HH + flooding_HH_yard 
  extract_sufficient <- extract_summary_pois(model_formula, "Sufficient quantities of water when needed", all_data_network)
  model_summary_sufficient <- extract_sufficient$model_summary
  result_sufficient <- extract_sufficient$result_df
  
  # Prev. of HH with HWISE score >=12
  ## Unadjusted
  all_data_network$HWISE_insecure_new <- as.numeric(all_data_network$HWISE_insecure_new)
  model_formula <- HWISE_insecure_new ~ combined_intervention
  extract_hwise <- extract_summary_pois(model_formula, "Water insecure", all_data_network)
  model_summary_hwise <- extract_hwise$model_summary
  result_hwise_unadjusted <- extract_hwise$result_df
  
  ## Adjusted
  model_formula <- HWISE_insecure_new ~ combined_intervention + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh + fixed_emp_pri + num_lt5 + num_HH + flooding_HH_yard
  extract_hwise <- extract_summary_pois(model_formula, "Water insecure", all_data_network)
  model_summary_hwise <- extract_hwise$model_summary
  result_hwise <- extract_hwise$result_df
  
  # Prev. of HH always being satisfied with water service 
  ## Unadjusted
  all_data_network$always_sat_service <- as.numeric(all_data_network$always_sat_service)
  model_formula <- always_sat_service ~ combined_intervention
  extract_sat_service <- extract_summary_pois(model_formula, "Satisfied with water service", all_data_network)
  model_summary_sat_service <- extract_sat_service$model_summary
  result_sat_service_unadjusted <- extract_sat_service$result_df
  
  ## Adjusted
  model_formula <- always_sat_service ~ combined_intervention + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh + fixed_emp_pri + num_lt5 + num_HH + flooding_HH_yard
  extract_sat_service <- extract_summary_pois(model_formula, "Satisfied with water service", all_data_network)
  model_summary_sat_service <- extract_sat_service$model_summary
  result_sat_service <- extract_sat_service$result_df
  
  #Prev. of HH always being satisfied with water affordability
  ## Unadjusted
  all_data_network$always_sat_afford <- as.numeric(all_data_network$always_sat_afford)
  model_formula <- always_sat_afford ~ combined_intervention
  extract_sat_afford <- extract_summary_pois(model_formula, "Satisfied with water affordability", all_data_network)
  model_summary_sat_afford <- extract_sat_afford$model_summary
  result_sat_afford_unadjusted <- extract_sat_afford$result_df
  
  ## Adjusted
  model_formula <- always_sat_afford ~ combined_intervention + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh + fixed_emp_pri + num_lt5 + num_HH + flooding_HH_yard
  extract_sat_afford <- extract_summary_pois(model_formula, "Satisfied with water affordability", all_data_network)
  model_summary_sat_afford <- extract_sat_afford$model_summary
  result_sat_afford <- extract_sat_afford$result_df
  
  
  
  # Prev. of HH always being satisfied with water availability
  ## Unadjusted
  all_data_network$always_sat_avail <- as.numeric(all_data_network$always_sat_avail)
  model_formula <- always_sat_avail ~ combined_intervention
  extract_sat_avail <- extract_summary_pois(model_formula, "Satisfied with water availability", all_data_network)
  model_summary_sat_avail <- extract_sat_avail$model_summary
  result_sat_avail_unadjusted <- extract_sat_avail$result_df
  
  ## Adjusted
  model_formula <- always_sat_avail ~ combined_intervention + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh + fixed_emp_pri + num_lt5 + num_HH + flooding_HH_yard
  extract_sat_avail <- extract_summary_pois(model_formula, "Satisfied with water availability", all_data_network)
  model_summary_sat_avail <- extract_sat_avail$model_summary
  result_sat_avail <- extract_sat_avail$result_df
  
  # Prev. of HH always being satisfied with water pressure
  ## Unadjusted
  model_formula <- always_sat_pressure ~ combined_intervention
  extract_sat_pressure <- extract_summary(model_formula, "Satisfied with water pressure", all_data_network)
  model_summary_sat_pressure <- extract_sat_pressure$model_summary
  result_sat_pressure_unadjusted <- extract_sat_pressure$result_df
  
  ## Adjusted
  model_formula <- always_sat_pressure ~ combined_intervention + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh + fixed_emp_pri + num_lt5 + num_HH + flooding_HH_yard
  extract_sat_pressure <- extract_summary(model_formula, "Satisfied with water pressure", all_data_network)
  model_summary_sat_pressure <- extract_sat_pressure$model_summary
  result_sat_pressure <- extract_sat_pressure$result_df
  
  # Prev. of HH always being satisfied with water color and appearance
  ## Unadjusted
  model_formula <- always_sat_appear ~ combined_intervention
  extract_sat_appear <- extract_summary(model_formula, "Satisfied with water color and appearance", all_data_network)
  model_summary_sat_appear <- extract_sat_appear$model_summary
  result_sat_appear_unadjusted <- extract_sat_appear$result_df
  
  ## Adjusted
  model_formula <- always_sat_appear ~ combined_intervention + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh + fixed_emp_pri + num_lt5 + num_HH + flooding_HH_yard
  extract_sat_appear <- extract_summary(model_formula, "Satisfied with water color and appearance", all_data_network)
  model_summary_sat_appear <- extract_sat_appear$model_summary
  result_sat_appear <- extract_sat_appear$result_df
  
  # Prev. of HH always being satisfied with water taste and smell
  ## Unadjusted
  model_formula <- always_sat_taste ~ combined_intervention
  extract_sat_taste <- extract_summary(model_formula, "Satisfied with water taste and smell", all_data_network)
  model_summary_sat_taste <- extract_sat_taste$model_summary
  result_sat_taste_unadjusted <- extract_sat_taste$result_df
  
  ## Adjusted
  model_formula <- always_sat_taste ~ combined_intervention + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh + fixed_emp_pri + num_lt5 + num_HH + flooding_HH_yard
  extract_sat_taste <- extract_summary(model_formula, "Satisfied with water taste and smell", all_data_network)
  model_summary_sat_taste <- extract_sat_taste$model_summary
  result_sat_taste <- extract_sat_taste$result_df
  
  ## Binary water use (respondent)  - did not converge with log binomial
  ## Unadjusted
  all_data_network$use_respond_bin <- as.numeric(all_data_network$use_respond_bin)
  model_formula <- use_respond_bin ~ combined_intervention
  extract_use_respond <- extract_summary_pois(model_formula, ">= 80 L/day of water use (respondent)", all_data_network)
  model_summary_use_respond <- extract_use_respond$model_summary
  result_use_respond_unadjusted <- extract_use_respond$result_df
  
  ## Adjusted
  model_formula <- use_respond_bin ~ combined_intervention + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh + fixed_emp_pri + num_lt5 + num_HH + flooding_HH_yard
  extract_use_respond <- extract_summary_pois(model_formula, ">= 80 L/day of water use (respondent)", all_data_network)
  model_summary_use_respond <- extract_use_respond$model_summary
  result_use_respond <- extract_use_respond$result_df
  
  # Concatenate all results into one dataframe
  table_5 <- rbind(result_prevecoli, result_prevecoli_stored, result_sufficient, result_hwise, result_sat_service, result_sat_afford, result_sat_avail, result_sat_pressure, result_sat_appear, result_sat_taste, result_use_respond, result_basicwater, result_onpremises)
  
  # Create separate datasets to make forest plots 
  ## Risk ratios expected to be below 1
  table_5_lower <- rbind(result_prevecoli, result_prevecoli_stored, result_hwise)
  
  ## Risk ratios expected to be above 1 
  table_5_higher <- rbind(result_sufficient, result_sat_service, result_sat_afford, result_sat_avail, result_sat_pressure, result_sat_appear, result_sat_taste, result_use_respond, result_basicwater, result_onpremises)
  
  table_5_unadjusted <- rbind(result_prevecoli_unadjusted, result_prevecoli_stored_unadjusted, result_sufficient_unadjusted, result_hwise_unadjusted, result_sat_service_unadjusted, result_sat_afford_unadjusted, result_sat_avail_unadjusted, result_sat_pressure_unadjusted, result_sat_appear_unadjusted, result_sat_taste_unadjusted, result_use_respond_unadjusted, result_basicwater_unadjusted, result_onpremises_unadjusted)
  
  table_5_unadjusted
  
  # Write table output to CSV 
  write.csv(table_5, "data-out/Table 5. Multivariate association between combined intervention status and water quality and access.csv")
  write.csv(table_5_unadjusted, "data-out/Table 5. Unadjusted association between combined intervention status and water quality and access.csv")

  ## Plot results 
  table_5 <- table_5 %>%
    mutate(status = ifelse(Coefficient == "combined_intervention2", "Intervention neighborhood, No HH connection",
                           ifelse(Coefficient == "combined_intervention3", "Control neighborhood, HH connection", "Intervention neighborhood, HH connection")))
  
  table_5$status <- factor(table_5$status, levels = c("Intervention neighborhood, No HH connection","Control neighborhood, HH connection", "Intervention neighborhood, HH connection"))
  
  library(RColorBrewer)
  nb.cols <- 3
  family_colors<-colorRampPalette(brewer.pal(3, "Paired"))(nb.cols)
  
  dodge <- position_dodge(width = 1)
  
  plot_5 <- ggplot(table_5, aes(x = Risk_Ratio, y = Outcome_Variable)) +
    geom_point(aes(color = status), position = dodge) +  # Add color for different levels of 'status' and dodge position
    geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper, color = status), height = 0, position = dodge) +  # Confidence intervals with dodge position and matching color
    geom_vline(xintercept = 1, linetype = "dashed", color = "black") +  # Dashed line at x = 1
    scale_y_discrete(labels = function(y) str_wrap(y, width = 35)) +
    scale_color_manual(values = family_colors) +  # Customize colors for 'status'
    labs(title = "Network Effect on Water Quality and Access", 
         x = "Risk Ratio", 
         y = "Outcome Variable") + 
    theme_minimal() +
    theme(legend.title = element_blank(),
          axis.text.y = element_text(size = 10),
          axis.title.y = element_text(size = 12),
          axis.title.x = element_text(size = 12),
          plot.title = element_text(size = 14, face = "bold"))
  
  # Add horizontal lines between levels of Outcome_Variable
  y_levels <- levels(factor(table_5$Outcome_Variable))
  y_positions <- seq_along(y_levels) - 0.5
  for (y in y_positions) {
    plot_5 <- plot_5 + geom_hline(yintercept = y, color = "darkgrey", linetype = "solid", linewidth = 0.5)
  }
  
  plot_5
  
  ggsave(plot_5, file = "plots/Figure 5. Combination intervention effect on water quality and access.png", width = 8, height = 6, units = "in", dpi = 300)
  
  
  ## Stratified models 
#  Update function to pull the right coefficients 

  ## Log binomial
  extract_summary <- function(model_formula, outcome_var_name, data) {
    library(gee)
    
    # Fit GEE model
    model <- gee(model_formula, id = code, data = data, family = binomial(link = "log"), corst = "independence")
    
    # Extract model summary
    model_summary <- summary(model)
    
    # Get the index of the 'arm' predictor in the model formula
    arm_index <- which(names(model$coefficients) == "arm")
    
    # Extract robust standard error for 'arm'
    robust_se_arm <- sqrt(model$robust.variance[arm_index, arm_index])
    
    # Extract coefficients and compute risk ratio, p-value, and confidence interval
    # Extract coefficients and their details
    coef_table <- model_summary$coefficients[2:4, ]  # Extracting coefficients for each level of intervention
    coef_values <- coef_table[, 1]
    robust_se_values <- coef_table[, 4]
    
    # Calculate p-values using robust z score
    p_values <- 2 * (1 - pnorm(abs(coef_table[, 5])))
    
    # Calculate confidence intervals
    ci_lower <- exp(coef_values - 1.96 * robust_se_values)
    ci_upper <- exp(coef_values + 1.96 * robust_se_values)
    
    # Calculate risk ratios
    risk_ratios <- exp(coef_values)
    
    # Create a dataframe for the results
    result_df <- data.frame(
      Outcome_Variable = outcome_var_name,
      Coefficient = rownames(coef_table),
      Risk_Ratio = risk_ratios,
      P_Value = p_values,
      CI_Lower = ci_lower,
      CI_Upper = ci_upper,
      Robust_se = robust_se_values
    )
    
    # Return a list containing both the result dataframe and the model summary object
    return(list(result_df = result_df, model_summary = model_summary))
  }
  
  
  
  extract_summary_pois <- function(model_formula, outcome_var_name, data) {
    library(gee)
    
    # Fit GEE model
    model <- gee(model_formula, id = code, data = data, family = poisson(link = "log"), corst = "independence")
    
    # Extract model summary
    model_summary <- summary(model)
    
    # Extract coefficients and their details
    coef_table <- model_summary$coefficients[2:4, ]  # Extracting coefficients for each level of intervention
    coef_values <- coef_table[, 1]
    robust_se_values <- coef_table[, 4]
    
    # Calculate p-values using robust z score
    p_values <- 2 * (1 - pnorm(abs(coef_table[, 5])))
    
    # Calculate confidence intervals
    ci_lower <- exp(coef_values - 1.96 * robust_se_values)
    ci_upper <- exp(coef_values + 1.96 * robust_se_values)
    
    # Calculate risk ratios
    risk_ratios <- exp(coef_values)
    
    # Create a dataframe for the results
    result_df <- data.frame(
      Outcome_Variable = outcome_var_name,
      Coefficient = rownames(coef_table),
      Risk_Ratio = risk_ratios,
      P_Value = p_values,
      CI_Lower = ci_lower,
      CI_Upper = ci_upper,
      Robust_se = robust_se_values
    )
    
    # Return a list containing both the result dataframe and the model summary object
    return(list(result_df = result_df, model_summary = model_summary))
  }
  
  # Use 'combined_intervention' variable 
  model_formula <- ecoli_bin_source ~ combined_intervention + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh + fixed_emp_pri + num_lt5 + num_HH + flooding_HH_yard
  extract_prevecoli <- extract_summary(model_formula, "E. coli in source water", all_data_network)
  model_summary_prevecoli <- extract_prevecoli$model_summary
  result_prevecoli <- extract_prevecoli$result_df
  
  model_formula <- ecoli_bin_stored ~ combined_intervention + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh  + fixed_emp_pri + num_lt5 + num_HH + flooding_HH_yard
  extract_prevecoli_stored <- extract_summary_pois(model_formula, "E. coli in stored water", all_data_network)
  model_summary_prevecoli_stored <- extract_prevecoli_stored$model_summary
  result_prevecoli_stored <- extract_prevecoli_stored$result_df
  
  model_formula <- basic_water ~ combined_intervention + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh + fixed_emp_pri + num_lt5 + num_HH + flooding_HH_yard
  extract_basicwater <- extract_summary_pois(model_formula, "Access to at least basic water source", all_data_network)
  model_summary_basicwater <- extract_basicwater$model_summary
  result_basicwater <- extract_basicwater$result_df
  
  model_formula <- onpremises ~ combined_intervention + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh + fixed_emp_pri  + num_lt5 + num_HH + flooding_HH_yard
  extract_onpremises <- extract_summary_pois(model_formula, "Access to water on the premises", all_data_network)
  model_summary_onpremises <- extract_onpremises$model_summary
  result_onpremises <- extract_onpremises$result_df
  
  model_formula <- sufficient ~ combined_intervention + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh + fixed_emp_pri  + num_lt5 + num_HH + flooding_HH_yard
  extract_sufficient <- extract_summary_pois(model_formula, "Sufficient quantities of water when needed", all_data_network)
  model_summary_sufficient <- extract_sufficient$model_summary
  result_sufficient <- extract_sufficient$result_df
  
  model_formula <- HWISE_insecure_new ~ combined_intervention + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh + fixed_emp_pri  + num_lt5 + num_HH + flooding_HH_yard
  extract_hwise <- extract_summary_pois(model_formula, "Water insecure", all_data_network)
  model_summary_hwise <- extract_hwise$model_summary
  result_hwise <- extract_hwise$result_df
  
  # Use 'combined_intervention' variable 
  model_formula <- always_sat_service ~ combined_intervention + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 +    months_in_hh + fixed_emp_pri + num_lt5 + num_HH + flooding_HH_yard
  extract_sat_service <- extract_summary_pois(model_formula, "Satisfied with water service", all_data_network)
  model_summary_sat_service <- extract_sat_service$model_summary
  result_sat_service <- extract_sat_service$result_df
  
  model_formula <- always_sat_afford ~ combined_intervention + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh + fixed_emp_pri + num_lt5 + num_HH + flooding_HH_yard
  extract_sat_afford <- extract_summary_pois(model_formula, "Satisfied with water affordability", all_data_network)
  model_summary_sat_afford <- extract_sat_afford$model_summary
  result_sat_afford <- extract_sat_afford$result_df
  
  model_formula <- always_sat_avail ~ combined_intervention + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh + fixed_emp_pri + num_lt5 + num_HH + flooding_HH_yard
  extract_sat_avail <- extract_summary_pois(model_formula, "Satisfied with water availability", all_data_network)
  model_summary_sat_avail <- extract_sat_avail$model_summary
  result_sat_avail <- extract_sat_avail$result_df
  
  model_formula <- always_sat_pressure ~ combined_intervention + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh + fixed_emp_pri + num_lt5 + num_HH + flooding_HH_yard
  extract_sat_pressure <- extract_summary(model_formula, "Satisfied with water pressure", all_data_network)
  model_summary_sat_pressure <- extract_sat_pressure$model_summary
  result_sat_pressure <- extract_sat_pressure$result_df
  
  model_formula <- always_sat_appear ~ combined_intervention + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh + fixed_emp_pri  + num_lt5 + num_HH + flooding_HH_yard
  extract_sat_appear <- extract_summary(model_formula, "Satisfied with water color and appearance", all_data_network)
  model_summary_sat_appear <- extract_sat_appear$model_summary
  result_sat_appear <- extract_sat_appear$result_df
  
  model_formula <- always_sat_taste ~ combined_intervention + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh + fixed_emp_pri + num_lt5 + num_HH + flooding_HH_yard
  extract_sat_taste <- extract_summary(model_formula, "Satisfied with water taste and smell", all_data_network)
  model_summary_sat_taste <- extract_sat_taste$model_summary
  result_sat_taste <- extract_sat_taste$result_df
  
  model_formula <- use_respond_bin ~ combined_intervention + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh + fixed_emp_pri + num_lt5 + num_HH + flooding_HH_yard
  extract_use_respond <- extract_summary_pois(model_formula, ">= 80 L/day of water use (respondent)", all_data_network)
  model_summary_use_respond <- extract_use_respond$model_summary
  result_use_respond <- extract_use_respond$result_df
  
  # Concatenate all results into one dataframe
  table_5 <- rbind(result_prevecoli, result_prevecoli_stored, result_sufficient, result_hwise, result_sat_service, result_sat_afford, result_sat_avail, result_sat_pressure, result_sat_appear, result_sat_taste, result_use_respond, result_basicwater, result_onpremises)
  
  table_5_filtered <- table_5 %>%
    filter(Coefficient == "combined_intervention4")
  
  # Write table output to CSV 
  write.csv(table_5, "data-out/Table 5. Multivariate association between combined intervention status and water quality and access.csv")
  
  
  extract_summary <- function(model_formula, outcome_var_name, data) {
    library(gee)
    
    # Fit GEE model
    model <- gee(model_formula, id = code, data = data, family = binomial(link = "log"), corstr = "independence")
    
    # Extract model summary
    model_summary <- summary(model)
    
    # Extract coefficients for arm, hh_water, and interaction term arm*hh_water
    coef_table <- model_summary$coefficients[c("arm", "hh_water", "arm:hh_water"), ]
    coef_values <- coef_table[, 1] # Coefficients for arm, hh_water, and their interaction
    robust_se_values <- coef_table[, 4] # Robust standard errors
    
    # Calculate p-values using robust z score
    p_values <- 2 * (1 - pnorm(abs(coef_table[, 5])))
    
    # Calculate confidence intervals
    ci_lower <- exp(coef_values - 1.96 * robust_se_values)
    ci_upper <- exp(coef_values + 1.96 * robust_se_values)
    
    # Calculate risk ratios
    risk_ratios <- exp(coef_values)
    
    # Calculate combined effect (arm = 1 and hh_water = 1)
    combined_coef <- coef_values["arm"] + coef_values["hh_water"] + coef_values["arm:hh_water"]
    combined_se <- sqrt(robust_se_values["arm"]^2 + robust_se_values["hh_water"]^2 + robust_se_values["arm:hh_water"]^2) # should use both variance and covariances- check with Lance to make sure this is right 
    
    combined_rr <- exp(combined_coef)
    combined_ci_lower <- exp(combined_coef - 1.96 * combined_se)
    combined_ci_upper <- exp(combined_coef + 1.96 * combined_se)
    combined_p_value <- 2 * (1 - pnorm(abs(combined_coef / combined_se)))
    
    # Create a dataframe for the results
    result_df <- data.frame(
      Outcome_Variable = outcome_var_name,
      Coefficient = c("arm", "hh_water", "arm:hh_water", "arm & hh_water (combined)"),
      Risk_Ratio = c(risk_ratios, combined_rr),
      P_Value = c(p_values, combined_p_value),
      CI_Lower = c(ci_lower, combined_ci_lower),
      CI_Upper = c(ci_upper, combined_ci_upper),
      Robust_se = c(robust_se_values, combined_se)
    )
    
    # Return a list containing both the result dataframe and the model summary object
    return(list(result_df = result_df, model_summary = model_summary))
  }
  
  
  extract_summary_pois <- function(model_formula, outcome_var_name, data) {
    library(gee)
    
    # Fit GEE model
    model <- gee(model_formula, id = code, data = data, family = poisson(link = "log"), corstr = "independence")
    
    # Extract model summary
    model_summary <- summary(model)
    
    # Extract coefficients for arm, hh_water, and interaction term arm*hh_water
    coef_table <- model_summary$coefficients[c("arm", "hh_water", "arm:hh_water"), ]
    coef_values <- coef_table[, 1] # Coefficients for arm, hh_water, and their interaction
    robust_se_values <- coef_table[, 4] # Robust standard errors
    
    # Calculate p-values using robust z score
    p_values <- 2 * (1 - pnorm(abs(coef_table[, 5])))
    
    # Calculate confidence intervals
    ci_lower <- exp(coef_values - 1.96 * robust_se_values)
    ci_upper <- exp(coef_values + 1.96 * robust_se_values)
    
    # Calculate risk ratios
    risk_ratios <- exp(coef_values)
    
    # Calculate combined effect (arm = 1 and hh_water = 1)
    combined_coef <- coef_values["arm"] + coef_values["hh_water"] + coef_values["arm:hh_water"]
    combined_se <- sqrt(robust_se_values["arm"]^2 + robust_se_values["hh_water"]^2 + robust_se_values["arm:hh_water"]^2)
    
    combined_rr <- exp(combined_coef)
    combined_ci_lower <- exp(combined_coef - 1.96 * combined_se)
    combined_ci_upper <- exp(combined_coef + 1.96 * combined_se)
    combined_p_value <- 2 * (1 - pnorm(abs(combined_coef / combined_se)))
    
    # Create a dataframe for the results
    result_df <- data.frame(
      Outcome_Variable = outcome_var_name,
      Coefficient = c("arm", "hh_water", "arm:hh_water", "arm & hh_water (combined)"),
      Risk_Ratio = c(risk_ratios, combined_rr),
      P_Value = c(p_values, combined_p_value),
      CI_Lower = c(ci_lower, combined_ci_lower),
      CI_Upper = c(ci_upper, combined_ci_upper),
      Robust_se = c(robust_se_values, combined_se)
    )
    
    # Return a list containing both the result dataframe and the model summary object
    return(list(result_df = result_df, model_summary = model_summary))
  }
  
  # Run model 
  model <- gee(model_formula, data = all_data_network, id = code, family = poisson(link = "log"), corstr = "independence")
  model_summary <- summary(model)
  
  # Extract covariance matrix for total effect estimation
  cov_matrix <- model$robust.variance
  
  # Extract coefficients for each term we care about 
  coef_model <- model_summary$coefficients[c(2,3,20), ]
  coef_values <- coef_model[, 1] # Coefficients for arm, hh_water, and their interaction
  robust_se_values <- coef_model[, 4] # Robust standard errors
  
  # Calculate p-values using robust z score
  p_values <- 2 * (1 - pnorm(abs(coef_model[, 5])))
  
  # Calculate confidence intervals
  ci_lower <- exp(coef_values - 1.96 * robust_se_values)
  ci_upper <- exp(coef_values + 1.96 * robust_se_values)
  
  # Calculate risk ratios
  risk_ratios <- exp(coef_values)
  
  ## Total Effect
  ### Coefficient
  B_sum <- coef_model[1,1] + coef_model[2,1] + coef_model[3,1]
  
  ### Variance
  var_sum <- cov_matrix["arm1", "arm1"] + cov_matrix["hh_water", "hh_water"] + cov_matrix["arm1:hh_water", "arm1:hh_water"] + 
    2 * cov_matrix["arm1", "hh_water"] + 2 * cov_matrix["arm1", "arm1:hh_water"] + 2 * cov_matrix["hh_water", "arm1:hh_water"]
  ### Robust standard errors
  se_sum <- sqrt(var_sum)
  ### Confidence intervals
  ci_lower_total <- exp(B_sum - 1.96 * se_sum) 
  ci_upper_total <- exp(B_sum + 1.96 * se_sum)
  ### Risk ratio 
  RR_total <- exp(B_sum)
  ### P-value 
  z_total <- B_sum / se_sum
  p_total <- 2 * pnorm(-abs(z_total))
  
  # Create a dataframe for the results
  result_df <- data.frame(
    Outcome_Variable = "No E. coli in source water",
    Coefficient = c("arm", "hh_water", "arm:hh_water", "arm & hh_water (combined)"),
    Risk_Ratio = c(risk_ratios, RR_total),
    P_Value = c(p_values, p_total),
    CI_Lower = c(ci_lower, ci_lower_total),
    CI_Upper = c(ci_upper, ci_upper_total),
    Robust_se = c(robust_se_values, se_sum))
  
  
  # old code 
  extract_prevecoli <- extract_summary_pois(model_formula, "No E. coli in source water", all_data_network)
  model_summary_prevecoli <- extract_prevecoli$model_summary
  result_prevecoli <- extract_prevecoli$result_df
  
  extract_prevecoli_stored <- extract_summary_pois(model_formula, "No E. coli in stored water", all_data_network)
  model_summary_prevecoli_stored <- extract_prevecoli_stored$model_summary
  result_prevecoli_stored <- extract_prevecoli_stored$result_df
  
  extract_basicwater <- extract_summary_pois(model_formula, "Access to at least basic water source", all_data_network)
  extract_basicwater
  model_summary_basicwater <- extract_basicwater$model_summary
  result_basicwater <- extract_basicwater$result_df
  
  extract_onpremises <- extract_summary_pois(model_formula, "Access to water on the premises", all_data_network)
  model_summary_onpremises <- extract_onpremises$model_summary
  result_onpremises <- extract_onpremises$result_df
  
  extract_sufficient <- extract_summary_pois(model_formula, "Sufficient quantities of water when needed", all_data_network)
  model_summary_sufficient <- extract_sufficient$model_summary
  result_sufficient <- extract_sufficient$result_df
  
  extract_hwise <- extract_summary_pois(model_formula, "Water secure", all_data_network)
  model_summary_hwise <- extract_hwise$model_summary
  result_hwise <- extract_hwise$result_df
  
  extract_sat_service <- extract_summary_pois(model_formula, "Satisfied with water service", all_data_network)
  model_summary_sat_service <- extract_sat_service$model_summary
  result_sat_service <- extract_sat_service$result_df
  
  extract_sat_afford <- extract_summary_pois(model_formula, "Satisfied with water affordability", all_data_network)
  model_summary_sat_afford <- extract_sat_afford$model_summary
  result_sat_afford <- extract_sat_afford$result_df
  
  extract_sat_avail <- extract_summary_pois(model_formula, "Satisfied with water availability", all_data_network)
  model_summary_sat_avail <- extract_sat_avail$model_summary
  result_sat_avail <- extract_sat_avail$result_df
  
  extract_sat_pressure <- extract_summary(model_formula, "Satisfied with water pressure", all_data_network)
  model_summary_sat_pressure <- extract_sat_pressure$model_summary
  result_sat_pressure <- extract_sat_pressure$result_df
  
  extract_sat_appear <- extract_summary(model_formula, "Satisfied with water color and appearance", all_data_network)
  model_summary_sat_appear <- extract_sat_appear$model_summary
  result_sat_appear <- extract_sat_appear$result_df
  
  extract_sat_taste <- extract_summary(model_formula, "Satisfied with water taste and smell", all_data_network)
  model_summary_sat_taste <- extract_sat_taste$model_summary
  result_sat_taste <- extract_sat_taste$result_df
  
  extract_use_respond <- extract_summary_pois(model_formula, ">= 80 L/day of water use (respondent)", all_data_network)
  model_summary_use_respond <- extract_use_respond$model_summary
  result_use_respond <- extract_use_respond$result_df
  
  
  
  # Make sure all variables are correctly classified in my dataset
  #factor_vars <- c("arm", "final_match_strata_3", "san_basic_obs_bl", "secondary_complete_bl", "ecoli_bin_source", "ecoli_bin_stored", "basic_water", "onpremises", "sufficient", "HWISE_insecure_new", "always_sat_service", "always_sat_afford", "always_sat_avail", "always_sat_pressure", "always_sat_appear", "always_sat_taste", "use_respond_bin", "no_ecoli_source", "no_ecoli_stored", "HWISE_secure")
  
  #all_data <- all_data %>%
  # dplyr::mutate(across(all_of(factor_vars), as.factor))
  
  
  # Identify crossover households 
  arm_changes <- all_data %>%
    dplyr::group_by(main_id) %>%
    dplyr::summarize(arm_changed = n_distinct(arm) > 1)
  
  arm_changes <- arm_changes %>%
    dplyr::mutate(crossover = ifelse(arm_changes$arm_changed == TRUE, 1, 0))
  
  all_data <- merge(all_data, arm_changes, by = "main_id", all.x = TRUE)
  summary(data$C_25)
  

  
  # Frequency table for new outcomes
  variables_new <- c("no_ecoli_source", "no_ecoli_stored", "HWISE_secure")
  
  
  
  # Network status - all timepoints
  data_network <- all_data %>%
    filter(crossover == 0)
  
  result_list <- frequency_tables_for_vars(data_network, variables_new, "arm")
  combined_results <- bind_rows(result_list)
  
  network_new <- combined_results %>%
    filter(variable_value == 1)
  
  # Network status- 12 months
  data_network_visit5 <- all_data %>%
    filter(visit == 5 & crossover == 0)
  
  result_list <- frequency_tables_for_vars(data_network_visit5, variables_new, "arm")
  combined_results <- bind_rows(result_list)
  
  network_new_visit5 <- combined_results %>%
    filter(variable_value == 1)
  
  # HH connection
  result_list <- frequency_tables_for_vars(all_data, variables_new, "hh_water")
  combined_results <- bind_rows(result_list)
  
  hh_new <- combined_results %>%
    filter(variable_value == 1)
  
  # HH connection - 12 months
  all_data_visit5 <- all_data %>%
    filter(visit == 5)
  
  result_list <- frequency_tables_for_vars(all_data_visit5, variables_new, "hh_water")
  combined_results <- bind_rows(result_list)
  
  hh_new_visit5 <- combined_results %>%
    filter(variable_value ==1)
  
  

  #### Model Construction - Risk Difference (Table 4a)
  # GEE function - binomial family with identity link 
  gee_riskdiff <- function(model_formula, outcome_var_name, data) {
    
    # Fit GEE model
    model <- gee(model_formula, id = code, data = data, family = binomial(link = "identity"), corst = "independence", na.action = na.omit)
    
    # Extract model summary
    model_summary <- summary(model)
    
    # Extract coefficients and compute risk ratio, p-value, and confidence interval
    coef <- model_summary$coefficients[2, 1]
    p_value <- 2 * (1-pnorm(abs(model_summary$coefficients[2, 5]))) # Calculated based on robust z
    robust_se <- abs(model_summary$coefficients[2, 4]) # Standard error from robust z
    ci_lower <- coef - 1.96 * robust_se
    ci_upper <- coef + 1.96 * robust_se
    
    
    # Create a dataframe for the result
    result_df <- data.frame(
      Outcome_Variable = outcome_var_name,
      Risk_Difference = coef,
      P_Value = p_value,
      CI_Lower = ci_lower,
      CI_Upper = ci_upper,
      Robust_se = robust_se
    )
    
    # Return a list containing both the result dataframe and the model summary object
    return(list(result_df = result_df, model_summary = model_summary))
  }
  
  # GEE function - poisson family with identity link 
  gee_riskdiff_pois <- function(model_formula, outcome_var_name, data) {
    
    # Fit GEE model
    model <- gee(model_formula, id = code, data = data, family = poisson(link = "identity"), corst = "independence", na.action = na.omit)
    
    # Extract model summary
    model_summary <- summary(model)
    
    # Extract coefficients and compute risk ratio, p-value, and confidence interval
    coef <- model_summary$coefficients[2, 1]
    p_value <- 2 * (1-pnorm(abs(model_summary$coefficients[2, 5]))) # Calculated based on robust z
    robust_se <- abs(model_summary$coefficients[2, 4]) # Standard error from robust z
    ci_lower <- coef - 1.96 * robust_se
    ci_upper <- coef + 1.96 * robust_se
    
    
    # Create a dataframe for the result
    result_df <- data.frame(
      Outcome_Variable = outcome_var_name,
      Risk_Difference = coef,
      P_Value = p_value,
      CI_Lower = ci_lower,
      CI_Upper = ci_upper,
      Robust_se = robust_se
    )
    
    # Return a list containing both the result dataframe and the model summary object
    return(list(result_df = result_df, model_summary = model_summary))
  }
  
  # Run models using function above 
  ## Prev. E. coli in source water 
  model_formula <- ecoli_bin_source ~ arm + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh
  extract_prevecoli <- gee_riskdiff(model_formula, "Prev. E. coli in source water", all_data_network)
  result_prevecoli <- extract_prevecoli$result_df
  
  ## Prev. E. coli in stored water 
  model_formula <- ecoli_bin_stored ~ arm + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh
  extract_prevecoli_stored <- gee_riskdiff(model_formula, "Prev. E. coli in stored water", all_data_network)
  result_prevecoli_stored <- extract_prevecoli_stored$result_df
  
  ## Prev. of access to at least basic water source - MODEL DID NOT CONVERGE
  #model_formula <- basic_water ~ arm + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh
  #extract_basicwater <- gee_riskdiff(model_formula, "Prev. of access to at least basic water source", all_data_network)
  #result_basicwater <- extract_basicwater$result_df
  
  ## Prev. of access to at least basic water source - poisson with identity link 
  model_formula <- basic_water ~ arm + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh
  extract_basicwater <- gee_riskdiff_pois(model_formula, "Prev. of access to at least basic water source", all_data_network)
  result_basicwater <- extract_basicwater$result_df
  
  ## Prev. of access to at water on premises - did not run with either poisson or binomial
  #model_formula <- onpremises ~ arm + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh
  #extract_onpremises <- gee_riskdiff_pois(model_formula, "Prev. of access to water on the premises", all_data)
  #result_onpremises <- extract_onpremises$result_df
  
  ## Prev. of HH with sufficient quantities of water when needed 
  model_formula <- sufficient ~ arm + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh
  extract_sufficient <- gee_riskdiff(model_formula, "Prev. of HH with sufficient quantities of water when needed", all_data_network)
  result_sufficient <- extract_sufficient$result_df
  
  ## Prev. of HH with HWISE score >=12
  model_formula <- HWISE_insecure_new ~ arm + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh
  extract_hwise <- gee_riskdiff(model_formula, "Prev. of HH with water insecurity", all_data_network)
  result_hwise <- extract_hwise$result_df
  
  ## Prev. of HH always being satisfied with water service 
  model_formula <- always_sat_service ~ arm + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh
  extract_sat_service <- gee_riskdiff(model_formula, "Prev. of HH always being satisfied with water service", all_data_network)
  result_sat_service <- extract_sat_service$result_df
  
  ## Prev. of HH always being satisfied with water affordability
  model_formula <- always_sat_afford ~ arm + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh
  extract_sat_afford <- gee_riskdiff(model_formula, "Prev. of HH always being satisfied with water affordability", all_data_network)
  result_sat_afford <- extract_sat_afford$result_df
  
  ## Prev. of HH always being satisfied with water availability
  model_formula <- always_sat_avail ~ arm + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh
  extract_sat_avail <- gee_riskdiff(model_formula, "Prev. of HH always being satisfied with water availability", all_data_network)
  result_sat_avail <- extract_sat_avail$result_df
  
  ## Prev. of HH always being satisfied with water pressure
  model_formula <- always_sat_pressure ~ arm + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh
  extract_sat_pressure <- gee_riskdiff(model_formula, "Prev. of HH always being satisfied with water pressure", all_data_network)
  result_sat_pressure <- extract_sat_pressure$result_df
  
  ## Prev. of HH always being satisfied with water color and appearance
  model_formula <- always_sat_appear ~ arm + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh
  extract_sat_appear <- gee_riskdiff(model_formula, "Prev. of HH always being satisfied with water color and appearance", all_data_network)
  result_sat_appear <- extract_sat_appear$result_df
  
  ## Prev. of HH always being satisfied with water taste and smell
  model_formula <- always_sat_taste ~ arm + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh
  extract_sat_taste <- gee_riskdiff(model_formula, "Prev. of HH always being satisfied with water taste and smell",all_data_network)
  result_sat_taste <- extract_sat_taste$result_df
  
  ## Binary water use (respondent)  
  model_formula <- use_respond_bin ~ arm + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh
  extract_use_respond <- gee_riskdiff(model_formula, "> 80 L/day of water use (respondent)", all_data_network)
  result_use_respond <- extract_use_respond$result_df
  
  # Concatenate all results into one dataframe
  table_4a_rd <- rbind(result_prevecoli, result_prevecoli_stored, result_sufficient, result_hwise, result_sat_service, result_sat_afford, result_sat_avail, result_sat_pressure, result_sat_appear, result_sat_taste, result_use_respond, result_basicwater)
  
  table_4a_rd
  
  # Write table output to CSV 
  write.csv(table_4a_rd, "data-out/Table 4a- Risk Differences. Multivariate association between neighborhood-intervention and water quality and access.csv")

  
  #### Model Construction - Risk Difference (Table 4b)
  ## Prev. E. coli in source water 
  model_formula <- ecoli_bin_source ~ hh_water + high_poverty + san_basic_obs_bl + secondary_complete_bl + months_in_hh + flooding_HH_yard + arm + final_match_strata_3
  extract_prevecoli <- gee_riskdiff(model_formula, "Prev. E. coli in source water", all_data)
  result_prevecoli <- extract_prevecoli$result_df
  
  ## Prev. E. coli in stored water 
  model_formula <- ecoli_bin_stored ~ hh_water + high_poverty + san_basic_obs_bl + secondary_complete_bl + months_in_hh + flooding_HH_yard + arm + final_match_strata_3
  extract_prevecoli_stored <- gee_riskdiff(model_formula, "Prev. E. coli in stored water", all_data)
  result_prevecoli_stored <- extract_prevecoli_stored$result_df
  
  ## Prev. of access to at least basic water source - MODEL DID NOT CONVERGE
  #model_formula <- basic_water ~ hh_water + high_poverty + san_basic_obs_bl + secondary_complete_bl + months_in_hh + flooding_HH_yard + arm + final_match_strata_3
  #extract_basicwater <- gee_riskdiff(model_formula, "Prev. of access to at least basic water source", all_data)
  #result_basicwater <- extract_basicwater$result_df
  
  ## Prev. of access to at least basic water source - poisson with identity link 
  model_formula <- basic_water ~ hh_water + high_poverty + san_basic_obs_bl + secondary_complete_bl + months_in_hh + flooding_HH_yard + arm + final_match_strata_3
  extract_basicwater <- gee_riskdiff_pois(model_formula, "Prev. of access to at least basic water source", all_data)
  result_basicwater <- extract_basicwater$result_df
  
  ## Prev. of access to at water on premises - did not converge
  #model_formula <- onpremises ~ hh_water + high_poverty + san_basic_obs_bl + secondary_complete_bl + months_in_hh + flooding_HH_yard + arm + final_match_strata_3
  #extract_onpremises <- gee_riskdiff(model_formula, "Prev. of access to water on the premises", all_data)
  #result_onpremises <- extract_onpremises$result_df
  
  ## Prev. of access to water on premises - poisson with identity link
  model_formula <- onpremises ~ hh_water + high_poverty + san_basic_obs_bl + secondary_complete_bl + months_in_hh + flooding_HH_yard + arm + final_match_strata_3
  extract_onpremises <- gee_riskdiff_pois(model_formula, "Prev. of access to water on the premises", all_data)
  result_onpremises <- extract_onpremises$result_df
  
  ## Prev. of HH with sufficient quantities of water when needed 
  model_formula <- sufficient ~ hh_water + high_poverty + san_basic_obs_bl + secondary_complete_bl + months_in_hh + flooding_HH_yard + arm + final_match_strata_3
  extract_sufficient <- gee_riskdiff(model_formula, "Prev. of HH with sufficient quantities of water when needed", all_data)
  result_sufficient <- extract_sufficient$result_df
  
  ## Prev. of HH with HWISE score >=12
  model_formula <- HWISE_insecure ~ hh_water + high_poverty + san_basic_obs_bl + secondary_complete_bl + months_in_hh + flooding_HH_yard + arm + final_match_strata_3
  extract_hwise <- gee_riskdiff_pois(model_formula, "Prev. of HH with water insecurity", all_data)
  result_hwise <- extract_hwise$result_df
  
  ## Prev. of HH always being satisfied with water service 
  model_formula <- always_sat_service ~ hh_water + high_poverty + san_basic_obs_bl + secondary_complete_bl + months_in_hh + flooding_HH_yard + arm + final_match_strata_3
  extract_sat_service <- gee_riskdiff(model_formula, "Prev. of HH always being satisfied with water service", all_data)
  result_sat_service <- extract_sat_service$result_df
  
  ## Prev. of HH always being satisfied with water affordability
  model_formula <- always_sat_afford ~ hh_water + high_poverty + san_basic_obs_bl + secondary_complete_bl + months_in_hh + flooding_HH_yard + arm + final_match_strata_3
  extract_sat_afford <- gee_riskdiff(model_formula, "Prev. of HH always being satisfied with water affordability", all_data)
  result_sat_afford <- extract_sat_afford$result_df
  
  ## Prev. of HH always being satisfied with water availability
  model_formula <- always_sat_avail ~ hh_water + high_poverty + san_basic_obs_bl + secondary_complete_bl + months_in_hh + flooding_HH_yard + arm + final_match_strata_3
  extract_sat_avail <- gee_riskdiff(model_formula, "Prev. of HH always being satisfied with water availability", all_data)
  result_sat_avail <- extract_sat_avail$result_df
  
  ## Prev. of HH always being satisfied with water pressure
  model_formula <- always_sat_pressure ~ hh_water + high_poverty + san_basic_obs_bl + secondary_complete_bl + months_in_hh + flooding_HH_yard + arm + final_match_strata_3
  extract_sat_pressure <- gee_riskdiff(model_formula, "Prev. of HH always being satisfied with water pressure", all_data)
  result_sat_pressure <- extract_sat_pressure$result_df
  
  ## Prev. of HH always being satisfied with water color and appearance
  model_formula <- always_sat_appear ~ hh_water + high_poverty + san_basic_obs_bl + secondary_complete_bl + months_in_hh + flooding_HH_yard + arm + final_match_strata_3
  extract_sat_appear <- gee_riskdiff(model_formula, "Prev. of HH always being satisfied with water color and appearance", all_data)
  result_sat_appear <- extract_sat_appear$result_df
  
  ## Prev. of HH always being satisfied with water taste and smell
  model_formula <- always_sat_taste ~ hh_water + high_poverty + san_basic_obs_bl + secondary_complete_bl + months_in_hh + flooding_HH_yard + arm + final_match_strata_3
  extract_sat_taste <- gee_riskdiff(model_formula, "Prev. of HH always being satisfied with water taste and smell", all_data)
  result_sat_taste <- extract_sat_taste$result_df
  
  ## Binary water use (respondent)  
  model_formula <- use_respond_bin ~ hh_water + high_poverty + san_basic_obs_bl + secondary_complete_bl + months_in_hh + flooding_HH_yard + arm + final_match_strata_3
  extract_use_respond <- gee_riskdiff(model_formula, "> 80 L/day of water use (respondent)", all_data)
  result_use_respond <- extract_use_respond$result_df
  
  # Concatenate all results into one dataframe
  table_4b_rd <- rbind(result_prevecoli, result_prevecoli_stored, result_sufficient, result_hwise, result_sat_service, result_sat_afford, result_sat_avail, result_sat_pressure, result_sat_appear, result_sat_taste, result_use_respond, result_basicwater, result_onpremises)
  
  table_4b_rd
  
  # Write table output to CSV 
  write.csv(table_4b_rd, "data-out/Table 4b- Risk Differences. Multivariate association between HH water connection status and water quality and access.csv")
  
  # Cleanup 
  rm(extract_basicwater, extract_hwise, extract_onpremises, extract_prevecoli, extract_prevecoli_stored, extract_sat_afford, extract_sat_appear, extract_sat_avail, extract_sat_pressure, extract_sat_service, extract_sat_taste, extract_sufficient, extract_use_respond, model_summary_basicwater, model_summary_hwise, model_summary_onpremises, model_summary_prevecoli, model_summary_prevecoli_stored, model_summary_sat_afford, model_summary_sat_taste, model_summary_sat_appear, model_summary_sat_avail, model_summary_sat_pressure, model_summary_sat_service, model_summary_set_taste, model_summary_sufficient, model_summary_use_respond, result_basicwater, result_hwise, result_onpremises, result_prevecoli, result_prevecoli_stored, result_sat_afford, result_sat_appear, result_sat_avail, result_sat_pressure, result_sat_service, result_sat_taste, result_sufficient, result_use_respond, model_formula, table_4a_independence, variable)

  ## Stratified models by visit
  # Make modified poisson outcomes numeric
  all_data_network$HWISE_insecure_new <- as.numeric(all_data_network$HWISE_insecure_new)
  all_data_network$visit <- as.factor(all_data_network$visit)
  
  network_strata <- split(all_data_network, all_data_network$visit)
  
  # Frequency table for HWISE_insecure across visit 
  results <- lapply(network_strata, function(sub_data) {
    frequency_table(sub_data, "HWISE_insecure_new", "arm")
  })
  
  combined_results <- do.call(rbind, results)
  
  # HWISE - stratified models 
  ## Function to apply model across all strata 
  model <- gee(HWISE_insecure_new ~ arm + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh + fixed_emp_pri + visit + arm*visit, id = code, data = all_data_network, family = poisson(link = "log"), corst = "independence") 
  
  
  cov_matrix <- model$robust.variance
  results <- summary(model)
  
  # Extract results to table
  # Visit 1 (coef 2)
  coef <- results$coefficients[2,]
  pr <- exp(coef[1])
  ci_lower <- exp(coef[1] - 1.96*coef[4])
  ci_upper <- exp(coef[1] + 1.96*coef[4])
  pval <- 2*(1-pnorm(abs(coef[5])))
  
  visit1_df <- data.frame(
    Visit = "1",
    Prevalence_Ratio = pr,
    P_Value = pval,
    CI_Lower = ci_lower,
    CI_Upper = ci_upper)
  
  # Visit 2 (coef 2, 16, 20)
  coef <- results$coefficients[c(2, 16, 20),]
  coef_sum <- sum(coef[c(1,2,3),1])
  pr <- exp(coef_sum)
  
  var_sum <- cov_matrix["arm", "arm"] + cov_matrix["visit2", "visit2"] + cov_matrix["arm:visit2", "arm:visit2"] + 
    2 * cov_matrix["arm", "visit2"] + 2 * cov_matrix["arm", "arm:visit2"] + 2 * cov_matrix["visit2", "arm:visit2"]
  
  se_sum <- sqrt(var_sum)
  
  ci_lower <- exp(coef_sum - 1.96 * se_sum) 
  ci_upper <- exp(coef_sum + 1.96 * se_sum)
  
  z_total <- coef_sum / se_sum
  pval<- 2 * pnorm(-abs(z_total))
  
  
  visit2_df <- data.frame(
    Visit = "2",
    Prevalence_Ratio = pr,
    P_Value = pval,
    CI_Lower = ci_lower,
    CI_Upper = ci_upper)
  
  # Visit 3 (coef 2, 17, 21)
  coef <- results$coefficients[c(2, 17, 21),]
  coef_sum <- sum(coef[c(1,2,3),1])
  pr <- exp(coef_sum)
  
  var_sum <- cov_matrix["arm", "arm"] + cov_matrix["visit3", "visit3"] + cov_matrix["arm:visit3", "arm:visit3"] + 
    2 * cov_matrix["arm", "visit3"] + 2 * cov_matrix["arm", "arm:visit3"] + 2 * cov_matrix["visit3", "arm:visit3"]
  
  se_sum <- sqrt(var_sum)
  
  ci_lower <- exp(coef_sum - 1.96 * se_sum) 
  ci_upper <- exp(coef_sum + 1.96 * se_sum)
  
  z_total <- coef_sum / se_sum
  pval<- 2 * pnorm(-abs(z_total))
  
  
  visit3_df <- data.frame(
    Visit = "3",
    Prevalence_Ratio = pr,
    P_Value = pval,
    CI_Lower = ci_lower,
    CI_Upper = ci_upper)
  
  # Visit 4 (coef 2, 18, 22)
  coef <- results$coefficients[c(2, 18, 22),]
  coef_sum <- sum(coef[c(1,2,3),1])
  pr <- exp(coef_sum)
  
  var_sum <- cov_matrix["arm", "arm"] + cov_matrix["visit4", "visit4"] + cov_matrix["arm:visit4", "arm:visit4"] + 
    2 * cov_matrix["arm", "visit4"] + 2 * cov_matrix["arm", "arm:visit4"] + 2 * cov_matrix["visit4", "arm:visit4"]
  
  se_sum <- sqrt(var_sum)
  
  ci_lower <- exp(coef_sum - 1.96 * se_sum) 
  ci_upper <- exp(coef_sum + 1.96 * se_sum)
  
  z_total <- coef_sum / se_sum
  pval<- 2 * pnorm(-abs(z_total))
  
  
  visit4_df <- data.frame(
    Visit = "4",
    Prevalence_Ratio = pr,
    P_Value = pval,
    CI_Lower = ci_lower,
    CI_Upper = ci_upper)
  
  # Visit 5 (coef 2, 19, 23)
  coef <- results$coefficients[c(2, 19, 23),]
  coef_sum <- sum(coef[c(1,2,3),1])
  pr <- exp(coef_sum)
  
  var_sum <- cov_matrix["arm", "arm"] + cov_matrix["visit5", "visit5"] + cov_matrix["arm:visit5", "arm:visit5"] + 
    2 * cov_matrix["arm", "visit5"] + 2 * cov_matrix["arm", "arm:visit5"] + 2 * cov_matrix["visit5", "arm:visit5"]
  
  se_sum <- sqrt(var_sum)
  
  ci_lower <- exp(coef_sum - 1.96 * se_sum) 
  ci_upper <- exp(coef_sum + 1.96 * se_sum)
  
  z_total <- coef_sum / se_sum
  pval<- 2 * pnorm(-abs(z_total))
  
  
  visit5_df <- data.frame(
    Visit = "5",
    Prevalence_Ratio = pr,
    P_Value = pval,
    CI_Lower = ci_lower,
    CI_Upper = ci_upper)
  
  result_hwise_network_stratified <- rbind(visit1_df, visit2_df, visit3_df, visit4_df, visit5_df)
  

  # Frequency table for always_sat_taste across visit 
  results <- lapply(network_strata, function(sub_data) {
    frequency_table(sub_data, "always_sat_taste", "arm")
  })
  
  combined_results <- do.call(rbind, results)
  
  ## Function to apply model across all strata 
  model <- gee(always_sat_taste ~ arm + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh + fixed_emp_pri + visit + arm*visit, id = code, data = all_data_network, family = poisson(link = "log"), corst = "independence") 
  
  
  2*(1-pnorm(abs(0.4217283119)))
  
  cov_matrix <- model$robust.variance
  results <- summary(model)
  
  # Extract results to table
  # Visit 1 (coef 2)
  coef <- results$coefficients[2,]
  pr <- exp(coef[1])
  ci_lower <- exp(coef[1] - 1.96*coef[4])
  ci_upper <- exp(coef[1] + 1.96*coef[4])
  pval <- 2*(1-pnorm(abs(coef[5])))
  
  visit1_df <- data.frame(
    Visit = "1",
    Prevalence_Ratio = pr,
    P_Value = pval,
    CI_Lower = ci_lower,
    CI_Upper = ci_upper)
  
  # Visit 2 (coef 2, 16, 20)
  coef <- results$coefficients[c(2, 16, 20),]
  coef_sum <- sum(coef[c(1,2,3),1])
  pr <- exp(coef_sum)
  
  var_sum <- cov_matrix["arm", "arm"] + cov_matrix["visit2", "visit2"] + cov_matrix["arm:visit2", "arm:visit2"] + 
    2 * cov_matrix["arm", "visit2"] + 2 * cov_matrix["arm", "arm:visit2"] + 2 * cov_matrix["visit2", "arm:visit2"]
  
  se_sum <- sqrt(var_sum)
  
  ci_lower <- exp(coef_sum - 1.96 * se_sum) 
  ci_upper <- exp(coef_sum + 1.96 * se_sum)
  
  z_total <- coef_sum / se_sum
  pval<- 2 * pnorm(-abs(z_total))
  
  
  visit2_df <- data.frame(
    Visit = "2",
    Prevalence_Ratio = pr,
    P_Value = pval,
    CI_Lower = ci_lower,
    CI_Upper = ci_upper)
  
  # Visit 3 (coef 2, 17, 21)
  coef <- results$coefficients[c(2, 17, 21),]
  coef_sum <- sum(coef[c(1,2,3),1])
  pr <- exp(coef_sum)
  
  var_sum <- cov_matrix["arm", "arm"] + cov_matrix["visit3", "visit3"] + cov_matrix["arm:visit3", "arm:visit3"] + 
    2 * cov_matrix["arm", "visit3"] + 2 * cov_matrix["arm", "arm:visit3"] + 2 * cov_matrix["visit3", "arm:visit3"]
  
  se_sum <- sqrt(var_sum)
  
  ci_lower <- exp(coef_sum - 1.96 * se_sum) 
  ci_upper <- exp(coef_sum + 1.96 * se_sum)
  
  z_total <- coef_sum / se_sum
  pval<- 2 * pnorm(-abs(z_total))
  
  
  visit3_df <- data.frame(
    Visit = "3",
    Prevalence_Ratio = pr,
    P_Value = pval,
    CI_Lower = ci_lower,
    CI_Upper = ci_upper)
  
  # Visit 4 (coef 2, 18, 22)
  coef <- results$coefficients[c(2, 18, 22),]
  coef_sum <- sum(coef[c(1,2,3),1])
  pr <- exp(coef_sum)
  
  var_sum <- cov_matrix["arm", "arm"] + cov_matrix["visit4", "visit4"] + cov_matrix["arm:visit4", "arm:visit4"] + 
    2 * cov_matrix["arm", "visit4"] + 2 * cov_matrix["arm", "arm:visit4"] + 2 * cov_matrix["visit4", "arm:visit4"]
  
  se_sum <- sqrt(var_sum)
  
  ci_lower <- exp(coef_sum - 1.96 * se_sum) 
  ci_upper <- exp(coef_sum + 1.96 * se_sum)
  
  z_total <- coef_sum / se_sum
  pval<- 2 * pnorm(-abs(z_total))
  
  
  visit4_df <- data.frame(
    Visit = "4",
    Prevalence_Ratio = pr,
    P_Value = pval,
    CI_Lower = ci_lower,
    CI_Upper = ci_upper)
  
  # Visit 5 (coef 2, 19, 23)
  coef <- results$coefficients[c(2, 19, 23),]
  coef_sum <- sum(coef[c(1,2,3),1])
  pr <- exp(coef_sum)
  
  var_sum <- cov_matrix["arm", "arm"] + cov_matrix["visit5", "visit5"] + cov_matrix["arm:visit5", "arm:visit5"] + 
    2 * cov_matrix["arm", "visit5"] + 2 * cov_matrix["arm", "arm:visit5"] + 2 * cov_matrix["visit5", "arm:visit5"]
  
  se_sum <- sqrt(var_sum)
  
  ci_lower <- exp(coef_sum - 1.96 * se_sum) 
  ci_upper <- exp(coef_sum + 1.96 * se_sum)
  
  z_total <- coef_sum / se_sum
  pval<- 2 * pnorm(-abs(z_total))
  
  
  visit5_df <- data.frame(
    Visit = "5",
    Prevalence_Ratio = pr,
    P_Value = pval,
    CI_Lower = ci_lower,
    CI_Upper = ci_upper)
  
  result_sattaste_stratified <- rbind(visit1_df, visit2_df, visit3_df, visit4_df, visit5_df)
  

  # Frequency table for always_sat_taste across visit 
  results <- lapply(network_strata, function(sub_data) {
    frequency_table(sub_data, "always_sat_appear", "arm")
  })
  
  combined_results <- do.call(rbind, results)
  
  ## Function to apply model across all strata 
  ## Function to apply model across all strata 
  model <- gee(always_sat_appear ~ arm + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh + fixed_emp_pri + visit + arm*visit, id = code, data = all_data_network, family = poisson(link = "log"), corst = "independence") 
  
  cov_matrix <- model$robust.variance
  results <- summary(model)
  
  # Extract results to table
  # Visit 1 (coef 2)
  coef <- results$coefficients[2,]
  pr <- exp(coef[1])
  ci_lower <- exp(coef[1] - 1.96*coef[4])
  ci_upper <- exp(coef[1] + 1.96*coef[4])
  pval <- 2*(1-pnorm(abs(coef[5])))
  
  visit1_df <- data.frame(
    Visit = "1",
    Prevalence_Ratio = pr,
    P_Value = pval,
    CI_Lower = ci_lower,
    CI_Upper = ci_upper)
  
  # Visit 2 (coef 2, 16, 20)
  coef <- results$coefficients[c(2, 16, 20),]
  coef_sum <- sum(coef[c(1,2,3),1])
  pr <- exp(coef_sum)
  
  var_sum <- cov_matrix["arm", "arm"] + cov_matrix["visit2", "visit2"] + cov_matrix["arm:visit2", "arm:visit2"] + 
    2 * cov_matrix["arm", "visit2"] + 2 * cov_matrix["arm", "arm:visit2"] + 2 * cov_matrix["visit2", "arm:visit2"]
  
  se_sum <- sqrt(var_sum)
  
  ci_lower <- exp(coef_sum - 1.96 * se_sum) 
  ci_upper <- exp(coef_sum + 1.96 * se_sum)
  
  z_total <- coef_sum / se_sum
  pval<- 2 * pnorm(-abs(z_total))
  
  
  visit2_df <- data.frame(
    Visit = "2",
    Prevalence_Ratio = pr,
    P_Value = pval,
    CI_Lower = ci_lower,
    CI_Upper = ci_upper)
  
  # Visit 3 (coef 2, 17, 21)
  coef <- results$coefficients[c(2, 17, 21),]
  coef_sum <- sum(coef[c(1,2,3),1])
  pr <- exp(coef_sum)
  
  var_sum <- cov_matrix["arm", "arm"] + cov_matrix["visit3", "visit3"] + cov_matrix["arm:visit3", "arm:visit3"] + 
    2 * cov_matrix["arm", "visit3"] + 2 * cov_matrix["arm", "arm:visit3"] + 2 * cov_matrix["visit3", "arm:visit3"]
  
  se_sum <- sqrt(var_sum)
  
  ci_lower <- exp(coef_sum - 1.96 * se_sum) 
  ci_upper <- exp(coef_sum + 1.96 * se_sum)
  
  z_total <- coef_sum / se_sum
  pval<- 2 * pnorm(-abs(z_total))
  
  
  visit3_df <- data.frame(
    Visit = "3",
    Prevalence_Ratio = pr,
    P_Value = pval,
    CI_Lower = ci_lower,
    CI_Upper = ci_upper)
  
  # Visit 4 (coef 2, 18, 22)
  coef <- results$coefficients[c(2, 18, 22),]
  coef_sum <- sum(coef[c(1,2,3),1])
  pr <- exp(coef_sum)
  
  var_sum <- cov_matrix["arm", "arm"] + cov_matrix["visit4", "visit4"] + cov_matrix["arm:visit4", "arm:visit4"] + 
    2 * cov_matrix["arm", "visit4"] + 2 * cov_matrix["arm", "arm:visit4"] + 2 * cov_matrix["visit4", "arm:visit4"]
  
  se_sum <- sqrt(var_sum)
  
  ci_lower <- exp(coef_sum - 1.96 * se_sum) 
  ci_upper <- exp(coef_sum + 1.96 * se_sum)
  
  z_total <- coef_sum / se_sum
  pval<- 2 * pnorm(-abs(z_total))
  
  
  visit4_df <- data.frame(
    Visit = "4",
    Prevalence_Ratio = pr,
    P_Value = pval,
    CI_Lower = ci_lower,
    CI_Upper = ci_upper)
  
  # Visit 5 (coef 2, 19, 23)
  coef <- results$coefficients[c(2, 19, 23),]
  coef_sum <- sum(coef[c(1,2,3),1])
  pr <- exp(coef_sum)
  
  var_sum <- cov_matrix["arm", "arm"] + cov_matrix["visit5", "visit5"] + cov_matrix["arm:visit5", "arm:visit5"] + 
    2 * cov_matrix["arm", "visit5"] + 2 * cov_matrix["arm", "arm:visit5"] + 2 * cov_matrix["visit5", "arm:visit5"]
  
  se_sum <- sqrt(var_sum)
  
  ci_lower <- exp(coef_sum - 1.96 * se_sum) 
  ci_upper <- exp(coef_sum + 1.96 * se_sum)
  
  z_total <- coef_sum / se_sum
  pval<- 2 * pnorm(-abs(z_total))
  
  
  visit5_df <- data.frame(
    Visit = "5",
    Prevalence_Ratio = pr,
    P_Value = pval,
    CI_Lower = ci_lower,
    CI_Upper = ci_upper)
  
  result_satappear_stratified <- rbind(visit1_df, visit2_df, visit3_df, visit4_df, visit5_df)
  
  
  
  
  
  # Code ordinal HWISE variable (https://iwaponline.com/washdev/article/doi/10.2166/washdev.2024.042/104925/Identifying-ordinal-categories-for-the-Water)
  data <- data %>%
    dplyr::mutate(HWISE_ordinal = ifelse(HWISE_scale < 3, 0,
                                         ifelse(HWISE_scale > 2 & HWISE_scale < 12, 1,
                                                ifelse(HWISE_scale > 11 & HWISE_scale < 24, 2, 3))))
  
  
  # Find range of dates for each visit
  date_ranges <- all_data %>%
    group_by(visit) %>%
    summarize(
      start_date = min(hh_date),  # Earliest date in each group
      end_date = max(hh_date)     # Latest date in each group
    )
  
  
  ## Calculate ICC at enrollment
  icc_fit <- lmer(ecoli_bin_source ~ (1|code), data = data_visit1)
  icc_enrollment <- performance::icc(icc_fit)
  icc_enrollment
  
  
  ## Calculate ICC at visit 5
  icc_fit <- lmer(ecoli_bin_source ~ (1|code), data = all_data_visit5)
  icc_visit5 <- performance::icc(icc_fit)
  icc_visit5
  
  # Calculate ICC 
  ## GLMM - outcome is y and random intercept for cluster id  (unadjusted model) - use package performance and do performance::icc and pull it 
  library(lme4)
  library(performance)
  icc_fit <- glmer(always_sat_service ~ (1|code), family=poisson(link="log"), data = all_data_network_visit5)
  icc <- performance::icc(icc_fit)
  icc
  
  table(all_data_network_visit5$code, all_data_network_visit5$ecoli_bin_source)
  
  # HWISE Ordinal variable 
  library(geepack)
  all_data_network$HWISE_ordinal <- factor(all_data_network$HWISE_ordinal, levels = c("0", "1", "2", "3"), ordered = TRUE)
  model_formula <- HWISE_ordinal ~ arm + high_poverty + san_basic_obs_bl + secondary_complete_bl + final_match_strata_3 + months_in_hh + fixed_emp_pri 
  hwise_ordinal <- ordgee(formula = model_formula, id = code, data = all_data_network, corstr ="independence", na.action = na.omit)
  print(hwise_ordinal)
  exp(-1.436066e+01)
  summary(hwise_ordinal)
  # Insufficient observations of each category for each arm 
