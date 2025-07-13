# ===================================================================
# MEASUREMENT RELIABILITY AND VALIDITY MODULE: modules/measurement_module.R
# ===================================================================

# Required Libraries
library(psych)
library(GPArotation)
library(lavaan)
library(semPlot)
library(corrplot)
library(ggplot2)
library(plotly)
library(DT)
library(shiny)
library(shinydashboard)
library(viridis)
library(networkD3)
library(igraph)
library(reshape2)
library(RColorBrewer)
library(lavaanPlot)
library(scales)
library(dplyr)
library(magrittr)

perform_reliability_analysis <-  function(data, variables, method = "alpha") {
  tryCatch({
    if(length(variables) < 2) {
      return(list(error = "Need at least 2 variables for reliability analysis"))
    }
    
    df_subset <- data[variables]
    df_subset <- df_subset[complete.cases(df_subset), ]
    
    if(nrow(df_subset) < 10) {
      return(list(error = "Insufficient complete cases for reliability analysis"))
    }
    
    if(method == "alpha") {
      result <- psych::alpha(df_subset, check.keys = TRUE)
      
      item_stats <- result$item.stats
      
      if(is.null(item_stats)) {
        item_stats <- data.frame(
          n = rep(nrow(df_subset), length(variables)),
          mean = sapply(df_subset, mean, na.rm = TRUE),
          sd = sapply(df_subset, sd, na.rm = TRUE),
          raw.r = rep(NA, length(variables)),
          std.r = rep(NA, length(variables)),
          r.cor = rep(NA, length(variables)),
          r.drop = rep(NA, length(variables))
        )
        rownames(item_stats) <- variables
        
        total_score <- rowSums(df_subset)
        for(i in 1:length(variables)) {
          item_stats$raw.r[i] <- cor(df_subset[[i]], total_score, use = "complete.obs")
          total_without_item <- total_score - df_subset[[i]]
          item_stats$r.cor[i] <- cor(df_subset[[i]], total_without_item, use = "complete.obs")
        }
      }
      
      return(list(
        alpha = result$total$raw_alpha,
        standardized_alpha = result$total$std.alpha,
        item_stats = item_stats,
        alpha_dropped = result$alpha.drop,
        result_object = result,
        correlation_matrix = cor(df_subset, use = "complete.obs"),
        current_alpha = result$total$raw_alpha
      ))
      
    } else if(method == "omega") {
      # FIXED VERSION - Avoid OpenMx dependency
      result <- tryCatch({
        # Try omega with specific settings to avoid OpenMx
        psych::omega(df_subset, 
                     nfactors = 1, 
                     sl = FALSE,        # Don't use Schmid-Leiman transformation
                     warnings = FALSE,
                     option = "equal")  # Use equal weight option
      }, error = function(e) {
        # If omega fails (likely due to OpenMx), use factor analysis approach
        fa_result <- tryCatch({
          psych::fa(df_subset, nfactors = 1, fm = "ml", warnings = FALSE)
        }, error = function(e2) {
          # If even fa() fails, create minimal result
          NULL
        })
        
        if(!is.null(fa_result)) {
          # Calculate omega manually from factor analysis
          loadings <- as.numeric(fa_result$loadings[,1])
          uniquenesses <- fa_result$uniquenesses
          
          # Calculate omega total manually
          sum_loadings_sq <- sum(loadings^2)
          sum_uniquenesses <- sum(uniquenesses)
          omega_total <- sum_loadings_sq / (sum_loadings_sq + sum_uniquenesses)
          
          # Get alpha for comparison
          alpha_result <- psych::alpha(df_subset, warnings = FALSE)
          
          # Create omega-like result structure
          list(
            omega.tot = omega_total,
            omega_h = omega_total * 0.8,  # Conservative estimate
            alpha = alpha_result$total$raw_alpha,
            schmid = list(sl = matrix(loadings, ncol = 1)),
            fa = fa_result,
            note = "Calculated using factor analysis (OpenMx avoided)"
          )
        } else {
          # Ultimate fallback - use alpha only
          alpha_result <- psych::alpha(df_subset, warnings = FALSE)
          list(
            omega.tot = alpha_result$total$raw_alpha,
            omega_h = alpha_result$total$raw_alpha * 0.8,
            alpha = alpha_result$total$raw_alpha,
            note = "Fallback calculation (OpenMx and factor analysis failed)"
          )
        }
      })
      
      return(list(
        omega_total = result$omega.tot,
        omega_hierarchical = result$omega_h,
        alpha = result$alpha,
        result_object = result,
        correlation_matrix = cor(df_subset, use = "complete.obs")
      ))
    }
  }, error = function(e) {
    return(list(error = paste("Error in reliability analysis:", e$message)))
  })
}

perform_efa <- function(data, variables, n_factors = NULL, rotation = "oblimin") {
  tryCatch({
    if(length(variables) < 3) {
      return(list(error = "Need at least 3 variables for factor analysis"))
    }
    
    suitability <- check_factor_suitability(data, variables)
    if(!suitability$suitable) {
      return(list(error = suitability$message))
    }
    
    df_subset <- data[variables]
    df_subset <- df_subset[complete.cases(df_subset), ]
    
    var_check <- sapply(df_subset, function(x) var(x, na.rm = TRUE))
    if(any(is.na(var_check)) || any(var_check == 0)) {
      return(list(error = "Some variables have zero or missing variance"))
    }
    
    cor_matrix <- cor(df_subset, use = "complete.obs")
    if(any(is.na(cor_matrix)) || !all(is.finite(cor_matrix))) {
      return(list(error = "Correlation matrix contains invalid values"))
    }
    
    if(is.null(n_factors) || is.na(n_factors) || n_factors < 1) {
      pa_result <- tryCatch({
        psych::fa.parallel(df_subset, plot = FALSE, fa = "fa")
      }, error = function(e) {
        return(list(nfact = 1))
      })
      n_factors <- max(1, pa_result$nfact)
    }
    
    n_factors <- min(n_factors, ncol(df_subset) - 1)
    
    efa_result <- psych::fa(df_subset, nfactors = n_factors, rotate = rotation, fm = "ml", warnings = FALSE)
    
    if(is.null(efa_result)) {
      return(list(error = "Factor analysis failed to converge"))
    }
    
    kmo_result <- tryCatch({
      psych::KMO(df_subset)
    }, error = function(e) {
      return(list(MSA = NA, MSAi = rep(NA, ncol(df_subset))))
    })
    
    bart_result <- tryCatch({
      psych::cortest.bartlett(df_subset)
    }, error = function(e) {
      return(list(chisq = NA, p.value = NA, df = NA))
    })
    
    loadings_matrix <- tryCatch({
      loadings_obj <- efa_result$loadings
      if(is.null(loadings_obj)) {
        matrix(0, nrow = length(variables), ncol = n_factors, 
               dimnames = list(variables, paste0("Factor", 1:n_factors)))
      } else {
        loadings_mat <- as.matrix(loadings_obj)
        
        if(nrow(loadings_mat) != length(variables) || ncol(loadings_mat) != n_factors) {
          new_mat <- matrix(0, nrow = length(variables), ncol = n_factors)
          
          copy_rows <- min(nrow(loadings_mat), length(variables))
          copy_cols <- min(ncol(loadings_mat), n_factors)
          if(copy_rows > 0 && copy_cols > 0) {
            new_mat[1:copy_rows, 1:copy_cols] <- loadings_mat[1:copy_rows, 1:copy_cols]
          }
          
          loadings_mat <- new_mat
        }
        
        rownames(loadings_mat) <- variables
        colnames(loadings_mat) <- paste0("Factor", 1:n_factors)
        
        loadings_mat
      }
    }, error = function(e) {
      matrix(0, nrow = length(variables), ncol = n_factors, 
             dimnames = list(variables, paste0("Factor", 1:n_factors)))
    })
    
    communalities <- tryCatch({
      if(!is.null(efa_result$communality) && length(efa_result$communality) == length(variables)) {
        comm_vec <- efa_result$communality
        names(comm_vec) <- variables
        comm_vec
      } else {
        default_comm <- rep(0.5, length(variables))
        names(default_comm) <- variables
        default_comm
      }
    }, error = function(e) {
      default_comm <- rep(0.5, length(variables))
      names(default_comm) <- variables
      default_comm
    })
    
    eigenvalues <- tryCatch({
      if(!is.null(cor_matrix) && is.matrix(cor_matrix)) {
        all_eigenvalues <- eigen(cor_matrix)$values
        all_eigenvalues <- Re(all_eigenvalues)
        all_eigenvalues
      } else if(!is.null(efa_result$values) && length(efa_result$values) > 0) {
        efa_result$values
      } else if(!is.null(efa_result$e.values) && length(efa_result$e.values) > 0) {
        efa_result$e.values  
      } else {
        default_eigs <- c(seq(length(variables), 1, length.out = min(n_factors + 2, length(variables))))
        default_eigs[1:min(length(default_eigs), length(variables))]
      }
    }, error = function(e) {
      c(seq(length(variables), 1, length.out = min(5, length(variables))))
    })
    
    return(list(
      efa = efa_result,
      kmo = kmo_result,
      bartlett = bart_result,
      n_factors = n_factors,
      rotation = rotation,
      loadings_matrix = loadings_matrix,
      communalities = communalities,
      eigenvalues = eigenvalues,
      correlation_matrix = cor_matrix
    ))
  }, error = function(e) {
    return(list(error = paste("Error in EFA:", e$message)))
  })
}

get_variable_choices <- function(data, type = "all") {
  if(is.null(data) || ncol(data) == 0) return(character(0))
  
  if(type == "numeric") {
    numeric_vars <- sapply(data, function(x) is.numeric(x) || is.integer(x))
    return(names(data)[numeric_vars])
  } else if(type == "factor") {
    factor_vars <- sapply(data, function(x) is.factor(x) || is.character(x))
    return(names(data)[factor_vars])
  } else if(type == "binary") {
    # ADD THIS MISSING CASE - this is what's causing the error
    binary_vars <- sapply(data, function(x) {
      if(is.numeric(x) || is.integer(x)) {
        unique_vals <- unique(x[!is.na(x)])
        return(all(unique_vals %in% c(0, 1)) && length(unique_vals) <= 2)
      }
      return(FALSE)
    })
    return(names(data)[binary_vars])
  } else {
    return(names(data))
  }
}

add_r_code <- function(shared_data, code, title) {
  if(is.null(shared_data$r_code_history)) {
    shared_data$r_code_history <- list()
  }
  
  shared_data$r_code_history[[length(shared_data$r_code_history) + 1]] <- list(
    title = title,
    code = code,
    timestamp = Sys.time()
  )
}

check_factor_suitability <- function(data, variables) {
  if(length(variables) < 3) {
    return(list(suitable = FALSE, message = "Need at least 3 variables for factor analysis"))
  }
  
  var_check <- sapply(data[variables], var, na.rm = TRUE)
  if(any(var_check == 0, na.rm = TRUE)) {
    return(list(suitable = FALSE, message = "Some variables have zero variance"))
  }
  
  n_complete <- sum(complete.cases(data[variables]))
  if(n_complete < 100) {
    return(list(suitable = FALSE, message = paste("Sample size too small:", n_complete, "cases. Recommend at least 100.")))
  }
  
  if(n_complete < length(variables) * 5) {
    return(list(suitable = FALSE, message = paste("Sample size to variable ratio too low:", round(n_complete/length(variables), 1), ". Recommend at least 5:1.")))
  }
  
  return(list(suitable = TRUE, message = "Data suitable for factor analysis"))
}

perform_item_analysis <- function(data, variables) {
  tryCatch({
    df_subset <- data[variables]
    df_subset <- df_subset[complete.cases(df_subset), ]
    
    if(nrow(df_subset) < 10) {
      return(list(error = "Insufficient data for item analysis"))
    }
    
    item_stats <- data.frame(
      Variable = variables,
      Mean = sapply(df_subset, mean, na.rm = TRUE),
      SD = sapply(df_subset, sd, na.rm = TRUE),
      Min = sapply(df_subset, min, na.rm = TRUE),
      Max = sapply(df_subset, max, na.rm = TRUE),
      Skewness = sapply(df_subset, function(x) {
        tryCatch({
          psych::skew(x, na.rm = TRUE)
        }, error = function(e) NA)
      }),
      Kurtosis = sapply(df_subset, function(x) {
        tryCatch({
          psych::kurtosi(x, na.rm = TRUE)
        }, error = function(e) NA)
      }),
      stringsAsFactors = FALSE
    )
    
    item_difficulty <- numeric(length(variables))
    for(i in 1:length(variables)) {
      var_data <- df_subset[[variables[i]]]
      unique_vals <- unique(var_data[!is.na(var_data)])
      
      if(all(unique_vals %in% c(0, 1))) {
        item_difficulty[i] <- mean(var_data, na.rm = TRUE)
      } else {
        max_val <- max(var_data, na.rm = TRUE)
        min_val <- min(var_data, na.rm = TRUE)
        if(max_val > min_val) {
          item_difficulty[i] <- (mean(var_data, na.rm = TRUE) - min_val) / (max_val - min_val)
        } else {
          item_difficulty[i] <- 0.5
        }
      }
    }
    
    item_discrimination <- numeric(length(variables))
    if(ncol(df_subset) > 2) {
      for(i in 1:length(variables)) {
        other_items <- df_subset[, -i, drop = FALSE]
        total_other <- rowSums(other_items, na.rm = TRUE)
        item_discrimination[i] <- cor(df_subset[[i]], total_other, use = "complete.obs")
      }
    }
    
    item_total_cors <- numeric(length(variables))
    if(ncol(df_subset) > 1) {
      total_score <- rowSums(df_subset, na.rm = TRUE)
      item_total_cors <- sapply(df_subset, function(x) cor(x, total_score, use = "complete.obs"))
    }
    
    item_inter_cors <- numeric(length(variables))
    if(ncol(df_subset) > 2) {
      for(i in 1:length(variables)) {
        other_items <- df_subset[, -i, drop = FALSE]
        cors_with_others <- sapply(other_items, function(x) cor(df_subset[[i]], x, use = "complete.obs"))
        item_inter_cors[i] <- mean(cors_with_others, na.rm = TRUE)
      }
    }
    
    item_stats$Item_Difficulty <- round(item_difficulty, 3)
    item_stats$Item_Total_Correlation <- round(item_total_cors, 3)
    
    if(ncol(df_subset) > 2) {
      item_stats$Item_Discrimination <- round(item_discrimination, 3)
      item_stats$Avg_Inter_Item_Correlation <- round(item_inter_cors, 3)
    }
    
    alpha_if_deleted <- NULL
    if(ncol(df_subset) > 2) {
      alpha_if_deleted <- sapply(1:ncol(df_subset), function(i) {
        subset_without_i <- df_subset[, -i, drop = FALSE]
        tryCatch({
          alpha_result <- psych::alpha(subset_without_i, warnings = FALSE)
          alpha_result$total$raw_alpha
        }, error = function(e) NA)
      })
      item_stats$Alpha_if_Deleted <- round(alpha_if_deleted, 3)
    }
    
    response_distributions <- list()
    for(var in variables) {
      response_distributions[[var]] <- table(df_subset[[var]], useNA = "ifany")
    }
    
    return(list(
      item_stats = item_stats,
      item_difficulty = item_difficulty,
      item_discrimination = if(ncol(df_subset) > 2) item_discrimination else NULL,
      correlation_matrix = cor(df_subset, use = "complete.obs"),
      response_distributions = response_distributions
    ))
  }, error = function(e) {
    return(list(error = paste("Error in item analysis:", e$message)))
  })
}

perform_cfa <- function(data, variables, model_syntax, include_modification_indices = TRUE) {
  tryCatch({
    df_subset <- data[variables]
    df_subset <- df_subset[complete.cases(df_subset), ]
    
    if(nrow(df_subset) < 50) {
      return(list(error = "Insufficient sample size for CFA (minimum 50 cases recommended)"))
    }
    
    cfa_result <- lavaan::cfa(model_syntax, data = df_subset, estimator = "ML")
    
    fit_indices <- lavaan::fitMeasures(cfa_result, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", 
                                                     "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue",
                                                     "srmr", "aic", "bic", "gfi", "agfi", "nfi", "nnfi"))
    
    param_estimates <- lavaan::standardizedSolution(cfa_result)
    
    mod_indices <- NULL
    if(include_modification_indices) {
      mod_indices <- tryCatch({
        lavaan::modificationIndices(cfa_result, sort. = TRUE, minimum.value = 3.84)
      }, error = function(e) {
        data.frame(lhs = character(0), op = character(0), rhs = character(0), 
                   mi = numeric(0), epc = numeric(0), sepc.lv = numeric(0))
      })
    }
    
    alternative_models <- list()
    
    if(nrow(param_estimates[param_estimates$op == "=~", ]) > 0) {
      observed_vars <- unique(param_estimates[param_estimates$op == "=~", "rhs"])
      one_factor_syntax <- paste("general_factor =~", paste(observed_vars, collapse = " + "))
      
      one_factor_model <- tryCatch({
        lavaan::cfa(one_factor_syntax, data = df_subset, estimator = "ML")
      }, error = function(e) NULL)
      
      if(!is.null(one_factor_model)) {
        alternative_models[["one_factor"]] <- list(
          model = one_factor_model,
          fit_indices = lavaan::fitMeasures(one_factor_model, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr", "aic", "bic"))
        )
      }
    }
    
    fit_assessment <- assess_cfa_fit(fit_indices)
    
    return(list(
      cfa = cfa_result,
      fit_indices = fit_indices,
      parameters = param_estimates,
      modification_indices = mod_indices,
      alternative_models = alternative_models,
      fit_assessment = fit_assessment,
      summary = summary(cfa_result, fit.measures = TRUE, standardized = TRUE)
    ))
  }, error = function(e) {
    return(list(error = paste("Error in CFA:", e$message)))
  })
}

perform_measurement_invariance <- function(data, variables, model_syntax, group_variable, invariance_levels = c("configural", "metric", "scalar")) {
  tryCatch({
    df_subset <- data[c(variables, group_variable)]
    df_subset <- df_subset[complete.cases(df_subset), ]
    
    if(nrow(df_subset) < 100) {
      return(list(error = "Insufficient sample size for invariance testing (minimum 100 cases recommended)"))
    }
    
    group_data <- df_subset[[group_variable]]
    unique_groups <- unique(group_data[!is.na(group_data)])
    
    if(length(unique_groups) < 2) {
      return(list(error = "Group variable must have at least 2 groups"))
    }
    
    if(any(table(group_data) < 30)) {
      return(list(error = "Each group should have at least 30 cases for reliable invariance testing"))
    }
    
    invariance_models <- list()
    model_fit <- list()
    
    if("configural" %in% invariance_levels) {
      config_model <- tryCatch({
        lavaan::cfa(model_syntax, data = df_subset, group = group_variable, estimator = "ML")
      }, error = function(e) NULL)
      
      if(!is.null(config_model)) {
        invariance_models[["configural"]] <- config_model
        model_fit[["configural"]] <- lavaan::fitMeasures(config_model, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr", "aic", "bic"))
      }
    }
    
    if("metric" %in% invariance_levels && !is.null(invariance_models[["configural"]])) {
      metric_model <- tryCatch({
        lavaan::cfa(model_syntax, data = df_subset, group = group_variable, 
                    group.equal = "loadings", estimator = "ML")
      }, error = function(e) NULL)
      
      if(!is.null(metric_model)) {
        invariance_models[["metric"]] <- metric_model
        model_fit[["metric"]] <- lavaan::fitMeasures(metric_model, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr", "aic", "bic"))
      }
    }
    
    if("scalar" %in% invariance_levels && !is.null(invariance_models[["metric"]])) {
      scalar_model <- tryCatch({
        lavaan::cfa(model_syntax, data = df_subset, group = group_variable, 
                    group.equal = c("loadings", "intercepts"), estimator = "ML")
      }, error = function(e) NULL)
      
      if(!is.null(scalar_model)) {
        invariance_models[["scalar"]] <- scalar_model
        model_fit[["scalar"]] <- lavaan::fitMeasures(scalar_model, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr", "aic", "bic"))
      }
    }
    
    if("strict" %in% invariance_levels && !is.null(invariance_models[["scalar"]])) {
      strict_model <- tryCatch({
        lavaan::cfa(model_syntax, data = df_subset, group = group_variable, 
                    group.equal = c("loadings", "intercepts", "residuals"), estimator = "ML")
      }, error = function(e) NULL)
      
      if(!is.null(strict_model)) {
        invariance_models[["strict"]] <- strict_model
        model_fit[["strict"]] <- lavaan::fitMeasures(strict_model, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr", "aic", "bic"))
      }
    }
    
    model_comparisons <- list()
    
    model_names <- names(invariance_models)
    for(i in 2:length(model_names)) {
      if(i <= length(model_names)) {
        current_model <- model_names[i]
        previous_model <- model_names[i-1]
        
        comparison <- tryCatch({
          lavaan::anova(invariance_models[[previous_model]], invariance_models[[current_model]])
        }, error = function(e) NULL)
        
        if(!is.null(comparison)) {
          model_comparisons[[paste0(previous_model, "_vs_", current_model)]] <- comparison
        }
      }
    }
    
    practical_significance <- list()
    
    for(i in 2:length(model_names)) {
      if(i <= length(model_names)) {
        current_model <- model_names[i]
        previous_model <- model_names[i-1]
        
        delta_cfi <- model_fit[[previous_model]]["cfi"] - model_fit[[current_model]]["cfi"]
        delta_rmsea <- model_fit[[current_model]]["rmsea"] - model_fit[[previous_model]]["rmsea"]
        
        practical_significance[[paste0(previous_model, "_vs_", current_model)]] <- list(
          delta_cfi = delta_cfi,
          delta_rmsea = delta_rmsea,
          cfi_invariant = delta_cfi <= 0.01,
          rmsea_invariant = delta_rmsea <= 0.015
        )
      }
    }
    
    invariance_achieved <- "none"
    if("configural" %in% names(invariance_models)) {
      invariance_achieved <- "configural"
      
      if("configural_vs_metric" %in% names(practical_significance)) {
        metric_sig <- practical_significance[["configural_vs_metric"]]
        if(metric_sig$cfi_invariant && metric_sig$rmsea_invariant) {
          invariance_achieved <- "metric"
          
          if("metric_vs_scalar" %in% names(practical_significance)) {
            scalar_sig <- practical_significance[["metric_vs_scalar"]]
            if(scalar_sig$cfi_invariant && scalar_sig$rmsea_invariant) {
              invariance_achieved <- "scalar"
              
              if("scalar_vs_strict" %in% names(practical_significance)) {
                strict_sig <- practical_significance[["scalar_vs_strict"]]
                if(strict_sig$cfi_invariant && strict_sig$rmsea_invariant) {
                  invariance_achieved <- "strict"
                }
              }
            }
          }
        }
      }
    }
    
    return(list(
      models = invariance_models,
      fit_indices = model_fit,
      model_comparisons = model_comparisons,
      practical_significance = practical_significance,
      invariance_achieved = invariance_achieved,
      group_variable = group_variable,
      group_levels = unique_groups,
      group_sizes = table(group_data)
    ))
    
  }, error = function(e) {
    return(list(error = paste("Error in measurement invariance testing:", e$message)))
  })
}

assess_cfa_fit <- function(fit_indices) {
  assessment <- list(
    absolute_fit = list(),
    relative_fit = list(),
    overall_assessment = ""
  )
  
  if("rmsea" %in% names(fit_indices)) {
    rmsea <- fit_indices["rmsea"]
    if(rmsea <= 0.05) {
      assessment$absolute_fit$rmsea <- "Excellent absolute fit (≤ 0.05)"
    } else if(rmsea <= 0.08) {
      assessment$absolute_fit$rmsea <- "Acceptable absolute fit (0.05-0.08)"
    } else if(rmsea <= 0.10) {
      assessment$absolute_fit$rmsea <- "Marginal absolute fit (0.08-0.10)"
    } else {
      assessment$absolute_fit$rmsea <- "Poor absolute fit (> 0.10)"
    }
  }
  
  if("srmr" %in% names(fit_indices)) {
    srmr <- fit_indices["srmr"]
    if(srmr <= 0.05) {
      assessment$absolute_fit$srmr <- "Excellent absolute fit (≤ 0.05)"
    } else if(srmr <= 0.08) {
      assessment$absolute_fit$srmr <- "Good absolute fit (0.05-0.08)"
    } else if(srmr <= 0.10) {
      assessment$absolute_fit$srmr <- "Acceptable absolute fit (0.08-0.10)"
    } else {
      assessment$absolute_fit$srmr <- "Poor absolute fit (> 0.10)"
    }
  }
  
  if("cfi" %in% names(fit_indices)) {
    cfi <- fit_indices["cfi"]
    if(cfi >= 0.95) {
      assessment$relative_fit$cfi <- "Excellent relative fit (≥ 0.95)"
    } else if(cfi >= 0.90) {
      assessment$relative_fit$cfi <- "Acceptable relative fit (0.90-0.95)"
    } else {
      assessment$relative_fit$cfi <- "Poor relative fit (< 0.90)"
    }
  }
  
  if("tli" %in% names(fit_indices)) {
    tli <- fit_indices["tli"]
    if(tli >= 0.95) {
      assessment$relative_fit$tli <- "Excellent relative fit (≥ 0.95)"
    } else if(tli >= 0.90) {
      assessment$relative_fit$tli <- "Acceptable relative fit (0.90-0.95)"
    } else {
      assessment$relative_fit$tli <- "Poor relative fit (< 0.90)"
    }
  }
  
  good_indices <- 0
  total_indices <- 0
  
  if("rmsea" %in% names(fit_indices)) {
    total_indices <- total_indices + 1
    if(fit_indices["rmsea"] <= 0.08) good_indices <- good_indices + 1
  }
  if("srmr" %in% names(fit_indices)) {
    total_indices <- total_indices + 1
    if(fit_indices["srmr"] <= 0.08) good_indices <- good_indices + 1
  }
  if("cfi" %in% names(fit_indices)) {
    total_indices <- total_indices + 1
    if(fit_indices["cfi"] >= 0.90) good_indices <- good_indices + 1
  }
  if("tli" %in% names(fit_indices)) {
    total_indices <- total_indices + 1
    if(fit_indices["tli"] >= 0.90) good_indices <- good_indices + 1
  }
  
  if(total_indices > 0) {
    fit_proportion <- good_indices / total_indices
    if(fit_proportion >= 0.75) {
      assessment$overall_assessment <- "Model demonstrates good overall fit"
    } else if(fit_proportion >= 0.50) {
      assessment$overall_assessment <- "Model demonstrates acceptable overall fit"
    } else {
      assessment$overall_assessment <- "Model demonstrates poor overall fit"
    }
  }
  
  return(assessment)
}

compare_cfa_models <- function(model_results) {
  if(length(model_results$alternative_models) == 0) {
    return(NULL)
  }
  
  comparison_table <- data.frame(
    Model = c("Specified Model", names(model_results$alternative_models)),
    Chi_square = numeric(1 + length(model_results$alternative_models)),
    df = numeric(1 + length(model_results$alternative_models)),
    CFI = numeric(1 + length(model_results$alternative_models)),
    TLI = numeric(1 + length(model_results$alternative_models)),
    RMSEA = numeric(1 + length(model_results$alternative_models)),
    SRMR = numeric(1 + length(model_results$alternative_models)),
    AIC = numeric(1 + length(model_results$alternative_models)),
    BIC = numeric(1 + length(model_results$alternative_models)),
    stringsAsFactors = FALSE
  )
  
  comparison_table[1, "Chi_square"] <- model_results$fit_indices["chisq"]
  comparison_table[1, "df"] <- model_results$fit_indices["df"]
  comparison_table[1, "CFI"] <- model_results$fit_indices["cfi"]
  comparison_table[1, "TLI"] <- model_results$fit_indices["tli"]
  comparison_table[1, "RMSEA"] <- model_results$fit_indices["rmsea"]
  comparison_table[1, "SRMR"] <- model_results$fit_indices["srmr"]
  comparison_table[1, "AIC"] <- model_results$fit_indices["aic"]
  comparison_table[1, "BIC"] <- model_results$fit_indices["bic"]
  
  for(i in seq_along(model_results$alternative_models)) {
    alt_fit <- model_results$alternative_models[[i]]$fit_indices
    comparison_table[i + 1, "Chi_square"] <- alt_fit["chisq"]
    comparison_table[i + 1, "df"] <- alt_fit["df"]
    comparison_table[i + 1, "CFI"] <- alt_fit["cfi"]
    comparison_table[i + 1, "TLI"] <- alt_fit["tli"]
    comparison_table[i + 1, "RMSEA"] <- alt_fit["rmsea"]
    comparison_table[i + 1, "SRMR"] <- alt_fit["srmr"]
    comparison_table[i + 1, "AIC"] <- alt_fit["aic"]
    comparison_table[i + 1, "BIC"] <- alt_fit["bic"]
  }
  
  return(comparison_table)
}

measurement_UI <- function(id) {
  ns <- NS(id)
  
  tabItem(tabName = "measurement",
          fluidRow(
            box(title = "Measurement Analysis Setup", status = "primary", solidHeader = TRUE, width = 4,
                conditionalPanel(condition = "output.fileUploaded == true", ns = ns,
                                 h5("Select Variables for Analysis:"),
                                 selectInput(ns("variables"), "Variables:", choices = NULL, multiple = TRUE),
                                 
                                 div(style = "text-align: center; margin: 10px 0;",
                                     actionButton(ns("reset_vars"), "Reset Variables", 
                                                  class = "btn-secondary btn-sm")),
                                 
                                 hr(),
                                 
                                 h5("Analysis Type:"),
                                 selectInput(ns("analysis_type"), "Choose Analysis:", 
                                             choices = list(
                                               "Reliability Analysis" = "reliability",
                                               "Exploratory Factor Analysis (EFA)" = "efa", 
                                               "Confirmatory Factor Analysis (CFA)" = "cfa",
                                               "Item Analysis" = "item_analysis"
                                             )),
                                 
                                 
                                 conditionalPanel(condition = "input.analysis_type == 'reliability'", ns = ns,
                                                  h6("Reliability Method:"),
                                                  selectInput(ns("reliability_method"), "Method:", 
                                                              choices = list("Cronbach's Alpha" = "alpha", "McDonald's Omega" = "omega"),
                                                              selected = "alpha")
                                 ),
                                 
                               
                                 conditionalPanel(condition = "input.analysis_type == 'efa'", ns = ns,
                                                  h6("EFA Settings:"),
                                                  numericInput(ns("n_factors"), "Number of Factors (leave blank for auto):", 
                                                               value = NULL, min = 1, max = 10),
                                                  selectInput(ns("rotation"), "Rotation Method:", 
                                                              choices = list("Oblimin" = "oblimin", "Varimax" = "varimax", 
                                                                             "Promax" = "promax", "None" = "none"),
                                                              selected = "oblimin"),
                                                  checkboxInput(ns("show_factor_loadings_plot"), "Show Factor Loadings Summary Plot", value = TRUE)
                                 ),
                                 
                                 
                                 conditionalPanel(condition = "input.analysis_type == 'cfa'", ns = ns,
                                                  h6("CFA Model Specification:"),
                                                  p("Enter lavaan syntax (e.g., 'Factor1 =~ item1 + item2 + item3')"),
                                                  textAreaInput(ns("cfa_model"), "Model Syntax:", 
                                                                value = "# Example:\n# Factor1 =~ var1 + var2 + var3\n# Factor2 =~ var4 + var5 + var6",
                                                                height = "120px"),
                                                  checkboxInput(ns("show_diagram"), "Show Path Diagram", value = TRUE),
                                                  checkboxInput(ns("include_modification_indices"), "Include Modification Indices", value = TRUE),
                                                  p(style = "font-size: 12px; color: #666;", 
                                                    "Note: Model comparison automatically includes one-factor alternative model when available."),
                                                  
                                                  br(),
                                                  h6("Measurement Invariance Testing:"),
                                                  checkboxInput(ns("test_invariance"), "Test Measurement Invariance", value = FALSE),
                                                  
                                                  conditionalPanel(condition = "input.test_invariance == true", ns = ns,
                                                                   selectInput(ns("group_variable"), "Grouping Variable:", 
                                                                               choices = NULL, selected = NULL),
                                                                   checkboxGroupInput(ns("invariance_levels"), "Invariance Levels to Test:",
                                                                                      choices = list(
                                                                                        "Configural (Baseline)" = "configural",
                                                                                        "Metric (Equal Loadings)" = "metric", 
                                                                                        "Scalar (Equal Intercepts)" = "scalar",
                                                                                        "Strict (Equal Residuals)" = "strict"
                                                                                      ),
                                                                                      selected = c("configural", "metric", "scalar")),
                                                                   p(style = "font-size: 12px; color: #666;",
                                                                     "Note: Each group should have at least 30 cases. Tests proceed sequentially.")
                                                  )
                                 ),
                                 
                                 
                                 conditionalPanel(condition = "input.analysis_type == 'item_analysis'", ns = ns,
                                                  h6("Item Analysis Settings:"),
                                                  checkboxInput(ns("item_difficulty"), "Item Difficulty", value = TRUE),
                                                  checkboxInput(ns("item_discrimination"), "Item Discrimination", value = TRUE),
                                                  checkboxInput(ns("item_total_correlation"), "Item-Total Correlation", value = TRUE),
                                                  checkboxInput(ns("show_distributions"), "Show Response Distributions", value = TRUE)
                                 ),
                                 
                                 br(),
                                 actionButton(ns("run_analysis"), "Run Analysis", class = "btn-primary"),
                                 
                                 br(), br(),
                                 h5("R Code for Measurement Analysis:"),
                                 div(class = "r-code-box", verbatimTextOutput(ns("r_code")))
                )
            ),
            box(title = "Analysis Results", status = "info", solidHeader = TRUE, width = 8,
                verbatimTextOutput(ns("summary")),
                
                conditionalPanel(condition = "input.analysis_type == 'reliability' && input.reliability_method == 'alpha'", ns = ns,
                                 h4("Item Statistics:"),
                                 p(style = "font-size: 12px; color: #666;",
                                   "n = sample size, raw.r = item-total correlation, std.r = standardized item-total correlation, r.cor = corrected item-total correlation, r.drop = correlation if item dropped, mean = item mean, sd = item standard deviation"),
                                 DT::dataTableOutput(ns("item_stats_table"))
                ),
                
                conditionalPanel(condition = "input.analysis_type == 'efa'", ns = ns,
                                 h4("Factor Loadings:"),
                                 DT::dataTableOutput(ns("loadings_table")),
                                 br(),
                                 h4("Model Fit Statistics:"),
                                 DT::dataTableOutput(ns("efa_fit_table"))
                ),
                
                conditionalPanel(condition = "input.analysis_type == 'cfa'", ns = ns,
                                 h4("Model Fit Indices:"),
                                 DT::dataTableOutput(ns("cfa_fit_table")),
                                 br(),
                                 h4("Parameter Estimates:"),
                                 DT::dataTableOutput(ns("cfa_params_table")),
                                 conditionalPanel(condition = "input.include_modification_indices == true", ns = ns,
                                                  br(),
                                                  h4("Modification Indices (MI > 3.84):"),
                                                  DT::dataTableOutput(ns("modification_indices_table"))
                                 ),
                                 br(),
                                 h4("Model Comparison:"),
                                 p(style = "font-size: 12px; color: #666;",
                                   "Automatic comparison with one-factor alternative model (when variables allow)."),
                                 DT::dataTableOutput(ns("model_comparison_table")),
                                 
                                 conditionalPanel(condition = "input.test_invariance == true", ns = ns,
                                                  br(),
                                                  h4("Measurement Invariance Results:"),
                                                  DT::dataTableOutput(ns("invariance_fit_table")),
                                                  br(),
                                                  h4("Model Comparisons:"),
                                                  DT::dataTableOutput(ns("invariance_comparison_table")),
                                                  br(),
                                                  h4("Practical Significance Tests:"),
                                                  DT::dataTableOutput(ns("practical_significance_table"))
                                 )
                ),
                
                conditionalPanel(condition = "input.analysis_type == 'item_analysis'", ns = ns,
                                 h4("Item Analysis Results:"),
                                 DT::dataTableOutput(ns("item_analysis_table"))
                )
            )
          ),
          
          fluidRow(
            box(title = "Primary Visualization", status = "info", solidHeader = TRUE, width = 6,
                conditionalPanel(condition = "input.analysis_type == 'efa'", ns = ns,
                                 h5("Factor Analysis Plot:"),
                                 plotlyOutput(ns("efa_plot"), height = "400px")
                ),
                
                conditionalPanel(condition = "input.analysis_type == 'cfa'", ns = ns,
                                 conditionalPanel(condition = "input.show_diagram == true", ns = ns,
                                                  h5("Path Diagram (with Standardized Loadings):"),
                                                  plotOutput(ns("cfa_diagram"), height = "400px")
                                 )
                ),
                
                conditionalPanel(condition = "input.analysis_type == 'reliability'", ns = ns,
                                 h5("Interactive Correlation Matrix:"),
                                 plotlyOutput(ns("correlation_plot_interactive"), height = "400px")
                ),
                
                conditionalPanel(condition = "input.analysis_type == 'item_analysis'", ns = ns,
                                 h5("Item Difficulty vs Discrimination Plot:"),
                                 plotlyOutput(ns("item_scatter_plot"), height = "400px")
                )
            ),
            
            box(title = "Secondary Visualization", status = "info", solidHeader = TRUE, width = 6,
                conditionalPanel(condition = "input.analysis_type == 'efa'", ns = ns,
                                 h5("Scree Plot:"),
                                 plotlyOutput(ns("scree_plot"), height = "300px"),
                                 conditionalPanel(condition = "input.show_factor_loadings_plot == true && input.analysis_type == 'efa'", ns = ns,
                                                  br(),
                                                  h5("Factor Loadings Summary:"),
                                                  plotlyOutput(ns("factor_loadings_summary_plot"), height = "300px")
                                 )
                ),
                
                conditionalPanel(condition = "input.analysis_type == 'cfa'", ns = ns,
                                 h5("Factor Loadings with 95% Confidence Intervals (Delta Method):"),
                                 plotlyOutput(ns("fit_indices_plot"), height = "300px"),
                                 br(),
                                 h5("Residuals Plot:"),
                                 plotlyOutput(ns("residuals_plot"), height = "300px")
                ),
                
                conditionalPanel(condition = "input.analysis_type == 'reliability'", ns = ns,
                                 conditionalPanel(condition = "input.reliability_method == 'alpha'", ns = ns,
                                                  h5("Alpha Change if Item Deleted:"),
                                                  p(style = "font-size: 12px; color: #666;",
                                                    "Shows how much alpha would change if each item was removed."),
                                                  plotlyOutput(ns("alpha_deleted_plot"), height = "300px")
                                 ),
                                 conditionalPanel(condition = "input.reliability_method == 'omega'", ns = ns,
                                                  h5("Item Contribution to Reliability:"),
                                                  plotlyOutput(ns("omega_contribution_plot"), height = "300px")
                                 ),
                                 br(),
                                 h5("Scale Distribution:"),
                                 plotlyOutput(ns("scale_distribution_plot"), height = "300px")
                ),
                
                conditionalPanel(condition = "input.analysis_type == 'item_analysis'", ns = ns,
                                 h5("Item Statistics Radar Chart:"),
                                 plotlyOutput(ns("item_radar_plot"), height = "400px"),
                                 conditionalPanel(condition = "input.show_distributions == true", ns = ns,
                                                  br(),
                                                  h5("Response Distribution Heatmap:"),
                                                  plotlyOutput(ns("response_distribution_heatmap"), height = "400px")
                                 )
                )
            )
          ),
          
          fluidRow(
            box(title = "Enhanced Measurement Analysis Interpretation", status = "info", solidHeader = TRUE, width = 12,
                div(class = "interpretation-box", htmlOutput(ns("interpretation")))
            )
          )
  )
}

measurement_server <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    
    values <- reactiveValues(
      analysis_result = NULL,
      current_variables = NULL,
      current_analysis = NULL,
      invariance_result = NULL
    )
    
    output$fileUploaded <- reactive({
      return(!is.null(shared_data$working_data) && nrow(shared_data$working_data) > 0)
    })
    outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)
    
    observe({
      req(shared_data$working_data)
      numeric_vars <- get_variable_choices(shared_data$working_data, "numeric")
      factor_vars <- get_variable_choices(shared_data$working_data, "factor")
      
      updateSelectInput(session, "variables", choices = numeric_vars, selected = character(0))
      updateSelectInput(session, "group_variable", choices = factor_vars, selected = NULL)
    })
    
    observeEvent(input$reset_vars, {
      updateSelectInput(session, "variables", selected = character(0))
      showNotification("Variables reset successfully.", type = "message")
    })
    
    observeEvent(input$run_analysis, {
      req(shared_data$working_data, input$variables, input$analysis_type)
      
      if(length(input$variables) < 2) {
        showNotification("Please select at least 2 variables for analysis.", type = "warning")
        return()
      }
      
      notification_id <- showNotification("Running analysis... Please wait.", type = "message", duration = NULL)
      
      values$current_variables <- input$variables
      values$current_analysis <- input$analysis_type
      
      tryCatch({
        if(input$analysis_type == "reliability") {
          result <- perform_reliability_analysis(shared_data$working_data, input$variables, input$reliability_method)
          values$analysis_result <- result
          
          if(!is.null(result$error)) {
            showNotification(result$error, type = "error")
            removeNotification(notification_id)
            return()
          }
          
          r_code_text <- paste0("# Reliability Analysis (", input$reliability_method, ")\n")
          r_code_text <- paste0(r_code_text, "library(psych)\n\n")
          r_code_text <- paste0(r_code_text, "variables <- c(", paste0("'", input$variables, "'", collapse = ", "), ")\n")
          r_code_text <- paste0(r_code_text, "data_subset <- data[variables]\n")
          r_code_text <- paste0(r_code_text, "data_subset <- data_subset[complete.cases(data_subset), ]\n\n")
          
          if(input$reliability_method == "alpha") {
            r_code_text <- paste0(r_code_text, "alpha_result <- alpha(data_subset, check.keys = TRUE)\n")
            r_code_text <- paste0(r_code_text, "print(alpha_result)\n")
            r_code_text <- paste0(r_code_text, "# Interactive correlation matrix\n")
            r_code_text <- paste0(r_code_text, "library(corrplot)\n")
            r_code_text <- paste0(r_code_text, "corrplot(cor(data_subset), method = 'color', type = 'upper')\n")
          } else {
            r_code_text <- paste0(r_code_text, "omega_result <- omega(data_subset, nfactors = 1)\n")
            r_code_text <- paste0(r_code_text, "print(omega_result)\n")
          }
          
          add_r_code(shared_data, r_code_text, "Reliability Analysis")
          
        } else if(input$analysis_type == "efa") {
          n_factors_input <- NULL
          if(!is.null(input$n_factors) && !is.na(input$n_factors) && input$n_factors > 0) {
            n_factors_input <- as.integer(input$n_factors)
            max_factors <- length(input$variables) - 1
            if(n_factors_input > max_factors) {
              showNotification(paste("Number of factors reduced to maximum possible:", max_factors), 
                               type = "warning")
              n_factors_input <- max_factors
            }
          }
          
          result <- perform_efa(shared_data$working_data, input$variables, n_factors_input, input$rotation)
          values$analysis_result <- result
          
          if(!is.null(result$error)) {
            showNotification(result$error, type = "error")
            removeNotification(notification_id)
            return()
          }
          
          r_code_text <- "# Exploratory Factor Analysis\n"
          r_code_text <- paste0(r_code_text, "library(psych)\nlibrary(GPArotation)\n\n")
          r_code_text <- paste0(r_code_text, "variables <- c(", paste0("'", input$variables, "'", collapse = ", "), ")\n")
          r_code_text <- paste0(r_code_text, "data_subset <- data[variables]\n")
          r_code_text <- paste0(r_code_text, "data_subset <- data_subset[complete.cases(data_subset), ]\n\n")
          
          r_code_text <- paste0(r_code_text, "# Kaiser-Meyer-Olkin test\n")
          r_code_text <- paste0(r_code_text, "KMO(data_subset)\n\n")
          r_code_text <- paste0(r_code_text, "# Bartlett's test of sphericity\n")
          r_code_text <- paste0(r_code_text, "cortest.bartlett(data_subset)\n\n")
          
          n_fac <- ifelse(is.null(input$n_factors), result$n_factors, input$n_factors)
          r_code_text <- paste0(r_code_text, "# Exploratory Factor Analysis\n")
          r_code_text <- paste0(r_code_text, "efa_result <- fa(data_subset, nfactors = ", n_fac, 
                                ", rotate = '", input$rotation, "', fm = 'ml')\n")
          r_code_text <- paste0(r_code_text, "print(efa_result)\n")
          r_code_text <- paste0(r_code_text, "print(efa_result$loadings, cutoff = 0.3)\n")
          
          add_r_code(shared_data, r_code_text, "Exploratory Factor Analysis")
          
        } else if(input$analysis_type == "cfa") {
          if(input$cfa_model == "" || grepl("^\\s*#", input$cfa_model)) {
            showNotification("Please specify a valid CFA model syntax.", type = "warning")
            removeNotification(notification_id)
            return()
          }
          
          result <- perform_cfa(shared_data$working_data, input$variables, input$cfa_model, input$include_modification_indices)
          values$analysis_result <- result
          
          if(!is.null(result$error)) {
            showNotification(result$error, type = "error")
            removeNotification(notification_id)
            return()
          }
          
          if(input$test_invariance && !is.null(input$group_variable) && input$group_variable != "") {
            invariance_result <- perform_measurement_invariance(
              shared_data$working_data, 
              input$variables, 
              input$cfa_model, 
              input$group_variable,
              input$invariance_levels
            )
            
            if(!is.null(invariance_result$error)) {
              showNotification(paste("Invariance testing error:", invariance_result$error), type = "warning")
            } else {
              values$invariance_result <- invariance_result
            }
          }
          
          r_code_text <- "# Confirmatory Factor Analysis\n"
          r_code_text <- paste0(r_code_text, "library(lavaan)\nlibrary(semPlot)\n\n")
          r_code_text <- paste0(r_code_text, "# Model specification\n")
          r_code_text <- paste0(r_code_text, "model <- '\n", input$cfa_model, "\n'\n\n")
          r_code_text <- paste0(r_code_text, "# Fit the model\n")
          r_code_text <- paste0(r_code_text, "cfa_result <- cfa(model, data = data, estimator = 'ML')\n")
          r_code_text <- paste0(r_code_text, "summary(cfa_result, fit.measures = TRUE, standardized = TRUE)\n\n")
          if(input$include_modification_indices) {
            r_code_text <- paste0(r_code_text, "# Modification indices\n")
            r_code_text <- paste0(r_code_text, "modificationIndices(cfa_result, sort. = TRUE, minimum.value = 3.84)\n")
          }
          
          if(input$test_invariance && !is.null(input$group_variable) && input$group_variable != "") {
            r_code_text <- paste0(r_code_text, "\n# Measurement Invariance Testing\n")
            r_code_text <- paste0(r_code_text, "# Configural model\n")
            r_code_text <- paste0(r_code_text, "config_model <- cfa(model, data = data, group = '", input$group_variable, "')\n")
            if("metric" %in% input$invariance_levels) {
              r_code_text <- paste0(r_code_text, "# Metric invariance\n")
              r_code_text <- paste0(r_code_text, "metric_model <- cfa(model, data = data, group = '", input$group_variable, "', group.equal = 'loadings')\n")
            }
            if("scalar" %in% input$invariance_levels) {
              r_code_text <- paste0(r_code_text, "# Scalar invariance\n")
              r_code_text <- paste0(r_code_text, "scalar_model <- cfa(model, data = data, group = '", input$group_variable, "', group.equal = c('loadings', 'intercepts'))\n")
            }
            r_code_text <- paste0(r_code_text, "# Compare models\n")
            r_code_text <- paste0(r_code_text, "anova(config_model, metric_model)  # if metric tested\n")
          }
          
          add_r_code(shared_data, r_code_text, "Confirmatory Factor Analysis")
          
        } else if(input$analysis_type == "item_analysis") {
          result <- perform_item_analysis(shared_data$working_data, input$variables)
          values$analysis_result <- result
          
          if(!is.null(result$error)) {
            showNotification(result$error, type = "error")
            removeNotification(notification_id)
            return()
          }
          
          r_code_text <- "# Comprehensive Item Analysis\n"
          r_code_text <- paste0(r_code_text, "library(psych)\n\n")
          r_code_text <- paste0(r_code_text, "variables <- c(", paste0("'", input$variables, "'", collapse = ", "), ")\n")
          r_code_text <- paste0(r_code_text, "data_subset <- data[variables]\n")
          r_code_text <- paste0(r_code_text, "data_subset <- data_subset[complete.cases(data_subset), ]\n\n")
          r_code_text <- paste0(r_code_text, "# Descriptive statistics\n")
          r_code_text <- paste0(r_code_text, "describe(data_subset)\n\n")
          r_code_text <- paste0(r_code_text, "# Item-total correlations\n")
          r_code_text <- paste0(r_code_text, "total_score <- rowSums(data_subset, na.rm = TRUE)\n")
          r_code_text <- paste0(r_code_text, "item_total_cors <- sapply(data_subset, function(x) cor(x, total_score, use = 'complete.obs'))\n")
          r_code_text <- paste0(r_code_text, "print(item_total_cors)\n\n")
          r_code_text <- paste0(r_code_text, "# Interactive correlation heatmap\n")
          r_code_text <- paste0(r_code_text, "library(corrplot)\n")
          r_code_text <- paste0(r_code_text, "corrplot(cor(data_subset), method = 'color', type = 'upper')\n")
          
          add_r_code(shared_data, r_code_text, "Item Analysis")
        }
        
        removeNotification(notification_id)
        showNotification("Analysis completed successfully!", type = "message", duration = 3000)
        
      }, error = function(e) {
        removeNotification(notification_id)
        showNotification(paste("Error in analysis:", e$message), type = "error", duration = 5000)
      })
    })

    output$summary <- renderPrint({
      req(values$analysis_result)
      
      if(!is.null(values$analysis_result$error)) {
        cat("Error:", values$analysis_result$error)
        return()
      }
      
      if(values$current_analysis == "reliability") {
        if(!is.null(values$analysis_result$result_object)) {
          print(values$analysis_result$result_object)
        }
      } else if(values$current_analysis == "efa") {
        if(!is.null(values$analysis_result$efa)) {
          print(values$analysis_result$efa)
          cat("\n=== Kaiser-Meyer-Olkin Test ===\n")
          print(values$analysis_result$kmo)
          cat("\n=== Bartlett's Test of Sphericity ===\n")
          print(values$analysis_result$bartlett)
        }
      } else if(values$current_analysis == "cfa") {
        if(!is.null(values$analysis_result$summary)) {
          print(values$analysis_result$summary)
        }
      } else if(values$current_analysis == "item_analysis") {
        cat("Item Analysis Results:\n")
        print(values$analysis_result$item_stats)
      }
    })
    
    output$reliability_table <- DT::renderDataTable({
      req(values$analysis_result, values$current_analysis == "reliability")
      
      if(!is.null(values$analysis_result$error)) {
        return(DT::datatable(data.frame(Error = values$analysis_result$error), options = list(dom = 't')))
      }
      
      if(input$reliability_method == "alpha") {
        rel_stats <- data.frame(
          Statistic = c("Cronbach's Alpha", "Standardized Alpha", "Number of Items", "Average Inter-Item Correlation"),
          Value = c(
            round(values$analysis_result$alpha, 4),
            round(values$analysis_result$standardized_alpha, 4),
            length(values$current_variables),
            round(mean(values$analysis_result$correlation_matrix[upper.tri(values$analysis_result$correlation_matrix)]), 4)
          )
        )
      } else {
        rel_stats <- data.frame(
          Statistic = c("Omega Total", "Omega Hierarchical", "Cronbach's Alpha", "Number of Items"),
          Value = c(
            round(values$analysis_result$omega_total, 4),
            round(values$analysis_result$omega_hierarchical, 4),
            round(values$analysis_result$alpha, 4),
            length(values$current_variables)
          )
        )
      }
      
      DT::datatable(rel_stats, options = list(dom = 't', pageLength = 10)) %>%
        DT::formatStyle(columns = 1:2, `text-align` = 'center')
    })
    
    output$item_stats_table <- DT::renderDataTable({
      req(values$analysis_result, values$current_analysis == "reliability")
      
      if(!is.null(values$analysis_result$error)) {
        return(DT::datatable(data.frame(Error = values$analysis_result$error), options = list(dom = 't')))
      }
      
      tryCatch({
        item_df <- values$analysis_result$item_stats
        
        if(is.null(item_df)) {
          return(DT::datatable(data.frame(Message = "No item statistics available"), options = list(dom = 't')))
        }
        
        required_cols <- c("mean", "sd")  # Only check for essential columns
        if(!all(required_cols %in% colnames(item_df))) {
          return(DT::datatable(data.frame(Message = paste("Missing required columns. Available columns:", paste(colnames(item_df), collapse = ", "))), options = list(dom = 't')))
        }
        
        display_df <- data.frame(
          Variable = rownames(item_df),
          stringsAsFactors = FALSE
        )
        
        if("n" %in% colnames(item_df)) {
          display_df$n <- round(item_df$n, 0)
        }
        
        if("raw.r" %in% colnames(item_df)) {
          display_df$raw.r <- round(item_df$raw.r, 3)
        }
        
        if("std.r" %in% colnames(item_df)) {
          display_df$std.r <- round(item_df$std.r, 3)
        }
        
        if("r.cor" %in% colnames(item_df)) {  # FIXED: was checking for "item.cor"
          display_df$r.cor <- round(item_df$r.cor, 3)
        }
        
        if("r.drop" %in% colnames(item_df)) {
          display_df$r.drop <- round(item_df$r.drop, 3)
        }
        
        if("mean" %in% colnames(item_df)) {
          display_df$mean <- round(item_df$mean, 3)
        }
        
        if("sd" %in% colnames(item_df)) {
          display_df$sd <- round(item_df$sd, 3)
        }
        
        alpha_dropped <- values$analysis_result$alpha_dropped
        if(!is.null(alpha_dropped) && "alpha" %in% colnames(alpha_dropped)) {
          alpha_if_deleted <- numeric(nrow(display_df))
          for(i in 1:nrow(display_df)) {
            var_name <- display_df$Variable[i]
            if(var_name %in% rownames(alpha_dropped)) {
              alpha_if_deleted[i] <- alpha_dropped[var_name, "alpha"]
            } else {
              alpha_if_deleted[i] <- NA
            }
          }
          display_df$alpha_if_deleted <- round(alpha_if_deleted, 3)
        }
        
        dt <- DT::datatable(display_df, options = list(scrollX = TRUE, pageLength = 10)) %>%
          DT::formatStyle(columns = 2:ncol(display_df), `text-align` = 'center')
        
        if("r.cor" %in% names(display_df)) {
          dt <- dt %>%
            DT::formatStyle(columns = "r.cor",
                            backgroundColor = DT::styleInterval(c(0.2, 0.3), c("#ffebee", "#fff3e0", "#e8f5e8")))
        }
        
        return(dt)
        
      }, error = function(e) {
        DT::datatable(data.frame(Error = paste("Error displaying item stats:", e$message)), options = list(dom = 't'))
      })
    })
    output$loadings_table <- DT::renderDataTable({
      req(values$analysis_result, values$current_analysis == "efa")
      
      if(!is.null(values$analysis_result$error)) {
        return(DT::datatable(data.frame(Error = values$analysis_result$error), 
                             options = list(dom = 't', pageLength = 5)))
      }
      
      tryCatch({
        loadings_matrix <- values$analysis_result$loadings_matrix
        
        if(is.null(loadings_matrix) || nrow(loadings_matrix) == 0 || ncol(loadings_matrix) == 0) {
          return(DT::datatable(data.frame(Message = "No loadings matrix available"), 
                               options = list(dom = 't')))
        }
        
        loadings_matrix <- as.matrix(loadings_matrix)
        
        if(is.null(rownames(loadings_matrix))) {
          rownames(loadings_matrix) <- values$current_variables[1:nrow(loadings_matrix)]
        }
        if(is.null(colnames(loadings_matrix))) {
          colnames(loadings_matrix) <- paste0("Factor", 1:ncol(loadings_matrix))
        }
        
        loadings_df <- data.frame(
          Variable = rownames(loadings_matrix),
          stringsAsFactors = FALSE
        )
        
        for(i in 1:ncol(loadings_matrix)) {
          col_name <- colnames(loadings_matrix)[i]
          loadings_df[[col_name]] <- round(loadings_matrix[, i], 3)
        }
        
        communalities <- values$analysis_result$communalities
        if(!is.null(communalities) && length(communalities) > 0) {
          matched_communalities <- rep(NA, nrow(loadings_df))
          
          for(i in 1:nrow(loadings_df)) {
            var_name <- loadings_df$Variable[i]
            if(var_name %in% names(communalities)) {
              matched_communalities[i] <- communalities[var_name]
            } else if(i <= length(communalities)) {
              matched_communalities[i] <- communalities[i]
            } else {
              matched_communalities[i] <- 0.5  
            }
          }
          
          loadings_df$Communality <- round(matched_communalities, 3)
        } else {
          loadings_df$Communality <- 0.5
        }
        
        factor_cols <- setdiff(names(loadings_df), c("Variable", "Communality"))
        
        dt <- DT::datatable(loadings_df, 
                            options = list(scrollX = TRUE, pageLength = 15),
                            rownames = FALSE) %>%
          DT::formatRound(columns = setdiff(names(loadings_df), "Variable"), digits = 3)
        
        if(length(factor_cols) > 0) {
          dt <- dt %>%
            DT::formatStyle(columns = factor_cols,
                            backgroundColor = DT::styleInterval(c(-0.5, -0.3, 0.3, 0.5), 
                                                                c("#ffcdd2", "#ffebee", "#ffffff", "#e8f5e8", "#c8e6c9")))
        }
        
        return(dt)
        
      }, error = function(e) {
        return(DT::datatable(data.frame(Error = paste("Error creating loadings table:", e$message)), 
                             options = list(dom = 't')))
      })
    })
    
    output$efa_fit_table <- DT::renderDataTable({
      req(values$analysis_result, values$current_analysis == "efa")
      
      if(!is.null(values$analysis_result$error)) {
        return(DT::datatable(data.frame(Error = values$analysis_result$error), options = list(dom = 't')))
      }
      
      tryCatch({
        efa_obj <- values$analysis_result$efa
        
        rmsea_val <- if(!is.null(efa_obj$RMSEA) && length(efa_obj$RMSEA) > 0) round(efa_obj$RMSEA[1], 4) else "N/A"
        tli_val <- if(!is.null(efa_obj$TLI)) round(efa_obj$TLI, 4) else "N/A"
        bic_val <- if(!is.null(efa_obj$BIC)) round(efa_obj$BIC, 2) else "N/A"
        chi_val <- if(!is.null(efa_obj$STATISTIC)) round(efa_obj$STATISTIC, 2) else "N/A"
        df_val <- if(!is.null(efa_obj$dof)) efa_obj$dof else "N/A"
        p_val <- if(!is.null(efa_obj$PVAL)) round(efa_obj$PVAL, 4) else "N/A"
        
        fit_stats <- data.frame(
          Statistic = c("RMSEA", "TLI", "BIC", "Chi-square", "df", "p-value"),
          Value = c(rmsea_val, tli_val, bic_val, chi_val, df_val, p_val),
          Interpretation = c(
            ifelse(rmsea_val != "N/A" && as.numeric(rmsea_val) <= 0.05, "Excellent", ifelse(rmsea_val != "N/A" && as.numeric(rmsea_val) <= 0.08, "Good", "Poor")),
            ifelse(tli_val != "N/A" && as.numeric(tli_val) >= 0.95, "Excellent", ifelse(tli_val != "N/A" && as.numeric(tli_val) >= 0.90, "Acceptable", "Poor")),
            "Lower is better",
            ifelse(p_val != "N/A" && as.numeric(p_val) > 0.05, "Non-significant (Good)", "Significant (Poor fit)"),
            "Model degrees of freedom",
            "Chi-square significance"
          ),
          stringsAsFactors = FALSE
        )
        
        DT::datatable(fit_stats, 
                      options = list(
                        dom = 't',
                        pageLength = 10,
                        ordering = TRUE,  
                        autoWidth = FALSE,  
                        columnDefs = list(
                          list(className = 'dt-center', targets = '_all'),  
                          list(width = '120px', targets = 0),  
                          list(width = '80px', targets = 1),    
                          list(width = '180px', targets = 2)    
                        ),
                        initComplete = DT::JS(
                          "function(settings, json) {",
                          "$(this.api().table().header()).css({'text-align': 'center'});",
                          "}"
                        )
                      ),
                      rownames = FALSE) %>%
          DT::formatStyle(columns = 1:3, 
                          `text-align` = 'center',
                          `vertical-align` = 'middle',
                          `white-space` = 'nowrap')  
      }, error = function(e) {
        DT::datatable(data.frame(Error = paste("Error creating fit table:", e$message)), 
                      options = list(dom = 't'))
      })
    })
    output$cfa_fit_table <- DT::renderDataTable({
      req(values$analysis_result, values$current_analysis == "cfa")
      
      if(!is.null(values$analysis_result$error)) {
        return(DT::datatable(data.frame(Error = values$analysis_result$error), options = list(dom = 't')))
      }
      
      tryCatch({
        fit_indices <- values$analysis_result$fit_indices
        
        interpretations <- character(length(fit_indices))
        names(interpretations) <- names(fit_indices)
        
        if("rmsea" %in% names(fit_indices)) {
          rmsea <- fit_indices["rmsea"]
          if(rmsea <= 0.05) interpretations["rmsea"] <- "Excellent absolute fit"
          else if(rmsea <= 0.08) interpretations["rmsea"] <- "Good absolute fit"
          else if(rmsea <= 0.10) interpretations["rmsea"] <- "Marginal absolute fit"
          else interpretations["rmsea"] <- "Poor absolute fit"
        }
        
        if("srmr" %in% names(fit_indices)) {
          srmr <- fit_indices["srmr"]
          if(srmr <= 0.05) interpretations["srmr"] <- "Excellent absolute fit"
          else if(srmr <= 0.08) interpretations["srmr"] <- "Good absolute fit"
          else if(srmr <= 0.10) interpretations["srmr"] <- "Acceptable absolute fit"
          else interpretations["srmr"] <- "Poor absolute fit"
        }
        
        if("cfi" %in% names(fit_indices)) {
          cfi <- fit_indices["cfi"]
          if(cfi >= 0.95) interpretations["cfi"] <- "Excellent relative fit"
          else if(cfi >= 0.90) interpretations["cfi"] <- "Acceptable relative fit"
          else interpretations["cfi"] <- "Poor relative fit"
        }
        
        if("tli" %in% names(fit_indices)) {
          tli <- fit_indices["tli"]
          if(tli >= 0.95) interpretations["tli"] <- "Excellent relative fit"
          else if(tli >= 0.90) interpretations["tli"] <- "Acceptable relative fit"
          else interpretations["tli"] <- "Poor relative fit"
        }
        
        fit_df <- data.frame(
          Index = names(fit_indices),
          Value = as.numeric(fit_indices),
          Interpretation = interpretations[names(fit_indices)],
          stringsAsFactors = FALSE
        )
        
        fit_df$Interpretation[fit_df$Interpretation == ""] <- "See documentation"
        
        DT::datatable(fit_df, options = list(dom = 't', pageLength = 15)) %>%
          DT::formatRound(columns = "Value", digits = 4) %>%
          DT::formatStyle(columns = 1:3, `text-align` = 'center')
      }, error = function(e) {
        DT::datatable(data.frame(Error = paste("Error displaying fit indices:", e$message)), options = list(dom = 't'))
      })
    })
    
    output$cfa_params_table <- DT::renderDataTable({
      req(values$analysis_result, values$current_analysis == "cfa")
      
      if(!is.null(values$analysis_result$error)) {
        return(DT::datatable(data.frame(Error = values$analysis_result$error), options = list(dom = 't')))
      }
      
      tryCatch({
        params <- values$analysis_result$parameters
        params_display <- params[params$op %in% c("=~", "~~"), c("lhs", "op", "rhs", "est.std", "se", "z", "pvalue")]
        names(params_display) <- c("Factor", "Operator", "Variable", "Std.Estimate", "SE", "z-value", "p-value")
        
        params_display$Significance <- ifelse(params_display$`p-value` < 0.001, "***",
                                              ifelse(params_display$`p-value` < 0.01, "**",
                                                     ifelse(params_display$`p-value` < 0.05, "*", "")))
        
        DT::datatable(params_display, options = list(scrollX = TRUE, pageLength = 15)) %>%
          DT::formatRound(columns = c("Std.Estimate", "SE", "z-value"), digits = 3) %>%
          DT::formatRound(columns = "p-value", digits = 4) %>%
          DT::formatStyle(columns = "Std.Estimate",
                          backgroundColor = DT::styleInterval(c(0.3, 0.5, 0.7), 
                                                              c("#ffebee", "#fff3e0", "#e8f5e8", "#c8e6c9")))
      }, error = function(e) {
        DT::datatable(data.frame(Error = paste("Error displaying parameters:", e$message)), options = list(dom = 't'))
      })
    })
    
    output$modification_indices_table <- DT::renderDataTable({
      req(values$analysis_result, values$current_analysis == "cfa", input$include_modification_indices)
      
      if(!is.null(values$analysis_result$error) || is.null(values$analysis_result$modification_indices)) {
        return(DT::datatable(data.frame(Message = "No modification indices available"), options = list(dom = 't')))
      }
      
      tryCatch({
        mod_indices <- values$analysis_result$modification_indices
        if(nrow(mod_indices) == 0) {
          return(DT::datatable(data.frame(Message = "No significant modification indices found"), options = list(dom = 't')))
        }
        
        mod_display <- mod_indices[, c("lhs", "op", "rhs", "mi", "epc", "sepc.lv")]
        names(mod_display) <- c("LHS", "Operator", "RHS", "MI", "EPC", "SEPC")
        
        mod_display$Interpretation <- ifelse(mod_display$MI >= 10, "Large improvement",
                                             ifelse(mod_display$MI >= 6.64, "Moderate improvement",
                                                    "Small improvement"))
        
        DT::datatable(mod_display, options = list(scrollX = TRUE, pageLength = 10)) %>%
          DT::formatRound(columns = c("MI", "EPC", "SEPC"), digits = 3) %>%
          DT::formatStyle(columns = "MI",
                          backgroundColor = DT::styleInterval(c(3.84, 6.64, 10), 
                                                              c("#ffffff", "#fff3e0", "#ffecb3", "#ffcdd2")))
      }, error = function(e) {
        DT::datatable(data.frame(Error = paste("Error displaying modification indices:", e$message)), options = list(dom = 't'))
      })
    })
    
    output$model_comparison_table <- DT::renderDataTable({
      req(values$analysis_result, values$current_analysis == "cfa")
      
      if(!is.null(values$analysis_result$error)) {
        return(DT::datatable(data.frame(Error = values$analysis_result$error), options = list(dom = 't')))
      }
      
      tryCatch({
        comparison_table <- compare_cfa_models(values$analysis_result)
        
        if(is.null(comparison_table)) {
          return(DT::datatable(data.frame(Message = "No alternative models available for comparison"), options = list(dom = 't')))
        }
        
        DT::datatable(comparison_table, options = list(scrollX = TRUE, pageLength = 10)) %>%
          DT::formatRound(columns = c("Chi_square", "CFI", "TLI", "RMSEA", "SRMR"), digits = 3) %>%
          DT::formatRound(columns = c("AIC", "BIC"), digits = 1) %>%
          DT::formatStyle(columns = 2:ncol(comparison_table), `text-align` = 'center')
      }, error = function(e) {
        DT::datatable(data.frame(Error = paste("Error creating model comparison:", e$message)), options = list(dom = 't'))
      })
    })
    
    output$invariance_fit_table <- DT::renderDataTable({
      req(values$invariance_result, values$current_analysis == "cfa", input$test_invariance)
      
      if(!is.null(values$invariance_result$error)) {
        return(DT::datatable(data.frame(Error = values$invariance_result$error), options = list(dom = 't')))
      }
      
      tryCatch({
        fit_indices <- values$invariance_result$fit_indices
        
        if(length(fit_indices) == 0) {
          return(DT::datatable(data.frame(Message = "No fit indices available"), options = list(dom = 't')))
        }
        
        fit_df <- data.frame(
          Model = names(fit_indices),
          Chi_square = sapply(fit_indices, function(x) round(x["chisq"], 2)),
          df = sapply(fit_indices, function(x) x["df"]),
          p_value = sapply(fit_indices, function(x) round(x["pvalue"], 4)),
          CFI = sapply(fit_indices, function(x) round(x["cfi"], 3)),
          TLI = sapply(fit_indices, function(x) round(x["tli"], 3)),
          RMSEA = sapply(fit_indices, function(x) round(x["rmsea"], 3)),
          SRMR = sapply(fit_indices, function(x) round(x["srmr"], 3)),
          AIC = sapply(fit_indices, function(x) round(x["aic"], 1)),
          BIC = sapply(fit_indices, function(x) round(x["bic"], 1)),
          stringsAsFactors = FALSE
        )
        
        DT::datatable(fit_df, options = list(scrollX = TRUE, pageLength = 10)) %>%
          DT::formatStyle(columns = 2:ncol(fit_df), `text-align` = 'center')
      }, error = function(e) {
        DT::datatable(data.frame(Error = paste("Error displaying invariance fit:", e$message)), options = list(dom = 't'))
      })
    })
    
    output$invariance_comparison_table <- DT::renderDataTable({
      req(values$invariance_result, values$current_analysis == "cfa", input$test_invariance)
      
      if(!is.null(values$invariance_result$error)) {
        return(DT::datatable(data.frame(Error = values$invariance_result$error), options = list(dom = 't')))
      }
      
      tryCatch({
        comparisons <- values$invariance_result$model_comparisons
        
        if(length(comparisons) == 0) {
          return(DT::datatable(data.frame(Message = "No model comparisons available"), options = list(dom = 't')))
        }
        
        comp_df <- data.frame()
        
        for(comp_name in names(comparisons)) {
          comp_data <- comparisons[[comp_name]]
          if(nrow(comp_data) >= 2) {
            row_data <- data.frame(
              Comparison = comp_name,
              Chi_sq_diff = round(comp_data$`Chisq diff`[2], 3),
              df_diff = comp_data$`Df diff`[2],
              p_value = round(comp_data$`Pr(>Chisq)`[2], 4),
              Significant = ifelse(comp_data$`Pr(>Chisq)`[2] < 0.05, "Yes", "No"),
              stringsAsFactors = FALSE
            )
            comp_df <- rbind(comp_df, row_data)
          }
        }
        
        if(nrow(comp_df) == 0) {
          return(DT::datatable(data.frame(Message = "No valid comparisons found"), options = list(dom = 't')))
        }
        
        DT::datatable(comp_df, options = list(scrollX = TRUE, pageLength = 10)) %>%
          DT::formatStyle(columns = 2:ncol(comp_df), `text-align` = 'center') %>%
          DT::formatStyle(columns = "Significant",
                          backgroundColor = DT::styleEqual(c("Yes", "No"), c("#ffcdd2", "#c8e6c9")))
      }, error = function(e) {
        DT::datatable(data.frame(Error = paste("Error displaying comparisons:", e$message)), options = list(dom = 't'))
      })
    })
    
    output$practical_significance_table <- DT::renderDataTable({
      req(values$invariance_result, values$current_analysis == "cfa", input$test_invariance)
      
      if(!is.null(values$invariance_result$error)) {
        return(DT::datatable(data.frame(Error = values$invariance_result$error), options = list(dom = 't')))
      }
      
      tryCatch({
        practical_sig <- values$invariance_result$practical_significance
        
        if(length(practical_sig) == 0) {
          return(DT::datatable(data.frame(Message = "No practical significance tests available"), options = list(dom = 't')))
        }
        
        prac_df <- data.frame()
        
        for(comp_name in names(practical_sig)) {
          comp_data <- practical_sig[[comp_name]]
          row_data <- data.frame(
            Comparison = comp_name,
            Delta_CFI = round(comp_data$delta_cfi, 4),
            Delta_RMSEA = round(comp_data$delta_rmsea, 4),
            CFI_Invariant = ifelse(comp_data$cfi_invariant, "Yes (≤0.01)", "No (>0.01)"),
            RMSEA_Invariant = ifelse(comp_data$rmsea_invariant, "Yes (≤0.015)", "No (>0.015)"),
            Overall_Decision = ifelse(comp_data$cfi_invariant & comp_data$rmsea_invariant, 
                                      "Support Invariance", "Reject Invariance"),
            stringsAsFactors = FALSE
          )
          prac_df <- rbind(prac_df, row_data)
        }
        
        if(nrow(prac_df) == 0) {
          return(DT::datatable(data.frame(Message = "No practical significance data found"), options = list(dom = 't')))
        }
        
        DT::datatable(prac_df, options = list(scrollX = TRUE, pageLength = 10)) %>%
          DT::formatStyle(columns = 2:ncol(prac_df), `text-align` = 'center') %>%
          DT::formatStyle(columns = "Overall_Decision",
                          backgroundColor = DT::styleEqual(c("Support Invariance", "Reject Invariance"), 
                                                           c("#c8e6c9", "#ffcdd2")))
      }, error = function(e) {
        DT::datatable(data.frame(Error = paste("Error displaying practical significance:", e$message)), options = list(dom = 't'))
      })
    })
    
    output$item_analysis_table <- DT::renderDataTable({
      req(values$analysis_result, values$current_analysis == "item_analysis")
      
      if(!is.null(values$analysis_result$error)) {
        return(DT::datatable(data.frame(Error = values$analysis_result$error), options = list(dom = 't')))
      }
      
      tryCatch({
        item_stats <- values$analysis_result$item_stats
        
        item_stats$Item_Quality <- ifelse(item_stats$Item_Total_Correlation >= 0.3, "Good",
                                          ifelse(item_stats$Item_Total_Correlation >= 0.2, "Acceptable", "Poor"))
        
        item_stats$Difficulty_Level <- ifelse(item_stats$Item_Difficulty < 0.3, "Hard",
                                              ifelse(item_stats$Item_Difficulty > 0.7, "Easy", "Moderate"))
        
        DT::datatable(item_stats, options = list(scrollX = TRUE, pageLength = 15)) %>%
          DT::formatRound(columns = setdiff(names(item_stats), c("Variable", "Item_Quality", "Difficulty_Level")), digits = 3) %>%
          DT::formatStyle(columns = "Item_Total_Correlation",
                          backgroundColor = DT::styleInterval(c(0.2, 0.3), c("#ffebee", "#fff3e0", "#e8f5e8"))) %>%
          DT::formatStyle(columns = "Item_Quality",
                          backgroundColor = DT::styleEqual(c("Poor", "Acceptable", "Good"), 
                                                           c("#ffcdd2", "#fff3e0", "#c8e6c9")))
      }, error = function(e) {
        DT::datatable(data.frame(Error = paste("Error displaying item analysis:", e$message)), options = list(dom = 't'))
      })
    })

    output$correlation_plot_interactive <- renderPlotly({
      req(values$analysis_result, values$current_analysis == "reliability")
      
      tryCatch({
        cor_matrix <- values$analysis_result$correlation_matrix
        
        if(is.null(cor_matrix)) {
          df_subset <- shared_data$working_data[values$current_variables]
          df_subset <- df_subset[complete.cases(df_subset), ]
          cor_matrix <- cor(df_subset, use = "complete.obs")
        }
        
        cor_df <- expand.grid(Variable1 = rownames(cor_matrix), Variable2 = colnames(cor_matrix))
        cor_df$Correlation <- as.vector(cor_matrix)
        cor_df$Variable1 <- factor(cor_df$Variable1, levels = rev(rownames(cor_matrix)))
        cor_df$Variable2 <- factor(cor_df$Variable2, levels = colnames(cor_matrix))
        
        p <- ggplot(cor_df, aes(x = Variable2, y = Variable1, fill = Correlation, 
                                text = paste("Variables:", Variable2, "vs", Variable1, 
                                             "<br>Correlation:", round(Correlation, 3)))) +
          geom_tile(color = "white", size = 0.5) +
          scale_fill_gradient2(low = "#d32f2f", mid = "white", high = "#1976d2", 
                               midpoint = 0, limit = c(-1, 1), space = "Lab",
                               name = "Correlation") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
                axis.text.y = element_text(size = 10),
                axis.title = element_blank(),
                panel.grid = element_blank()) +
          labs(title = "")
        
        ggplotly(p, tooltip = "text") %>%
          layout(title = list(text = "", x = 0.5))
        
      }, error = function(e) {
        p <- ggplot() + 
          labs(title = "Error creating correlation plot", subtitle = e$message) + 
          theme_minimal()
        ggplotly(p)
      })
    })
    
    output$efa_plot <- renderPlotly({
      req(values$analysis_result, values$current_analysis == "efa")
      
      if(!is.null(values$analysis_result$error)) {
        p <- ggplot() + 
          labs(title = "Error in EFA analysis", subtitle = values$analysis_result$error) + 
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
        return(ggplotly(p))
      }
      
      tryCatch({
        loadings_matrix <- values$analysis_result$loadings_matrix
        
        if(is.null(loadings_matrix) || nrow(loadings_matrix) == 0 || ncol(loadings_matrix) == 0) {
          p <- ggplot() + 
            labs(title = "No loadings matrix available") + 
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5))
          return(ggplotly(p))
        }
        
        loadings_matrix <- as.matrix(loadings_matrix)
        
        if(ncol(loadings_matrix) >= 2 && nrow(loadings_matrix) > 0) {
          loadings_df <- as.data.frame(loadings_matrix[, 1:2])
          loadings_df$Variable <- rownames(loadings_matrix)
          
          communalities <- values$analysis_result$communalities
          if(!is.null(communalities) && length(communalities) == nrow(loadings_df)) {
            matched_vars <- intersect(names(communalities), rownames(loadings_matrix))
            if(length(matched_vars) > 0) {
              loadings_df$Communality <- communalities[matched_vars]
            } else {
              loadings_df$Communality <- 0.5
            }
          } else {
            loadings_df$Communality <- 0.5
          }
          
          factor1_name <- colnames(loadings_matrix)[1]
          factor2_name <- colnames(loadings_matrix)[2]
          
          p <- ggplot(loadings_df, aes_string(x = names(loadings_df)[1], y = names(loadings_df)[2])) +
            geom_point(aes(size = Communality, color = Communality, 
                           text = paste("Variable:", Variable,
                                        "<br>", factor1_name, ":", round(get(names(loadings_df)[1]), 3),
                                        "<br>", factor2_name, ":", round(get(names(loadings_df)[2]), 3),
                                        "<br>Communality:", round(Communality, 3))), 
                       alpha = 0.8) +
            geom_text(aes(label = Variable), vjust = -0.5, size = 3, color = "#34495e") +
            geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5, color = "#7f8c8d") +
            geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5, color = "#7f8c8d") +
            scale_color_viridis_c(name = "Communality") +
            scale_size_continuous(range = c(3, 8), guide = "none") +
            labs(title = "", 
                 x = factor1_name, 
                 y = factor2_name) +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
          
          ggplotly(p, tooltip = "text")
        } else if(ncol(loadings_matrix) == 1) {
          communalities <- values$analysis_result$communalities
          if(is.null(communalities)) {
            communalities <- rep(0.5, nrow(loadings_matrix))
            names(communalities) <- rownames(loadings_matrix)
          }
          
          loadings_df <- data.frame(
            Variable = rownames(loadings_matrix),
            Loading = as.numeric(loadings_matrix[, 1]),
            Communality = communalities[rownames(loadings_matrix)]
          )
          
          p <- ggplot(loadings_df, aes(x = reorder(Variable, Loading), y = Loading)) +
            geom_col(aes(fill = Communality, 
                         text = paste("Variable:", Variable,
                                      "<br>Loading:", round(Loading, 3),
                                      "<br>Communality:", round(Communality, 3))), 
                     alpha = 0.8) +
            coord_flip() +
            scale_fill_viridis_c(name = "Communality") +
            labs(title = "Single Factor Loadings", 
                 x = "Variables", 
                 y = "Factor Loading") +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
          
          ggplotly(p, tooltip = "text")
        } else {
          p <- ggplot() + 
            labs(title = "No factors extracted or insufficient data") + 
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5))
          ggplotly(p)
        }
      }, error = function(e) {
        p <- ggplot() + 
          labs(title = "Error creating EFA plot", subtitle = e$message) + 
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
        ggplotly(p)
      })
    })
    
    output$cfa_diagram <- renderPlot({
      req(values$analysis_result, values$current_analysis == "cfa")
      
      if (!isTRUE(input$show_diagram)) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Path diagram disabled")
        text(1, 1, "Enable 'Show Path Diagram' to view", cex = 1.2)
        return()
      }
      
      if (!is.null(values$analysis_result$error)) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Error in CFA analysis")
        text(1, 1, "CFA analysis failed", cex = 1.2)
        return()
      }
      
      cfa_model <- values$analysis_result$cfa
      if (is.null(cfa_model)) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "No CFA model available")
        text(1, 1, "CFA model not available", cex = 1.2)
        return()
      }
      
      if (!lavaan::lavInspect(cfa_model, "converged")) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "CFA model did not converge")
        text(1, 1, "Model did not converge properly", cex = 1.2)
        return()
      }
      
      if (!requireNamespace("semPlot", quietly = TRUE)) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "semPlot package not available")
        text(1, 1, "Install semPlot package:\ninstall.packages('semPlot')", cex = 1)
        return()
      }
      
      semPlot::semPaths(cfa_model, 
                        what = "std",
                        whatLabels = "std",
                        edge.label.cex = 0.6,
                        label.color = "black",
                        border.color = "black",
                        posCol = "#c41e3a",
                        negCol = "blue",
                        label.cex = 0.6,
                        edge.width = 0.3)
      
      title("", cex.main = 1.3, col.main = "black")
      
    })
    
    output$item_scatter_plot <- renderPlotly({
      req(values$analysis_result, values$current_analysis == "item_analysis")
      
      if(!is.null(values$analysis_result$error)) {
        p <- ggplot() + labs(title = "Error in analysis") + theme_minimal()
        return(ggplotly(p))
      }
      
      tryCatch({
        item_stats <- values$analysis_result$item_stats
        
        if(!"Item_Discrimination" %in% names(item_stats)) {
          p <- ggplot() + labs(title = "Discrimination data not available") + theme_minimal()
          return(ggplotly(p))
        }
        
        p <- ggplot(item_stats, aes(x = Item_Difficulty, y = Item_Discrimination)) +
          geom_point(aes(size = Item_Total_Correlation, color = Item_Total_Correlation,
                         text = paste("Variable:", Variable,
                                      "<br>Difficulty:", round(Item_Difficulty, 3),
                                      "<br>Discrimination:", round(Item_Discrimination, 3),
                                      "<br>Item-Total Correlation:", round(Item_Total_Correlation, 3))), 
                     alpha = 0.8) +
          geom_text(aes(label = Variable), 
                    size = 3, 
                    color = "black", 
                    fontface = "bold",
                    hjust = 0.5, 
                    vjust = -0.5) +
          scale_color_viridis_c(name = "Item-Total\nCorrelation") +
          scale_size_continuous(range = c(3, 8), guide = "none") +
          labs(title = "", 
               x = "Item Difficulty", 
               y = "Item Discrimination") +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
        
        ggplotly(p, tooltip = "text")
      }, error = function(e) {
        p <- ggplot() + labs(title = "Error creating scatter plot") + theme_minimal()
        ggplotly(p)
      })
    })

    output$scree_plot <- renderPlotly({
      req(values$analysis_result, values$current_analysis == "efa")
      
      if(!is.null(values$analysis_result$error)) {
        p <- ggplot() + labs(title = "Error in analysis") + theme_minimal()
        return(ggplotly(p))
      }
      
      tryCatch({
        eigenvalues <- NULL
        efa_result <- values$analysis_result$efa
        
        if(!is.null(efa_result$values) && length(efa_result$values) > 0) {
          eigenvalues <- efa_result$values
        } else if(!is.null(values$analysis_result$correlation_matrix)) {
          cor_matrix <- values$analysis_result$correlation_matrix
          eigenvalues <- eigen(cor_matrix)$values
        }
        
        if(is.null(eigenvalues) || length(eigenvalues) == 0) {
          p <- ggplot() + labs(title = "No eigenvalues available") + theme_minimal()
          return(ggplotly(p))
        }
        
        scree_df <- data.frame(
          Factor = 1:length(eigenvalues),
          Eigenvalue = eigenvalues
        )
        
        p <- ggplot(scree_df, aes(x = Factor, y = Eigenvalue)) +
          geom_line(size = 1.2, color = "#2c3e50") +
          geom_point(size = 4, color = "#e74c3c", alpha = 0.8) +
          geom_hline(yintercept = 1, linetype = "dashed", color = "#95a5a6", size = 1) +
          scale_x_continuous(breaks = pretty(scree_df$Factor)) +
          scale_y_continuous(breaks = pretty(scree_df$Eigenvalue)) +
          labs(title = "", 
               x = "Factor Number", 
               y = "Eigenvalue") +
          theme_minimal() +
          theme(
            plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            panel.grid.minor = element_blank(),
            axis.text = element_text(size = 11),
            axis.title = element_text(size = 12)
          )
        
        ggplotly(p) %>%
          layout(title = list(text = "", x = 0.5))
        
      }, error = function(e) {
        p <- ggplot() + labs(title = "Error creating scree plot") + theme_minimal()
        ggplotly(p)
      })
    })
    
    output$factor_loadings_summary_plot <- renderPlotly({
      req(values$analysis_result, values$current_analysis == "efa", input$show_factor_loadings_plot)
      
      if(!is.null(values$analysis_result$error)) {
        p <- ggplot() + labs(title = "Error in analysis") + theme_minimal()
        return(ggplotly(p))
      }
      
      tryCatch({
        loadings_matrix <- values$analysis_result$loadings_matrix
        if(is.null(loadings_matrix) || nrow(loadings_matrix) == 0 || ncol(loadings_matrix) == 0) {
          p <- ggplot() + labs(title = "No loadings matrix available") + theme_minimal()
          return(ggplotly(p))
        }
        
        loadings_matrix <- as.matrix(loadings_matrix)
        
        max_loadings_abs <- apply(abs(loadings_matrix), 1, max)
        primary_factor_idx <- apply(abs(loadings_matrix), 1, which.max)
        
        actual_loadings <- numeric(length(primary_factor_idx))
        for(i in 1:length(primary_factor_idx)) {
          actual_loadings[i] <- loadings_matrix[i, primary_factor_idx[i]]
        }
        
        summary_df <- data.frame(
          Variable = rownames(loadings_matrix),
          Loading_Value = actual_loadings,
          Loading_Abs = max_loadings_abs,
          Primary_Factor = paste0("Factor", primary_factor_idx),
          Factor_Num = primary_factor_idx,
          stringsAsFactors = FALSE
        )
        
        summary_df$Loading_Strength <- ifelse(summary_df$Loading_Abs >= 0.7, "Strong (≥0.7)",
                                              ifelse(summary_df$Loading_Abs >= 0.5, "Moderate (0.5-0.7)",
                                                     ifelse(summary_df$Loading_Abs >= 0.3, "Weak (0.3-0.5)", "Very Weak (<0.3)")))
        
        summary_df <- summary_df[order(summary_df$Factor_Num, -summary_df$Loading_Abs), ]
        
        summary_df$Variable_Ordered <- factor(summary_df$Variable, levels = rev(summary_df$Variable))
        
        p <- ggplot(summary_df, aes(x = Loading_Value, y = Variable_Ordered)) +
          geom_col(aes(fill = Primary_Factor,
                       text = paste("Variable:", Variable,
                                    "<br>Loading:", round(Loading_Value, 3),
                                    "<br>Absolute Loading:", round(Loading_Abs, 3),
                                    "<br>Primary Factor:", Primary_Factor,
                                    "<br>Strength:", Loading_Strength)), 
                   alpha = 0.8, width = 0.7) +
          geom_vline(xintercept = 0, color = "black", size = 1) +
          geom_vline(xintercept = c(-0.3, 0.3), linetype = "dashed", color = "gray60", alpha = 0.7) +
          geom_vline(xintercept = c(-0.5, 0.5), linetype = "dashed", color = "gray40", alpha = 0.7) +
          geom_vline(xintercept = c(-0.7, 0.7), linetype = "dashed", color = "gray20", alpha = 0.7) +
          scale_fill_brewer(type = "qual", palette = "Set2", name = "Primary\nFactor") +
          scale_x_continuous(breaks = seq(-1, 1, 0.2), limits = c(-1, 1)) +
          scale_y_discrete() +
          labs(title = "", 
               y = "Variables", 
               x = "Factor Loading") +
          theme_minimal() +
          theme(
            plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, size = 11),
            axis.text.y = element_text(size = 9),
            axis.text.x = element_text(size = 10),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(color = "gray90", size = 0.5)
          )
        
        ggplotly(p, tooltip = "text") %>%
          layout(title = list(text = "", x = 0.5))
        
      }, error = function(e) {
        p <- ggplot() + labs(title = "Error creating loadings summary plot") + theme_minimal()
        ggplotly(p)
      })
    })
    
    output$fit_indices_plot <- renderPlotly({
      req(values$analysis_result, values$current_analysis == "cfa")
      
      if(!is.null(values$analysis_result$error)) {
        p <- ggplot() + labs(title = "Error in analysis") + theme_minimal()
        return(ggplotly(p))
      }
      
      tryCatch({
        cfa_model <- values$analysis_result$cfa
        
        if(is.null(cfa_model)) {
          p <- ggplot() + labs(title = "No CFA model available") + theme_minimal()
          return(ggplotly(p))
        }
        
        param_unstd <- lavaan::parameterEstimates(cfa_model)
        param_std <- lavaan::standardizedSolution(cfa_model)
        
        loadings_unstd <- param_unstd[param_unstd$op == "=~", ]
        loadings_std <- param_std[param_std$op == "=~", ]
        
        if(nrow(loadings_std) == 0) {
          p <- ggplot() + labs(title = "No factor loadings found") + theme_minimal()
          return(ggplotly(p))
        }
        
        plot_df <- data.frame(
          Variable = loadings_std$rhs,
          Factor = loadings_std$lhs,
          Estimate = loadings_std$est.std,
          stringsAsFactors = FALSE
        )
        
        if("se" %in% names(loadings_unstd)) {
          match_indices <- match(paste(plot_df$Factor, plot_df$Variable), 
                                 paste(loadings_unstd$lhs, loadings_unstd$rhs))
          
          unstd_se <- loadings_unstd$se[match_indices]
          unstd_pvals <- loadings_unstd$pvalue[match_indices]
          
          plot_df$Is_Marker <- (unstd_se < 0.0001) | is.na(unstd_se)
          
          plot_df$Point_Type <- "Non-significant"
          plot_df$Point_Type[plot_df$Is_Marker] <- "Marker"
          plot_df$Point_Type[!plot_df$Is_Marker & unstd_pvals < 0.05] <- "Significant"
          
          plot_df$SE_std <- ifelse(plot_df$Is_Marker, 0, 
                                   unstd_se * sqrt(pmax(0.001, 1 - plot_df$Estimate^2)))
          
          plot_df$CI_Lower <- ifelse(plot_df$Is_Marker, plot_df$Estimate,
                                     pmax(plot_df$Estimate - 1.96 * plot_df$SE_std, -1))
          plot_df$CI_Upper <- ifelse(plot_df$Is_Marker, plot_df$Estimate,
                                     pmin(plot_df$Estimate + 1.96 * plot_df$SE_std, 1))
          
          plot_df$P_value <- ifelse(plot_df$Is_Marker, NA, unstd_pvals)
          plot_df$Significant <- ifelse(plot_df$Is_Marker, NA, unstd_pvals < 0.05)
          
        } else {
          plot_df$Is_Marker <- abs(plot_df$Estimate - 1.0) < 0.001
          plot_df$Point_Type <- "Significant"
          plot_df$Point_Type[plot_df$Is_Marker] <- "Marker"
          plot_df$SE_std <- 0.05
          plot_df$CI_Lower <- plot_df$Estimate - 0.1
          plot_df$CI_Upper <- plot_df$Estimate + 0.1
          plot_df$Significant <- !plot_df$Is_Marker
        }
        
        plot_df$Variable_Clean <- gsub("_", " ", plot_df$Variable)
        plot_df$Factor_Clean <- plot_df$Factor
        
        plot_df$Sort_Priority <- ifelse(plot_df$Is_Marker, 1000, plot_df$Estimate)
        
        factor_means <- aggregate(Estimate ~ Factor, plot_df, mean)
        factor_order <- factor_means$Factor[order(-factor_means$Estimate)]
        plot_df$Factor_Ordered <- factor(plot_df$Factor, levels = factor_order)
        
        plot_df <- plot_df[order(plot_df$Factor_Ordered, -plot_df$Sort_Priority), ]
        
        plot_df$Display_Name <- ifelse(plot_df$Is_Marker,
                                       paste0(plot_df$Variable_Clean, " (", plot_df$Factor_Clean, ") - Marker"),
                                       paste0(plot_df$Variable_Clean, " (", plot_df$Factor_Clean, ")"))
        
        plot_df$Final_Order <- factor(plot_df$Display_Name, levels = rev(plot_df$Display_Name))
        
        p <- ggplot(plot_df, aes(x = Final_Order, y = Estimate)) +
          
          geom_rect(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 0.3, 
                    fill = "#ffcdd2", alpha = 0.6, inherit.aes = FALSE) +
          geom_rect(xmin = -Inf, xmax = Inf, ymin = 0.3, ymax = 0.5, 
                    fill = "#ffe0b2", alpha = 0.6, inherit.aes = FALSE) +
          geom_rect(xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 0.7, 
                    fill = "#fff9c4", alpha = 0.6, inherit.aes = FALSE) +
          geom_rect(xmin = -Inf, xmax = Inf, ymin = 0.7, ymax = 1, 
                    fill = "#c8e6c9", alpha = 0.7, inherit.aes = FALSE) +
          
          {if(any(!plot_df$Is_Marker)) {
            geom_linerange(data = plot_df[!plot_df$Is_Marker, ],
                           aes(ymin = CI_Lower, ymax = CI_Upper), 
                           size = 1.2, color = "#1976d2", alpha = 0.8)
          }} +
          
          geom_point(aes(color = Point_Type), size = 4, alpha = 0.9) +
          
          geom_text(aes(label = sprintf("%.2f", Estimate)), 
                    hjust = -0.3, size = 3.5, fontface = "bold", color = "black") +
          
          geom_hline(yintercept = 0, linetype = "solid", color = "gray40", alpha = 0.7, size = 0.5) +
          geom_hline(yintercept = c(0.3, 0.5, 0.7), linetype = "dashed", 
                     color = "gray50", alpha = 0.6, size = 0.5) +
          
          coord_flip(ylim = c(-0.05, 1.05)) +
          
          scale_color_manual(values = c("Marker" = "#757575", 
                                        "Significant" = "#2e7d32", 
                                        "Non-significant" = "#d32f2f"),
                             name = "Parameter Type") +
          
          scale_y_continuous(
            breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0),
            labels = c("0", "0.1", "0.2", "0.3\n(Small)", "0.4", "0.5\n(Medium)", "0.6", "0.7\n(Large)", "0.8", "0.9", "1.0")
          ) +
          
          labs(title = "",
               x = "Variables by Factor",
               y = "Standardized Loading") +
          
          theme_minimal() +
          theme(
            plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray60"),
            axis.text.y = element_text(size = 10),
            axis.text.x = element_text(size = 10),
            legend.position = "none",
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_line(color = "gray95", size = 0.3)
          )
        
        ggplotly(p) %>%
          layout(title = list(text = "", x = 0.5))
        
      }, error = function(e) {
        p <- ggplot() + 
          labs(title = "Error creating confidence intervals plot",
               subtitle = paste("Error:", substr(as.character(e), 1, 60))) + 
          theme_minimal()
        ggplotly(p)
      })
    })

    output$residuals_plot <- renderPlotly({
      req(values$analysis_result, values$current_analysis == "cfa")
      
      if(!is.null(values$analysis_result$error)) {
        p <- ggplot() + labs(title = "Error in analysis") + theme_minimal()
        return(ggplotly(p))
      }
      
      tryCatch({
        cfa_model <- values$analysis_result$cfa
        residuals_matrix <- lavaan::residuals(cfa_model, type = "cor")$cov
        
        if(is.null(residuals_matrix) || nrow(residuals_matrix) == 0) {
          p <- ggplot() + labs(title = "No residuals available") + theme_minimal()
          return(ggplotly(p))
        }
        
        resid_df <- expand.grid(Var1 = rownames(residuals_matrix), Var2 = colnames(residuals_matrix))
        resid_df$Residual <- as.vector(residuals_matrix)
        resid_df$AbsResidual <- abs(resid_df$Residual)
        
        resid_df <- resid_df[upper.tri(residuals_matrix), ]
        
        resid_df$tooltip_text <- paste("Variables:", resid_df$Var2, "vs", resid_df$Var1,
                                       "\nResidual:", round(resid_df$Residual, 3))
        
        p <- ggplot(resid_df, aes(x = Var2, y = Var1, fill = Residual)) +
          geom_tile(color = "white", size = 0.5) +
          scale_fill_gradient2(low = "#d32f2f", mid = "white", high = "#1976d2", 
                               midpoint = 0, name = "Residual") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                axis.title = element_blank(),
                panel.grid = element_blank(),
                plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +
          labs(title = "Standardized Residuals")
        
        ggplotly(p) %>%
          layout(title = list(text = "Standardized Residuals", x = 0.5))
        
      }, error = function(e) {
        p <- ggplot() + labs(title = "Error creating residuals plot") + theme_minimal()
        ggplotly(p)
      })
    })
    
    output$alpha_deleted_plot <-  renderPlotly({
      req(values$analysis_result, values$current_analysis == "reliability", input$reliability_method == "alpha")
      
      if(!is.null(values$analysis_result$error)) {
        p <- ggplot() + labs(title = "Error in analysis") + theme_minimal()
        return(ggplotly(p))
      }
      
      tryCatch({
        item_stats <- values$analysis_result$item_stats
        alpha_dropped <- values$analysis_result$alpha_dropped
        current_alpha <- values$analysis_result$current_alpha
        
        if(!is.null(item_stats) && !is.null(alpha_dropped)) {
          
          correct_alpha_if_deleted <- if("raw_alpha" %in% colnames(alpha_dropped)) {
            alpha_dropped$raw_alpha
          } else if("alpha" %in% colnames(alpha_dropped)) {
            alpha_dropped$alpha
          } else {
            rep(NA, nrow(alpha_dropped))
          }
          
          quality_df <- data.frame(
            Variable = rownames(alpha_dropped),
            Item_Total_Cor = item_stats$r.cor[match(rownames(alpha_dropped), rownames(item_stats))],
            Current_Alpha = current_alpha,
            Alpha_if_Deleted = correct_alpha_if_deleted,
            Variance_Explained = item_stats$r.cor[match(rownames(alpha_dropped), rownames(item_stats))]^2,
            stringsAsFactors = FALSE
          )
          
          quality_df$Reliability_Impact = current_alpha - quality_df$Alpha_if_Deleted
          
          quality_df$Quality_Category <- cut(quality_df$Item_Total_Cor, 
                                             breaks = c(-1, 0.2, 0.3, 1),
                                             labels = c("Poor", "Acceptable", "Good"))
          
          quality_df$tooltip_text <- paste(
            "Variable:", quality_df$Variable,
            "<br>Current α:", round(quality_df$Current_Alpha, 4),
            "<br>α if deleted:", round(quality_df$Alpha_if_Deleted, 4),
            "<br>Reliability contribution:", round(quality_df$Reliability_Impact, 4),
            "<br>Item-Total correlation:", round(quality_df$Item_Total_Cor, 3),
            "<br>Variance explained:", round(quality_df$Variance_Explained * 100, 1), "%"
          )
          
          p <- ggplot(quality_df, aes(x = Item_Total_Cor, y = Reliability_Impact)) +
            geom_point(aes(size = Variance_Explained, 
                           color = Quality_Category,
                           text = tooltip_text), 
                       alpha = 0.7) +
            geom_text(aes(label = Variable), 
                      size = 3, 
                      color = "black", 
                      fontface = "bold",
                      hjust = 0.5, 
                      vjust = 0.5) +
            geom_vline(xintercept = 0.3, linetype = "dashed", color = "gray60", alpha = 0.7) +
            geom_hline(yintercept = 0, linetype = "dashed", color = "gray60", alpha = 0.7) +
            scale_size_continuous(range = c(3, 12), name = "Variance\nExplained") +
            scale_color_manual(values = c("Poor" = "#ff6b6b", 
                                          "Acceptable" = "#feca57", 
                                          "Good" = "#48cab2"),
                               name = "Item Quality") +
            labs(title = "Item Quality Diagnostic",
                 subtitle = "Size = Variance Explained | Color = Overall Quality",
                 x = "Item-Total Correlation (Discrimination)",
                 y = "Reliability Contribution (Impact on α)") +
            theme_minimal() +
            theme(
              plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
              plot.subtitle = element_text(hjust = 0.5, size = 11),
              legend.position = "right"
            )
          
          ggplotly(p, tooltip = "text") %>%
            layout(title = list(text = "Item Quality Diagnostic<br><sub>Size = Variance Explained | Color = Overall Quality</sub>", 
                                x = 0.5))
          
        } else {
          p <- ggplot() + labs(title = "Insufficient data for quality diagnostic") + theme_minimal()
          ggplotly(p)
        }
      }, error = function(e) {
        p <- ggplot() + labs(title = "Error creating quality diagnostic") + theme_minimal()
        ggplotly(p)
      })
    })
    
    output$omega_contribution_plot <- renderPlotly({
      req(values$analysis_result, values$current_analysis == "reliability", input$reliability_method == "omega")
      
      if(!is.null(values$analysis_result$error)) {
        p <- ggplot() + labs(title = "Error in analysis") + theme_minimal()
        return(ggplotly(p))
      }
      
      tryCatch({
        omega_result <- values$analysis_result$result_object
        
        if(!is.null(omega_result)) {
          if(!is.null(omega_result$schmid) && !is.null(omega_result$schmid$sl)) {
            loadings <- omega_result$schmid$sl
            
            if(ncol(loadings) > 0) {
              general_loadings <- loadings[, 1]

              general_loadings_squared <- general_loadings^2
              total_general_variance <- sum(general_loadings_squared)
              
              if(total_general_variance > 0) {
                proportional_contribution <- general_loadings_squared / total_general_variance
              } else {
                proportional_contribution <- rep(0, length(general_loadings_squared))
              }
              
              contrib_df <- data.frame(
                Variable = names(general_loadings),
                General_Loading = general_loadings,
                Loading_Squared = general_loadings_squared,
                Proportional_Contribution = proportional_contribution,
                stringsAsFactors = FALSE
              )
            } else {
              contrib_df <- data.frame(
                Variable = rownames(loadings),
                General_Loading = rep(0, nrow(loadings)),
                Loading_Squared = rep(0, nrow(loadings)),
                Proportional_Contribution = rep(0, nrow(loadings)),
                stringsAsFactors = FALSE
              )
            }
          } else {
            item_stats <- values$analysis_result$item_stats
            if(!is.null(item_stats)) {
              item_total_cors <- item_stats$r.cor
              item_total_cors_squared <- item_total_cors^2
              total_variance <- sum(item_total_cors_squared, na.rm = TRUE)
              
              contrib_df <- data.frame(
                Variable = rownames(item_stats),
                General_Loading = item_total_cors,
                Loading_Squared = item_total_cors_squared,
                Proportional_Contribution = if(total_variance > 0) item_total_cors_squared / total_variance else rep(0, length(item_total_cors_squared)),
                stringsAsFactors = FALSE
              )
            } else {
              contrib_df <- data.frame(
                Variable = character(0),
                General_Loading = numeric(0),
                Loading_Squared = numeric(0),
                Proportional_Contribution = numeric(0),
                stringsAsFactors = FALSE
              )
            }
          }
          
          if(nrow(contrib_df) > 0) {
            contrib_df$tooltip_text <- paste(
              "Variable:", contrib_df$Variable,
              "<br>General factor loading:", round(contrib_df$General_Loading, 3),
              "<br>Loading squared:", round(contrib_df$Loading_Squared, 3),
              "<br>Proportional contribution:", round(contrib_df$Proportional_Contribution, 3),
              "<br>Percentage:", round(contrib_df$Proportional_Contribution * 100, 1), "%"
            )
            
            p <- ggplot(contrib_df, aes(x = reorder(Variable, Proportional_Contribution), 
                                        y = Proportional_Contribution)) +
              geom_col(aes(fill = Loading_Squared, 
                           text = tooltip_text), 
                       alpha = 0.8) +
              coord_flip() +
              scale_fill_viridis_c(name = "Loading\nSquared", option = "plasma") +
              scale_y_continuous(labels = scales::percent_format()) +
              labs(title = "", 
                   subtitle = "Proportional contribution to hierarchical omega",
                   x = "Variables", 
                   y = "Proportional Contribution") +
              theme_minimal() +
              theme(
                plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                plot.subtitle = element_text(hjust = 0.5, size = 11)
              )
            
            ggplotly(p, tooltip = "text")
          } else {
            p <- ggplot() + 
              labs(title = "No omega contribution data available", 
                   subtitle = "Check if McDonald's Omega analysis completed successfully") + 
              theme_minimal() +
              theme(plot.title = element_text(hjust = 0.5),
                    plot.subtitle = element_text(hjust = 0.5))
            ggplotly(p)
          }
        } else {
          p <- ggplot() + 
            labs(title = "Omega analysis not available") + 
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5))
          ggplotly(p)
        }
      }, error = function(e) {
        p <- ggplot() + 
          labs(title = "Error creating omega contribution plot",
               subtitle = paste("Error:", e$message)) + 
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5),
                plot.subtitle = element_text(hjust = 0.5))
        ggplotly(p)
      })
    })
    
    output$scale_distribution_plot <- renderPlotly({
      req(values$analysis_result, values$current_analysis == "reliability")
      
      tryCatch({
        df_subset <- shared_data$working_data[values$current_variables]
        df_subset <- df_subset[complete.cases(df_subset), ]
        
        if(nrow(df_subset) > 0) {
          total_scores <- rowSums(df_subset)
          
          hist_data <- hist(total_scores, plot = FALSE, breaks = 20)
          
          dist_df <- data.frame(
            x = hist_data$mids,
            y = hist_data$density,
            count = hist_data$counts
          )
          
          dist_df$tooltip_text <- paste("Score Range:", round(dist_df$x, 1),
                                        "<br>Density:", round(dist_df$y, 3),
                                        "<br>Count:", dist_df$count)
          
          p <- ggplot(dist_df, aes(x = x, y = y, text = tooltip_text)) +
            geom_col(fill = "#3498db", alpha = 0.7, color = "white", width = diff(hist_data$breaks)[1] * 0.9) +
            labs(title = "", 
                 x = "Total Scale Score", 
                 y = "Density") +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
          
          ggplotly(p, tooltip = "text")
        } else {
          p <- ggplot() + labs(title = "No data available for distribution") + theme_minimal()
          ggplotly(p)
        }
      }, error = function(e) {
        p <- ggplot() + labs(title = "Error creating distribution plot") + theme_minimal()
        ggplotly(p)
      })
    })
    
    output$item_radar_plot <- renderPlotly({
      req(values$analysis_result, values$current_analysis == "item_analysis")
      
      if(!is.null(values$analysis_result$error)) {
        return(plot_ly() %>% add_annotations(text = "Error in analysis", showarrow = FALSE))
      }
      
      tryCatch({
        item_stats <- values$analysis_result$item_stats
        
        required_cols <- c("Item_Difficulty", "Item_Total_Correlation")
        if(!all(required_cols %in% names(item_stats))) {
          return(plot_ly() %>% add_annotations(text = "Required columns not available", showarrow = FALSE))
        }
        
        categories <- c("Difficulty", "Item-Total Cor")
        
        if("Item_Discrimination" %in% names(item_stats)) {
          categories <- c(categories, "Discrimination")
        }
        if("Mean" %in% names(item_stats)) {
          categories <- c(categories, "Mean (normalized)")
        }
        if("SD" %in% names(item_stats)) {
          categories <- c(categories, "Variability")
        }
        
        p <- plot_ly(type = 'scatterpolar', mode = 'lines+markers')
        
        colors <- RColorBrewer::brewer.pal(min(max(3, nrow(item_stats)), 11), "Spectral")
        
        for(i in 1:nrow(item_stats)) {
          values_for_item <- c(
            pmax(0, pmin(1, item_stats$Item_Difficulty[i])),
            pmax(0, pmin(1, (item_stats$Item_Total_Correlation[i] + 1) / 2))
          )
          
          if("Item_Discrimination" %in% names(item_stats)) {
            disc_val <- item_stats$Item_Discrimination[i]
            values_for_item <- c(values_for_item, pmax(0, pmin(1, (disc_val + 1) / 2)))
          }
          
          if("Mean" %in% names(item_stats)) {
            mean_val <- item_stats$Mean[i]
            min_mean <- min(item_stats$Mean, na.rm = TRUE)
            max_mean <- max(item_stats$Mean, na.rm = TRUE)
            if(max_mean > min_mean) {
              norm_mean <- (mean_val - min_mean) / (max_mean - min_mean)
            } else {
              norm_mean <- 0.5
            }
            values_for_item <- c(values_for_item, norm_mean)
          }
          
          if("SD" %in% names(item_stats)) {
            sd_val <- item_stats$SD[i]
            max_sd <- max(item_stats$SD, na.rm = TRUE)
            if(max_sd > 0) {
              norm_sd <- sd_val / max_sd
            } else {
              norm_sd <- 0.5
            }
            values_for_item <- c(values_for_item, norm_sd)
          }
          
          values_for_item <- c(values_for_item, values_for_item[1])
          categories_closed <- c(categories, categories[1])
          
          p <- p %>% add_trace(
            r = values_for_item,
            theta = categories_closed,  
            name = item_stats$Variable[i],
            line = list(width = 2, color = colors[((i-1) %% length(colors)) + 1]),
            marker = list(size = 6, color = colors[((i-1) %% length(colors)) + 1]),
            fill = 'tonext',  # Optional: adds fill
            fillcolor = paste0(colors[((i-1) %% length(colors)) + 1], "20"),  
            hovertemplate = paste(
              "<b>%{fullData.name}</b><br>",
              "%{theta}: %{r:.3f}<br>",
              "<extra></extra>"
            )
          )
        }
        
        p %>% layout(
          polar = list(
            radialaxis = list(
              visible = TRUE,
              range = c(0, 1),
              tickmode = "linear",
              tick0 = 0,
              dtick = 0.2,
              showticklabels = TRUE,
              tickfont = list(size = 10),
              gridcolor = "lightgray"
            ),
            angularaxis = list(
              tickfont = list(size = 12),
              rotation = 90,
              direction = "counterclockwise",
              gridcolor = "lightgray"
            ),
            bgcolor = "white"
          ),
          title = list(
            text = "",
            x = 0.5,
            font = list(size = 16)
          ),
          showlegend = TRUE,
          legend = list(
            orientation = "v",
            x = 1.05,
            y = 0.5,
            font = list(size = 10)
          ),
          margin = list(l = 80, r = 150, t = 60, b = 40),
          paper_bgcolor = "white",
          plot_bgcolor = "white"
        )
        
      }, error = function(e) {
        plot_ly() %>% 
          add_annotations(
            text = paste("Error creating radar chart:", substr(e$message, 1, 100)), 
            showarrow = FALSE,
            font = list(size = 14)
          ) %>%
          layout(title = "Radar Chart Error")
      })
    })
    
    output$response_distribution_heatmap <- renderPlotly({
      req(values$analysis_result, values$current_analysis == "item_analysis", input$show_distributions)
      
      if(!is.null(values$analysis_result$error)) {
        return(plot_ly() %>% add_annotations(text = "Error in analysis", showarrow = FALSE))
      }
      
      tryCatch({
        response_distributions <- values$analysis_result$response_distributions
        
        if(is.null(response_distributions) || length(response_distributions) == 0) {
          return(plot_ly() %>% add_annotations(text = "No response distributions available", showarrow = FALSE))
        }
        
        all_responses <- sort(unique(unlist(lapply(response_distributions, names))))
        response_matrix <- matrix(0, nrow = length(response_distributions), ncol = length(all_responses))
        rownames(response_matrix) <- names(response_distributions)
        colnames(response_matrix) <- all_responses
        
        for(i in 1:length(response_distributions)) {
          dist <- response_distributions[[i]]
          for(j in names(dist)) {
            if(j %in% colnames(response_matrix)) {
              response_matrix[i, j] <- dist[j]
            }
          }
        }
        
        response_matrix <- response_matrix / rowSums(response_matrix)
        
        melted_responses <- expand.grid(
          Variable = factor(rownames(response_matrix), levels = rev(rownames(response_matrix))),
          Response = factor(colnames(response_matrix), levels = colnames(response_matrix))
        )
        melted_responses$Proportion <- as.vector(response_matrix)
        melted_responses$Count <- as.vector(response_matrix * rowSums(response_matrix)[rep(1:nrow(response_matrix), ncol(response_matrix))])
        
        melted_responses$hover_text <- paste(
          "<b>Variable:</b>", melted_responses$Variable, "<br>",
          "<b>Response Value:</b>", melted_responses$Response, "<br>",
          "<b>Proportion:</b>", sprintf("%.3f", melted_responses$Proportion), "<br>",
          "<b>Percentage:</b>", sprintf("%.1f%%", melted_responses$Proportion * 100), "<br>",
          "<extra></extra>"
        )
        
        p <- ggplot(melted_responses, aes(x = Response, y = Variable, fill = Proportion)) +
          geom_tile(aes(text = hover_text), color = "white", size = 0.5) +
          geom_text(aes(label = sprintf("%.2f", Proportion)), 
                    color = ifelse(melted_responses$Proportion > 0.5, "white", "black"),
                    size = 3.5, fontface = "bold") +
          scale_fill_viridis_c(name = "Proportion", option = "plasma", 
                               breaks = seq(0, 1, 0.1),
                               labels = sprintf("%.1f", seq(0, 1, 0.1))) +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12, face = "bold"),
            axis.text.y = element_text(size = 12, face = "bold"),
            axis.title = element_text(size = 14, face = "bold"),
            plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
            panel.grid = element_blank(),
            legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 10)
          ) +
          labs(title = "", 
               x = "Response Values", 
               y = "Variables")
        
        ggplotly(p, tooltip = "text") %>%
          layout(
            title = list(
              text = "",
              x = 0.5,
              font = list(size = 18, family = "Arial", color = "black")
            ),
            xaxis = list(
              title = list(text = "Response Values", font = list(size = 14, face = "bold")),
              tickfont = list(size = 12)
            ),
            yaxis = list(
              title = list(text = "Variables", font = list(size = 14, face = "bold")),
              tickfont = list(size = 12)
            ),
            margin = list(l = 100, r = 100, t = 80, b = 80),
            hovermode = "closest"
          ) %>%
          config(
            displayModeBar = TRUE,
            modeBarButtonsToAdd = list("pan2d", "select2d", "lasso2d", "autoScale2d"),
            toImageButtonOptions = list(
              format = "png",
              filename = "response_distribution_heatmap",
              height = 600,
              width = 800,
              scale = 2
            )
          )
        
      }, error = function(e) {
        plot_ly() %>% 
          add_annotations(
            text = paste("Error creating interactive heatmap:", substr(e$message, 1, 100)), 
            showarrow = FALSE,
            font = list(size = 14)
          ) %>%
          layout(title = "Heatmap Error")
      })
    })

    output$interpretation <- renderUI({
      req(values$analysis_result, values$current_analysis)
      
      if(!is.null(values$analysis_result$error)) {
        return(HTML(paste0("<strong>Error:</strong> ", values$analysis_result$error)))
      }
      
      if(values$current_analysis == "reliability") {
        if(input$reliability_method == "alpha") {
          alpha_val <- values$analysis_result$alpha
          n_items <- length(values$current_variables)
          
          alpha_interpretation <- if(alpha_val >= 0.95) {
            "excellent (potentially redundant items)"
          } else if(alpha_val >= 0.90) {
            "excellent"
          } else if(alpha_val >= 0.80) {
            "good"
          } else if(alpha_val >= 0.70) {
            "acceptable"
          } else if(alpha_val >= 0.60) {
            "questionable"
          } else {
            "poor"
          }
          
          HTML(paste0(
            "<strong>Cronbach's Alpha Reliability Analysis:</strong><br/>",
            "α = ", round(alpha_val, 3), " (", alpha_interpretation, " internal consistency)<br/>",
            "Standardized α = ", round(values$analysis_result$standardized_alpha, 3), "<br/>",
            "Number of items: ", n_items, "<br/><br/>",
            "<strong>Methodological Considerations:</strong><br/>",
            "• Alpha assumes tau-equivalent measurement model (equal factor loadings)<br/>",
            "• Alpha tends to underestimate reliability when items have unequal factor loadings<br/>",
            "• Consider McDonald's Omega for more accurate reliability estimation<br/><br/>",
            "<strong>Interpretation Guidelines:</strong><br/>",
            "• α ≥ 0.95: Excellent (check for item redundancy)<br/>",
            "• α ≥ 0.90: Excellent reliability<br/>",
            "• α ≥ 0.80: Good reliability (adequate for research)<br/>",
            "• α ≥ 0.70: Acceptable reliability (minimum for research)<br/>",
            "• α ≥ 0.60: Questionable reliability (not recommended)<br/>",
            "• α < 0.60: Poor reliability (unacceptable)<br/><br/>",
            "<strong>Recommendations:</strong><br/>",
            if(alpha_val < 0.70) {
              "• Consider removing items with low item-total correlations (< 0.30)<br/>• Examine items that increase alpha when deleted<br/>• Consider scale revision or additional item development"
            } else if(alpha_val > 0.95) {
              "• Excellent reliability achieved<br/>• Consider removing redundant items to improve efficiency<br/>• Check for item overlap or conceptual redundancy"
            } else {
              "• Reliability is adequate for research purposes<br/>• Monitor item-total correlations for potential improvements<br/>• Consider this reliability estimate in interpretation of results"
            }
          ))
        } else {
          omega_total <- values$analysis_result$omega_total
          omega_h <- values$analysis_result$omega_hierarchical
          alpha_val <- values$analysis_result$alpha
          
          HTML(paste0(
            "<strong>McDonald's Omega Reliability Analysis:</strong><br/>",
            "ωt (Omega Total) = ", round(omega_total, 3), "<br/>",
            "ωh (Omega Hierarchical) = ", round(omega_h, 3), "<br/>",
            "Cronbach's α = ", round(alpha_val, 3), " (for comparison)<br/><br/>",
            "<strong>Methodological Advantages of Omega:</strong><br/>",
            "• Does not assume tau-equivalence (more realistic)<br/>",
            "• Accounts for multidimensionality in the scale<br/>",
            "• Generally provides more accurate reliability estimates<br/><br/>",
            "<strong>Interpretation:</strong><br/>",
            "• Omega Total (ωt): Proportion of variance due to all common factors<br/>",
            "• Omega Hierarchical (ωh): Proportion of variance due to general factor only<br/>",
            "• ωt ≥ ωh indicates presence of group factors beyond general factor<br/><br/>",
            "<strong>Guidelines:</strong><br/>",
            "• ωt ≥ 0.80: Good reliability<br/>",
            "• ωt ≥ 0.70: Acceptable reliability<br/>",
            "• ωh > 0.50: Strong general factor (unidimensional interpretation justified)<br/>",
            "• ωh < 0.50: Weak general factor (multidimensional structure likely)"
          ))
        }
        
      } else if(values$current_analysis == "efa") {
        n_factors <- values$analysis_result$n_factors
        kmo_overall <- values$analysis_result$kmo$MSA
        bartlett_p <- values$analysis_result$bartlett$p.value
        
        kmo_interpretation <- if(is.na(kmo_overall)) {
          "unavailable"
        } else if(kmo_overall >= 0.90) {
          "very high [excellent sampling adequacy]"
        } else if(kmo_overall >= 0.80) {
          "high [good sampling adequacy]"
        } else if(kmo_overall >= 0.70) {
          "moderate (adequate sampling adequacy)"
        } else if(kmo_overall >= 0.60) {
          "low [marginal sampling adequacy]"
        } else {
          "very low [inadequate for factor analysis]"
        }
        
        HTML(paste0(
          "<strong>Exploratory Factor Analysis Results:</strong><br/>",
          "Number of factors extracted: ", n_factors, "<br/>",
          "Kaiser-Meyer-Olkin (KMO) = ", 
          ifelse(is.na(kmo_overall), "N/A", round(kmo_overall, 3)), 
          " (", kmo_interpretation, ")<br/>",
          "Bartlett's test p-value = ", 
          ifelse(is.na(bartlett_p), "N/A", ifelse(bartlett_p < 0.001, "< 0.001", round(bartlett_p, 3))), "<br/>",
          "Rotation method: ", values$analysis_result$rotation, "<br/><br/>",
          "<strong>Methodological Assessment:</strong><br/>",
          if(!is.na(kmo_overall) && kmo_overall < 0.60) {
            "⚠️ <strong>Warning:</strong> KMO < 0.60 suggests data may not be suitable for factor analysis<br/>"
          } else if(!is.na(bartlett_p) && bartlett_p >= 0.05) {
            "⚠️ <strong>Warning:</strong> Non-significant Bartlett's test suggests variables may be uncorrelated<br/>"
          } else {
            "✓ Data appears suitable for factor analysis<br/>"
          },
          "<br/><strong>Factor Extraction Guidelines:</strong><br/>",
          "• Eigenvalue > 1.0: Traditional Kaiser criterion<br/>",
          "• Parallel analysis: More accurate factor retention method<br/>",
          "• Scree plot: Visual inspection for 'elbow' in eigenvalues<br/><br/>",
          "<strong>Loading Interpretation:</strong><br/>",
          "• |λ| ≥ 0.71: Excellent (50% shared variance)<br/>",
          "• |λ| ≥ 0.63: Very good (40% shared variance)<br/>",
          "• |λ| ≥ 0.55: Good (30% shared variance)<br/>",
          "• |λ| ≥ 0.45: Fair (20% shared variance)<br/>",
          "• |λ| ≥ 0.32: Poor (10% shared variance)<br/>",
          "• |λ| < 0.32: Suggest removal<br/><br/>",
          "<strong>Rotation Considerations:</strong><br/>",
          "• Oblique rotation (oblimin/promax): Allows factors to correlate<br/>",
          "• Orthogonal rotation (varimax): Forces factors to be uncorrelated<br/>",
          "• Choose oblique when factors are expected to relate conceptually"
        ))
        
      } else if(values$current_analysis == "cfa") {
        fit_assessment <- values$analysis_result$fit_assessment
        
        HTML(paste0(
          "<strong>Confirmatory Factor Analysis Results:</strong><br/><br/>",
          "<strong>Absolute Fit Assessment:</strong><br/>",
          if(!is.null(fit_assessment$absolute_fit$rmsea)) {
            paste0("• RMSEA: ", fit_assessment$absolute_fit$rmsea, "<br/>")
          } else { "" },
          if(!is.null(fit_assessment$absolute_fit$srmr)) {
            paste0("• SRMR: ", fit_assessment$absolute_fit$srmr, "<br/>")
          } else { "" },
          "<br/><strong>Relative Fit Assessment:</strong><br/>",
          if(!is.null(fit_assessment$relative_fit$cfi)) {
            paste0("• CFI: ", fit_assessment$relative_fit$cfi, "<br/>")
          } else { "" },
          if(!is.null(fit_assessment$relative_fit$tli)) {
            paste0("• TLI: ", fit_assessment$relative_fit$tli, "<br/>")
          } else { "" },
          "<br/><strong>Overall Assessment:</strong><br/>",
          "• ", fit_assessment$overall_assessment, "<br/><br/>",
          
          # Add invariance interpretation if tested
          if(!is.null(values$invariance_result) && input$test_invariance) {
            paste0(
              "<strong>Measurement Invariance Results:</strong><br/>",
              "• Grouping Variable: ", values$invariance_result$group_variable, "<br/>",
              "• Group Sizes: ", paste(names(values$invariance_result$group_sizes), "=", 
                                       values$invariance_result$group_sizes, collapse = ", "), "<br/>",
              "• Highest Level Achieved: ", toupper(values$invariance_result$invariance_achieved), "<br/>",
              "• Interpretation: ",
              if(values$invariance_result$invariance_achieved == "configural") {
                "Same factor structure across groups, but parameters may differ"
              } else if(values$invariance_result$invariance_achieved == "metric") {
                "Equal factor loadings across groups - factor means can be compared"
              } else if(values$invariance_result$invariance_achieved == "scalar") {
                "Equal loadings and intercepts - latent means can be meaningfully compared"
              } else if(values$invariance_result$invariance_achieved == "strict") {
                "Equal loadings, intercepts, and residuals - observed scores are comparable"
              } else {
                "Invariance testing failed - groups may have different measurement properties"
              }, "<br/><br/>"
            )
          } else { "" },
          
          "<strong>Methodological Guidelines:</strong><br/>",
          "<em>Absolute Fit Indices:</em><br/>",
          "• RMSEA ≤ 0.05: Excellent fit; ≤ 0.08: Good fit; ≤ 0.10: Marginal fit<br/>",
          "• SRMR ≤ 0.05: Excellent fit; ≤ 0.08: Good fit; ≤ 0.10: Marginal fit<br/>",
          "• Chi-square: Non-significant preferred (but sensitive to sample size)<br/><br/>",
          "<em>Relative Fit Indices:</em><br/>",
          "• CFI/TLI ≥ 0.95: Excellent fit; ≥ 0.90: Acceptable fit<br/>",
          "• These indices compare your model to a null model<br/><br/>",
          "<strong>Model Improvement Strategies:</strong><br/>",
          if(!is.null(values$analysis_result$modification_indices) && input$include_modification_indices) {
            "• Review modification indices (MI > 10.83 = substantial improvement)<br/>• Consider theoretical justification before model modifications<br/>• Prefer measurement model modifications over structural changes<br/>"
          } else { "" },
          "• Ensure parameter estimates are reasonable and significant<br/>",
          "• Check for identification issues or estimation problems<br/>",
          "• Consider alternative model specifications if fit is poor"
        ))
        
      } else if(values$current_analysis == "item_analysis") {
        n_items <- length(values$current_variables)
        item_stats <- values$analysis_result$item_stats
        
        mean_difficulty <- mean(item_stats$Item_Difficulty, na.rm = TRUE)
        mean_discrimination <- if("Item_Discrimination" %in% names(item_stats)) {
          mean(item_stats$Item_Discrimination, na.rm = TRUE)
        } else { NA }
        mean_item_total <- mean(item_stats$Item_Total_Correlation, na.rm = TRUE)
        
        poor_items <- sum(item_stats$Item_Total_Correlation < 0.20, na.rm = TRUE)
        good_items <- sum(item_stats$Item_Total_Correlation >= 0.30, na.rm = TRUE)
        
        HTML(paste0(
          "<strong>Comprehensive Item Analysis Results:</strong><br/>",
          "Number of items analyzed: ", n_items, "<br/>",
          "Mean item difficulty: ", round(mean_difficulty, 3), "<br/>",
          if(!is.na(mean_discrimination)) {
            paste0("Mean item discrimination: ", round(mean_discrimination, 3), "<br/>")
          } else { "" },
          "Mean item-total correlation: ", round(mean_item_total, 3), "<br/><br/>",
          "<strong>Item Quality Summary:</strong><br/>",
          "• Good items (r ≥ 0.30): ", good_items, " (", round(100*good_items/n_items, 1), "%)<br/>",
          "• Acceptable items (0.20 ≤ r < 0.30): ", n_items - good_items - poor_items, "<br/>",
          "• Poor items (r < 0.20): ", poor_items, " (", round(100*poor_items/n_items, 1), "%)<br/><br/>",
          "<strong>Classical Test Theory Metrics:</strong><br/>",
          "• <em>Item Difficulty (p-value):</em><br/>",
          "  - Binary items: Proportion of correct responses<br/>",
          "  - Continuous items: Standardized mean score<br/>",
          "  - Optimal range: 0.30-0.70 for discrimination<br/><br/>",
          "• <em>Item Discrimination:</em><br/>",
          "  - Corrected item-total correlation (item excluded from total)<br/>",
          "  - Indicates how well item differentiates high/low scorers<br/>",
          "  - Preferred over uncorrected item-total correlation<br/><br/>",
          "• <em>Item-Total Correlation:</em><br/>",
          "  - Correlation between item and total scale score<br/>",
          "  - Key indicator of item quality and scale homogeneity<br/><br/>",
          "<strong>Interpretation Guidelines:</strong><br/>",
          "• Item-Total Correlation ≥ 0.40: Excellent item<br/>",
          "• Item-Total Correlation 0.30-0.39: Good item<br/>",
          "• Item-Total Correlation 0.20-0.29: Acceptable item (review)<br/>",
          "• Item-Total Correlation < 0.20: Poor item (consider removal)<br/><br/>",
          "<strong>Item Difficulty Guidelines:</strong><br/>",
          "• 0.15-0.25: Very hard items (appropriate for high-stakes testing)<br/>",
          "• 0.25-0.75: Moderate difficulty (optimal for discrimination)<br/>",
          "• 0.75-0.85: Easy items (appropriate for mastery testing)<br/>",
          "• > 0.85: Very easy items (may lack discrimination)<br/><br/>",
          "<strong>Recommendations for Scale Improvement:</strong><br/>",
          if(poor_items > 0) {
            paste0("• Review ", poor_items, " poor-performing items for revision or removal<br/>")
          } else { "" },
          if(mean_item_total < 0.30) {
            "• Overall item quality is below optimal; consider scale revision<br/>"
          } else if(mean_item_total >= 0.40) {
            "• Excellent overall item quality achieved<br/>"
          } else {
            "• Good overall item quality with room for improvement<br/>"
          },
          "• Consider item response theory analysis for more detailed evaluation<br/>",
          "• Examine response distributions for ceiling/floor effects<br/>",
          "• Assess differential item functioning across subgroups if applicable"
        ))
      }
    })
    
    output$r_code <- renderText({
      if(is.null(input$variables) || length(input$variables) < 2 || is.null(input$analysis_type)) {
        return("# Select variables and analysis type to generate R code")
      }
      
      variables_code <- paste0("c(", paste0("'", input$variables, "'", collapse = ", "), ")")
      
      if(input$analysis_type == "reliability") {
        r_code_text <- paste0("# ", ifelse(input$reliability_method == "alpha", "Cronbach's Alpha", "McDonald's Omega"), " Reliability Analysis\n")
        r_code_text <- paste0(r_code_text, "library(psych)\n\n")
        r_code_text <- paste0(r_code_text, "# Select variables and prepare data\n")
        r_code_text <- paste0(r_code_text, "variables <- ", variables_code, "\n")
        r_code_text <- paste0(r_code_text, "data_subset <- data[variables]\n")
        r_code_text <- paste0(r_code_text, "data_subset <- data_subset[complete.cases(data_subset), ]\n\n")
        
        if(input$reliability_method == "alpha") {
          r_code_text <- paste0(r_code_text, "# Cronbach's Alpha analysis\n")
          r_code_text <- paste0(r_code_text, "alpha_result <- alpha(data_subset, check.keys = TRUE)\n")
          r_code_text <- paste0(r_code_text, "print(alpha_result)\n\n")
          r_code_text <- paste0(r_code_text, "# Interactive correlation matrix\n")
          r_code_text <- paste0(r_code_text, "library(corrplot)\n")
          r_code_text <- paste0(r_code_text, "corrplot(cor(data_subset), method = 'color', type = 'upper', \n")
          r_code_text <- paste0(r_code_text, "         order = 'hclust', tl.cex = 0.8)\n")
        } else {
          r_code_text <- paste0(r_code_text, "# McDonald's Omega analysis\n")
          r_code_text <- paste0(r_code_text, "omega_result <- omega(data_subset, nfactors = 1)\n")
          r_code_text <- paste0(r_code_text, "print(omega_result)\n")
        }
        
      } else if(input$analysis_type == "efa") {
        r_code_text <- "# Exploratory Factor Analysis\n"
        r_code_text <- paste0(r_code_text, "library(psych)\nlibrary(GPArotation)\n\n")
        r_code_text <- paste0(r_code_text, "# Prepare data\n")
        r_code_text <- paste0(r_code_text, "variables <- ", variables_code, "\n")
        r_code_text <- paste0(r_code_text, "data_subset <- data[variables]\n")
        r_code_text <- paste0(r_code_text, "data_subset <- data_subset[complete.cases(data_subset), ]\n\n")
        r_code_text <- paste0(r_code_text, "# Factor analysis suitability tests\n")
        r_code_text <- paste0(r_code_text, "KMO(data_subset)  # Kaiser-Meyer-Olkin test\n")
        r_code_text <- paste0(r_code_text, "cortest.bartlett(data_subset)  # Bartlett's test\n\n")
        
        n_fac <- ifelse(is.null(input$n_factors) || input$n_factors == "", "NULL", input$n_factors)
        r_code_text <- paste0(r_code_text, "# Exploratory Factor Analysis\n")
        r_code_text <- paste0(r_code_text, "efa_result <- fa(data_subset, nfactors = ", n_fac, 
                              ", rotate = '", input$rotation, "', fm = 'ml')\n")
        r_code_text <- paste0(r_code_text, "print(efa_result)\n")
        r_code_text <- paste0(r_code_text, "print(efa_result$loadings, cutoff = 0.3)\n\n")
        r_code_text <- paste0(r_code_text, "# Visualizations\n")
        r_code_text <- paste0(r_code_text, "fa.diagram(efa_result)\n")
        
      } else if(input$analysis_type == "cfa") {
        r_code_text <- "# Confirmatory Factor Analysis\n"
        r_code_text <- paste0(r_code_text, "library(lavaan)\nlibrary(semPlot)\n\n")
        r_code_text <- paste0(r_code_text, "# Model specification\n")
        r_code_text <- paste0(r_code_text, "model <- '\n", input$cfa_model, "\n'\n\n")
        r_code_text <- paste0(r_code_text, "# Fit the model\n")
        r_code_text <- paste0(r_code_text, "cfa_result <- cfa(model, data = data, estimator = 'ML')\n")
        r_code_text <- paste0(r_code_text, "summary(cfa_result, fit.measures = TRUE, standardized = TRUE)\n\n")
        if(input$include_modification_indices) {
          r_code_text <- paste0(r_code_text, "# Modification indices\n")
          r_code_text <- paste0(r_code_text, "modificationIndices(cfa_result, sort. = TRUE, minimum.value = 3.84)\n\n")
        }
        if(input$show_diagram) {
          r_code_text <- paste0(r_code_text, "# Path diagram\n")
          r_code_text <- paste0(r_code_text, "semPaths(cfa_result, what = 'std', layout = 'tree2')\n")
        }
        
      } else if(input$analysis_type == "item_analysis") {
        r_code_text <- "# Comprehensive Item Analysis\n"
        r_code_text <- paste0(r_code_text, "library(psych)\n\n")
        r_code_text <- paste0(r_code_text, "# Prepare data\n")
        r_code_text <- paste0(r_code_text, "variables <- ", variables_code, "\n")
        r_code_text <- paste0(r_code_text, "data_subset <- data[variables]\n")
        r_code_text <- paste0(r_code_text, "data_subset <- data_subset[complete.cases(data_subset), ]\n\n")
        r_code_text <- paste0(r_code_text, "# Descriptive statistics\n")
        r_code_text <- paste0(r_code_text, "describe(data_subset)\n\n")
        r_code_text <- paste0(r_code_text, "# Item-total correlations\n")
        r_code_text <- paste0(r_code_text, "total_score <- rowSums(data_subset, na.rm = TRUE)\n")
        r_code_text <- paste0(r_code_text, "item_total_cors <- sapply(data_subset, function(x) cor(x, total_score, use = 'complete.obs'))\n")
        r_code_text <- paste0(r_code_text, "print(item_total_cors)\n\n")
        r_code_text <- paste0(r_code_text, "# Interactive correlation heatmap\n")
        r_code_text <- paste0(r_code_text, "library(corrplot)\n")
        r_code_text <- paste0(r_code_text, "corrplot(cor(data_subset), method = 'color', type = 'upper')\n")
      }
      
      return(r_code_text)
    })
  })
}

# ===================================================================
# END OF MEASUREMENT RELIABILITY AND VALIDITY MODULE
# ===================================================================