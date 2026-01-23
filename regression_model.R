library(tidyverse)
library(glue)
library(car)
library(sf)
library(spgwr)
library(terra)
library(caret)

pollen_data <- read.csv("/Users/wenggeiwong/pollen_mapping_data/land_use_data/results.csv",header=TRUE)

logTransform <- function(pollen_col_name){
    logs <- log(pollen_data[[pollen_col_name]])
    log_col_name <- paste(pollen_col_name,"log",sep="_")
    pollen_data[[log_col_name]] <<- logs
    return(log_col_name)
}

removeLogOutliers <- function(log_col_name){ # 3 SDs above or below mean are removed
    std_dev = sd(pollen_data[[log_col_name]],na.rm=TRUE)
    mean_val = mean(pollen_data[[log_col_name]],na.rm=TRUE)
    lower = mean_val - (std_dev*3)
    upper = mean_val + (std_dev*3)

    for(i in 1:nrow(pollen_data)){
        pollen_amt <- pollen_data[[log_col_name]][i]
        if(is.na(pollen_amt) | pollen_amt > upper | pollen_amt < lower){
            pollen_data[[log_col_name]][i] <<- NA
            print("set to NA")
        }
    }

    return(pollen_data)
}

#determine which/if a buffer and land_use_var should be included
evaluateBufferCorrelation <- function(log_col_name, land_use_var){
    buffers <- list(50,100,250,500,1000)
    max_correlation <- NULL
    max_correlation_col <- NULL
    pollen_type <- pollen_data[[log_col_name]]

    for(buffer in buffers){
        col_name <- paste0(land_use_var,"_",buffer,"m")
        buffer_var <- pollen_data[[col_name]]

        correlation <- cor.test(buffer_var,pollen_type,method="pearson") # calculate correlation between land use variable and pollen type in format (x,y)
        if(col_name == "tree_canopy_pct_500m"){
          print((correlation))
        }
        if(correlation[[3]] <= 0.05){
            if(is.null(max_correlation) || correlation[[4]][[1]] > max_correlation[[4]][[1]]){
                max_correlation <- correlation
                max_correlation_col <- col_name                 
            }
        }
    }

    if(is.null(max_correlation)){
      print(glue("{land_use_var} cannot be tested in LUR: all p_vals for buffers > 0.05"))
      return(NULL)
    }

    r <- max_correlation[[4]][[1]]
    p_val <- correlation[[3]]
    print(glue("{max_correlation_col} - p: {p_val}, r: {r}"))
    return(list(max_correlation_col,r))
}

evaluatePointCorrelation <- function(log_col_name, land_use_var){ # returns false if cannot be used in LUR, list of statistics if it can
    point_var <- pollen_data[[land_use_var]]
    pollen_type <- pollen_data[[log_col_name]]

    correlation <- cor.test(pollen_type,point_var,method="pearson")
    p_val <- correlation[[3]]
    r <- correlation[[4]]
    r <- r[[1]]

    
    
    if(p_val <= 0.05){
        print(glue("{land_use_var} - p: {p_val}, r: {r}"))
        return(list(land_use_var,r))
    }else{
        print(glue("{land_use_var} cannot be tested in LUR (p > 0.05): - p: {p_val}, r: {r}"))
        return(NULL)
    }
}

selectVars <- function(log_col_name){
    selected_vars <- list()

    possible_vars <- list('tree_canopy_pct','point_elevation','elevation_mean','elevation_min','elevation_max','distance_water','building_vol_density')

    for(var in possible_vars){
        if(var=='point_elevation'| var=='distance_water'){
            result <- evaluatePointCorrelation(log_col_name,var)
            if(any(!is.na(result))){
                selected_vars[[var]] <- list(result)
            }
        }else{
            result <- evaluateBufferCorrelation(log_col_name,var)
            if(any(!is.na(result))){ # if returned NULL value only and not list
                selected_vars[[var]] <- result
            }
        }
    }

    # also create interaction term
    selected_tree_canopy <- (selected_vars$"tree_canopy_pct"[[1]])
    selected_building_density <- (selected_vars$"building_vol_density"[[1]])
    interaction_col_name <- paste0(selected_tree_canopy, "_x_", selected_building_density)
    interaction <- unlist(pollen_data[selected_tree_canopy]) * unlist(pollen_data[selected_building_density])
    pollen_data[interaction_col_name] <<- interaction
    interaction_result <- evaluatePointCorrelation(log_col_name,interaction_col_name)
    if(any(!is.na(result))){
        selected_vars[['interaction']] <- interaction_result
    }

    # order by r value
    selected_vars <- selected_vars[order(sapply(selected_vars, "[[", 2))]
    selected_var_cols <- sapply(selected_vars, "[[", 1)
    return(selected_var_cols)
}

basicLUR <- function(log_col_name,selected_var_cols){
    # r for 500m tree coverage buffer is .736 and 250m is .743 but 500m may be better because plot of residuals has less of a pattern
    vars <- unlist(selected_var_cols)
    vars <- vars[vars != "tree_canopy_pct_250m_x_building_vol_density_1000m"]# exclude for now bc it inflates vif
    current_formula <- as.formula(paste(log_col_name, "~1"))
    current_model <- lm(current_formula, data=pollen_data)
    current_adjusted_r2 <- summary(current_model)$adj.r.squared

    included_vars <- c()
    for(var_name in vars){
        #create new formula
        test_vars <- c(included_vars,var_name)
        new_formula <- as.formula(paste(log_col_name,"~",paste(test_vars,collapse=" + ")))
        new_model <- lm(new_formula,data=pollen_data)

        coef_summary <- summary(new_model)$coefficients
        var_p_val <- coef_summary[var_name, "Pr(>|t|)"]
        new_adjusted_r2 <- summary(new_model)$adj.r.squared

        if(length(test_vars)>1){
            vif_values <- vif(new_model)
            var_vif <- vif_values[var_name]
        }else{ 
            var_vif <- 1.0
        }

        # check if current var_name is included in model

        if(var_p_val < 0.05 && new_adjusted_r2 > current_adjusted_r2){ # && var_vif < 2.0
            included_vars <- c(included_vars, var_name) 
            current_model <- new_model
            current_formula <- new_formula
            current_adjusted_r2 <- new_adjusted_r2
            print(glue("Kept {var_name}: p={round(var_p_val,4)}, adjusted r2={round(new_adjusted_r2,4)}, vif={round(var_vif,4)}\n"))
        }else{
            print(glue("Excluded {var_name}: p={round(var_p_val,4)}, adjusted r2={round(new_adjusted_r2,4)}, vif={round(var_vif,4)}\n"))
        }
    }
    return(list(model=current_model,formula=current_formula))
}

createPolyConfig <- function(selected_vars_list,max_degree=3){
    var_names <- names(selected_vars_list)
    var_names <- var_names[var_names != "interaction"]

    config_list = list()
    for(var_name in var_names){
        config_list[[var_name]] <- 1:max_degree
    }

    poly_configs <- expand.grid(config_list)

    return(poly_configs)
}

buildPolyFormula <- function(log_col_name, selected_vars_list,config){
    formula_terms <- c()

    for(var in names(selected_vars_list)){
        if(var=='interaction') next

        col_name <- selected_vars_list[[var]]
        degree <- config[[var]]

        if(degree==1){
            formula_terms <- c(formula_terms,col_name)
        }else{
            formula_terms <- c(formula_terms,paste0("poly(",col_name,", ",degree,", raw=TRUE)"))
        }
    }

    formula_str <- paste(log_col_name,"~",paste(formula_terms,collapse = " + "))

    return(formula_str)

}

crossValidation <- function(log_col_name,selected_vars_list){
    k_folds=10
    selected_vars <- unlist(selected_vars_list)
    complete_data <- pollen_data[complete.cases(pollen_data[c(log_col_name, selected_vars)]), ]
    poly_configs <- createPolyConfig(selected_vars_list)

    cv_results <- list()

    for(i in 1:nrow(poly_configs)){
        config <- poly_configs[i,]
        formula_str <- buildPolyFormula(log_col_name,selected_vars_list,config)
        model_formula <- as.formula(formula_str)

        set.seed(12345)
        train_control = trainControl(method="cv",number=k_folds)

        cv_model <- train(model_formula, data=complete_data,method="lm",trControl=train_control)

        cv_results[[i]] <- list(
            config=config,
            formula=formula_str,
            rmse=cv_model$results$RMSE,
            r_squared=cv_model$results$r_squared,
            mae=cv_model$results$MAE
        )

        config_str <- paste(names(config),"=",config,collapse=", ")

        print(glue("Config {i}: {config_str} | RMSE={round(cv_model$results$RMSE, 4)}, R2={round(cv_model$results$Rsquared, 4)}\n"))
    }

    rmse_results <- sapply(cv_results,function(x) x$rmse)
    best_i <- which.min(rmse_results)
    best_config <- cv_results[[best_i]]

    print("\nBest Configuration:")
    for(var in names(best_config$config)){
        print(glue("{var} degree: {best_config$config[[var]]}\n"))
    }

    print(glue("Cross-validated RMSE: {best_config$rmse}"))
    print(glue("Cross-validated R2: {best_config$r_squared}"))
    print(glue("Formula: {best_config$formula}"))
    
    resultToDataFrame <- function(result) {
        df <- as.data.frame(result$config)
        df$rmse <- result$rmse
        df$r_squared <- result$r_squared
        df$mae <- result$mae
        return(df)
    }

    results_df <- do.call(rbind, lapply(cv_results, resultToDataFrame))

    return(list(
        best_config = best_config,
        all_results = cv_results,
        results_df = results_df
    ))
}

polynomialModel <- function(cv_results){
    formula_str <- cv_results$best_config$formula
    formula <- as.formula(formula_str)
    model <- lm(formula,data=pollen_data)
    return(list(model=model,formula=formula_str))
}

residualChecks <- function(model,formula){
    # influence measures
    influence_measures <- influence.measures(model)
    summary(influence_measures)

    #cook's distance (influence of data point on overall model)
    #
    cooks_d <- cooks.distance(model)
    plot.new()
    plot(cooks_d, type="h",title("residual check cooks"))
    abline(h = 4/length(cooks_d), col="red", lty=2)

    # examine outlier rows
    # print(pollen_data[c(3,6,19),])

    # create a model w/ no outliers
    formula <- as.formula(model$terms)
    model_no_outliers <- lm(formula, data = pollen_data[-c(3, 6, 19), ])
    summary(model_no_outliers)
    
    plot(model)
}

convertToRaster <- function()

# prepare data
log_col_name <- logTransform("Influx_trees")
removeLogOutliers(log_col_name)
selected_vars <- selectVars(log_col_name)

# basic model
# basic_model_result <- basicLUR(log_col_name,selected_vars)
# basic_model <- basic_model_result$model
# basic_model_formula <- basic_model_result$formula
# print(summary(basic_model))

# polynomial model w/ k-fold cross verification has improved performance; residuals seem to indicate acceptable fit
cv_results <- crossValidation(log_col_name,selected_vars)
poly_model_result <- polynomialModel(cv_results)
poly_model <- poly_model_result$model
poly_model_formula <- poly_model_result$formula
print(summary(poly_model))

# check residuals
# residualChecks(basic_model,basic_model_formula)



