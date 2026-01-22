library(tidyverse)
library(glue)
library(car)
library(sf)
library(spgwr)
library(terra)

pollen_data <- read.csv("/Users/wenggeiwong/pollen_mapping_data/land_use_data/results.csv",header=TRUE)

logTransform <- function(pollen_col_name){
    logs <- log(pollen_data[[pollen_col_name]])
    log_col_name <- paste(pollen_col_name,"log",sep="_")
    pollen_data[[log_col_name]] <- logs
    return(pollen_data)
}

removeLogOutliers <- function(log_col_name){ # 3 SDs above or below mean are removed
    std_dev = sd(pollen_data[[log_col_name]],na.rm=TRUE)
    mean_val = mean(pollen_data[[log_col_name]],na.rm=TRUE)
    lower = mean_val - (std_dev*3)
    upper = mean_val + (std_dev*3)

    for(i in 1:nrow(pollen_data)){
        pollen_amt = pollen_data[[log_col_name]][i]
        if(!is.na(pollen_amt) | pollen_amt > upper | pollen_amt < lower){
            pollen_data[[log_col_name]][i] <- NA
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

buildLUR <- function(log_col_name,selected_var_cols){
    vars <- unlist(selected_var_cols)
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
            current_adjusted_r2 <- new_adjusted_r2
            print(glue("Kept {var_name}: p={round(var_p_val,4)}, adjusted r2={round(new_adjusted_r2,4)}, vif={round(var_vif,4)}\n"))
        }else{
            print(glue("Excluded {var_name}: p={round(var_p_val,4)}, adjusted r2={round(new_adjusted_r2,4)}, vif={round(var_vif,4)}\n"))
        }
    }
    return(current_model)
}

logTransform("Influx_trees")
removeLogOutliers("Influx_trees")
selected_vars <- selectVars("Influx_trees")

model <- buildLUR("Influx_trees",selected_vars)
print(summary(model))
plot(model,which=1)



