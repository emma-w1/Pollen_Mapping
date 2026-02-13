# TO-DO: compute healthcare access (asthma, allergies), amount of trees per district
library(tidyverse)
library(glue)
library(car)
library(sf)
library(spgwr)
library(terra)
library(caret)
library(dplyr)
library(spatialreg)
library(spdep)

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
        correlation <- cor.test(buffer_var,pollen_type,method="pearson", use=complete.obs) # calculate correlation between land use variable and pollen type in format (x,y)
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

    # # also create interaction term
    # selected_tree_canopy <- (selected_vars$"tree_canopy_pct"[[1]])
    # selected_building_density <- (selected_vars$"building_vol_density"[[1]])
    # interaction_col_name <- paste0(selected_tree_canopy, "_x_", selected_building_density)
    # interaction <- unlist(pollen_data[selected_tree_canopy]) * unlist(pollen_data[selected_building_density])
    # pollen_data[interaction_col_name] <<- interaction
    # interaction_result <- evaluatePointCorrelation(log_col_name,interaction_col_name)
    # if(any(!is.na(result))){
    #     selected_vars[['interaction']] <- interaction_result
    # }

    # order by r value
    selected_vars <- selected_vars[order(sapply(selected_vars, "[[", 2))]
    selected_var_cols <- sapply(selected_vars, "[[", 1)
    return(selected_var_cols)
}

basicLUR <- function(log_col_name,selected_var_cols){
    # r for 500m tree coverage buffer is .736 and 250m is .743 but 500m may be better because plot of residuals has less of a pattern
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

        if(var_p_val < 0.05 && new_adjusted_r2 > current_adjusted_r2 && var_vif < 2.0){ 
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

createPolyConfig <- function(selected_vars_list,max_degree=4){
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
    
    var_combinations <- list()
    # create all possible combinations of selected vars w/ all possible lengths
    for(n in 1:length(selected_vars_list)){
        var_combinations <- c(var_combinations, combn(names(selected_vars_list),n, simplify=FALSE))
    }

    baseline_formula <- as.formula(paste(log_col_name,"~",paste(selected_vars,collapse=" + ")))
    baseline_lm <- lm(baseline_formula, data=complete_data)
    baseline_adjusted_r2 <- summary(baseline_lm)$adj.r.squared

    print(glue("Baseline adj. r2: {baseline_adjusted_r2}"))

    cv_results <- list()

    result_counter <- 0
    for(var_combo in var_combinations){
        current_vars_list <- selected_vars_list[var_combo]
        poly_configs <- createPolyConfig(current_vars_list)

        for(i in 1:nrow(poly_configs)){
            result_counter = result_counter + 1
            config <- as.list(poly_configs[i,])
            names(config) <- names(poly_configs)
            formula_str <- buildPolyFormula(log_col_name,current_vars_list,config)
            model_formula <- as.formula(formula_str)

            set.seed(12345)
            train_control = trainControl(method="cv",number=k_folds)

            cv_model <- train(model_formula, data=complete_data,method="lm",trControl=train_control)

            lm_fit <- cv_model$finalModel
            lm_summary <- summary(lm_fit)
            print(lm_summary)
            p_vals <- lm_summary$coefficients[-1,"Pr(>|t|)"] # for each variable
            all_significant <- all(p_vals <= 0.05)

            adj_r2 <- lm_summary$adj.r.squared
            adj_r2_improves <- adj_r2 > baseline_adjusted_r2

            if(length(current_vars_list[names(current_vars_list) != "interaction"]) < 2){
              vifs <- NULL
              vif_passes <- TRUE
            } else {
              vifs <- vif(lm_fit)
              vif_passes <- all(vifs < 2)
            }



            cv_results[[result_counter]] <- list(
                config=config,
                formula=formula_str,
                rmse=cv_model$results$RMSE,
                r_squared=cv_model$results$Rsquared,
                mae=cv_model$results$MAE,
                adj_r2 = adj_r2,
                all_significant = all_significant,
                vif_passes = vif_passes,
                passes_all_reqs = all_significant & vif_passes & adj_r2_improves,
                variables_used = var_combo
            )

            config_str <- paste(names(config),"=",config,collapse=", ")

            print(glue("Config {result_counter}: {config_str} | Vars: {var_combo} | P={all_significant} | adj_r2={adj_r2_improves} | VIF={vif_passes} | RMSE={round(cv_model$results$RMSE, 4)}, R2={round(cv_model$results$Rsquared, 4)}\n"))
        }
    }

    valid_results <- Filter(function(x) x$passes_all_reqs==TRUE, cv_results)
    print(valid_results)
    rmse_results <- sapply(valid_results,function(x) x$rmse)
    best_i <- which.min(rmse_results)
    
    if(length(best_i)==0 || is.na(best_i) ){
      print("No polynomial expression available: using basic LUR")
      return(basicLUR(log_col_name, selected_vars_list))
    }
    
    best_config <- valid_results[[best_i]]

    print("Best Configuration:")
    for(var in names(best_config$config)){
        print(glue("{var} degree: {best_config$config[[var]]}\n"))
    }

    print(glue("Variables used: {paste(best_config$variables_used, collapse=', ')}"))

    print(glue("Cross-validated RMSE: {best_config$rmse}"))
    print(glue("Cross-validated R2: {best_config$r_squared}"))
    print(glue("Formula: {best_config$formula}"))
    
    resultToDataFrame <- function(result) {
        config_vals <- as.list(result$config)
        df <- data.frame(
            rmse <- result$rmse,
            r_squared <- result$r_squared,
            mae <- result$mae,
            adj_r2 <- result$adj_r2,
            all_significant <- result$all_significant,
            vif_passes <- result$vif_passes,
            passes_all_reqs <- result$passes_all_reqs,
            variables_used <- paste(result$variables_used, collapse=", "),
            stringsAsFactors = FALSE
        )

        for(name in names(config_vals)){
            df[[paste0("degree_",name)]] <- config_vals[[name]]
        }

        return(df)
    }

    all_degree_cols <- unique(unlist(lapply(cv_results, function(x) paste0("degree_", names(x$config)))))

    results_list <- lapply(cv_results, function(result){
        df <- resultToDataFrame(result)
        for(col in all_degree_cols){
            if(!(col %in% names(df))) df[[col]] <- NA
        }
        return(df)
    })

    results_df <- do.call(rbind, results_list)
    return(list(
      best_config = best_config,
      all_results = cv_results,
      results_df = results_df
    ))

}

polynomialModel <- function(cv_results){
    formula_str <- cv_results$best_config$formula
    formula <- as.formula(formula_str)
    relevant_vars <- vars <- all.vars(formula)[-1]
    complete_data <- pollen_data[complete.cases(pollen_data[, relevant_vars]), ]
    print(formula_str)
    model <- lm(formula,data=pollen_data)
    return(list(model=model,formula=formula_str))
}

createGWR <- function(formula){
    formula <- as.formula(formula)
    pollen_sp <- st_as_sf(pollen_data, coords=c("Longitude","Latitude"), crs=4326)
    pollen_sp <- as_Spatial(pollen_sp)
    bandwidth <- gwr.sel(formula, data=pollen_sp, adapt=TRUE)
    gwr_results <- gwr(formula, data=pollen_sp, adapt=bandwidth, hatmatrix=TRUE)
    return(gwr_results)
}

createLagModel <- function(log_col_name, formula){
    formula <- as.formula(formula)
    kept_vars <- all.vars(formula)

    pollen_unique <- pollen_data %>%
      group_by(Latitude, Longitude) %>%
      summarise(across(all_of(kept_vars), \(x) mean(x, na.rm = TRUE)), .groups = "drop")

    pollen_sp <- st_as_sf(pollen_unique,coords=c("Longitude","Latitude"),crs=4326)
    coords <- st_coordinates(pollen_sp)
    # find r nearest neighbors and convert to neighbors
    knn <- knearneigh(coords, k=3)
    nb <- knn2nb(knn)
    listw <- nb2listw(nb, style = "W") #create spatial weights
    lag_results <- lagsarlm(formula, data=pollen_sp, listw=listw)
    return(lag_results)
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

tree_canopy_50m_path_10ft <- "" # USE QGIS TO DO LATER
tree_canopy_500m_path_10ft <- "/Users/wenggeiwong/pollen_mapping_data/rasters/tree_500m_buffer_10ft_res.tif"
tree_canopy_100m_path_10ft <- "/Users/wenggeiwong/pollen_mapping_data/rasters/manhattan_tree_100m_buffer_10ft_res.tif"

elevation_min_500m_path <- "/Users/wenggeiwong/pollen_mapping_data/rasters/elevation_min_500m.tif"
building_vol_1000m_path <- "TEMP"

LURToRaster <- function(pollen_type,model,model_type,borough,resolution=10){
    # # area_bbox = c(40.68291695, 40.91553278,-74.04772963,-73.76533244) # this is for only manhattan and bronx
    # area_bbox = c(913153.3, 1073155, 112931.8, 272933.8)
    # 
    # r <- rast(
    #     xmin = area_bbox[1],
    #     xmax = area_bbox[2],
    #     ymin = area_bbox[3],
    #     ymax = area_bbox[4],
    #     crs = "EPSG:2263" # to match raster files from NYC Open Data
    # )
    # coords <- crds(r)

    # prediction rasters
    print('Hi')
    # tree_canopy50 <- rast(tree_canopy_50m_path_10ft)
    tree_canopy500 <- rast(tree_canopy_500m_path_10ft)
    tree_canopy100 <- rast(tree_canopy_100m_path_10ft)
    elevation <- rast(elevation_min_500m_path)
    # building_1000 <- rast(building_vol_1000m_path)
    print("passes")
    
    # variables used based on polynomial cross validation when possible, basic LUR otherwise
    if(model_type == "LUR" && (borough == "" || borough == "bronx")){ # for influx & influx_alrg for all boroughs and for specifically the Bronx, the formula is the same
        predictors <- c(tree_canopy500)
        names(predictors) <- c("tree_canopy_pct_500m")
        print("passes")
    }# else if(model_type=="LUR" && borough=="manhattan" && pollen_type == "Influx_trees"){
    #     predictors <- c(tree_canopy100,building_1000)
    #     names(predictors) <- c("tree_canopy_pct_100m","building_vol_density_1000m")
    # }else if(model_type=="LUR" && borough=="manhattan" && pollen_type == "Influx_trees_alrg"){
    #     predictors <- c(building_1000, tree_canopy50)
    #     predictors <- c("building_vol_density_1000m","tree_canopy_pct_50m")
    # }
    
    # print("Predictor names:")
    # print(names(predictors))
    # print("Model variables:")
    # print(all.vars(formula(model))[-1])
    # 
    

    predictions <- predict(model = model, object = predictors, na.rm= TRUE) 
    # convert from log transform to actual values
    predictions <- exp(predictions)
    
    if(borough==""){
      writeRaster(predictions, glue("/Users/wenggeiwong/pollen_mapping_data/rasters/pollen_prediction_rasters/{pollen_type}_{model_type}.tif"))
    }else{
      writeRaster(predictions, glue("/Users/wenggeiwong/pollen_mapping_data/rasters/pollen_prediction_rasters/{borough}_{pollen_type}_{model_type}.tif"))
    }
    return(predictions)
}


# prepare data
pollen_data <- read.csv("/Users/wenggeiwong/pollen_mapping_data/land_use_data/results.csv",header=TRUE)
pollen_data <- pollen_data[pollen_data$Borough == "Bronx",]
col_name <- "Influx_trees_alrg" 
log_col_name <- logTransform(col_name)
removeLogOutliers(log_col_name)
selected_vars <- selectVars(log_col_name)




# polynomial model w/ k-fold cross verification run when possible; residuals seem to indicate acceptable fit
cv_results <- crossValidation(log_col_name,selected_vars)
print("done!")
if(!all(is.null(cv_results$best_config)) && !all(is.na(cv_results$best_config)) && length(cv_results$best_config) != 0){
    lur_model_result <- polynomialModel(cv_results)
    lur_model <- lur_model_result$model
    lur_model_formula <- lur_model_result$formula
    print(summary(lur_model))
}else{
    lur_model <- cv_results$model
    lur_model_formula <- cv_results$formula
    print(summary(lur_model))
}

gwr_model <- createGWR(lur_model_formula)
print(gwr_model$SDF)

lag_model <- createLagModel(log_col_name, lur_model_formula)
print(lag_model)

print(glue("GWR AIC: {gwr_model$results$AICb}"))
print(glue("LUR AIC: {AIC(lur_model)}"))
print(glue("Lag AIC: {AIC(lag_model)}"))

raster <- LURToRaster(col_name,lur_model, "LUR", "bronx")

# check residuals
# residualChecks(basic_model,basic_model_formula)










