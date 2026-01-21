library(tidyverse)
library(glue)
library(car)

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

        correlation <- cor.test(buffer_var,pollen_type) # calculate correlation between land use variable and pollen type in format (x,y)
        
        if(correlation[[3]] <= 0.05){
            if(is.null(max_correlation) || correlation[[4]][[1]] > max_correlation[[4]][[1]]){
                max_correlation <- correlation
                max_correlation_col <- col_name                 
            }
        }
    }

    if(is.null(max_correlation)){
        return(FALSE)
    }

    r <- max_correlation[[4]][[1]]
    p_val <- correlation[[3]]

    return(list(land_use_var, r, p_val))
}

evaluatePointCorrelation <- function(log_col_name, land_use_var){ # returns false if cannot be used in LUR, list of statistics if it can
    max_correlation <- NULL
    point_var <- pollen_data[[land_use_var]]
    pollen_type <- pollen_data[[log_col_name]]

    correlation <- cor.test(pollen_type,point_var)
    p_val <- correlation[[3]]
    r <- correlation[[4]]
    r <- r[[1]]
    
    if(p_val <= 0.05){
        return(list(land_use_var, r ,p_val))
    }else{
        return(FALSE)
    }
}

calculateVIF <- function(r){
    r_squared <- r^2
    vif <- 1 / (1-r_squared)
    return(vif)
}



# logTransform("Influx_trees")
# removeLogOutliers("Influx_trees")
hi <- (evaluatePointCorrelation("Influx_trees","point_elevation"))
hello <- evaluateBufferCorrelation("Influx_trees","tree_canopy_pct")
