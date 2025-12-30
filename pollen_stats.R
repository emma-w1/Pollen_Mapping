library(sf)
library(ggplot2)
library(tmap)
library(tmaptools)
library(leaflet)
library(dplyr)
library(nycgeo)
library(dplyr)
library(htmltools)
library(terra)
library(httr)
library(jsonlite)
library(dotenv)
library(glue)
library(stringr)
library(car)
library(broom)
library(rstatix)


# TO-DO: reformat file structure so that unsignificant & significant findings are separated in anova_results & welch's anova results

options(scipen = 999)


levene_test <- function(general_df){ # checks if variances in groups are similar
  levene_result <- leveneTest(mean_pollen ~ zone, data = general_df)
  levene_df = as.data.frame(levene_result)
  levene_pval = levene_df["group","Pr(>F)"]
  
  if(levene_pval > 0.05){ 
    print(glue("Levene test: ANOVA valid w/ levene p-value of {levene_pval}"))
    return(TRUE)
  }else{
    print(glue("Levene test: ANOVA invalid approach w/ levene p-value of {levene_pval}"))
    return(FALSE)
  }
}
  

group_by_zone <- function(full_df_filepath){
  df <- read.csv(full_df_filepath)
  df$zone <-substr(df$zone_id,1,1)
  # remove industrial and commercial zones
  df <- df[df$zone_id != 'Industrial',]
  df <- df[df$zone_id != 'Commercial',]
  df <- df[df$zone != 'E',] # we have to remove zone E b/c anova and welch's anova cannot be performed w/ one value
  return(df)
}

get_file_label <- function(filepath){
  # deletes string up to 6th / 
  title_str <- strsplit(filepath,split="_data/")[[1]] #double brackets to go from list of vectors to vector of split
  title_str <- title_str[3] # vector w/ length of 3 to vector w/ length 2
  title_str <- gsub(".csv","",title_str)
  return(title_str)
}

shapiro_wilk_test <- function(general_df){ #determines if normal
  shapiro_result <- shapiro.test(general_df$mean_pollen)
  shapiro_pval <- shapiro_result[[2]]
  if(shapiro_pval <= 0.05){ 
    print(glue("Shapiro-Wilk: Distribution of data is not normal with p-value of {shapiro_pval}"))
    return(FALSE)
  }else{
    print(glue("Shapiro Wilk: Distribution of data is approx. normal with p-value of {shapiro_pval}"))
    return(TRUE)
  }
}

anova_test <- function(df,title_label){ # this is specifically based on mean
  
  anova_result <- aov(mean_pollen~zone,data=df)
  anova_df <- as.data.frame(summary(anova_result)[[1]])
  
  write.csv(anova_df,file=glue("/Users/wenggeiwong/Pollen_Mapping/nyccas_results/anova_results/anova_test_{title_label}.csv"))

  return(anova_result)
}

anova_post_hoc_required <- function(df_anova){
  anova_pval <- df_anova["zone","Pr(>F)"]
  if(anova_pval <= 0.05){
    print(glue("ANOVA: Further post-hoc testing required w/ ANOVA p-val of {anova_pval}"))
    return(TRUE)
  }else{
    print(glue("ANOVA: Further post-hoc testing not required w/ ANOVA p-val of {anova_pval}"))
    return(FALSE)
  }
}

# tukey test
tukey_test <- function(anova_result,title_label){
  tukey_result <- TukeyHSD(anova_result)
  tukey_df <- as.data.frame(tukey_result$zone)
  png(file=glue("/Users/wenggeiwong/Pollen_Mapping/nyccas_results/anova_results/tukey_results/bronx_tukey_results/bronx_tukey_plots/tukey_plot_{title_label}.png"))
  plot(tukey_result) 
  dev.off()
  write.csv(tukey_df,file=glue("/Users/wenggeiwong/Pollen_Mapping/nyccas_results/anova_results/tukey_results/tukey_test_{title_label}.csv"))
  print("Tukey Test Complete:")
}

welchs_anova_test <- function(general_df,title_label){
  welchs_anova_result <- oneway.test(mean_pollen ~ zone, data = general_df, var.equal = FALSE)
  welchs_anova_df <- tidy(welchs_anova_result)
  welchs_anova_df$method <- NULL
  write.csv(welchs_anova_df,file=glue("/Users/wenggeiwong/Pollen_Mapping/nyccas_results/welchs_anova_results/welchs_anova_test_{title_label}.csv"))
  return(welchs_anova_result)
}

welchs_anova_pos_hoc_required <- function(df_welchs_anova){
  welchs_anova_pval <- df_welchs_anova[1,"p.value"]
  if(welchs_anova_pval <= 0.05){
    print(glue("Welch's ANOVA: Further post-hoc testing required w/ Welch's ANOVA p-val of {welchs_anova_pval}"))
    return(TRUE)
  }else{
    print(glue("Welch's ANOVA: Further post-hoc testing not required w/ Welch's ANOVA p-val of {welchs_anova_pval}"))
    return(FALSE)
  }
}

games_howell_test <- function(general_df,title_label){
  games_howell_result <- games_howell_test(general_df, mean_pollen~zone)
  write.csv(games_howell_result,file=glue("/Users/wenggeiwong/Pollen_Mapping/nyccas_results/welchs_anova_results/games_howell_results/games_howell_tests_{title_label}.csv"))
  print("Games-Howell Test Complete")
}

main <- function(){
  filepath1 <- "/Users/wenggeiwong/Pollen_Mapping/data/joined_data/bronx_data/O3_2023_bronx.csv"
  label <- get_file_label(filepath1)
  general_df <- group_by_zone(filepath1)
  is_normal <- shapiro_wilk_test(general_df)
  similar_intra_variance <- levene_test(general_df)
  welchs_anova_test(general_df,label)
  if(similar_intra_variance){
    # anova
    result_anova <- anova_test(general_df,label)
    df_anova <- as.data.frame(summary(result_anova)[[1]])
    is_post_hoc_anova <- anova_post_hoc_required(df_anova)
    if(is_post_hoc_anova){
      tukey_test(result_anova,label)
    }
  }else{
    if(is_normal){
      # welch's anova implementation 
      result_welchs_anova <- welchs_anova_test(general_df,label)
      welchs_anova_result <- welchs_anova_test(general_df,label)
      welchs_anova_df <- tidy(welchs_anova_result)
      welchs_anova_df$method <- NULL
      is_post_hoc_welchs_anova <- welchs_anova_post_hoc_required(welchs_anova_df)
      # if significant, games-howell test for post-hoc
      if(is_post_hoc_welchs_anova){
        games_howell_test(general_df,label)
      }
    }else{
      # kruskal-wallis implementation
      # if significant, dunn test for post-hoc
    }
  }
  
  print("Complete!!!")
}

main()


# after testing, anova is applicable for all bronx and manhattan csvs, must retest w/ pollen
anova_for_folder <- function(folder_path){ # checks if anova is valid for whole folder !! RETEST W/ POLLEN DATA
  files <- list.files(path = folder_path, full.names = TRUE)
  for(file in files){
    label <- get_file_label(file)
    general_df <- group_by_zone(file)
    similar_intra_variance <- levene_test(general_df)
    if(similar_intra_variance){
      cat(file,"anova valid\n\n\n\n")
    }else{
      cat(file,"anova invalid\n\n\n")
    }
  }
}