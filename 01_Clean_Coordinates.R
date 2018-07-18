# Purpose: Clean up Vamos Ler! Data for Igor in Mozambique - fix Lat/Lon
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2018_07_18

# Source 00_Clean_VamosLer.R if not already executed.
source("00_Clean_VamosLer.R")

# Load Latitude and Longitude Data ----------------------------------------

coord_data <- "Vamos Ler Intervention schools 2018 _ Contacts_ VL_TMD_FINAL.xlsx"

geo_df <- read_excel(file.path(datapath, coord_data), 
                             sheet = "Coordinates")

# Is school code really unique? Appears not to be -- 89 are missing, quite a few dups
# Appears that the first 1 is unique and sorting order is preserved
  geo_df %>% 
    group_by(`Código da escola`, `Nome da Escola`) %>% 
    tally() %>% 
    filter(n != 1) %>% 
    arrange(-n)
  
# Keep only what is needed to fix Lat/Lon; Flag schools with completed data
  geo_df_sub <- 
    geo_df %>% 
    select(latitude = Lat.,
           longitude = Long., 
           school_id = X__1, 
           school_name = `Nome da Escola`) %>% 
    
    # One school has a negative sign missing from the lat / lon; is a character no
    mutate(latitude = ifelse(school_id == 158, str_c("-", latitude), latitude),
      coord_flag = (grepl(pattern = "-", latitude))) 
  

# Schools with completed latitude longitude -------------------------------

  geo_df_filled <- 
    geo_df_sub %>% 
    filter(coord_flag == "TRUE") %>% 
    mutate_at(vars(latitude, longitude), 
              funs(as.numeric(.)))

# Schools with missing info -----------------------------------------------
  
  geo_df_miss <- 
    geo_df_sub %>% 
    filter(coord_flag == "FALSE" & latitude == "S/dados") %>% 
    # Coerce all the missing values to NA numerica values
    mutate_at(vars(latitude, longitude), 
              funs(as.numeric(.)))
  
  write_csv(geo_df_miss,
            file.path(datapath, "2018_VamosLer_School_Missing_Coordinates.csv"))  
  
# Fix degress minutes seconds entries -------------------------------------


# We are below the equator, so we can use the "-" of the latitude field to flag observations
# that have coordinates that are likely numbers already
# grepl -- returns a logical based on a grep condition (here, that a latitude contains a negative)
  geo_df_decdeg <- 
    geo_df_sub  %>% 
    filter(coord_flag == "FALSE" & latitude != "S/dados") %>% 
    
    #Using the DDMMSS pattern to create a separable entry
    mutate(latitude = str_replace_all(latitude, fixed(" "), ""), 
        lat_tmp = gsub('°|\"|\'', '_', latitude), 
        
        longitude = str_replace_all(longitude, fixed(" "), ""),
        lon_tmp = gsub('°|\"|\'', '_', longitude)) %>% 
          
    separate(lat_tmp, c("deg", "min", "sec"), sep = "_") %>% 
    separate(lon_tmp, c("deg_l", "min_l", "sec_l"), sep = "_") %>% 
    mutate_at(vars(deg, min, sec, deg_l, min_l, sec_l), funs(as.numeric(.))) %>% 
    
  # Now rebuild the coordinates; multiply latitude by -1 for Southern Hemisphere
    mutate(latitude = (deg + (min/60) + (sec/3600)),
           latitude = (latitude * -1),
           longitude = (deg_l + (min_l/60) + (sec_l/3600))) %>% 
    select(-matches("sec|min|deg"))


# Row bind everything into final dataset ----------------------------------
  # Append the new dataframe this to the original data, first slicing the missing vars away
  geo_df_schools <- bind_rows(geo_df_filled, geo_df_decdeg, geo_df_miss) %>% 
    arrange(school_id) 
  
  # Rejoin to original data
  geo_df_final <- 
    geo_df %>% 
    left_join(geo_df_schools, by = c("X__1" = "school_id"))

# Map out coordinates to see if they make sense
  geo_df_final %>% 
    ggplot(aes(x = longitude, y = latitude, colour = Distrito )) +
    geom_point(size = 3)

# Merge with the tidy dataset for cleaned up version and write to csv
  tidy_df %>% 
    left_join(geo_df_schools, by = c("school_id")) %>% 
    write_csv(., file.path(datapath, "2018_VamosLer_tidy.csv"))
  

# Export final data sets --------------------------------------------------

  write_csv(geo_df_schools, 
            file.path(datapath, "2018_VamosLer_School_Coordinates_clean.csv"))
  
  write_csv(geo_df_schools, 
            file.path(datapath, "2018_VamosLer_School_Coordinates_clean_merged.csv"))
  
    
    
  