# Purpose: Clean up Vamos Ler! Data for Igor in Mozambique - fix Lat/Lon
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2018_07_18

# Source 00_Clean_VamosLer.R if not already executed.


# Load Latitude and Longitude Data ----------------------------------------

coord_data <- "Vamos Ler Intervention schools 2018 _ Contacts_ VL_TMD_FINAL.xlsx"

geo_df <- read_excel(file.path(datapath, ler_data), 
                             sheet = "Coordinates")

# Is school code really unique? Appears not to be -- 89 are missing, quite a few dups
# Appears that the first 1 is unique and sorting order is preserved
  geo_df %>% 
    group_by(`Código da escola`, `Nome da Escola`) %>% 
    tally() %>% 
    filter(n != 1) %>% 
    arrange(-n)

# Keep only what is needed to fix Lat/Lon
  geo_df <- 
    geo_df %>% 
    select(latitude = Lat.,
           longitude = Long., 
           school_id = X__1, 
           school_name = `Nome da Escola`) %>% 
    mutate(latitude = ifelse(school_id == 158, str_c("-", latitude), latitude),
      coord_flag = (grepl(pattern = "-", latitude)))

# We are below the equator, so we can use the "-" of the latitude field to flag observations
# that have coordinates that are likely numbers already
# grepl -- returns a logical based on a grep condition (here, that a latitude contains a negative)
  coord_df <- 
    geo_df  %>% 
    filter(coord_flag == "FALSE" & latitude != "S/dados") %>% 
    mutate(latitude = str_replace_all(latitude, fixed(" "), ""), 
        lat_tmp = gsub('°|\"|\'', '_', latitude), 
        
        longitude = str_replace_all(longitude, fixed(" "), ""),
        lon_tmp = gsub('°|\"|\'', '_', longitude)) %>% 
          
    separate(lat_tmp, c("deg", "min", "sec"), sep = "_") %>% 
    separate(lon_tmp, c("deg_l", "min_l", "sec_l"), sep = "_") %>% 
    
  # Now rebuild the coordinates
    mutate(lat_dd = as.numeric(deg) + (as.numeric(min)/60) + (as.numeric(sec)/3600),
           lon_dd = as.numeric(deg_l) + (as.numeric(min_l)/60) + (as.numeric(sec_l)/3600)) %>% 
    select(-matches("sec|min|deg"))

  
# TODO: Coerce valid Lat/Lon from the original data to numeric  
  
  # Rejoin this to the original data
  geo_coord_df <- 
    geo_df %>% 
    left_join(coord_df, by = c("school_id")) %>% 
    mutate()
    
    
  