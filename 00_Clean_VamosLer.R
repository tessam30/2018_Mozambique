# Purpose: Clean up Vamos Ler! Data for Igor in Mozambique
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2018_07_

# Load libraries and data -------------------------------------------------

# INSTALL LIBRARIES
pacman::p_load("tidyverse", "lubridate", "sf", "extrafont", "readxl")

dir.create("Data")
datapath <- "Data"

ler_data <- "Vamos Ler Intervention schools 2018 _ Contacts_ VL_TMD_FINAL.xlsx"

# Import the Excel data from -- 3 more rows than the excel file, so strip out th emissing
df <- read_excel(file.path(datapath, ler_data), 
                 sheet = "Lista Geral das Escolas")


# Load and clean data -----------------------------------------------------

# The first 4 rows are gibberish and need to be cleaned up, so let's extract the 5:length(df)
  df_data <- df[5:dim(df)[1], ]

# Now need to fix up the first four rows
# Step 1. Identify all columns that have totals -- drop them
tot_rm <- c("X__26", "X__29", "X__32", "X__35", "X__38", "X__41", "X__44", 
            "X__47", "X__48", "X__50", "X__51")
df <- 
  df %>% 
  select(-tot_rm)

# Step 2. Create standardized names for each column that will facilitate a reshape
df %>% 
  rename( school_id = `Levantamento de dados das escolas dos distritos de Zambezia e Nampula`,
          province = "X__1",
          district = "X__@")


# Step 3. 
            
            