# Purpose: Clean up Vamos Ler! Data for Igor in Mozambique
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2018_07_18

# Load libraries and data -------------------------------------------------

# INSTALL LIBRARIES
pacman::p_load("tidyverse", "lubridate", "sf", "extrafont", "readxl", "measurements")

dir.create("Data")
datapath <- "Data"

ler_data <- "Vamos Ler Intervention schools 2018 _ Contacts_ VL_TMD_FINAL.xlsx"

# Import the Excel data from -- Imported two cuts, the header (first four rows) and the full data frame
# By chunking this out into two calls, R will read the numbers correctly rather than making them numbers
# Could also read the whold DF and then use mutate_if to coerce the numbers back.
  df_head <- read_excel(file.path(datapath, ler_data), 
                        sheet = "Lista Geral das Escolas", 
                        n_max = 4)
  
  df <- read_excel(file.path(datapath, ler_data), 
                   sheet = "Lista Geral das Escolas")
  str(df)

# Load and clean data -----------------------------------------------------

# # The first 4 rows are gibberish and need to be cleaned up, so let's extract the 5:length(df)
#   df_data <- df[5:dim(df)[1], ]

# Now need to fix up the first four rows
# Step 1. Identify all columns that have totals -- drop them. The can be recalculated later
  tot_rm <- c("X__26", "X__29", "X__32", "X__35", "X__38", "X__41", "X__44", 
              "X__47", "X__48", "X__49", "X__50", "X__51")
  df <- 
    df %>% 
    select(-tot_rm)

# Step 2. Combine colums with redundant data; For example, X__10 and X__11 
# (Urban / Rural) have redudant data. We can create these 
  df <- 
    df %>% 
    mutate(urban = case_when(
      X__10 == "Sim" ~ "urban",
      X__10 == "Não" ~ "rural",
      TRUE ~ NA_character_))

# Step 2. Create standardized names for each column that will facilitate a reshape
df_mod <- 
  df %>% 
  rename( school_id = `Levantamento de dados das escolas dos distritos de Zambezia e Nampula`,
          province = X__1,
          district = X__2,
          admin_post = X__3,
          zip_name_no = X__4,
          school_name = X__5,
          school_code = X__6,
          language_school = X__7,
          other_ngos = X__8,
          ngo_name = X__9,
          #urban = X__10 -- fixed above,
          bilingual = X__12,
          aea = X__13,
          latitude = X__14,
          longitude = X__15,
          periodtaught_first = X__16,
          periodtaught_second = X__17,
          periodtaught_third = X__18,
          TBD_first = X__19, # Need consistent naming starting here
          TBD_second = X__20,
          TBD_third = X__21,
          TBD_fourth = X__22,
          TBD_fifth = X__23,
          TBD_sixth = X__24,
          TBD_seventh = X__25,
          classsize_first_male    = X__27,
          classsize_first_female  = X__28,
          classsize_second_male   = X__30,
          classsize_second_female = X__31,
          classsize_third_male    = X__33,
          classsize_third_female  = X__34,
          classsize_fourth_male   = X__36,
          classsize_fourth_female = X__37,
          classsize_fifth_male    = X__39,
          classsize_fifth_female  = X__40,
          classsize_sixth_male    = X__42,
          classsize_sixth_female  = X__43,
          classsize_seventh_male  = X__45, 
          classsize_seventh_female= X__46
          ) %>% 
  slice(-(1:4)) %>% 
  select(-"X__10", - "X__11")
  
# Check variable names, column structure and uniqueness of the school_id variable
  df_mod %>% names()
  str(df_mod)
  sum(duplicated(df_mod$school_id)) # if this is anythign different than 0, not unique
  sum(duplicated(df_mod$school_code))


# Step 4. Start gathering, separating to reshape the data long
  # We are anticipating that each row will be multipled by 7 because we are creating 
  # seven levels of the classsize variable, one for each grade 1 - 7.
  # To make the reshaping, separating and spreading clear, we will divide the process into
  # three steps: 1) reshape variables w/ two dimensions, 2) reshape vars w/ 3, then combine the two
  
  tmp <- df_mod %>% 
    select(-starts_with("classsize")) %>% 
    gather(., key = "key", value = "value", periodtaught_first:TBD_seventh) %>% 
    separate(key, c("metric", "grade")) %>% 
    spread(metric, value) 
  
tmp_gend <- df_mod %>% 
    select(starts_with("classsize"), school_id) %>% 
    gather(., key = "key", value = "value", classsize_first_male:classsize_seventh_female) %>% 
    separate(key, c("metric", "grade", "sex")) %>% 
    spread(sex, value) %>% 
    select(-metric) %>% 
    rename(male_classsize = male, female_classsize = female)

# Merge the two dataframes back together and convert two characters to numeric
   tidy_df <- 
     tmp %>% 
     left_join(x = tmp, 
               y = tmp_gend, 
               by = c("school_id", "grade")) %>%
     
     # Convert columns that are truly numbers to numeric, create new variables as well
     mutate_at(vars(school_id, TBD, female_classsize, male_classsize), 
               funs(as.numeric(.))) %>% 
     mutate(grade_classsize = female_classsize + male_classsize,
            grade_mf_ratio = male_classsize / female_classsize) %>% 
     group_by(school_id) %>% 
     mutate(total_classsize = sum(grade_classsize, na.rm = TRUE), 
            total_classsize_female = sum(female_classsize, na.rm = TRUE),
            total_classsize_male = sum(male_classsize, na.rm = TRUE), 
            total_mf_ratio = total_classsize_male/total_classsize_female) %>% 
     ungroup() %>% 
     arrange(school_id)
   
# In browsing the data, there appear to be some grades with 0 students and other grades
   # where the student ratio is not reasonable
   summary(tidy_df)

# Create one more cut where male female classsize is reshaped one more time
   more_tidy_df <- 
     tidy_df %>% 
     gather(key = "student_sex", value = "classize", female_classsize:male_classsize) %>% 
     separate(student_sex, c("sex", "remove_me")) %>% 
     select(-remove_me)


            
            