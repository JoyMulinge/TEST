

# Loading libraries -------------------------------------------------------

library(readr)

library(tidyverse)

library(lubridate)

library(arsenal)

library(flextable)

library(gtsummary)

library(dplyr)

library(tibble)

library(stringr)

library(knitr)

library(janitor)

library(scales)

#install.packages("viridis")

library(RColorBrewer)

library(viridis)

library(ggforce)

#install.packages("ggforce")  # for zoom

library(tidyverse)

library(forcats)  # for fct_reorder

library(pacman)

p_load(dplyr, tidyverse, janitor)

library(MASS)

library(forecast)

library(mgcv)
#install.packages("raster")
#install.packages("sf")
#install.packages("terra")
#install.packages("exactextractr")
library(raster)   # for raster data
library(terra)    # modern replacement for raster
library(sf)       # for spatial points
library(dplyr)
library(exactextractr)
#install.packages("tsibble")
#install.packages("fable")
#install.packages("feasts")
library(tsibble)
library(fable)
library(feasts)






# Importing data ----------------------------------------------------------

Livestock_data <- read.csv("Downloads/Surveillance_data25 (2).csv")





# Exploring the data ------------------------------------------------------

#first 10 rows

head(Livestock_data,10)

#Last 10 rows

tail(Livestock_data,10)

#no.of obs and variables

dim(Livestock_data)

#column names

names(Livestock_data)

#summary statistics

summary(Livestock_data)

#Check for missing values-how many NAs are in each column

colSums(is.na(Livestock_data))

#Explore specific columns

table(Livestock_data$county)

table(Livestock_data$sub_county)

table(Livestock_data$Species_Affected)

#cattle=24056







# Cleaning the data -------------------------------------------------------

#cleaning county names

Livestock_data<- Livestock_data|>
  
  mutate(
    
    county = case_when(
      
      county == "Tharaka Nithi" ~ "Tharaka-Nithi",
      
      county == "Elgeyo Marakwet" ~ "Elgeyo-Marakwet",
      
      TRUE ~ county)
    
  )



#Changing Report date to date

livestock_clean_data <- Livestock_data %>%
  
  mutate(Report_Date = as.Date(Report_Date, format = "%d/%m/%Y"))



str(livestock_clean_data$Report_Date)



## Cleaning disease conditions and categorizing them



clean_disease_data <- function(df, col_name) {
  
  
  
  # 1. Create a standardized text column (lower case, trimmed)
  
  df <- df %>%
    
    mutate(temp_search = str_trim(str_to_lower(!!sym(col_name))))
  
  
  
  # 2. Define the Cleaning Logic (Standardizing specific diseases)
  
  # NOTE: detailed items must come before generic items (e.g., 'african swine fever' before 'fever')
  
  
  
  df <- df %>%
    
    mutate(disease_clean_name = case_when(
      
      # --- INFECTIOUS: VIRAL ---
      
      str_detect(temp_search, "african horse") ~ "African Horse Sickness",
      
      str_detect(temp_search, "african swine") ~ "African Swine Fever",
      
      str_detect(temp_search, "avian influenza|hpai") ~ "Avian Influenza",
      
      str_detect(temp_search, "avian leucosis") ~ "Avian Leucosis",
      
      str_detect(temp_search, "blue tongue") ~ "Blue Tongue",
      
      str_detect(temp_search, "bovine ephemeral|three day") ~ "Bovine Ephemeral Fever",
      
      str_detect(temp_search, "camel pox") ~ "Camel Pox",
      
      str_detect(temp_search, "canine distemper|panleukemia") ~ "Canine Distemper",
      
      str_detect(temp_search, "ecythma|ecthyma|orf|off disease") ~ "Contagious Ecthyma (Orf)",
      
      str_detect(temp_search, "porcine paralysis") ~ "Contagious Porcine Paralysis",
      
      str_detect(temp_search, "equine influenza") ~ "Equine Influenza",
      
      str_detect(temp_search, "fiv|feline immuno") ~ "Feline Immunodeficiency Virus",
      
      str_detect(temp_search, "feline leukemia") ~ "Feline Leukemia Virus",
      
      str_detect(temp_search, "foot and mouth|fmd") ~ "FMD",
      
      str_detect(temp_search, "fowl pox") ~ "Fowl Pox",
      
      str_detect(temp_search, "goat pox") ~ "Goat Pox",
      
      str_detect(temp_search, "gumboro|bursal") ~ "Infectious Bursal Disease",
      
      str_detect(temp_search, "infectious bronchitis") ~ "Infectious Bronchitis",
      
      str_detect(temp_search, "lumpy skin|lsd") ~ "Lumpy Skin Disease",
      
      str_detect(temp_search, "mers") ~ "MERS-CoV",
      
      str_detect(temp_search, "myxomatosis") ~ "Myxomatosis",
      
      str_detect(temp_search, "newcastle|ncd|nd") ~ "Newcastle Disease",
      
      str_detect(temp_search, "papilloma|papillimatosis") ~ "Papillomatosis",
      
      str_detect(temp_search, "parvovirus") ~ "Parvovirus",
      
      str_detect(temp_search, "peste des|ppr") ~ "PPR",
      
      str_detect(temp_search, "rabies") ~ "Rabies",
      
      str_detect(temp_search, "rift valley|rvf") ~ "Rift Valley Fever",
      
      str_detect(temp_search, "sheep pox") ~ "Sheep Pox",
      
      
      
      # --- INFECTIOUS: BACTERIAL ---
      
      str_detect(temp_search, "actinobacillosis") ~ "Actinobacillosis",
      
      str_detect(temp_search, "actinomycosis") ~ "Actinomycosis",
      
      str_detect(temp_search, "american foulbrood") ~ "American Foulbrood",
      
      str_detect(temp_search, "anaplasmosis") ~ "Anaplasmosis",
      
      str_detect(temp_search, "anthrax") ~ "Anthrax",
      
      str_detect(temp_search, "atrophic rhinitis") ~ "Atrophic Rhinitis",
      
      str_detect(temp_search, "black quarter") ~ "Black Quarter",
      
      str_detect(temp_search, "brucellosis") ~ "Brucellosis",
      
      str_detect(temp_search, "coryza|corryza") ~ "Coryza",
      
      str_detect(temp_search, "calf diphtheria") ~ "Calf Diphtheria",
      
      str_detect(temp_search, "caseous") ~ "Caseous Lymphadenitis",
      
      str_detect(temp_search, "cellulitis") ~ "Cellulitis",
      
      str_detect(temp_search, "clostrid") ~ "Clostridia",
      
      str_detect(temp_search, "cbpp|contagious bovine pleuro") ~ "CBPP",
      
      str_detect(temp_search, "ccpp|contagious caprine pleuro") ~ "CCPP",
      
      str_detect(temp_search, "diamond") ~ "Diamond Skin Disease",
      
      str_detect(temp_search, "enterotoxaemia|enterotoximea") ~ "Enterotoxaemia",
      
      str_detect(temp_search, "european foulbrood") ~ "European Foulbrood",
      
      str_detect(temp_search, "foot rot") ~ "Foot Rot",
      
      str_detect(temp_search, "gangrenous") ~ "Gangrenous Disease",
      
      str_detect(temp_search, "haemorrhagic|hemorrhagic|heamorhagic|heamoligic") ~ "Haemorrhagic Septicemia",
      
      str_detect(temp_search, "johnes") ~ "Johnes Disease",
      
      str_detect(temp_search, "leptospirosis") ~ "Leptospirosis",
      
      str_detect(temp_search, "listeriosis") ~ "Listeriosis",
      
      str_detect(temp_search, "lyme") ~ "Lyme Disease",
      
      str_detect(temp_search, "malignant catarrhal") ~ "Malignant Catarrhal Fever",
      
      str_detect(temp_search, "mastitis") ~ "Mastitis",
      
      str_detect(temp_search, "meningitis") ~ "Meningitis",
      
      str_detect(temp_search, "pasteurellosis") ~ "Pasteurellosis",
      
      str_detect(temp_search, "pink eye|keratoconjunctivitis|keratoconjutivitis") ~ "Pink Eye",
      
      str_detect(temp_search, "pullorium") ~ "Pullorum Disease",
      
      str_detect(temp_search, "salmonella") ~ "Salmonellosis",
      
      str_detect(temp_search, "septicemia|septicaemia|scepticaemia") ~ "Septicemia",
      
      str_detect(temp_search, "strepto") ~ "Streptococcosis",
      
      str_detect(temp_search, "swine erysipelas") ~ "Swine Erysipelas",
      
      str_detect(temp_search, "swine dysentery") ~ "Swine Dysentery",
      
      str_detect(temp_search, "tetanus|locked jaw") ~ "Tetanus",
      
      str_detect(temp_search, "tuberculosis|btb") ~ "Tuberculosis",
      
      str_detect(temp_search, "vibriosis") ~ "Vibriosis",
      
      str_detect(temp_search, "wooden tongue") ~ "Wooden Tongue",
      
      
      
      # --- INFECTIOUS: PROTOZOAL ---
      
      str_detect(temp_search, "babesiosis|red urine|redwater") ~ "Babesiosis",
      
      str_detect(temp_search, "coccid") ~ "Coccidiosis",
      
      str_detect(temp_search, "ehrlichiosis") ~ "Ehrlichiosis",
      
      str_detect(temp_search, "giardia") ~ "Giardia",
      
      str_detect(temp_search, "haemonchosis|hemonchosis") ~ "Haemonchosis",
      
      str_detect(temp_search, "heart water|cowdriosis") ~ "Heartwater",
      
      str_detect(temp_search, "theileriosis|east coast fever") ~ "East Coast Fever",
      
      str_detect(temp_search, "trypanosomiasis|tryps|trapnomiasis") ~ "Trypanosomiasis",
      
      
      
      # --- INFECTIOUS: PARASITIC ---
      
      str_detect(temp_search, "bottle jow") ~ "Bottle Jaw",
      
      str_detect(temp_search, "demedicosis") ~ "Demodicosis",
      
      str_detect(temp_search, "helminth|worms") ~ "Helminthiasis",
      
      str_detect(temp_search, "fascioliasis|liver fluke") ~ "Fascioliasis",
      
      str_detect(temp_search, "taeniosis") ~ "Taeniosis",
      
      str_detect(temp_search, "warble") ~ "Warbles",
      
      str_detect(temp_search, "ear canker") ~ "Ear Canker",
      
      str_detect(temp_search, "flea") ~ "Flea Infestation",
      
      str_detect(temp_search, "lice") ~ "Lice Infestation",
      
      str_detect(temp_search, "mange|mnage|scab") ~ "Mange",
      
      str_detect(temp_search, "myiasis|miases|myasis") ~ "Myiasis",
      
      str_detect(temp_search, "nasalbot|oestrus ovis") ~ "Nasal Bot",
      
      str_detect(temp_search, "tick|paralysis") ~ "Tick Infestation",
      
      str_detect(temp_search, "thelezia") ~ "Thelazia",
      
      
      
      # --- INFECTIOUS: FUNGAL ---
      
      str_detect(temp_search, "dermatomycosis") ~ "Dermatomycosis",
      
      str_detect(temp_search, "dermatophytosis") ~ "Dermatophytosis",
      
      str_detect(temp_search, "mycosis") ~ "Mycosis",
      
      str_detect(temp_search, "ringworm") ~ "Ringworm",
      
      str_detect(temp_search, "skin fungal") ~ "Skin Fungal Infection",
      
      
      
      # --- NON-INFECTIOUS: METABOLIC ---
      
      str_detect(temp_search, "avitaminosis|vitamin def") ~ "Vitamin Deficiency",
      
      str_detect(temp_search, "calcium|milk fever|hypocalc") ~ "Calcium Deficiency/Milk Fever",
      
      str_detect(temp_search, "iron") ~ "Iron Deficiency",
      
      str_detect(temp_search, "mineral") ~ "Mineral Deficiency",
      
      str_detect(temp_search, "white muscle") ~ "White Muscle Disease",
      
      str_detect(temp_search, "bloat|ruminal tympany") ~ "Bloat",
      
      str_detect(temp_search, "colic") ~ "Colic",
      
      str_detect(temp_search, "enteritis|rumen acidosis|ruminal acidosis") ~ "Enteritis/Acidosis",
      
      str_detect(temp_search, "grass tetany") ~ "Grass Tetany",
      
      str_detect(temp_search, "hypoglycaemia") ~ "Hypoglycaemia",
      
      str_detect(temp_search, "ketosis") ~ "Ketosis",
      
      str_detect(temp_search, "malnutrition|protein def") ~ "Malnutrition",
      
      str_detect(temp_search, "grain overload") ~ "Grain Overload",
      
      str_detect(temp_search, "water intoxication") ~ "Water Intoxication",
      
      str_detect(temp_search, "poison|aflatoxin|mycotoxicosis|toxin|selenite") ~ "Poisoning/Toxicosis",
      
      
      
      # --- NON-INFECTIOUS: NEOPLASTIC ---
      
      str_detect(temp_search, "tumor|tumour|benign") ~ "Tumor",
      
      str_detect(temp_search, "sarcoid") ~ "Sarcoid",
      
      str_detect(temp_search, "warts|wart infection") ~ "Warts",
      
      str_detect(temp_search, "tvt|venereal tumour") ~ "TVT",
      
      
      
      # --- NON-INFECTIOUS: TRAUMATIC ---
      
      str_detect(temp_search, "abrasion|cut|laceration|wound|skin wound") ~ "Wound/Laceration",
      
      str_detect(temp_search, "bite|attack|predation|snake") ~ "Bite/Predation",
      
      str_detect(temp_search, "abscess") ~ "Abscess",
      
      str_detect(temp_search, "atresia") ~ "Atresia",
      
      str_detect(temp_search, "bee sting|insect bite|stinging") ~ "Insect Sting",
      
      str_detect(temp_search, "broken|fracture") ~ "Fracture",
      
      str_detect(temp_search, "dehorning") ~ "Dehorning",
      
      str_detect(temp_search, "dislocation") ~ "Dislocation",
      
      str_detect(temp_search, "eye injury|traumatic of eye") ~ "Eye Injury",
      
      str_detect(temp_search, "gun shot") ~ "Gunshot Wound",
      
      str_detect(temp_search, "hernia") ~ "Hernia",
      
      str_detect(temp_search, "injury|trauma|bruise|thorn|prick") ~ "General Trauma",
      
      str_detect(temp_search, "lameness|limping") ~ "Lameness",
      
      str_detect(temp_search, "puncture") ~ "Puncture Wound",
      
      str_detect(temp_search, "septic wound") ~ "Septic Wound",
      
      str_detect(temp_search, "sprain") ~ "Sprain",
      
      str_detect(temp_search, "reticuloperitonitis|reticulopericarditis") ~ "Traumatic Reticuloperitonitis",
      
      str_detect(temp_search, "choke") ~ "Choke",
      
      
      
      # --- NON-INFECTIOUS: REPRODUCTIVE ---
      
      str_detect(temp_search, "agalactia") ~ "Agalactia",
      
      str_detect(temp_search, "anestrus|anaestrus") ~ "Anestrus",
      
      str_detect(temp_search, "calving") ~ "Calving Issue",
      
      str_detect(temp_search, "castration") ~ "Castration",
      
      str_detect(temp_search, "downer cow") ~ "Downer Cow Syndrome",
      
      str_detect(temp_search, "dystocia|distocia") ~ "Dystocia",
      
      str_detect(temp_search, "endometritis") ~ "Endometritis",
      
      str_detect(temp_search, "mummification|mummified") ~ "Fetal Mummification",
      
      str_detect(temp_search, "infertility|repeat breeder") ~ "Infertility",
      
      str_detect(temp_search, "locia") ~ "Lochia",
      
      str_detect(temp_search, "metritis") ~ "Metritis",
      
      str_detect(temp_search, "miscarriage|abortion") ~ "Abortion/Miscarriage",
      
      str_detect(temp_search, "parturient|postpartum") ~ "Post-Parturient Condition",
      
      str_detect(temp_search, "placenta|afterbirth") ~ "Retained Placenta",
      
      str_detect(temp_search, "prolapse") ~ "Prolapse",
      
      str_detect(temp_search, "pyometra") ~ "Pyometra",
      
      str_detect(temp_search, "reproductive disorder") ~ "General Reproductive Disorder",
      
      str_detect(temp_search, "uterine") ~ "Uterine Infection/Issue",
      
      str_detect(temp_search, "vaginitis") ~ "Vaginitis",
      
      str_detect(temp_search, "heat synchronization") ~ "Heat Synchronization",
      
      
      
      # --- SYNDROMIC ---
      
      str_detect(temp_search, "camel disease|camel cough") ~ "Camel Respiratory/Syndrome",
      
      str_detect(temp_search, "diarrhea|diarrhoea|diahorrea|scours|dystentry|git") ~ "Diarrhea/GIT",
      
      str_detect(temp_search, "pneumonia|respiratory|shipping fever|cough|pheumonia") ~ "Pneumonia/Respiratory",
      
      str_detect(temp_search, "skin infection|skin nodules") ~ "Skin Infection (Non-Specific)",
      
      
      
      # --- OTHERS/MANAGEMENT/SYMPTOMS ---
      
      str_detect(temp_search, "aggressiveness|loneliness") ~ "Behavioral",
      
      str_detect(temp_search, "ascites") ~ "Ascites",
      
      str_detect(temp_search, "circling|nervous|neurological|wry|torticolis|rolling") ~ "Neurological",
      
      str_detect(temp_search, "cirrhosis") ~ "Liver Cirrhosis",
      
      str_detect(temp_search, "conjunctivitis|eye infection") ~ "Eye Infection",
      
      str_detect(temp_search, "dermatitis|rash|skin deases|necrosis|pruritus") ~ "Dermatitis/Skin",
      
      str_detect(temp_search, "dexamethasone") ~ "Drug Effect",
      
      str_detect(temp_search, "emaciation|poor body|general condition") ~ "Body Condition/General",
      
      str_detect(temp_search, "fever") ~ "Fever (Unspecified)", # Matches only if not matched by specific fevers above
      
      str_detect(temp_search, "management|trimming|horn|routine|check up") ~ "Management",
      
      str_detect(temp_search, "laminitis") ~ "Laminitis",
      
      str_detect(temp_search, "lymph|gland|swelling") ~ "Swelling/Lymph",
      
      str_detect(temp_search, "suspected|not confirmed|unknown") ~ "Unconfirmed/Suspected",
      
      str_detect(temp_search, "stomatitis|glossitis|sores") ~ "Stomatitis",
      
      str_detect(temp_search, "^o$|blank") ~ "Unknown",
      
      
      
      TRUE ~ "Other/Unclassified" # Catch all for anything missed
      
    )) %>%
    
    
    
    # 3. Map Groups based on Clean Name
    
    mutate(disease_group = case_when(
      
      disease_clean_name %in% c("African Horse Sickness", "African Swine Fever", "Avian Influenza", 
                                
                                "Avian Leucosis", "Blue Tongue", "Bovine Ephemeral Fever", "Camel Pox", 
                                
                                "Canine Distemper", "Contagious Ecthyma (Orf)", "Contagious Porcine Paralysis",
                                
                                "Equine Influenza", "Feline Immunodeficiency Virus", "Feline Leukemia Virus",
                                
                                "FMD", "Fowl Pox", "Goat Pox", "Infectious Bursal Disease", "Infectious Bronchitis",
                                
                                "Lumpy Skin Disease", "MERS-CoV", "Myxomatosis", "Newcastle Disease", 
                                
                                "Papillomatosis", "Parvovirus", "PPR", "Rabies", "Rift Valley Fever", "Sheep Pox") ~ "Viral",
      
      
      
      disease_clean_name %in% c("Actinobacillosis", "Actinomycosis", "American Foulbrood", "Anaplasmosis", 
                                
                                "Anthrax", "Atrophic Rhinitis", "Black Quarter", "Brucellosis", "Coryza", 
                                
                                "Calf Diphtheria", "Caseous Lymphadenitis", "Cellulitis", "Clostridia", 
                                
                                "CBPP", "CCPP", "Diamond Skin Disease", "Enterotoxaemia", "European Foulbrood", 
                                
                                "Foot Rot", "Gangrenous Disease", "Haemorrhagic Septicemia", "Johnes Disease", 
                                
                                "Leptospirosis", "Listeriosis", "Lyme Disease", "Malignant Catarrhal Fever", 
                                
                                "Mastitis", "Meningitis", "Pasteurellosis", "Pink Eye", "Pullorum Disease", 
                                
                                "Salmonellosis", "Septicemia", "Streptococcosis", "Swine Erysipelas", 
                                
                                "Swine Dysentery", "Tetanus", "Tuberculosis", "Vibriosis", "Wooden Tongue") ~ "Bacterial",
      
      
      
      disease_clean_name %in% c("Babesiosis", "Coccidiosis", "Ehrlichiosis", "Giardia", "Haemonchosis", 
                                
                                "Heartwater", "East Coast Fever", "Trypanosomiasis") ~ "Protozoal",
      
      
      
      disease_clean_name %in% c("Bottle Jaw", "Demodicosis", "Helminthiasis", "Fascioliasis", "Taeniosis", 
                                
                                "Warbles", "Ear Canker", "Flea Infestation", "Lice Infestation", "Mange", 
                                
                                "Myiasis", "Nasal Bot", "Tick Infestation", "Thelazia") ~ "Parasitic",
      
      
      
      disease_clean_name %in% c("Dermatomycosis", "Dermatophytosis", "Mycosis", "Ringworm", "Skin Fungal Infection") ~ "Fungal",
      
      
      
      disease_clean_name %in% c("Vitamin Deficiency", "Calcium Deficiency/Milk Fever", "Iron Deficiency", 
                                
                                "Mineral Deficiency", "White Muscle Disease", "Bloat", "Colic", 
                                
                                "Enteritis/Acidosis", "Grass Tetany", "Hypoglycaemia", "Ketosis", 
                                
                                "Malnutrition", "Grain Overload", "Water Intoxication", "Poisoning/Toxicosis") ~ "Metabolic/Nutritional",
      
      
      
      disease_clean_name %in% c("Tumor", "Sarcoid", "Warts", "TVT") ~ "Neoplastic",
      
      
      
      disease_clean_name %in% c("Wound/Laceration", "Bite/Predation", "Abscess", "Atresia", "Insect Sting", 
                                
                                "Fracture", "Dehorning", "Dislocation", "Eye Injury", "Gunshot Wound", 
                                
                                "Hernia", "General Trauma", "Lameness", "Puncture Wound", "Septic Wound", 
                                
                                "Sprain", "Traumatic Reticuloperitonitis", "Choke") ~ "Traumatic/Physical",
      
      
      
      disease_clean_name %in% c("Agalactia", "Anestrus", "Calving Issue", "Castration", "Downer Cow Syndrome", 
                                
                                "Dystocia", "Endometritis", "Fetal Mummification", "Infertility", "Lochia", 
                                
                                "Metritis", "Abortion/Miscarriage", "Post-Parturient Condition", 
                                
                                "Retained Placenta", "Prolapse", "Pyometra", "General Reproductive Disorder", 
                                
                                "Uterine Infection/Issue", "Vaginitis", "Heat Synchronization") ~ "Reproductive",
      
      
      
      disease_clean_name %in% c("Camel Respiratory/Syndrome", "Diarrhea/GIT", "Pneumonia/Respiratory", 
                                
                                "Skin Infection (Non-Specific)") ~ "Syndromic",
      
      
      
      TRUE ~ "Other"
      
    )) %>%
    
    
    
    # 4. Map Category based on Group
    
    mutate(disease_category = case_when(
      
      disease_group %in% c("Viral", "Bacterial", "Protozoal", "Parasitic", "Fungal") ~ "Infectious",
      
      disease_group %in% c("Metabolic/Nutritional", "Neoplastic", "Traumatic/Physical", "Reproductive") ~ "Non-Infectious",
      
      disease_group == "Syndromic" ~ "Syndromic",
      
      TRUE ~ "Others"
      
    )) %>%
    
    
    
    # Remove temp column
    
    dplyr::select(-temp_search)
  
  
  
  return(df)
  
}





# livestock_clean_data ----------------------------------------------------

livestock_clean_data <- clean_disease_data(livestock_clean_data, "Disease_Condition")



# Continue cleaning -------------------------------------------------------

#Putting disease names in full

livestock_clean_data <- livestock_clean_data %>%
  
  mutate(disease_clean_name = recode(disease_clean_name,
                                     
                                     "LSD" = "Lumpy Skin Disease",
                                     
                                     "Goat pox" = "Goat Pox",
                                     
                                     "CCPP" = "Contagious Caprine Pleuropneumonia",
                                     
                                     "PPR" = "Peste des Petits Ruminants",
                                     
                                     "FMD" = "Foot and Mouth Disease"))



#Categorizing others

livestock_clean_data <- livestock_clean_data %>%
  
  mutate(
    
    disease_category = case_when(
      
      
      
      # Keep everything that is NOT Others unchanged
      
      disease_category != "Others" ~ disease_category,
      
      
      
      # --- NON-DISEASE ENTRIES ---
      
      disease_clean_name %in% c(
        
        "Management",
        
        "Drug Effect",
        
        "Behavioral",
        
        "Body Condition/General"
        
      ) ~ "Non-Disease",
      
      
      
      # --- NON-SPECIFIC CLINICAL CONDITIONS ---
      
      disease_clean_name %in% c(
        
        "Ascites",
        
        "Dermatitis/Skin",
        
        "Eye Infection",
        
        "Fever (Unspecified)",
        
        "Neurological",
        
        "Laminitis",
        
        "Liver Cirrhosis"
        
      ) ~ "Non-Specific Clinical Conditions",
      
      
      
      # --- EVERYTHING ELSE stays as Others ---
      
      TRUE ~ "Others"
      
    )
    
  )



livestock_clean_data <- livestock_clean_data %>%
  
  mutate(
    
    disease_category = case_when(
      
      
      
      # Leave existing correct categories unchanged
      
      disease_category != "Others" ~ disease_category,
      
      
      
      # Non-Disease
      
      disease_clean_name %in% c("Management","Drug Effect","Behavioral","Body Condition/General") ~ "Non-Disease",
      
      
      
      # Non-Specific Clinical Conditions
      
      disease_clean_name %in% c("Ascites","Dermatitis/Skin","Eye Infection",
                                
                                "Fever (Unspecified)","Neurological","Laminitis","Liver Cirrhosis") ~ "Non-Specific Clinical Conditions",
      
      
      
      # Missed infectious diseases
      
      disease_clean_name %in% c("Salmonellosis","Foot Rot","Johnes Disease","Mastitis") ~ "Infectious",
      
      
      
      # Everything else stays as Others
      
      TRUE ~ "Others"
      
    )
    
  )









# Exploring the clean data ------------------------------------------------
# Create year variable
livestock_clean_data <- livestock_clean_data %>%
  mutate(year = format(Report_Date, "%Y"))
#Range of report time

range(livestock_clean_data$year, na.rm = TRUE)

# Count number of reports per year

reports_per_year <- table(livestock_clean_data$year)

reports_per_year

#Counts of reports by disease category

# Frequency of species affected

table(livestock_clean_data$Species_Affected)

# Frequency of disease group

table(livestock_clean_data$disease_group)

# Frequency of disease category

table(livestock_clean_data$disease_category)







# Descriptive Statistics --------------------------------------------------



# Which species are most affected? ----------------------------------------

# 1. Aggregate data FIRST to prevent multiple labels per bar

risk_summary <- livestock_clean_data %>%
  
  group_by(Species_Affected) %>%
  
  summarise(Number_at_Risk = sum(Number_at_Risk, na.rm = TRUE)) %>%
  
  ungroup()



total_at_risk <- sum(risk_summary$Number_at_Risk)

total_label <- scales::comma(total_at_risk)



# 2. Calculate percentages on the aggregated data

risk_data_pct <- risk_summary %>%
  
  mutate(
    
    Percentage = Number_at_Risk / total_at_risk,
    
    Pct_Label = scales::percent(Percentage, accuracy = 0.1)
    
  )



# 3. Create the clean plot

p_risk_pct <- ggplot(risk_data_pct, 
                     
                     aes(x = Percentage, 
                         
                         y = reorder(Species_Affected, Percentage))) +
  
  geom_col(fill = "#2c3e50", width = 0.7) +
  
  
  
  # Put labels slightly PAST the bar (hjust = -0.2)
  
  geom_text(
    
    aes(label = Pct_Label),
    
    hjust = -0.2, 
    
    size = 4.5, 
    
    fontface = "bold",
    
    color = "black" # Ensures visibility against the white background
    
  ) +
  
  
  
  # Set scale to 100% (limits = c(0, 1))
  
  scale_x_continuous(
    
    labels = scales::percent_format(),
    
    limits = c(0, 1),              # This forces the scale to 100%
    
    breaks = seq(0, 1, by = 0.1),  # Adds markers every 10%
    
    expand = expansion(mult = c(0, 0.05)) 
    
  ) +
  
  
  
  labs(
    
    title = "Percentage of Population at Risk by Species",
    
    subtitle = paste0("Total Animals at Risk: ", total_label),
    
    x = "Percentage of Total Animals at Risk",
    
    y = "Species Affected"
    
  ) +
  
  
  
  theme_minimal(base_size = 14) +
  
  theme(
    
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    
    plot.subtitle = element_text(face = "bold", size = 14, hjust = 0.5, color = "grey30"),
    
    axis.title = element_text(face = "bold", size = 16),
    
    axis.text = element_text(face = "bold", color = "black"),
    
    panel.grid.major.y = element_blank(),
    
    panel.grid.minor = element_blank()
    
  )



p_risk_pct



# Calculate percentages per species
percentage_per_species <- livestock_clean_data %>%
  
  group_by(Species_Affected) %>%
  
  mutate(
    Total_per_Species = sum(Number_Sick, na.rm = TRUE),
    Percent = Number_Sick / Total_per_Species * 100
  ) %>%
  
  ungroup() %>%
  
  arrange(desc(Total_per_Species))

# Create species summary
species_summary <- livestock_clean_data %>%
  
  group_by(Species_Affected) %>%
  
  summarise(
    
    No_Sick = sum(Number_Sick, na.rm = TRUE),
    
    No_Dead = sum(Number_Dead, na.rm = TRUE)
    
  ) %>%
  
  arrange(desc(No_Sick))


#Create species outcome_long
species_outcomes_long <- species_summary %>%
  
  pivot_longer(
    
    cols = c(No_Sick, No_Dead),
    
    names_to = "Outcome",
    
    values_to = "Count"
    
  ) %>%
  
  mutate(
    
    Outcome = recode(
      
      Outcome,
      
      No_Sick = "Sick",
      
      No_Dead = "Dead"
      
    )
    
  )


#Plot 2-Disease outcomes

p2 <- ggplot(species_outcomes_long,
             aes(x = reorder(Species_Affected, Count, sum),
                 y = Count,
                 fill = Outcome)) +
  
  geom_col(width = 0.75) +
  
  coord_flip() +
  
  scale_y_continuous(labels = scales::comma) +
  
  labs(
    title = "Disease Outcomes by Species",
    x = "Species",
    y = "Number of Animals",
    fill = "Outcome"
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 16)
  )

p2

p_risk_pct|p2





# Question one ------------------------------------------------------------

#QUESTION ONE 

#Which species are most affected by disease across counties?

species_by_county <- livestock_clean_data %>%
  
  group_by(county, Species_Affected) %>%
  
  summarise(
    
    total_sick = sum(Number_Sick, na.rm = TRUE),
    
    total_at_risk = sum(Number_at_Risk, na.rm = TRUE)
    
  ) %>%
  
  mutate(
    
    percent_sick = round((total_sick / total_at_risk) * 100, 2)
    
  ) %>%
  
  arrange(county, desc(percent_sick)) %>%
  
  ungroup()



species_by_county

View(species_by_county)



#top 5 species per county

species_by_county %>%
  
  group_by(county) %>%
  
  slice_max(percent_sick, n = 5)



#Visualize

library(ggplot2)



ggplot(species_by_county, aes(x = reorder(Species_Affected, -percent_sick), y = percent_sick, fill = county)) +
  
  geom_bar(stat = "identity", position = "dodge") +
  
  labs(title = "Top Species Affected by Disease per County", y = "% Sick", x = "Species") +
  
  theme_minimal() +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



species_overall <- species_by_county %>%
  
  group_by(Species_Affected) %>%
  
  summarise(
    
    mean_percent_sick = mean(percent_sick, na.rm = TRUE)
    
  ) %>%
  
  arrange(desc(mean_percent_sick))



ggplot(species_overall,
       
       aes(x = reorder(Species_Affected, mean_percent_sick),
           
           y = mean_percent_sick)) +
  
  geom_col() +
  
  coord_flip() +
  
  labs(
    
    x = "Species Affected",
    
    y = "Percentage Sick",
    
    title = "Percentage of Sick Animals by Species"
    
  ) +
  
  theme_minimal(base_size = 14) +
  
  
  
  theme_minimal()





#plot

livestock_clean_data <- livestock_clean_data %>%
  
  mutate(
    
    Number_Sick = as.numeric(Number_Sick),
    
    Number_at_Risk = as.numeric(Number_at_Risk),
    
    percent_sick = (Number_Sick / Number_at_Risk) * 100
    
  ) %>%
  
  filter(Number_at_Risk > 10)





ggplot(livestock_clean_data,
       
       aes(
         
         y = county,
         
         fill = Species_Affected
         
       )) +
  
  geom_bar(position = "fill") +
  
  scale_x_continuous(labels = scales::percent) +
  
  labs(
    
    title = "Proportion of Species Affected within Each County",
    
    x = "Proportion",
    
    y = "County",
    
    fill = "Species Affected"
    
  ) +
  
  theme(
    
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    
    plot.subtitle = element_text(face = "bold", size = 14, hjust = 0.5, color = "grey30"),
    
    axis.title = element_text(face = "bold", size = 16),
    
    axis.text = element_text(face = "bold", color = "black"),
    
    panel.grid.major.y = element_blank(),
    
    panel.grid.minor = element_blank()
    
  )

theme_minimal()





# #QUESTION 2 #WHAT IS THE DISTRIBUTION OF DISEASE TYPES (Disease_ --------

# ----- Disease Condition -----

disease_counts <- livestock_clean_data %>%
  
  group_by(Disease_Condition) %>%
  
  summarise(count = n()) %>%
  
  arrange(desc(count)) %>%
  
  mutate(percent = paste0(round((count / sum(count)) * 100, 2), "%"))



# ----- Disease Group -----

group_counts <- livestock_clean_data %>%
  
  group_by(disease_group) %>%
  
  summarise(count = n()) %>%
  
  arrange(desc(count)) %>%
  
  mutate(percent = paste0(round((count / sum(count)) * 100, 2), "%"))



# ----- Disease Category -----

category_counts <- livestock_clean_data %>%
  
  group_by(disease_category) %>%
  
  summarise(count = n()) %>%
  
  arrange(desc(count)) %>%
  
  mutate(percent = paste0(round((count / sum(count)) * 100, 2), "%"))





# ----- Disease Condition -----

ft_disease <- flextable(disease_counts) %>%
  
  colformat_num(j = "percent", digits = 2) %>%  # ensure 2 decimal places
  
  set_header_labels(count = "Count", percent = "Percent (%)") %>%
  
  autofit() %>%
  
  theme_box()  # makes it look professional



# ----- Disease Group -----

ft_group <- flextable(group_counts) %>%
  
  colformat_num(j = "percent", digits = 2) %>%
  
  set_header_labels(count = "Count", percent = "Percent (%)") %>%
  
  autofit() %>%
  
  theme_box()



# ----- Disease Category -----

ft_category <- flextable(category_counts) %>%
  
  colformat_num(j = "percent", digits = 2) %>%
  
  set_header_labels(count = "Count", percent = "Percent (%)") %>%
  
  autofit() %>%
  
  theme_box()



#top 10 disease conditions

# Summarize disease counts with percentages rounded

disease_counts <- livestock_clean_data %>%
  
  group_by(Disease_Condition) %>%
  
  summarise(count = n()) %>%
  
  arrange(desc(count)) %>%
  
  mutate(percent = paste0(round((count / sum(count)) * 100, 2), "%"))





# Keep only the top 10 diseases

top10_diseases <- head(disease_counts, 10)



flextable(top10_diseases)



#Plot top 10 disease conditions

top10_names <- disease_counts %>%
  
  slice_head(n = 10) %>%
  
  pull(Disease_Condition)



# Filter original data to top 10 diseases

stacked_data <- livestock_clean_data %>%
  
  filter(Disease_Condition %in% top10_names) %>%
  
  group_by(Disease_Condition, Species_Affected) %>%
  
  summarise(count = n(), .groups = "drop")



#Stacked bar plot

ggplot(stacked_data,
       
       aes(x = reorder(Disease_Condition, count),
           
           y = count,
           
           fill = Species_Affected)) +
  
  geom_col() +
  
  coord_flip() +
  
  labs(
    
    title = "Top 10 Disease Conditions by Species Affected",
    
    x = "Disease Condition",
    
    y = "Number of Reports",
    
    fill = "Species Affected"
    
  ) +
  
  scale_fill_brewer(palette = "Set3") +
  
  theme_minimal()



#Disease categories by species affected

# 1. Summarize counts by disease_category and species

plot_data <- livestock_clean_data %>%
  
  group_by(disease_category, Species_Affected) %>%
  
  summarise(count = n(), .groups = "drop")



# 2. Order categories by total count (most frequent at top)

plot_data <- plot_data %>%
  
  group_by(disease_category) %>%
  
  mutate(total_count = sum(count)) %>%
  
  ungroup() %>%
  
  mutate(disease_category = fct_reorder(disease_category, total_count, .desc = FALSE))



# 3. Species color palette (adjust to match your species)

species_palette <- c(
  
  "Camel"   = "#66C2A5",  # Teal
  
  "Cattle"  = "#FFFFB3",  # Yellow/Cream  
  
  "Chicken" = "#BEBADA",  # Lavender
  
  "Dogs"    = "#FB8072",  # Red/Coral
  
  "Donkey"  = "#80B1D3",  # Blue
  
  "Goats"   = "#FDB462",  # Orange
  
  "Other"   = "#B3DE69",  # Green
  
  "Pigs"    = "#FCCDE5",  # Pink
  
  "Sheep"   = "#D9D9D9",  # Grey
  
  "Bees"    = "#E5D8BD",  # Tan
  
  "Poultry" = "#FDDAEC"   # Light pink
  
)



# 4. Plot horizontal bars

ggplot(plot_data, 
       
       aes(x = count, 
           
           y = disease_category,
           
           fill = Species_Affected)) +
  
  geom_bar(stat = "identity", 
           
           color = "white",        # White borders between segments
           
           linewidth = 0.4) +
  
  scale_fill_manual(values = species_palette) +
  
  scale_x_continuous(expand = c(0, 0),
                     
                     breaks = scales::pretty_breaks(n = 6),
                     
                     labels = scales::comma) +
  
  labs(
    
    title = "Disease Categories by Species Affected",
    
    subtitle = "Now showing Non-Disease and Non-Specific Clinical Conditions clearly",
    
    x = "Number of Reports",
    
    y = "Disease Category",
    
    fill = "Species Affected"
    
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    
    plot.title = element_text(, size= 18, face = "bold", hjust = 0.5),
    
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray40"),
    
    axis.title = element_text(face = "bold", size = 11),
    
    axis.text.y = element_text(size = 10, face = "bold"),  # Bold category names
    
    axis.text.x = element_text(size = 10),
    
    legend.position = "right",
    
    legend.title = element_text(face = "bold", size = 10),
    
    panel.grid.major.y = element_blank(),    # Remove horizontal grid lines
    
    panel.grid.minor = element_blank(),
    
    panel.border = element_blank()
    
  )





# Question 3 --------------------------------------------------------------



# #Which counties,sub-counties,wards have the highest number of sick --------

#Create a disease burden variable

livestock_clean_data <- livestock_clean_data %>%
  
  mutate(disease_burden = Number_Sick + Number_Dead)



#Hotspots by COUNTY

#Aggregate and rank counties

#Table

#County percentages

county_hotspots <- livestock_clean_data %>%
  
  mutate(disease_burden = Number_Sick + Number_Dead) %>%
  
  group_by(county) %>%
  
  summarise(
    
    total_sick = sum(Number_Sick, na.rm = TRUE),
    
    total_dead = sum(Number_Dead, na.rm = TRUE),
    
    total_burden = sum(disease_burden, na.rm = TRUE)
    
  ) %>%
  
  ungroup() %>%
  
  mutate(
    
    percent_burden = round((total_burden / sum(total_burden)) * 100, 2),
    
    percent_burden_label = paste0(percent_burden, "%")
    
  ) %>%
  
  arrange(desc(total_burden))



#Table

county_ft <- county_hotspots %>%
  
  dplyr::select(
    
    County = county,
    
    Total_Sick = total_sick,
    
    Total_Dead = total_dead,
    
    Total_Burden = total_burden,
    
    `% of Total Burden` = percent_burden_label
    
  ) %>%
  
  flextable() %>%
  
  theme_vanilla() %>%
  
  autofit() %>%
  
  bold(part = "header") %>%
  
  flextable::align(align = "center", part = "all") %>%
  
  set_caption("County-level Livestock Disease Hotspots")



county_ft

#Plot

# Prepare data for plotting: gather sick and dead into one column

plot_data <- county_hotspots %>%
  
  dplyr::select(county, total_sick, total_dead) %>%
  
  pivot_longer(
    
    cols = c(total_sick, total_dead),
    
    names_to = "Outcome",
    
    values_to = "Count"
    
  ) %>%
  
  mutate(
    
    Outcome = ifelse(Outcome == "total_sick", "Sick", "Dead")
    
  ) %>%
  
  group_by(county) %>%
  
  mutate(
    
    Percent = Count / sum(Count) * 100
    
  ) %>%
  
  ungroup()



# Stacked bar plot: percentages

ggplot(plot_data, aes(x = Percent, y = reorder(county, Percent), fill = Outcome)) +
  
  geom_col() +
  
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  
  labs(
    
    title = "Percentage of Sick and Dead Animals by County",
    
    x = "Percentage of Animals",
    
    y = "County",
    
    fill = "Outcome"
    
  ) +
  
  theme_minimal(base_size = 14)



## Calculate percentages per county

county_percentages <- livestock_clean_data %>%
  group_by(county) %>%
  summarise(
    Total_Sick = sum(Number_Sick, na.rm = TRUE),
    Total_Dead = sum(Number_Dead, na.rm = TRUE)
  ) %>%
  mutate(
    Total_Burden = Total_Sick + Total_Dead,
    Percent_Sick = round((Total_Sick / Total_Burden) * 100, 2),
    Percent_Dead = round((Total_Dead / Total_Burden) * 100, 2)
  ) %>%
  arrange(desc(Total_Burden))



# Create flextable

ft_county_percent <- flextable(county_percentages) %>%
  
  set_header_labels(
    
    county = "County",
    
    Total_Sick = "Total Sick",
    
    Total_Dead = "Total Dead",
    
    Total_Burden = "Total Affected",
    
    Percent_Sick = "% Sick",
    
    Percent_Dead = "% Dead"
    
  ) %>%
  
  theme_vanilla() %>%
  
  autofit() %>%
  
  bold(part = "header") %>%
  
  flextable::align(j = c("county","Total_Sick","Total_Dead","Total_Burden","Percent_Sick","Percent_Dead"),
                   align = "center",
                   part = "all") %>%
  
  set_caption("Percentage of Sick and Dead Animals per County")



ft_county_percent







#Identify top 10 counties by number of sick animals

top10_sick_counties <- livestock_clean_data %>%
  
  group_by(county) %>%
  
  summarise(total_sick = sum(Number_Sick, na.rm = TRUE)) %>%
  
  arrange(desc(total_sick)) %>%
  
  slice(1:10)



#Prepare data by species for those counties

plot_data <- livestock_clean_data %>%
  
  filter(county %in% top10_sick_counties$county) %>%
  
  group_by(county, Species_Affected) %>%
  
  summarise(
    
    sick = sum(Number_Sick, na.rm = TRUE),
    
    .groups = "drop"
    
  )



#Stacked bar plot (county × species)

ggplot(plot_data,
       
       aes(x = reorder(county, sick, sum),
           
           y = sick,
           
           fill = Species_Affected)) +
  
  geom_col(width = 0.75) +
  
  coord_flip() +
  
  scale_y_continuous(labels = comma) +
  
  labs(
    
    title = "Top 10 Counties by Number of Sick Animals",
    
    subtitle = "Stacked by Species Affected",
    
    x = "County",
    
    y = "Number of Sick Animals",
    
    fill = "Species Affected"
    
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    
    axis.text.y = element_text(face = "bold"),
    
    legend.position = "right"
    
  )



#Top 10 counties by number of dead animals

top10_dead_counties <- livestock_clean_data %>%
  
  group_by(county) %>%
  
  summarise(total_dead = sum(Number_Dead, na.rm = TRUE)) %>%
  
  arrange(desc(total_dead)) %>%
  
  slice(1:10)



#Prepare data by species

plot_data_dead <- livestock_clean_data %>%
  
  filter(county %in% top10_dead_counties$county) %>%
  
  group_by(county, Species_Affected) %>%
  
  summarise(
    
    dead = sum(Number_Dead, na.rm = TRUE),
    
    .groups = "drop"
    
  )



#Scatter plot by county and species

ggplot(plot_data_dead,
       
       aes(x = reorder(county, dead, sum),
           
           y = dead,
           
           fill = Species_Affected)) +
  
  geom_col(width = 0.75) +
  
  coord_flip() +
  
  scale_y_continuous(labels = comma) +
  
  labs(
    
    title = "Top 10 Counties by Number of Dead Animals",
    
    subtitle = "Stacked by Species Affected",
    
    x = "County",
    
    y = "Number of Dead Animals",
    
    fill = "Species Affected"
    
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    
    axis.text.y = element_text(face = "bold"),
    
    legend.position = "right"
    
  )



#Compute TOTAL disease burden (global denominator)

total_burden_all <- livestock_clean_data %>%
  
  summarise(
    
    total_burden = sum(Number_Sick + Number_Dead, na.rm = TRUE)
    
  ) %>%
  
  pull(total_burden)



#Sub-county burden + global percentage

subcounty_burden <- livestock_clean_data %>%
  
  group_by(county, sub_county) %>%
  
  summarise(
    
    disease_burden = sum(Number_Sick + Number_Dead, na.rm = TRUE),
    
    .groups = "drop"
    
  ) %>%
  
  mutate(
    
    percent_global = disease_burden / total_burden_all * 100,
    
    label = paste0(sub_county, " (", county, ")")
    
  )



# Select top 15 sub-counties

top15_subcounties <- subcounty_burden %>%
  
  arrange(desc(disease_burden)) %>%
  
  slice(1:15)



#total burden in the country

# Total sick + dead animals across all counties / sub-counties

# Total sick + dead across all sub-counties

total_animals <- sum(livestock_clean_data$Number_Sick + livestock_clean_data$Number_Dead, na.rm = TRUE)





# Plot: Sub-counties

ggplot(top15_subcounties,
       
       aes(x = reorder(label, disease_burden),
           
           y = disease_burden)) +
  
  geom_col(fill = "steelblue", width = 0.75) +
  
  geom_text(
    
    aes(label = paste0(round(percent_global, 1), "%")),
    
    hjust = -0.1,
    
    size = 3.6
    
  ) +
  
  coord_flip() +
  
  scale_y_continuous(
    
    labels = scales::comma,
    
    expand = expansion(mult = c(0, 0.15))
    
  ) +
  
  labs(
    
    title = "Top 15 Sub-Counties by Disease Burden",
    
    subtitle = paste("Total animals affected:", scales::comma(total_animals)),
    
    x = "Sub-County (County)",
    
    y = "Number of Animals"
    
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(axis.text.y = element_text(face = "bold"))





#PLOT 2: Top 15 Wards by Disease Burden



#(Percent of all counties combined)



# Ward burden + global percentage

ward_burden <- livestock_clean_data %>%
  
  group_by(county, sub_county, Ward) %>%
  
  summarise(
    
    disease_burden = sum(Number_Sick + Number_Dead, na.rm = TRUE),
    
    .groups = "drop"
    
  ) %>%
  
  mutate(
    
    percent_global = disease_burden / total_burden_all * 100,
    
    label = paste0(Ward, " (", sub_county, ", ", county, ")")
    
  )



# Select top 15 wards

top15_wards <- ward_burden %>%
  
  arrange(desc(disease_burden)) %>%
  
  slice(1:15)



## Total sick + dead across all wards

total_animals <- sum(livestock_clean_data$Number_Sick + livestock_clean_data$Number_Dead, na.rm = TRUE)



# Plot: Wards

ggplot(top15_wards,
       
       aes(x = reorder(label, disease_burden),
           
           y = disease_burden)) +
  
  geom_col(fill = "darkred", width = 0.75) +
  
  geom_text(
    
    aes(label = paste0(round(percent_global, 1), "%")),
    
    hjust = -0.1,
    
    size = 3.6
    
  ) +
  
  coord_flip() +
  
  scale_y_continuous(
    
    labels = scales::comma,
    
    expand = expansion(mult = c(0, 0.15))
    
  ) +
  
  labs(
    
    title = "Top 15 Wards by Disease Burden",
    
    subtitle = paste("Total animals affected:", scales::comma(total_animals)),
    
    x = "Ward (Sub-County, County)",
    
    y = "Number of Animals"
    
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(axis.text.y = element_text(face = "bold"))





# # Question 4:Trend of Disease Cases Over time ------------------ --------

#Create a Year–Month variable.

livestock_time <- livestock_clean_data %>%
  
  mutate(
    
    Report_Date = as.Date(Report_Date),
    
    year = year(Report_Date),
    
    month = month(Report_Date, label = TRUE),
    
    year_month = floor_date(Report_Date, "month")
    
  )



#Aggregate total sick animals per month

monthly_trend <- livestock_time %>%
  
  group_by(year_month) %>%
  
  summarise(
    
    total_sick = sum(Number_Sick, na.rm = TRUE),
    
    .groups = "drop"
    
  )

#plot

ggplot(monthly_trend, aes(x = year_month, y = total_sick)) +
  
  geom_line(linewidth = 1) +
  
  geom_point() +
  
  scale_x_date(
    
    date_breaks = "3 months",
    
    date_labels = "%b %Y"
    
  ) +
  
  scale_y_continuous(labels = comma) +
  
  labs(
    
    title = "Trend of Livestock Disease Cases Over Time",
    
    x = "Time (Month–Year)",
    
    y = "Number of Sick Animals"
    
  ) +
  
  theme_minimal() +
  
  theme(
    
    axis.text.x = element_text(angle = 45, hjust = 1)
    
  )





#Seasonality

#Aggregate by month (across all years)

seasonality <- livestock_time %>%
  
  group_by(month) %>%
  
  summarise(
    
    avg_cases = mean(Number_Sick, na.rm = TRUE),
    
    .groups = "drop"
    
  )



#Plot seasonal pattern

ggplot(seasonality, aes(x = month, y = avg_cases, fill = month)) +
  
  geom_col(show.legend = FALSE) +
  
  labs(
    
    title = "Seasonal Pattern of Livestock Disease Cases",
    
    x = "Month",
    
    y = "Average Number of Sick Animals"
    
  ) +
  
  theme_minimal()



#Using median

seasonality_median <- livestock_time %>%
  
  group_by(month) %>%
  
  summarise(
    
    median_cases = median(Number_Sick, na.rm = TRUE),
    
    .groups = "drop"
    
  )



seasonality_monthly <- livestock_time %>%
  
  group_by(year, month) %>%
  
  summarise(
    
    monthly_total = sum(Number_Sick, na.rm = TRUE),
    
    .groups = "drop"
    
  ) %>%
  
  group_by(month) %>%
  
  summarise(
    
    avg_monthly_cases = mean(monthly_total),
    
    .groups = "drop"
    
  )



seasonality_no_outbreak <- livestock_time %>%
  
  filter(Number_Sick < quantile(Number_Sick, 0.99, na.rm = TRUE)) %>%
  
  group_by(month) %>%
  
  summarise(
    
    avg_cases = mean(Number_Sick, na.rm = TRUE)
    
  )



ggplot(seasonality_median,
       
       aes(x = month, y = median_cases, fill = month)) +
  
  geom_col(color = "black") +
  
  labs(
    
    title = "Seasonal Pattern of Livestock Disease Cases (Median)",
    
    x = "Month",
    
    y = "Median Number of Sick Animals"
    
  ) +
  
  theme_minimal()



##by disease category

# Aggregate data by year and disease category

yearly_by_category <- livestock_time %>%
  
  group_by(year, disease_category) %>%
  
  summarise(
    
    total_sick = sum(Number_Sick, na.rm = TRUE),
    
    .groups = "drop"
    
  )



# Plot

# Stacked horizontal bar chart

ggplot(yearly_by_category, aes(x = factor(year), y = total_sick, fill = disease_category)) +
  
  geom_col(color = "black", width = 0.7) +  # black border for clarity
  
  scale_y_continuous(labels = scales::comma) +
  
  labs(
    
    title = "Annual Livestock Disease Cases by Disease Category",
    
    x = "Year",
    
    y = "Number of Sick Animals",
    
    fill = "Disease Category"
    
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    
    legend.position = "bottom",
    
    legend.title = element_text(face = "bold")
    
  ) +
  
  coord_flip()  # makes it horizontal



# Ensure Report_Date is Date format and remove NA

livestock_clean_data <- livestock_clean_data %>%
  
  mutate(Report_Date = as.Date(Report_Date, format = "%Y-%m-%d")) %>%
  
  filter(!is.na(Report_Date) & !is.na(Number_Dead))



# Create year and quarter

livestock_clean_data <- livestock_clean_data %>%
  
  mutate(year = year(Report_Date),
         
         quarter = quarter(Report_Date))



# Summarize total deaths by year and quarter

quarterly_deaths <- livestock_clean_data %>%
  
  group_by(year, quarter) %>%
  
  summarise(total_deaths = sum(Number_Dead, na.rm = TRUE), .groups = "drop")



# Plot quarterly trend

ggplot(quarterly_deaths, aes(x = factor(quarter), y = total_deaths, group = year, color = factor(year))) +
  
  geom_line(size = 1.2) +
  
  geom_point(size = 3) +
  
  labs(title = "Quarterly Trend of Animal Deaths",
       
       x = "Quarter",
       
       y = "Total Deaths",
       
       color = "Year") +
  
  theme_minimal()







# Extract year and month

livestock_clean_data <- livestock_clean_data %>%
  
  mutate(
    
    year = year(Report_Date),
    
    month = month(Report_Date),
    
    year_month = make_date(year = year, month = month, day = 1)  # first day of month
    
  )



# Summarize total deaths by year-month

monthly_deaths <- livestock_clean_data %>%
  
  group_by(year_month) %>%
  
  summarise(total_deaths = sum(Number_Dead, na.rm = TRUE), .groups = "drop")



#plot

# Total deaths

total_deaths_all <- sum(livestock_clean_data$Number_Dead, na.rm = TRUE)



ggplot(monthly_deaths, aes(x = year_month, y = total_deaths)) +
  
  geom_line(color = "#1f77b4", size = 1.2) +
  
  geom_point(color = "#ff7f0e", size = 2.5) +
  
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +  # labels every 3 months
  
  scale_y_continuous(labels = scales::comma) +
  
  labs(
    
    title = "Monthly Trend of Animal Deaths",
    
    subtitle = paste("Total deaths across all months:", scales::comma(total_deaths_all)),
    
    x = "Month-Year",
    
    y = "Number of Deaths"
    
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))









# Question 5:What is the vaccine coverage ---------------------------------

#Calculate vaccine coverage overall

overall_vaccine_coverage <- livestock_clean_data %>%
  
  summarise(
    
    total_at_risk = sum(Number_at_Risk, na.rm = TRUE),
    
    total_vaccinated = sum(Number_Vaccinated, na.rm = TRUE),
    
    vaccine_coverage = (total_vaccinated / total_at_risk) * 100
    
  )



overall_vaccine_coverage


#Create county_vaccine
county_vaccine <- livestock_clean_data %>%
  
  group_by(county) %>%
  
  summarise(
    
    total_vaccinated = sum(Number_Vaccinated, na.rm = TRUE),
    
    total_at_risk = sum(Number_at_Risk, na.rm = TRUE)
    
  ) %>%
  
  mutate(
    
    vaccine_coverage = (total_vaccinated / total_at_risk) * 100
    
  )
# Categorize counties by vaccine coverage

county_vaccine <- county_vaccine %>%
  
  mutate(coverage_level = case_when(
    
    vaccine_coverage >= 70 ~ "Good",
    
    vaccine_coverage >= 50 & vaccine_coverage < 70 ~ "Average",
    
    vaccine_coverage < 50 ~ "Low"
    
  ))



# Plot with color by coverage level

ggplot(county_vaccine,
       
       aes(x = reorder(county, vaccine_coverage),
           
           y = vaccine_coverage,
           
           fill = coverage_level)) +
  
  geom_col() +
  
  geom_text(
    
    aes(label = paste0(round(vaccine_coverage, 1), "%")),
    
    hjust = -0.2,
    
    size = 3
    
  ) +
  
  scale_fill_manual(values = c("Good" = "green", "Average" = "yellow", "Low" = "red")) +
  
  coord_flip() +
  
  labs(
    
    title = "Vaccine Coverage by County",
    
    x = "County",
    
    y = "Vaccine Coverage (%)",
    
    fill = "Coverage Level"
    
  ) +
  
  theme_minimal() +
  
  theme(
    
    axis.text.y = element_text(size = 12),   # 👈 increase disease names
    
    axis.text.x = element_text(size = 11),   # optional (numbers)
    
    axis.title = element_text(size = 13),
    
    plot.title = element_text(size = 16, face = "bold"),
    
    plot.subtitle = element_text(size = 13)
    
  ) +
  
  expand_limits(y = max(county_vaccine$vaccine_coverage) * 1.2)





#Vaccination coverage by disease category

population_level <- livestock_clean_data %>%
  
  group_by(county, Species_Affected, year) %>%
  
  summarise(
    
    at_risk = max(Number_at_Risk, na.rm = TRUE),
    
    vaccinated = max(Number_Vaccinated, na.rm = TRUE),
    
    .groups = "drop"
    
  ) %>%
  
  mutate(
    
    vaccine_coverage = (vaccinated / at_risk) * 100
    
  )



#Linking population level to disease categories

category_vaccine_coverage <- livestock_clean_data %>%
  
  dplyr::select(county, Species_Affected, year, disease_category) %>%
  
  distinct() %>%
  
  left_join(population_level,
            
            by = c("county", "Species_Affected", "year")) %>%
  
  group_by(disease_category) %>%
  
  summarise(
    
    vaccine_coverage = mean(vaccine_coverage, na.rm = TRUE),
    
    .groups = "drop"
    
  )



#Plot

ggplot(category_vaccine_coverage,
       
       aes(x = reorder(disease_category, vaccine_coverage),
           
           y = vaccine_coverage)) +
  
  geom_col(fill = "steelblue") +
  
  labs(
    
    title = "Average Vaccine Coverage by Disease Category",
    
    x = "Disease Category",
    
    y = "Vaccine Coverage (%)"
    
  ) +
  
  theme_minimal() +
  
  theme(
    
    axis.text.x = element_text(angle = 45, hjust = 1)
    
  ) +
  
  ylim(0, 100)



#Calculate vaccine coverage for TOP 10 diseases only

top10_vaccine_coverage <- livestock_clean_data %>%
  
  group_by(Disease_Condition) %>%
  
  summarise(
    
    at_risk = sum(Number_at_Risk, na.rm = TRUE),
    
    vaccinated = sum(Number_Vaccinated, na.rm = TRUE),
    
    vaccine_coverage = (vaccinated / at_risk) * 100,
    
    .groups = "drop"
    
  ) %>%
  
  arrange(vaccine_coverage) |>
  
  filter(Disease_Condition %in% c(top10_diseases$Disease_Condition))





#Plot

ggplot(top10_vaccine_coverage,
       
       aes(x = reorder(Disease_Condition, vaccine_coverage),
           
           y = vaccine_coverage)) +
  
  geom_col(fill = "steelblue") +
  
  geom_text(aes(label = paste0(round(vaccine_coverage, 1), "%")),
            
            vjust = -0.3, size = 3) +
  
  coord_flip() +
  
  labs(
    
    title = "Vaccine Coverage Among Top 10 Disease Conditions",
    
    x = "Disease Condition",
    
    y = "Vaccine Coverage (%)"
    
  ) +
  
  theme_minimal() +
  
  theme(
    
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    
    axis.text = element_text(size = 14)
    
  ) +
  
  ylim(0, 100)



# Vaccine preventable diseases only

vaccine_diseases <- c("Lumpy Skin Disease",
                      
                      "Goat Pox",
                      
                      "Sheep Pox",
                      
                      "Contagious Caprine Pleuropneumonia",
                      
                      "Contagious Bovine Pleuropneumonia",
                      
                      "Peste des Petits Ruminants",
                      
                      "Foot and Mouth Disease",
                      
                      "Rabies",
                      
                      "Avian Influenza",
                      
                      "Newcastle Disease",
                      
                      "Fowl Pox",
                      
                      "Camel Pox",
                      
                      "Anthrax",
                      
                      "Enterotoxaemia",
                      
                      "Haemorrhagic Septicemia",
                      
                      "Tetanus",
                      
                      "Black Quarter",
                      
                      "Brucellosis")



# Calculate vaccine coverage for only vaccine-preventable diseases

vaccine_coverage_vaccines_only <- livestock_clean_data %>%
  
  filter(disease_clean_name %in% vaccine_diseases) %>%
  
  group_by(disease_clean_name) %>%
  
  summarise(
    
    at_risk = sum(Number_at_Risk, na.rm = TRUE),
    
    vaccinated = sum(Number_Vaccinated, na.rm = TRUE),
    
    vaccine_coverage = (vaccinated / at_risk) * 100,
    
    .groups = "drop"
    
  ) %>%
  
  mutate(coverage_category = case_when(
    
    vaccine_coverage >= 80 ~ "Good",
    
    vaccine_coverage >= 50 ~ "Average",
    
    TRUE ~ "Poor"
    
  )) %>%
  
  arrange(vaccine_coverage)



# Define colors

coverage_colors <- c("Good" = "forestgreen",
                     
                     "Average" = "goldenrod",
                     
                     "Poor" = "firebrick")



# Plot

ggplot(vaccine_coverage_vaccines_only,
       
       aes(x = reorder(disease_clean_name, vaccine_coverage),
           
           y = vaccine_coverage,
           
           fill = coverage_category)) +
  
  geom_col() +
  
  geom_text(aes(label = paste0(round(vaccine_coverage, 1), "%")),
            
            vjust = -0.3, size = 3) +
  
  coord_flip(ylim = c(0, 100)) +
  
  scale_fill_manual(values = coverage_colors) +
  
  labs(
    
    title = "Vaccine Coverage for Vaccine-Preventable Diseases",
    
    x = "Disease",
    
    y = "Vaccine Coverage (%)",
    
    fill = "Coverage Level"
    
  ) +
  
  theme_minimal() +
  
  theme(
    
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    
    axis.text = element_text(size = 14)
    
  )





# Question 6:# Top 30 disease conditions affecting cattle,goats,sh --------

#Cattle

top30_cattle_diseases <- livestock_clean_data %>%
  
  filter(Species_Affected %in% c("Cattle")) %>%
  
  filter(!disease_clean_name %in% c("Coccidiosis", "Newcastle Disease")) %>%
  
  group_by(disease_clean_name) %>%
  
  summarise(total_sick_cattle = sum(Number_Sick, na.rm = TRUE)) %>%
  
  arrange(desc(total_sick_cattle)) %>%
  
  slice_head(n = 30)



top30_cattle_diseases

#plot

# Calculate total sick cattle

total_cattle_sick <- sum(top30_cattle_diseases$total_sick_cattle, na.rm = TRUE)



Cattle <- ggplot(top30_cattle_diseases,
                 
                 aes(x = reorder(disease_clean_name, total_sick_cattle),
                     
                     y = total_sick_cattle,
                     
                     fill = total_sick_cattle)) +
  
  geom_col() +
  
  geom_text(aes(label = total_sick_cattle),
            
            hjust = -0.1,
            
            size = 3) +
  
  scale_fill_gradient(
    
    low = "#add8e6",
    
    high = "#1f77b4",
    
    labels = scales::comma
    
  ) +
  
  scale_y_continuous(labels = scales::comma) + 
  
  coord_flip() +
  
  labs(
    
    title = "Top 30 Diseases Affecting Cattle",
    
    subtitle = paste("Total number of sick cattle:", scales::comma(total_cattle_sick)),
    
    x = "Disease",
    
    y = "Total Number Sick",
    
    fill = "Number Sick"
    
  ) +
  
  theme_minimal() +
  
  theme(
    
    axis.text.y = element_text(size = 12),   # 👈 increase disease names
    
    axis.text.x = element_text(size = 11),   # optional (numbers)
    
    axis.title = element_text(size = 13),
    
    plot.title = element_text(size = 16, face = "bold"),
    
    plot.subtitle = element_text(size = 13)
    
  ) +
  
  expand_limits(y = max(top30_cattle_diseases$total_sick_cattle) * 1.1)



Cattle



#Goats

top30_Goats_diseases <- livestock_clean_data %>%
  
  filter(Species_Affected %in% c("Goats")) %>%
  
  filter(!disease_clean_name %in% c("Coccidiosis", "Newcastle Disease")) %>%
  
  group_by(disease_clean_name) %>%
  
  summarise(total_sick_Goats = sum(Number_Sick, na.rm = TRUE)) %>%
  
  arrange(desc(total_sick_Goats)) %>%
  
  slice_head(n = 30)



top30_Goats_diseases

#plot

# Calculate total sick Goats

total_Goats_sick <- sum(top30_Goats_diseases$total_sick_Goats, na.rm = TRUE)



Goats <- ggplot(top30_Goats_diseases,
                
                aes(x = reorder(disease_clean_name, total_sick_Goats),
                    
                    y = total_sick_Goats,
                    
                    fill = total_sick_Goats)) +
  
  geom_col() +
  
  geom_text(aes(label = total_sick_Goats),
            
            hjust = -0.1,
            
            size = 3) +
  
  scale_fill_gradient(
    
    low = "#90ee90",
    
    high = "#006400",
    
    labels = scales::comma
    
  ) +
  
  scale_y_continuous(labels = scales::comma) + 
  
  coord_flip() +
  
  labs(
    
    title = "Top 30 Diseases Affecting Goats",
    
    subtitle = paste("Total number of sick Goats:", scales::comma(total_Goats_sick)),
    
    x = "Disease",
    
    y = "Total Number Sick",
    
    fill = "Number Sick"
    
  ) +
  
  theme_minimal() +
  
  theme(
    
    axis.text.y = element_text(size = 12),   # 👈 increase disease names
    
    axis.text.x = element_text(size = 11),   # optional (numbers)
    
    axis.title = element_text(size = 13),
    
    plot.title = element_text(size = 16, face = "bold"),
    
    plot.subtitle = element_text(size = 13)
    
  ) +
  
  expand_limits(y = max(top30_Goats_diseases$total_sick_Goats) * 1.1)



Goats



#Sheep

top30_Sheep_diseases <- livestock_clean_data %>%
  
  filter(Species_Affected %in% c("Sheep")) %>%
  
  filter(!disease_clean_name %in% c("Coccidiosis", "Newcastle Disease")) %>%
  
  group_by(disease_clean_name) %>%
  
  summarise(total_sick_Sheep = sum(Number_Sick, na.rm = TRUE)) %>%
  
  arrange(desc(total_sick_Sheep)) %>%
  
  slice_head(n = 30)



top30_Sheep_diseases

#plot

# Calculate total sick Sheep

total_Sheep_sick <- sum(top30_Sheep_diseases$total_sick_Sheep, na.rm = TRUE)



Sheep <- ggplot(top30_Sheep_diseases,
                
                aes(x = reorder(disease_clean_name, total_sick_Sheep),
                    
                    y = total_sick_Sheep,
                    
                    fill = total_sick_Sheep)) +
  
  geom_col() +
  
  geom_text(aes(label = total_sick_Sheep),
            
            hjust = -0.1,
            
            size = 3) +
  
  scale_fill_gradient(
    
    low = "#FFA500",
    
    high = "#FF8C00",
    
    labels = scales::comma
    
  ) +
  
  scale_y_continuous(labels = scales::comma) + 
  
  coord_flip() +
  
  labs(
    
    title = "Top 30 Diseases Affecting Sheep",
    
    subtitle = paste("Total number of sick Sheep:", scales::comma(total_Sheep_sick)),
    
    x = "Disease",
    
    y = "Total Number Sick",
    
    fill = "Number Sick"
    
  ) +
  
  theme_minimal() +
  
  theme(
    
    axis.text.y = element_text(size = 12),   # 👈 increase disease names
    
    axis.text.x = element_text(size = 11),   # optional (numbers)
    
    axis.title = element_text(size = 13),
    
    plot.title = element_text(size = 16, face = "bold"),
    
    plot.subtitle = element_text(size = 13)
    
  ) +
  
  expand_limits(y = max(top30_Sheep_diseases$total_sick_Sheep) * 1.1)



Sheep

Cattle|Goats|Sheep





# Chicken -----------------------------------------------------------------

#Chicken

top30_Chicken_diseases <- livestock_clean_data %>%
  
  filter(Species_Affected %in% c("Chicken")) %>%
  
  filter(!disease_clean_name %in% c("Milk fever", "PPR", "Enterotoxaemia")) %>%
  
  group_by(disease_clean_name) %>%
  
  summarise(total_sick_Chicken = sum(Number_Sick, na.rm = TRUE)) %>%
  
  arrange(desc(total_sick_Chicken)) %>%
  
  slice_head(n = 30)



top30_Chicken_diseases

#plot

# Calculate total sick Chicken

total_Chicken_sick <- sum(top30_Chicken_diseases$total_sick_Chicken, na.rm = TRUE)



ggplot(top30_Chicken_diseases,
       
       aes(x = reorder(disease_clean_name, total_sick_Chicken),
           
           y = total_sick_Chicken,
           
           fill = total_sick_Chicken)) +
  
  geom_col() +
  
  geom_text(aes(label = total_sick_Chicken),
            
            hjust = -0.1,
            
            size = 3) +
  
  scale_fill_gradient(
    
    low = "#add8e6",
    
    high = "#1f77b4",
    
    labels = scales::comma
    
  ) +
  
  scale_y_continuous(labels = scales::comma) + 
  
  coord_flip() +
  
  labs(
    
    title = "Top 30 Diseases Affecting Chicken",
    
    subtitle = paste("Total number of sick Chicken:", scales::comma(total_Chicken_sick)),
    
    x = "Disease",
    
    y = "Total Number Sick",
    
    fill = "Number Sick"
    
  ) +
  
  theme_minimal() +
  
  theme(
    
    axis.text.y = element_text(size = 12),   # 👈 increase disease names
    
    axis.text.x = element_text(size = 11),   # optional (numbers)
    
    axis.title = element_text(size = 13),
    
    plot.title = element_text(size = 16, face = "bold"),
    
    plot.subtitle = element_text(size = 13)
    
  ) +
  
  expand_limits(y = max(top30_Chicken_diseases$total_Chicken_sick) * 1.1)







# ALL species combined

# Calculate total sick per species

# Summarise total sick per species

total_sick_species <- livestock_clean_data %>%
  
  filter(Species_Affected %in% c("Cattle","Goats","Sheep")) %>%
  
  group_by(Species_Affected) %>%
  
  summarise(total_sick = sum(Number_Sick, na.rm = TRUE))



# Extract individual totals

total_cattle <- total_sick_species$total_sick[total_sick_species$Species_Affected == "Cattle"]

total_goats  <- total_sick_species$total_sick[total_sick_species$Species_Affected == "Goats"]

total_sheep  <- total_sick_species$total_sick[total_sick_species$Species_Affected == "Sheep"]



# Plot total sick animals per species

ggplot(total_sick_species, aes(x = Species_Affected, y = total_sick, fill = Species_Affected)) +
  
  geom_col() +
  
  geom_text(aes(label = scales::comma(total_sick)), vjust = -0.5) +
  
  scale_y_continuous(labels = scales::comma) +
  
  labs(
    
    title = "Total Number of Sick Animals by Species",
    
    subtitle = paste0("Cattle: ", scales::comma(total_cattle),
                      
                      " | Goats: ", scales::comma(total_goats),
                      
                      " | Sheep: ", scales::comma(total_sheep)),
    
    x = "Species",
    
    y = "Number of Sick Animals",
    
    fill = "Species"
    
  ) +
  
  theme_minimal()

#top 30 diseases

# Get top 30 diseases across Cattle, Goats, Sheep

top30_diseases <- livestock_clean_data %>%
  
  filter(Species_Affected %in% c("Cattle","Goats","Sheep")) %>%
  
  group_by(disease_clean_name, Species_Affected) %>%
  
  summarise(total_sick = sum(Number_Sick, na.rm = TRUE)) %>%
  
  ungroup() %>%
  
  group_by(disease_clean_name) %>%
  
  summarise(total_sick = sum(total_sick)) %>%  # sum across species for overall total
  
  arrange(desc(total_sick)) %>%
  
  slice_head(n = 30)



# Plot top 30 diseases

ggplot(top30_diseases, aes(x = reorder(disease_clean_name, total_sick), 
                           
                           y = total_sick, 
                           
                           fill = disease_clean_name)) +
  
  geom_col(show.legend = FALSE) +
  
  geom_text(aes(label = scales::comma(total_sick)), hjust = -0.1, size = 3)+
  
  coord_flip() +
  
  scale_y_continuous(labels = scales::comma) +
  
  labs(
    
    title = "Top 30 Diseases by Number of Sick Animals",
    
    subtitle = paste0("Cattle: ", scales::comma(total_cattle),
                      
                      " | Goats: ", scales::comma(total_goats),
                      
                      " | Sheep: ", scales::comma(total_sheep)),
    
    x = "Disease",
    
    y = "Number of Sick Animals"
    
  ) +
  
  theme_minimal()





#Bees

top30_Bees_diseases <- Livestock_data %>%
  
  filter(Species_Affected %in% c("Bees")) %>%
  
  group_by(Disease_Condition) %>%
  
  summarise(total_sick_Bees = sum(Number_Sick, na.rm = TRUE)) %>%
  
  arrange(desc(total_sick_Bees)) %>%
  
  slice_head(n = 30)



top30_Bees_diseases



# Total sick bees

total_Bees_sick <- sum(top30_Bees_diseases$total_sick_Bees, na.rm = TRUE)



# Plot

Bees <- ggplot(top30_Bees_diseases,
               
               aes(
                 
                 x = reorder(Disease_Condition, total_sick_Bees),  # Correct column name
                 
                 y = total_sick_Bees,
                 
                 fill = total_sick_Bees
                 
               )) +
  
  geom_col() +
  
  geom_text(aes(label = total_sick_Bees),
            
            hjust = -0.1,
            
            size = 3) +
  
  scale_fill_gradient(
    
    low = "#FFA500",   # light orange
    
    high = "#FF8C00",  # dark orange
    
    labels = comma
    
  ) +
  
  scale_y_continuous(labels = comma) + 
  
  coord_flip() +
  
  labs(
    
    title = "Top 30 Diseases Affecting Bees",
    
    subtitle = paste("Total number of sick Bees:", comma(total_Bees_sick)),
    
    x = "Disease",
    
    y = "Total Number Sick",
    
    fill = "Number Sick"
    
  ) +
  
  theme_minimal() +
  
  theme(
    
    axis.text.y = element_text(size = 12),   # 👈 increase disease names
    
    axis.text.x = element_text(size = 11),   # optional (numbers)
    
    axis.title = element_text(size = 13),
    
    plot.title = element_text(size = 16, face = "bold"),
    
    plot.subtitle = element_text(size = 13)
    
  ) +
  
  expand_limits(y = max(top30_Bees_diseases$total_Bees_sick) * 1.1)



# Show plot

Bees



# #Loading precipitation.tif file -----------------------------------------


precip <- rast("Downloads/era5_monthly_precip_mm_kenya-2.tif")

# Check raster info
precip
plot(precip)

#Select only layers from 2020 onward
# Extract layer names
layer_names <- names(precip)

# Keep layers from 2020 onward
layers_feb2020_onward <- layer_names[as.Date(paste0(layer_names, "_01"), format="%Y_%m_%d") >= as.Date("2020-02-01")]

# Subset the raster
precip_2020 <- precip[[layers_feb2020_onward]]

# Check
precip_2020
names(precip_2020)[1:12]  # first year’s months

# Extract year and month from Report_Date
livestock_clean_data <- livestock_clean_data %>%
  mutate(
    year_month = format(Report_Date, "%Y_%m")
  )


# #Loading Kenya shape files ----------------------------------------------
# Kenya counties
kenya_counties <- st_read("Downloads/Kenya 2/gadm41_KEN_1.shp")

# Sub-counties
kenya_subcounties <- st_read("Downloads/Kenya 2/gadm41_KEN_2.shp")

# Wards
kenya_wards <- st_read("Downloads/Kenya 2/gadm41_KEN_3.shp")

# Rename columns for easier join
kenya_counties <- kenya_counties %>%
  rename(county = NAME_1)

kenya_subcounties <- kenya_subcounties %>%
  rename(sub_county = NAME_2,
         county = NAME_1)  

kenya_wards <- kenya_wards %>%
  rename(ward = NAME_3,
         sub_county = NAME_2,
         county = NAME_1)

#Transform CRS of shapefiles to match raster
kenya_counties <- st_transform(kenya_counties, crs(terra::crs(precip_2020)))
kenya_subcounties <- st_transform(kenya_subcounties, crs(terra::crs(precip_2020)))
kenya_wards <- st_transform(kenya_wards, crs(terra::crs(precip_2020)))

# Extract average precipitation per polygon per month ---------------------
extract_precip <- function(shapefile, shapefile_id) {
  
  precip_list <- list()
  
  for(layer_name in names(precip_2020)) {
    layer <- precip_2020[[layer_name]]
    
    mean_precip <- exact_extract(layer, shapefile, 'mean')
    
    df <- shapefile %>%
      st_drop_geometry() %>%
      as_tibble() %>%                # convert to tibble
      dplyr::select(all_of(shapefile_id)) %>%  # explicitly call dplyr::select
      mutate(
        year_month = layer_name,
        precip = mean_precip
      )
    
    precip_list[[layer_name]] <- df
  }
  
  bind_rows(precip_list)
}


# --- 1. Standardize names in livestock data ---
livestock_clean_data <- livestock_clean_data %>%
  mutate(
    Ward = str_trim(Ward),
    sub_county = str_trim(sub_county),
    county = str_trim(county)
  )
## Standardize county names
livestock_clean_data <- livestock_clean_data %>%
  mutate(county = str_to_title(str_trim(county)))

county_precip <- county_precip %>%
  mutate(county = str_to_title(str_trim(county)))


# --- Standardize shapefiles ---
kenya_subcounties <- kenya_subcounties %>% distinct(sub_county, .keep_all = TRUE)
kenya_wards <- kenya_wards %>% distinct(ward, .keep_all = TRUE) %>%
  mutate(ward = gsub(" Ward$", "", ward), ward = trimws(ward))

# --- Extract precipitation for each admin ---
county_precip <- extract_precip(kenya_counties, "county") %>%
  rename(precip_county = precip)

subcounty_precip <- extract_precip(kenya_subcounties, "sub_county") %>%
  rename(precip_sub_county = precip) %>%
  distinct(sub_county, year_month, .keep_all = TRUE)

ward_precip <- extract_precip(kenya_wards, "ward") %>%
  rename(precip_ward = precip) %>%
  distinct(ward, year_month, .keep_all = TRUE)

#convert to year month
livestock_clean_data <- livestock_clean_data %>%
  mutate(year_month = gsub("_", " ", year_month))
# Standardize year_month format 
county_precip <- county_precip %>%
  mutate(year_month = yearmonth(year_month))

subcounty_precip <- subcounty_precip %>%
  mutate(year_month = yearmonth(year_month))

ward_precip <- ward_precip %>%
  mutate(year_month = yearmonth(year_month))

livestock_clean_data <- livestock_clean_data %>%
  mutate(year_month = yearmonth(year_month))

#merge precipitation data with livestock clean data
livestock_clean_data <- livestock_clean_data %>%
  left_join(county_precip, by = c("county", "year_month")) %>%
  left_join(subcounty_precip, by = c("sub_county", "year_month")) %>%
  left_join(ward_precip, by = c("Ward" = "ward", "year_month"))



#Monthly time series
monthly_fmd <- livestock_clean_data %>%
  filter(disease_clean_name == "Foot and Mouth Disease") %>%
  group_by(year_month) %>%
  summarise(
    cases = sum(Number_Sick, na.rm = TRUE),
    precip = mean(precip_county, na.rm = TRUE)   # Use the correct column
  ) %>%
  as_tsibble(index = year_month)

#removing April outlier
monthly_fmd_clean <- monthly_fmd %>% 
  filter(year_month != yearmonth("2021 Apr"))










