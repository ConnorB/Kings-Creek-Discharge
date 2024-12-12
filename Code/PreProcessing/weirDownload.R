# Load required packages
library(tidyverse)
library(lubridate)
library(here)
# Define directory to save CSV files in cache and final Q data
cache_dir <- here("Data/.cache/") 
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
dataDir <- here("Data/LTER/")
dir.create(dataDir, recursive = TRUE, showWarnings = FALSE)


# Define datasets and column names
datasets <- list(
  "https://pasta.lternet.edu/package/data/eml/knb-lter-knz/7/18/52e4f408e6f2cdfa99fa894576052bc0",
  "https://pasta.lternet.edu/package/data/eml/knb-lter-knz/10/18/783ae0e61f96667a1aa45ff39871340b",
  "https://pasta.lternet.edu/package/data/eml/knb-lter-knz/9/19/3d7fc82a0d49506099946c16fec3ca28",
  "https://pasta.lternet.edu/package/data/eml/knb-lter-knz/8/18/24005e46d514cccc92e665503568113a"
)

# Generate filenames for local storage
file_names <- map(datasets, ~file.path(cache_dir, basename(.x)))

# Define column names and types
col_names_1 <- c(
  "DataCode", "RecType", "RecYear", "RecMonth", "RecDay", "Watershed",
  "DayofYear", "RecHour", "Discharge", "Sheight", "CorrectedSheight",
  "Height", "LogFlag", "QualFlag"
)

col_types_1 <- cols(
  DataCode = col_character(),
  RecType = col_character(),
  RecYear = col_double(),
  RecMonth = col_double(),
  RecDay = col_double(),
  Watershed = col_character(),
  DayofYear = col_double(),
  RecHour = col_character(),
  Discharge = col_double(),
  Sheight = col_double(),
  CorrectedSheight = col_double(),
  Height = col_double(),
  LogFlag = col_character(),
  QualFlag = col_character()
)

# Function to download and save data if needed
download_if_needed <- function(url, file_name) {
  if (!file.exists(file_name) || 
      as.numeric(difftime(Sys.time(), file.info(file_name)$mtime, units = "days")) > 60) {
    message("Downloading ", url)
    download.file(url, file_name, method = "curl")
  } else {
    message("Using cached file: ", file_name)
  }
}

# Download files
walk2(datasets, file_names, download_if_needed)

# Function to read and clean data
read_and_clean <- function(file_name, col_names, col_types) {
  data <- read_delim(file_name, delim = ",", skip = 1, quote = '"', col_names = col_names, col_types = col_types, na = c(" ", ".", "NA", ""))
  
  # Handle problematic "." values separately for numeric columns
  data <- data |>
    mutate(across(where(is.numeric), ~suppressWarnings(as.numeric(trimws(.)))), # Convert "." to NA
           across(where(is.character), ~na_if(trimws(.), "."))) # Treat "." as NA in character columns
  
  return(data)
}

# Read and combine all weir data
lterWeir <- file_names |>
  map_dfr(~read_and_clean(.x, col_names_1, col_types_1))

# Process the data
lterWeir <- lterWeir |>
  mutate(
    RecDay = ifelse(RecDay == 0, 1, RecDay),  # Replace 0 with 1 for RecDay
    RecHour = case_when(
      nchar(RecHour) == 1 ~ paste0("000", RecHour),
      nchar(RecHour) == 2 ~ paste0("00", RecHour),
      nchar(RecHour) == 3 ~ paste0("0", RecHour),
      nchar(RecHour) == 4 ~ RecHour,
      TRUE ~ NA_character_
    ),
    dateTime = ymd_hm(paste(RecYear, RecMonth, RecDay, RecHour), tz = "UTC")
  )

# Define the 15-minute interval sequence for each watershed
lterWeir <- lterWeir |>
  select(Watershed, dateTime, Discharge, Sheight, CorrectedSheight, Height, LogFlag, QualFlag) |> 
  group_by(Watershed) |>
  complete(
    dateTime = seq(floor_date(min(dateTime), "15 min"), floor_date(max(dateTime), "15 min"), by = "15 min"), # Create a full 15-minute sequence
    fill = list(Discharge = NA, CorrectedSheight = NA, Height = NA, Sheight = NA, LogFlag = NA, QualFlag = NA) # Default fill for missing rows
  ) |>
  arrange(dateTime) |> 
  ungroup()

# Interpolate missing numeric values and fill qual flags down
lterWeir <- lterWeir |>
  arrange(Watershed, dateTime) |>
  group_by(Watershed) |>
  mutate(
    Discharge = zoo::na.approx(Discharge, na.rm = F, maxgap = 96),
    CorrectedSheight = zoo::na.approx(CorrectedSheight, na.rm = F, maxgap = 96), 
    Height = zoo::na.approx(Height, na.rm = F, maxgap = 96),
    Sheight = zoo::na.approx(Sheight, na.rm = F, maxgap = 96),
  ) |>
  mutate(
    LogFlag = if_else(is.na(LogFlag), "NA", LogFlag),  # Replace NA with "NA" for LogFlag
    QualFlag = if_else(is.na(QualFlag), "NA", QualFlag) # Replace NA with "NA" for QualFlag
  ) |>
  #Only keep 15 minute data
  filter(minute(dateTime) %% 15 == 0) |>
  ungroup() |> 
  rename(Q_cms = Discharge, stageHeight_cm = Sheight, corrStageHeight_cm = CorrectedSheight, manualStage_cm = Height) |> 
  write_csv(paste0(dataDir, "weirQ.csv"))
