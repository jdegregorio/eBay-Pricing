# The purpose of this script is to gather listing data for analysis.

# SETUP -----------------------------------------------------------------------

# Clean workspace
rm(list = ls())

# Load packages
library(tidyverse)
library(ebaypi)
library(here)
library(yaml)

# Load configuration file
config <- read_yaml("config.yaml")

# Set API token
token <- keyring::key_get("ebay_api")  # use keyring to store api token
set_ebay_token(token)

# Load input sheet
df.input <- readxl::read_xlsx(
  here(config$input_file),
  col_types = c("text", "text", "text", "numeric", "numeric")
)


# RUN QUERY -------------------------------------------------------------------

# Gather active and completed listings
df.listings <- df.input %>%
  mutate(
    item_filters = pmap(
      list(price_min, price_max),
      ~list(MinPrice = ..1, MaxPrice = ..2)
    )
  ) %>%
  mutate(
    data_active = pmap(
      list(keywords, categories, item_filters),
      ~search_ebay(
        keywords = ..1,
        type = "active",
        categories = ..2,
        item_filters = ..3,
        n_results = 1000
      )
    ),
    data_completed = pmap(
      list(keywords, categories, item_filters),
      ~search_ebay(
        keywords = ..1,
        type = "completed",
        categories = ..2,
        item_filters = ..3,
        n_results = 1000
      )
    )
  )



# SAVE DATA -------------------------------------------------------------------

# Create folders
map(
  df.listings$item_id,
  ~dir.create(here("data", "01_raw", .x), showWarnings = FALSE)
)

# Save raw active files
map2(
  df.listings$data_active, 
  df.listings$item_id, 
  function (data, name) {write_rds(data, here("data", "01_raw", name, "active.rds"))}
)

# Save raw completed files
map2(
  df.listings$data_completed, 
  df.listings$item_id, 
  function (data, name) {write_rds(data, here("data", "01_raw", name, "completed.rds"))}
)
