# The purpose of this script is to clean and prepare listing data.

# SETUP -----------------------------------------------------------------------

# Clean workspace
rm(list = ls())

# Load packages
library(tidyverse)
library(here)
library(yaml)

# Load configuration file
config <- read_yaml("config.yaml")

# Identify files
items <- list.files(path =here("data", "01_raw"))


# DEFINE MERGING TEMPLATES ----------------------------------------------------

# Extract features and renaming scheme from config file
feats <- map(config$features, "name_raw")
feats <- as.character(feats)
names(feats) <- names(config$features)

# Create template
df.template <- rep(NA, length(feats))
names(df.template) <- feats
df.template <- 
  enframe(df.template) %>% 
  mutate(id = 1) %>% 
  pivot_wider(
    id_cols = id, 
    names_from = name, 
    values_from = value
  ) %>%
  select(-id)


# PREPARE DATA ----------------------------------------------------------------

item <- items[[1]]
df.active <- read_rds(here("data", "01_raw", item, "active.rds"))
df.completed <- read_rds(here("data", "01_raw", item, "completed.rds"))

# Merge and rename data
df.listings <- 
  bind_rows(
    df.template,
    df.active,
    df.completed
  ) %>%
  select(feats) %>% 
  drop_na(rank)

# Extract types from config file
types <- map(config$features, "type")

# Change types
df.listings <- df.listings %>%
  mutate_at(names(types[types == "character"]), as.character) %>%
  mutate_at(names(types[types == "factor"]), as_factor) %>%
  mutate_at(names(types[types == "factor"]), fct_explicit_na, na_level = "NA") %>%
  mutate_at(names(types[types == "numeric"]), as.numeric) %>%
  mutate_at(names(types[types == "integer"]), as.integer) %>%
  mutate_at(names(types[types == "logical"]), as.logical) %>%
  mutate_at(names(types[types == "date"]), as.Date)

# Update levels
tmp.relevel <- map(config$features, "relevel")
tmp.relevel <- Filter(Negate(is.null), tmp.relevel)
tmp.relevel <- enframe(tmp.relevel)
for (i in 1:nrow(tmp.relevel)) {
  levels(df.listings[[tmp.relevel$name[[i]]]]) <- tmp.relevel$value[[i]]
}

# Fill NA Values
tmp.replacena <- map(config$features, "replace_na")
tmp.replacena <- Filter(Negate(is.null), tmp.replacena)
df.listings <- replace_na(df.listings, tmp.replacena)

# Apply filters
tmp.filter <- map(config$features, "filter_in")
tmp.filter <- Filter(Negate(is.null), tmp.filter)
tmp.filter <- enframe(tmp.filter)
for (i in 1:nrow(tmp.filter)) {
  df.listings <- 
    filter(df.listings, !!as.name(tmp.filter$name[[i]]) %in% tmp.filter$value[[i]])
}

# MANUALLY PREPARE FEATURES ---------------------------------------------------

# Merge title and subtitle
df.listings <- df.listings %>%
  mutate(
    title = str_c(title, " ", subtitle)
  ) %>%
  select(-subtitle)

# Split state information into two columns
df.listings <- df.listings %>%
  mutate(
    state_complete = (state %in% c("Sold", "No Sale")),
    state_sold = (state %in% c("Sold"))
  ) %>%
  select(-state)

# Calculate listing duration and age
df.listings <- df.listings %>%
  mutate(
    time_duration = time_end - time_start,
    time_remaining = time_end - Sys.Date()
  )
  
# Determine median cost for flat rate shipping
shipping.cost.median <- 
  df.listings %>% 
  filter(shipping_type == "Flat") %>% 
  drop_na(shipping_cost) %>%
  pull(shipping_cost) %>%
  median()

# Impute median value for "Calculated" shipping
df.listings <- df.listings %>%
  mutate(
    shipping_cost = ifelse(
      shipping_type == "Calculated",
      shipping.cost.median,
      shipping_cost
    )
  )

# Calculated adjusted price
df.listings <- df.listings %>%
  mutate(
    price_current_total = price_current + shipping_cost,
    price_buyitnow_total = price_buyitnow + shipping_cost,
    price_current_adj = ifelse(
      shipping_type == "Pickup",
      price_current_total,
      price_current_total - shipping.cost.median
    ),
    price_buyitnow_adj = ifelse(
      shipping_type == "Pickup",
      price_buyitnow_total,
      price_buyitnow_total - shipping.cost.median
    )
  )


# SAVE DATA -------------------------------------------------------------------

write_rds(df.listings, here("data", "02_prepared", "data_listings.rds"))
