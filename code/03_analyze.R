# The purpose of this script is to analyze eBay prices

# SETUP -----------------------------------------------------------------------

# Clean workspace
rm(list = ls())

# Load packages
library(tidyverse)
library(here)
library(yaml)

# Load configuration file
config <- read_yaml("config.yaml")

# Load data
df.listings <- read_rds(here("data", "02_prepared", "data_listings.rds"))


# EXPLORATORY ANALYSIS --------------------------------------------------------

tmp.plot <- df.listings %>%
  mutate(
    flag_active_auction = (!state_complete) & (listing_type == "Auction"),
    price_analyze = ifelse(
      flag_active_auction,
      price_buyitnow_adj,
      price_current_adj
    ),
    listing_group = case_when(
      (listing_type == "Fixed") & state_complete & state_sold ~ "Complete (Sold) - Fixed Price",
      (listing_type == "Auction") & state_complete & state_sold ~ "Complete (Sold) - Auction",
      (listing_type == "Fixed") & state_complete & !state_sold ~ "Complete (Not Sold) - Fixed Price",
      (listing_type == "Auction") & state_complete & !state_sold ~ "Complete (Not Sold) - Auction",
      (listing_type == "Fixed") & !state_complete & !state_sold ~ "Active (Not Sold) - Fixed Price",
      (listing_type == "Auction") & !state_complete & !state_sold ~ "Active (Not Sold) - Auction",
      TRUE ~ "Error"
    )
  )

# Remove these categories:
#  - Active (Not Sold) - Auction
#  - Complete (Not Sold) - Fixed Price
#  - Complete (Not Sold) - Auction

# Filter out broken
# Combine like new and used

# Include these categories - Keep Seperate and report metrics for each individually
#  - Active (Not Sold) - Fixed Price
#  - Complete (Sold) - Auction
#  - Complete (Sold) - Fixed Price


ggplot(tmp.plot, aes(x = condition, y = price_analyze, color = condition)) +
  geom_jitter(alpha = 0.25) +
  theme_light() +
  scale_y_continuous(limits = c(0, 400)) +
  facet_wrap(~listing_group)


df.listings %>%
  mutate() %>%
  select() %>%
  rename()