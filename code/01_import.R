# The purpose of this script is to gather listing data for analysis.

# SETUP -----------------------------------------------------------------------

# Load packages
library(tidyverse)
library(ebaypi)
library(here)

# Set API token
token <- keyring::key_get("ebay_api")  # use keyring to store api token
set_ebay_token(token)

# Load input sheet
df.input <- readxl::read_xlsx(
  here("input.xlsx"),
  col_types = c("text", "text", "text", "numeric", "numeric")
)


# RUN QUERY -------------------------------------------------------------------

df.input <- df.input %>%
  slice(1) %>%
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
        item_filters = ..3
      )
    )
  )


keywords <- df.input$keywords[[1]]
categories <- NA
if (is.na(categories)) categories <- NULL

tmp <- search_ebay(keywords, categories = categories)



