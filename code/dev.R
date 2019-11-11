
# Create summary table
df.sum <- df.items %>%
  select(
    category = primaryCategory.categoryName,
    price = sellingStatus.currentPrice
  ) %>%
  mutate(price = as.numeric(price)) %>%
  group_by(category) %>%
  summarize(
    count_listings = n(),
    price_mean = mean(price, na.rm = TRUE),
    price_median = median(price, na.rm = TRUE),
    price_q1 = quantile(price, probs = 0.25, na.rm = TRUE),
    price_q3 = quantile(price, probs = 0.75, na.rm = TRUE),
    price_sd = sd(price, na.rm = TRUE),
    price_max = max(price, na.rm = TRUE),
    price_min = min(price, na.rm = TRUE)
  ) %>%
  ungroup()


# Plot results
ggplot(df.sum %>% filter(count_listings > 10),
       aes(x = category, y = price_median, ymin = price_q1, ymax = price_q3)) +
  geom_linerange(alpha = 0.8) +
  geom_point(size = 3, alpha = 0.5, color = "firebrick3") +
  scale_y_continuous(labels = scales::dollar) +
  theme_classic() +
  coord_flip() +
  labs(
    title = "Categorical Price Comparison",
    x = "Category\n",
    y = "Inner Quartile Price Range"
  )
