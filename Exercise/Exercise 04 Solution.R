# ## Exercise 4
# 
# In RStudio on Posit Cloud, create a new R script and do the following.
# 
# 1. Load the tidyverse.
# 2. Import and join `customer_data` and `store_transactions`.
# 3. Explore this combined dataset using the functions we've covered.
# 4. Provide at least one interesting numeric summary and one interesting 
# visualization that include continuous variables.
# 5. Practice good coding conventions as discussed.
# 6. Export the R script and upload to Canvas.

# Five points total, one point each for:
#   
# - Loading the tidyverse.
# - Importing and joining customer data and store_transactions.
# - Using the functions we've covered to summarize the data as specified.
# - Following good coding conventions (provide feedback on this point).
# - Submitting an R script.

# Load the tidyverse.
library(tidyverse)

# Import data.
customer_data <- read_csv("customer_data.csv")
store_transactions <- read_csv("store_transactions.csv")

crm_data <- customer_data |> 
  left_join(store_transactions, join_by(customer_id))

# At least one interesting numeric summary.
crm_data |> 
  group_by(region) |> 
  summarize(
    n = n(),
    recent_transactions = sum(dec_2018)
  ) |> 
  mutate(avg_transactions = recent_transactions / n)

# At least one interesting visualization.
crm_data |> 
  mutate(age = 2021 - birth_year) |> 
  ggplot(aes(x = dec_2018, y = age)) +
  geom_jitter(size = 3, alpha = 0.5, aes(color = gender)) +
  geom_smooth(method = "lm", se = FALSE) + 
  facet_grid(~ region) +
  labs(
    title = "December 2018 Purchases by Age, Region, and Gender",
    x = "December 2018 Purchases",
    y = "Age"
  ) +
  scale_color_manual(
    name = "Gender",
    values = c("violet", "purple", "turquoise")
  )

