# Load libraries 
library(car)
library(knitr)

# Estimate regression model
model <- lm(Leverage ~ Firmsize + ROA + ROE + Liquidity + fcftotalassets +
              Collaterals + Effectivetaxrate + TobinQ + Dividendpayoutratio + 
              GDP + Inflationrate + Covid,
            data = P)

# Compute VIF
vif_values <- vif(model)
print(vif_values)

# Average VIF
average_vif <- mean(vif_values)
print(average_vif)

# Create VIF table
vif_table <- data.frame(
  Variable = names(vif_values),
  VIF = round(vif_values, 2)
)

# Add the average VIF row
vif_table <- rbind(vif_table, data.frame(Variable = "Average VIF",
             VIF = round(average_vif, 2)))

# Print the table
kable(vif_table, caption = "Variance Inflation Factor Results")

# Correlation matrix
independent_vars <- P[, c("Firmsize", "ROA", "ROE", "Liquidity",
                          "fcftotalassets", "Collaterals",
                          "Effectivetaxrate", "TobinQ",
                          "Dividendpayoutratio", "GDP",
                          "Inflationrate", "Covid")]

# Compute correlation matrix
cor_matrix <- cor(independent_vars, use = "pairwise.complete.obs")

# Print the correlation matrix
print(cor_matrix)