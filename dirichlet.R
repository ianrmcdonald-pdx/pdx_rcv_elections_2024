# Load the package
library(MCMCpack)

# Define alpha parameters
alpha <- c(3, 100)

# Generate 5 random samples
samples <- rdirichlet(3, alpha)
print(samples)

library(MCMCpack)
library(ggplot2)
library(reshape2)

# Set alpha parameters
alpha <- c(rene_voters_normal_candidates$n)

# Generate 500 samples
samples <- rdirichlet(500, alpha)

# Convert samples to a data frame
df <- as.data.frame(samples)
colnames(df) <- c("Category1", "Category2", "Category3")

# Convert to long format for ggplot2
df_long <- melt(df)

# Plot the distributions
ggplot(df_long, aes(x = value, fill = variable)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
  labs(title = "Dirichlet Distribution", x = "Proportion", y = "Frequency") +
  theme_minimal()
# Different alpha vectors
alpha1 <- c(1, 1, 1)
alpha2 <- c(2, 5, 10)
alpha3 <- c(0.5, 0.5, 0.5)

# Generate samples for each alpha
samples1 <- rdirichlet(500, alpha1)
samples2 <- rdirichlet(500, alpha2)
samples3 <- rdirichlet(500, alpha3)

# Combine into a single data frame
df_combined <- rbind(
  data.frame(samples1, alpha_group = "Alpha = (100, 100, 100)"),
  data.frame(samples2, alpha_group = "Alpha = (2, 5, 10)"),
  data.frame(samples3, alpha_group = "Alpha = (0.5, 0.5, 0.5)")
)

# Convert to long format
df_long_combined <- melt(df_combined, id.vars = "alpha_group")

# Plot the impact of different alpha values
ggplot(df_long_combined, aes(x = value, fill = variable)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
  facet_wrap(~alpha_group, scales = "free") +
  labs(title = "Impact of Different Alpha Values on Dirichlet Distribution",
       x = "Proportion", y = "Frequency") +
  theme_minimal()


###########################################
# Install and load the package
install.packages("DirichletReg")
library(DirichletReg)

# Prepare compositional data (probabilities summing to 1)
# Example: a matrix with proportions
data <- matrix(c(0.1, 0.4, 0.5,
                 0.2, 0.3, 0.5,
                 0.1, 0.6, 0.3,
                  0.1, 0.4, 0.5,
                  0.2, 0.3, 0.5,
                  0.1, 0.6, 0.3), ncol=1, byrow=TRUE)

# Convert to a DirichletRegData object
dr_data <- DR_data(data)

# Estimate parameters by Maximum Likelihood
fit <- DirichReg(dr_data ~ 1)

# Extract the fitted alpha values
alphas <- coef(fit)$alpha
print(alphas)




# Install and load MGLM
install.packages("MGLM")
library(MGLM)

# Example: a matrix of counts for 3 categories
counts <- matrix(c(10000, 2000, 1000,
                   1000, 2000, 10000,
                   200, 400, 200,
                   1500, 1000, 500,
                   100, .200, 300,
                   300, .200, 100), ncol=3, byrow=TRUE)


# Fit the Dirichlet-Multinomial distribution
fit_dm <- MGLMfit(counts, dist="DM")

# Extract estimated alpha parameters
alphas_dm <- fit_dm@estimate
print(alphas_dm)

###############################################

# Install and load the MGLM package
install.packages("MGLM")
library(MGLM)

# 1. Create dummy data: 5 districts (rows) and 10 candidates (columns)
# Replace this with your actual dataset
set.seed(42)
districts <- 5
candidates <- 10

# Simulating random vote counts per district
vote_matrix <- matrix(
  rpois(districts * candidates, lambda = 100), 
  nrow = districts, 
  ncol = candidates
)

# Labeling your candidates
colnames(vote_matrix) <- paste0("Cand_", 1:10)
rownames(vote_matrix) <- paste0("District_", 1:5)

# View your data structure
print(head(vote_matrix))

# 2. Fit the Dirichlet-Multinomial model
fit_dm <- MGLMfit(data = vote_matrix, dist = "DM")

# 3. Extract the 10 alpha parameters
alphas <- fit_dm@estimate
names(alphas) <- colnames(vote_matrix)

print("Estimated Alpha Parameters:")
print(alphas)



# Install and load the package
install.packages("MCMCpack")
library(MCMCpack)

# Define your alpha vector (parameters)
alphas <- carmen_voters_peacock_candidates$n

# 1. Generate random samples from the distribution
# n = number of samples to draw
samples <- rdirichlet(n = 8, alpha = alphas)
print("Random Samples:")
print(samples)

# 2. Compute the probability density for a specific point
# The point vector must sum to 1
point <- c(0.2, 0.5, 0.3)
density <- ddirichlet(x = point, alpha = alphas)
print("Probability Density:")
print(density)
