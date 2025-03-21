# Load necessary library
library(ggplot2)

# (a) Read the data file into a dataframe
expression_data <- read.csv("expression.csv")
print(colnames(expression_data))

# (b) Calculate means for control and treatment values
control.mean <- rowMeans(expression_data[, grep("control", colnames(expression_data))])
treatment.mean <- rowMeans(expression_data[, grep("treatment", colnames(expression_data))])

# Compute RatioCol
RatioCol <- control.mean / treatment.mean
print(head(RatioCol, 10))  # Print first 10 elements

# (c) Plot histograms and scatter plots
par(mfrow = c(2, 2))  # Divide plotting area into 2x2

hist(expression_data$control3, main = "Histogram of control3", col = "lightblue")
hist(expression_data$treatment3, main = "Histogram of treatment3", col = "lightgreen")
plot(expression_data$control1, expression_data$control3, main = "Control1 vs Control3", xlab = "Control1", ylab = "Control3")
plot(expression_data$control6, expression_data$treatment6, main = "Control6 vs Treatment6", xlab = "Control6", ylab = "Treatment6")

dev.off()


# (i) Subset dataframe where control1 and control2 values > 50
sub1 <- subset(expression_data, control1 > 50 & control2 > 50)

# (ii) Create a subset for specific genes
genes_of_interest <- c("gene4", "gene-20", "gene-37", "gene-100")
subrow <- expression_data[expression_data$Gene %in% genes_of_interest, ]

# (iii) Randomly sample 200 rows
set.seed(123)  # For reproducibility
samp <- expression_data[sample(nrow(expression_data), 200), ]

# (iv) Create a boxplot
boxplot(expression_data$control4, expression_data$control5, expression_data$control6,
        expression_data$treatment4, expression_data$treatment5, expression_data$treatment6,
        names = c("C4", "C5", "C6", "T4", "T5", "T6"),
        main = "Boxplot of control and treatment values", col = c("red", "blue"))



# (i) Generate 10000 random deviates from Gaussian distribution and plot histogram
set.seed(123)
random_values <- rnorm(10000, mean = 20, sd = 4)
hist(random_values, main = "Gaussian Distribution (mean=20, sd=4)", col = "skyblue")

# (ii) Compute Full Width at Half Maximum (FWHM)
fwhm <- 2.355 * sd(random_values)
print(paste("Full Width at Half Maximum:", fwhm))

# (iii) Gaussian PDF
gaussian_pdf <- function(x, mu, sigma) {
  (1 / (sigma * sqrt(2 * pi))) * exp(-0.5 * ((x - mu) / sigma)^2)
}

x_vals <- seq(-35, 50, by = 0.1)
y_vals <- gaussian_pdf(x_vals, mu = 30, sigma = 6)
plot(x_vals, y_vals, type = "l", col = "red", main = "Gaussian PDF (μ=30, σ=6)", ylab = "Density", xlab = "X")



# (i) Generate Poisson-distributed random numbers and histogram
set.seed(123)
poisson_data <- rpois(10000, lambda = 7)
hist(poisson_data, freq = FALSE, col = "lightblue", main = "Poisson Distribution (λ=7)", xlab = "Value")

# Fit a Gaussian with the same mean and standard deviation
mean_poisson <- mean(poisson_data)
sd_poisson <- sd(poisson_data)

x_vals <- seq(min(poisson_data), max(poisson_data), length = 100)
y_vals <- dnorm(x_vals, mean = mean_poisson, sd = sd_poisson)

# Overlay Gaussian curve
lines(x_vals, y_vals, col = "red", lwd = 2)
legend("topright", legend = c("Poisson", "Gaussian Fit"), col = c("blue", "red"), lwd = 2)
