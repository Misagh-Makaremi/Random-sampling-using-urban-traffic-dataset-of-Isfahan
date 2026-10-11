############################################################
# 1. Load data and required package
############################################################
# If you don’t already have dplyr installed, run: install.packages("dplyr")
library(dplyr)            # convenient sampler and summariser
N  <- 350                 # known population size
df <- read.csv("DataT.csv")  # path to your CSV

# Explanation:
# - We keep N as a separate object because we’ll need it for the finite‑population correction (FPC).
# - dplyr::sample_n() handles sampling without replacement by default.

############################################################
# 2. Fix a random seed for reproducibility
############################################################
set.seed(1234)

# Explanation:
# - Setting the seed means that every time you rerun the code
#   you’ll draw the same 20 streets – useful when writing a report.

############################################################
# 3. Draw an SRSWOR of size n = 20
############################################################
n  <- 20
samp <- sample_n(df, n, replace = FALSE)   # SRS without replacement

# Explanation:
# - replace = FALSE enforces “بدون جایگذاری”.
# - samp now contains only the 20 selected rows.

############################################################
# 4. Calculate sample statistics for Y
############################################################
# 4a. Sample mean (point estimator of μ_Y)
ybar <- mean(samp$Y)

# 4b. Sample variance s² (unbiased estimator of population variance S²)
s2   <- var(samp$Y)        # by default uses denominator (n‑1)

# 4c. Estimated variance of ȳ with FPC:
var_ybar_hat <- (1 - n/N) * s2 / n

# Explanation:
# - ybar estimates the unknown population mean μ_Y.
# - s2 is the usual unbiased estimator Σ(y_i − ȳ)² / (n − 1).
# - For SRSWOR, Var( ȳ ) = (1 − n/N) · S² / n.  
#   Since S² is unknown, we plug in s² → var_ybar_hat.

############################################################
# 5. Build a 95 % two‑sided confidence interval for μ_Y
############################################################
alpha      <- 0.05
t_crit     <- qt(1 - alpha/2, df = n - 1)       # t_{0.975,19}
margin     <- t_crit * sqrt(var_ybar_hat)
ci_lower   <- ybar - margin
ci_upper   <- ybar + margin

# Explanation:
# - We use the t‑distribution because σ² is unknown and n = 20 is small.
# - The finite‑population correction is already inside var_ybar_hat,
#   so no extra adjustment is needed in the margin.

############################################################
# 6. Show the results
############################################################
cat("Sample mean (Ȳ):                 ", ybar, "\n")
cat("Estimated Var(Ȳ):               ", var_ybar_hat, "\n")
cat("Estimated SD(Ȳ):                ", sqrt(var_ybar_hat), "\n")
cat("95% CI for population mean μ_Y: [", ci_lower, ", ", ci_upper, "]\n")

