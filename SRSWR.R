library(dplyr)            # convenient sampler and summariser
df <- read.csv("DataT.csv")  # path to your CSV

############################################################
# 1. تنظیم اولیه: تعداد کل و اندازه نمونه
############################################################
N <- 350     # حجم جامعه
n <- 20      # اندازه نمونه

set.seed(1234)  # برای بازتولیدپذیری

# با جایگذاری: یعنی ممکنه یک ردیف چند بار انتخاب بشه
samp_wr <- df[sample(1:N, size = n, replace = TRUE), ]

# میانگین نمونه
ybar_os <- mean(samp_wr$Y)

# واریانس نمونه (با n‑1)
s2_os   <- var(samp_wr$Y)

# تخمین واریانس میانگین نمونه
var_ybar_os_hat <- s2_os / n

# فاصله اطمینان ۹۵٪
t_crit   <- qt(0.975, df = n - 1)
margin   <- t_crit * sqrt(var_ybar_os_hat)
ci_lower <- ybar_os - margin
ci_upper <- ybar_os + margin

cat("برآورد میانگین معمولی (Ȳ_os):      ", ybar_os, "\n")
cat("برآورد واریانس (Var(Ȳ_os)):        ", var_ybar_os_hat, "\n")
cat("95% فاصله اطمینان برای μ_Y:        [", ci_lower, ", ", ci_upper, "]\n")

##################################
# تعداد دفعاتی که هر ردیف (index) انتخاب شده
tab <- table(as.integer(rownames(samp_wr)))

# تبدیل به اندیس و شمارنده
ids <- as.integer(names(tab))     # شماره ردیف‌ها (index در df)
freqs <- as.integer(tab)          # تعداد تکرار هر ردیف

# محاسبه صورت کسر: مجموع وزن‌ها × مقدار Y
numerator <- sum(df$Y[ids] * freqs)

# محاسبه مخرج طبق فرمول
denominator <- n * (1 - 1/n)^n

# برآوردگر Ȳⁿˢ
ybar_ns <- numerator / denominator

# نمایش نتیجه
cat("برآوردگر دوم (Ȳⁿˢ):                 ", ybar_ns, "\n")

##################################
# یکتا کردن ردیف‌های انتخاب شده
samp_unique <- samp_wr %>% distinct()

ybar_alt <- mean(samp_unique$Y)
cat("برآوردگر سوم (Ȳ_alt):               ", ybar_alt, "\n")

