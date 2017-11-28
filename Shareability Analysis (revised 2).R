# Load data
package_s <- read.csv("click_package_shareability.csv")
test_s <- read.csv("click_test_shareability.csv")
nugget_s <- read.csv("nugget_shareability.csv")

package_s <- data.frame(package_s)
test_s <- data.frame(test_s)
nugget_s <- data.frame(nugget_s)


# Rename variables

names(package_s) <- c("click_test_id", "click_package_id", "shares", "clicks")
names(test_s) <- c("click_test_id", "shares", "clicks")
names(nugget_s) <- c("nugget_id", "shares", "clicks")


# Compute shareability

package_s$shareability <- round(((package_s$shares/package_s$clicks)*100),2)
test_s$shareability <- round(((test_s$shares/test_s$clicks)*100),2)
nugget_s$shareability <- round(((nugget_s$shares/nugget_s$clicks)*100),2)


# Classify shareability

package_s$s_level <- ifelse(package_s$shareability < 1, "Low", ifelse(package_s$shareability >= 1 & package_s$shareability < 2.5, "Moderate", ifelse(package_s$shareability >= 2.5 & package_s$shareability < 5, "Good", ifelse(package_s$shareability >= 5, "Excellent", NA))))
test_s$s_level <- ifelse(test_s$shareability < 1, "Low", ifelse(test_s$shareability >= 1 & test_s$shareability < 2.5, "Moderate", ifelse(test_s$shareability >= 2.5 & test_s$shareability < 5, "Good", ifelse(test_s$shareability >= 5, "Excellent", NA))))
nugget_s$s_level <- ifelse(nugget_s$shareability < 1, "Low", ifelse(nugget_s$shareability >= 1 & nugget_s$shareability < 2.5, "Moderate", ifelse(nugget_s$shareability >= 2.5 & nugget_s$shareability < 5, "Good", ifelse(nugget_s$shareability >= 5, "Excellent", NA))))

package_s$s_level <- factor(package_s$s_level, levels = c("Low", "Moderate", "Good", "Excellent"), labels = c("Low", "Moderate", "Good", "Excellent"))
test_s$s_level <- factor(test_s$s_level, levels = c("Low", "Moderate", "Good", "Excellent"), labels = c("Low", "Moderate", "Good", "Excellent"))
nugget_s$s_level <- factor(nugget_s$s_level, levels = c("Low", "Moderate", "Good", "Excellent"), labels = c("Low", "Moderate", "Good", "Excellent"))


summary(package_s$s_level)
summary(test_s$s_level)
summary(nugget_s$s_level)


# Compute CIs (using Wilson's score interval)

package_s$shareability_ci_u <- round(((1/(1+((1/package_s$clicks)*1.96^2)))*((package_s$shareability/100)+(1.96^2/(2*package_s$clicks))+(1.96*sqrt((((package_s$shareability/100)*(1-(package_s$shareability/100)))/package_s$clicks)+(1.96^2/(4*package_s$clicks^2)))))*100),2)
package_s$shareability_ci_l <- round(((1/(1+((1/package_s$clicks)*1.96^2)))*((package_s$shareability/100)+(1.96^2/(2*package_s$clicks))-(1.96*sqrt((((package_s$shareability/100)*(1-(package_s$shareability/100)))/package_s$clicks)+(1.96^2/(4*package_s$clicks^2)))))*100),2)

test_s$shareability_ci_u <- round(((1/(1+((1/test_s$clicks)*1.96^2)))*((test_s$shareability/100)+(1.96^2/(2*test_s$clicks))+(1.96*sqrt((((test_s$shareability/100)*(1-(test_s$shareability/100)))/test_s$clicks)+(1.96^2/(4*test_s$clicks^2)))))*100),2)
test_s$shareability_ci_l <- round(((1/(1+((1/test_s$clicks)*1.96^2)))*((test_s$shareability/100)+(1.96^2/(2*test_s$clicks))-(1.96*sqrt((((test_s$shareability/100)*(1-(test_s$shareability/100)))/test_s$clicks)+(1.96^2/(4*test_s$clicks^2)))))*100),2)

nugget_s$shareability_ci_u <- round(((1/(1+((1/nugget_s$clicks)*1.96^2)))*((nugget_s$shareability/100)+(1.96^2/(2*nugget_s$clicks))+(1.96*sqrt((((nugget_s$shareability/100)*(1-(nugget_s$shareability/100)))/nugget_s$clicks)+(1.96^2/(4*nugget_s$clicks^2)))))*100),2)
nugget_s$shareability_ci_l <- round(((1/(1+((1/nugget_s$clicks)*1.96^2)))*((nugget_s$shareability/100)+(1.96^2/(2*nugget_s$clicks))-(1.96*sqrt((((nugget_s$shareability/100)*(1-(nugget_s$shareability/100)))/nugget_s$clicks)+(1.96^2/(4*nugget_s$clicks^2)))))*100),2)



package_s$shareability_se <- round(((sqrt(((package_s$shareability/100)*(1-(package_s$shareability/100)))/package_s$clicks))*100),2)
test_s$shareability_se <- round(((sqrt(((test_s$shareability/100)*(1-(test_s$shareability/100)))/test_s$clicks))*100),2)
nugget_s$shareability_se <- round(((sqrt(((nugget_s$shareability/100)*(1-(nugget_s$shareability/100)))/nugget_s$clicks))*100),2)


# Winsorize data

package_s$shares_w <- ifelse(package_s$shares >= 5, 5, package_s$shares)
package_s$clicks_w <- ifelse(package_s$clicks >= 100, 100, package_s$clicks)
package_s$shareability_w <- ifelse(package_s$shareability >= 10, 10, package_s$shareability)

test_s$shares_w <- ifelse(test_s$shares >= 10, 10, test_s$shares)
test_s$clicks_w <- ifelse(test_s$clicks >= 300, 300, test_s$clicks)
test_s$shareability_w <- ifelse(test_s$shareability >= 10, 10, test_s$shareability)

nugget_s$shares_w <- ifelse(nugget_s$shares >= 50, 50, nugget_s$shares)
nugget_s$clicks_w <- ifelse(nugget_s$clicks >= 1000, 1000, nugget_s$clicks)
nugget_s$shareability_w <- ifelse(nugget_s$shareability >= 10, 10, nugget_s$shareability)


# Histograms

hist_package_shares <- hist(package_s$shares_w, freq = FALSE, breaks=seq(0,5,by=1), xlab = "# Shares (per Package)", ylab = "Density", main = "# Shares per Click Package")
hist_package_clicks <- hist(package_s$clicks_w, freq = FALSE, breaks=seq(0,100,by=10), xlab = "# Clicks (per Package)", ylab = "Density", main = "# Clicks per Click Package")
hist_package_shareability <- hist(package_s$shareability_w, freq = FALSE, breaks=seq(0,10,by=1), xlab = "Shareability (per Package)", ylab = "Density", main = "Shareability per Click Package")


hist_test_shares <- hist(test_s$shares_w, freq = FALSE, breaks=seq(0,10,by=2), xlab = "# Shares (per Test)", ylab = "Density", main = "# Shares per Clickability Test")
hist_test_clicks <- hist(test_s$clicks_w, freq = FALSE, breaks=seq(0,300,by=50), xlab = "# Clicks (per Test)", ylab = "Density", main = "# Clicks per Clickability Test")
hist_test_shareability <- hist(test_s$shareability_w, freq = FALSE, breaks=seq(0,10,by=1), xlab = "Shareability (per Test)", ylab = "Density", main = "Shareability per Clickability Test")


hist_nugget_shares <- hist(nugget_s$shares_w, freq = FALSE, breaks=seq(0,50,by=10), xlab = "# Shares (per Nugget)", ylab = "Density", main = "# Shares per Nugget")
hist_nugget_clicks <- hist(nugget_s$clicks_w, freq = FALSE, breaks=seq(0,1000,by=200), xlab = "# Clicks (per Nugget)", ylab = "Density", main = "# Clicks per Nugget")
hist_nugget_shareability <- hist(nugget_s$shareability_w, freq = FALSE, breaks=seq(0,10,by=1), xlab = "Shareability (per Nugget)", ylab = "Density", main = "Shareability per Nugget")



# Scatterplots

ggplot(package_s, aes(clicks, shares)) + geom_point(size = 1) + geom_smooth(method = "lm")
ggplot(test_s, aes(clicks, shares)) + geom_point(size = 1) + geom_smooth(method = "lm")
ggplot(nugget_s, aes(clicks, shares)) + geom_point(size = 1) + geom_smooth(method = "lm")


ggplot(package_s, aes(clicks, shareability)) + geom_point(size = 1) + geom_smooth(method = "lm")
ggplot(test_s, aes(clicks, shareability)) + geom_point(size = 1) + geom_smooth(method = "lm")
ggplot(nugget_s, aes(clicks, shareability)) + geom_point(size = 1) + geom_smooth(method = "lm")


# Filter out cases with 0 clicks

library("dplyr")

package_s.f <- filter(package_s, clicks != 0)
test_s.f <- filter(test_s, clicks != 0)
nugget_s.f <- filter(nugget_s, clicks != 0)

# Classify reliable shareability metrics

package_s.f$reliable <- ifelse(package_s.f$s_level == "Low" & (package_s.f$shareability_ci_l < 0 | package_s.f$shareability_ci_u >= 1),0,ifelse(package_s.f$s_level == "Moderate" & (package_s.f$shareability_ci_l < 1 | package_s.f$shareability_ci_u >= 2.5),0,ifelse(package_s.f$s_level == "Good" & (package_s.f$shareability_ci_l < 2.5 | package_s.f$shareability_ci_u >= 5),0,ifelse(package_s.f$s_level == "Excellent" & package_s.f$shareability_ci_l < 5,0,1))))

test_s.f$reliable <- ifelse(test_s.f$s_level == "Low" & (test_s.f$shareability_ci_l < 0 | test_s.f$shareability_ci_u >= 1),0,ifelse(test_s.f$s_level == "Moderate" & (test_s.f$shareability_ci_l < 1 | test_s.f$shareability_ci_u >= 2.5),0,ifelse(test_s.f$s_level == "Good" & (test_s.f$shareability_ci_l < 2.5 | test_s.f$shareability_ci_u >= 5),0,ifelse(test_s.f$s_level == "Excellent" & test_s.f$shareability_ci_l < 5,0,1))))

nugget_s.f$reliable <- ifelse(nugget_s.f$s_level == "Low" & (nugget_s.f$shareability_ci_l < 0 | nugget_s.f$shareability_ci_u >= 1),0,ifelse(nugget_s.f$s_level == "Moderate" & (nugget_s.f$shareability_ci_l < 1 | nugget_s.f$shareability_ci_u >= 2.5),0,ifelse(nugget_s.f$s_level == "Good" & (nugget_s.f$shareability_ci_l < 2.5 | nugget_s.f$shareability_ci_u >= 5),0,ifelse(nugget_s.f$s_level == "Excellent" & nugget_s.f$shareability_ci_l < 5,0,1))))


package_s.f$reliable2 <- ifelse(package_s.f$s_level == "Low" & (package_s.f$shareability_ci_l < 0 | package_s.f$shareability_ci_u >= 2.5),0,ifelse(package_s.f$s_level == "Moderate" & (package_s.f$shareability_ci_l < 0 | package_s.f$shareability_ci_u >= 5),0,ifelse(package_s.f$s_level == "Good" & (package_s.f$shareability_ci_l < 1 | package_s.f$shareability_ci_u >= 10),0,ifelse(package_s.f$s_level == "Excellent" & package_s.f$shareability_ci_l <= 2.5,0,1))))

test_s.f$reliable2 <- ifelse(test_s.f$s_level == "Low" & (test_s.f$shareability_ci_l < 0 | test_s.f$shareability_ci_u >= 2.5),0,ifelse(test_s.f$s_level == "Moderate" & (test_s.f$shareability_ci_l < 0 | test_s.f$shareability_ci_u >= 5),0,ifelse(test_s.f$s_level == "Good" & (test_s.f$shareability_ci_l < 1 | test_s.f$shareability_ci_u >= 10),0,ifelse(test_s.f$s_level == "Excellent" & test_s.f$shareability_ci_l <= 2.5,0,1))))

nugget_s.f$reliable2 <- ifelse(nugget_s.f$s_level == "Low" & (nugget_s.f$shareability_ci_l < 0 | nugget_s.f$shareability_ci_u >= 2.5),0,ifelse(nugget_s.f$s_level == "Moderate" & (nugget_s.f$shareability_ci_l < 0 | nugget_s.f$shareability_ci_u >= 5),0,ifelse(nugget_s.f$s_level == "Good" & (nugget_s.f$shareability_ci_l < 1 | nugget_s.f$shareability_ci_u >= 10),0,ifelse(nugget_s.f$s_level == "Excellent" & nugget_s.f$shareability_ci_l <= 2.5,0,1))))


# Factorize reliability variable

package_s.f$reliable <- factor(package_s.f$reliable, levels = c(0,1), labels = c("Unreliable", "Reliable"))
test_s.f$reliable <- factor(test_s.f$reliable, levels = c(0,1), labels = c("Unreliable", "Reliable"))
nugget_s.f$reliable <- factor(nugget_s.f$reliable, levels = c(0,1), labels = c("Unreliable", "Reliable"))

package_s.f$reliable2 <- factor(package_s.f$reliable2, levels = c(0,1), labels = c("Unreliable", "Reliable"))
test_s.f$reliable2 <- factor(test_s.f$reliable2, levels = c(0,1), labels = c("Unreliable", "Reliable"))
nugget_s.f$reliable2 <- factor(nugget_s.f$reliable2, levels = c(0,1), labels = c("Unreliable", "Reliable"))


# Run logit models

logit_package <- glm(reliable ~ clicks, family = "binomial", data = package_s.f)
summary(logit_package)

logit_test <- glm(reliable ~ clicks, family = "binomial", data = test_s.f)
summary(logit_test)

logit_nugget <- glm(reliable ~ clicks, family = "binomial", data = nugget_s.f)
summary(logit_nugget)


logit_package2 <- glm(reliable2 ~ clicks, family = "binomial", data = package_s.f)
summary(logit_package2)

logit_test2 <- glm(reliable2 ~ clicks, family = "binomial", data = test_s.f)
summary(logit_test2)

logit_nugget2 <- glm(reliable2 ~ clicks, family = "binomial", data = nugget_s.f)
summary(logit_nugget2)



# Prepare for plotting

package_s.f$reliable_num <- revalue(package_s.f$reliable, c("Unreliable" = 0, "Reliable" = 1))
test_s.f$reliable_num <- revalue(test_s.f$reliable, c("Unreliable" = 0, "Reliable" = 1))
nugget_s.f$reliable_num <- revalue(nugget_s.f$reliable, c("Unreliable" = 0, "Reliable" = 1))


package_s.f$reliable_num <- as.numeric(as.character(package_s.f$reliable_num))
test_s.f$reliable_num <- as.numeric(as.character(test_s.f$reliable_num))
nugget_s.f$reliable_num <- as.numeric(as.character(nugget_s.f$reliable_num))





package_s.f$reliable_num2 <- revalue(package_s.f$reliable2, c("Unreliable" = 0, "Reliable" = 1))
test_s.f$reliable_num2 <- revalue(test_s.f$reliable2, c("Unreliable" = 0, "Reliable" = 1))
nugget_s.f$reliable_num2 <- revalue(nugget_s.f$reliable2, c("Unreliable" = 0, "Reliable" = 1))


package_s.f$reliable_num2 <- as.numeric(as.character(package_s.f$reliable_num2))
test_s.f$reliable_num2 <- as.numeric(as.character(test_s.f$reliable_num2))
nugget_s.f$reliable_num2 <- as.numeric(as.character(nugget_s.f$reliable_num2))


# Plot logit models

logit_package <- ggplot(package_s.f, aes(x=clicks, y=reliable_num)) + geom_point() + stat_smooth(method="glm", family="binomial", se=F, aes(y=reliable_num)) + xlab("# Clicks") + ylab("Probability of Reliable Shareability Metric") + ggtitle("Probability of Reliable Shareability Metric as Function of # Clicks (Package-Level)")
logit_package

logit_test <- ggplot(test_s.f, aes(x=clicks, y=reliable_num)) + geom_point() + stat_smooth(method="glm", family="binomial", se=F, aes(y=reliable_num)) + xlab("# Clicks") + ylab("Probability of Reliable Shareability Metric") + ggtitle("Probability of Reliable Shareability Metric as Function of # Clicks (Test-Level)")
logit_test

logit_nugget <- ggplot(nugget_s.f, aes(x=clicks, y=reliable_num)) + geom_point() + stat_smooth(method="glm", family="binomial", se=F, aes(y=reliable_num)) + xlab("# Clicks") + ylab("Probability of Reliable Shareability Metric") + ggtitle("Probability of Reliable Shareability Metric as Function of # Clicks (Nugget-Level)")
logit_nugget




logit_package2 <- ggplot(package_s.f, aes(x=clicks, y=reliable_num2)) + geom_point() + stat_smooth(method="glm", family="binomial", se=F, aes(y=reliable_num2)) + xlab("# Clicks") + ylab("Probability of Reliable Shareability Metric") + ggtitle("Probability of Reliable Shareability Metric as Function of # Clicks (Package-Level)")
logit_package2

logit_test2 <- ggplot(test_s.f, aes(x=clicks, y=reliable_num2)) + geom_point() + stat_smooth(method="glm", family="binomial", se=F, aes(y=reliable_num2)) + xlab("# Clicks") + ylab("Probability of Reliable Shareability Metric") + ggtitle("Probability of Reliable Shareability Metric as Function of # Clicks (Test-Level)")
logit_test2

logit_nugget2 <- ggplot(nugget_s.f, aes(x=clicks, y=reliable_num2)) + geom_point() + stat_smooth(method="glm", family="binomial", se=F, aes(y=reliable_num2)) + coord_cartesian(xlim=c(0,500)) + xlab("# Clicks") + ylab("Probability of Reliable Shareability Metric") + ggtitle("Probability of Reliable Shareability Metric as Function of # Clicks (Nugget-Level)")
logit_nugget2





# Try 90% CIs


# Compute CIs (using Wilson's score interval)

package_s$shareability_ci90_u <- round(((1/(1+((1/package_s$clicks)*1.64^2)))*((package_s$shareability/100)+(1.64^2/(2*package_s$clicks))+(1.64*sqrt((((package_s$shareability/100)*(1-(package_s$shareability/100)))/package_s$clicks)+(1.64^2/(4*package_s$clicks^2)))))*100),2)
package_s$shareability_ci90_l <- round(((1/(1+((1/package_s$clicks)*1.64^2)))*((package_s$shareability/100)+(1.64^2/(2*package_s$clicks))-(1.64*sqrt((((package_s$shareability/100)*(1-(package_s$shareability/100)))/package_s$clicks)+(1.64^2/(4*package_s$clicks^2)))))*100),2)

test_s$shareability_ci90_u <- round(((1/(1+((1/test_s$clicks)*1.64^2)))*((test_s$shareability/100)+(1.64^2/(2*test_s$clicks))+(1.64*sqrt((((test_s$shareability/100)*(1-(test_s$shareability/100)))/test_s$clicks)+(1.64^2/(4*test_s$clicks^2)))))*100),2)
test_s$shareability_ci90_l <- round(((1/(1+((1/test_s$clicks)*1.64^2)))*((test_s$shareability/100)+(1.64^2/(2*test_s$clicks))-(1.64*sqrt((((test_s$shareability/100)*(1-(test_s$shareability/100)))/test_s$clicks)+(1.64^2/(4*test_s$clicks^2)))))*100),2)

nugget_s$shareability_ci90_u <- round(((1/(1+((1/nugget_s$clicks)*1.64^2)))*((nugget_s$shareability/100)+(1.64^2/(2*nugget_s$clicks))+(1.64*sqrt((((nugget_s$shareability/100)*(1-(nugget_s$shareability/100)))/nugget_s$clicks)+(1.64^2/(4*nugget_s$clicks^2)))))*100),2)
nugget_s$shareability_ci90_l <- round(((1/(1+((1/nugget_s$clicks)*1.64^2)))*((nugget_s$shareability/100)+(1.64^2/(2*nugget_s$clicks))-(1.64*sqrt((((nugget_s$shareability/100)*(1-(nugget_s$shareability/100)))/nugget_s$clicks)+(1.64^2/(4*nugget_s$clicks^2)))))*100),2)

# Filter out cases with 0 clicks

library("dplyr")

package_s.f <- filter(package_s, clicks != 0)
test_s.f <- filter(test_s, clicks != 0)
nugget_s.f <- filter(nugget_s, clicks != 0)

# Classify reliable shareability metrics

package_s.f$reliable90a <- ifelse(package_s.f$s_level == "Low" & (package_s.f$shareability_ci90_l < 0 | package_s.f$shareability_ci90_u >= 1),0,ifelse(package_s.f$s_level == "Moderate" & (package_s.f$shareability_ci90_l < 1 | package_s.f$shareability_ci90_u >= 2.5),0,ifelse(package_s.f$s_level == "Good" & (package_s.f$shareability_ci90_l < 2.5 | package_s.f$shareability_ci90_u >= 5),0,ifelse(package_s.f$s_level == "Excellent" & package_s.f$shareability_ci90_l < 5,0,1))))

test_s.f$reliable90a <- ifelse(test_s.f$s_level == "Low" & (test_s.f$shareability_ci90_l < 0 | test_s.f$shareability_ci90_u >= 1),0,ifelse(test_s.f$s_level == "Moderate" & (test_s.f$shareability_ci90_l < 1 | test_s.f$shareability_ci90_u >= 2.5),0,ifelse(test_s.f$s_level == "Good" & (test_s.f$shareability_ci90_l < 2.5 | test_s.f$shareability_ci90_u >= 5),0,ifelse(test_s.f$s_level == "Excellent" & test_s.f$shareability_ci90_l < 5,0,1))))

nugget_s.f$reliable90a <- ifelse(nugget_s.f$s_level == "Low" & (nugget_s.f$shareability_ci90_l < 0 | nugget_s.f$shareability_ci90_u >= 1),0,ifelse(nugget_s.f$s_level == "Moderate" & (nugget_s.f$shareability_ci90_l < 1 | nugget_s.f$shareability_ci90_u >= 2.5),0,ifelse(nugget_s.f$s_level == "Good" & (nugget_s.f$shareability_ci90_l < 2.5 | nugget_s.f$shareability_ci90_u >= 5),0,ifelse(nugget_s.f$s_level == "Excellent" & nugget_s.f$shareability_ci90_l < 5,0,1))))


package_s.f$reliable90b <- ifelse(package_s.f$s_level == "Low" & (package_s.f$shareability_ci90_l < 0 | package_s.f$shareability_ci90_u >= 2.5),0,ifelse(package_s.f$s_level == "Moderate" & (package_s.f$shareability_ci90_l < 0 | package_s.f$shareability_ci90_u >= 5),0,ifelse(package_s.f$s_level == "Good" & (package_s.f$shareability_ci90_l < 1 | package_s.f$shareability_ci90_u >= 10),0,ifelse(package_s.f$s_level == "Excellent" & package_s.f$shareability_ci90_l <= 2.5,0,1))))

test_s.f$reliable90b <- ifelse(test_s.f$s_level == "Low" & (test_s.f$shareability_ci90_l < 0 | test_s.f$shareability_ci90_u >= 2.5),0,ifelse(test_s.f$s_level == "Moderate" & (test_s.f$shareability_ci90_l < 0 | test_s.f$shareability_ci90_u >= 5),0,ifelse(test_s.f$s_level == "Good" & (test_s.f$shareability_ci90_l < 1 | test_s.f$shareability_ci90_u >= 10),0,ifelse(test_s.f$s_level == "Excellent" & test_s.f$shareability_ci90_l <= 2.5,0,1))))

nugget_s.f$reliable90b <- ifelse(nugget_s.f$s_level == "Low" & (nugget_s.f$shareability_ci90_l < 0 | nugget_s.f$shareability_ci90_u >= 2.5),0,ifelse(nugget_s.f$s_level == "Moderate" & (nugget_s.f$shareability_ci90_l < 0 | nugget_s.f$shareability_ci90_u >= 5),0,ifelse(nugget_s.f$s_level == "Good" & (nugget_s.f$shareability_ci90_l < 1 | nugget_s.f$shareability_ci90_u >= 10),0,ifelse(nugget_s.f$s_level == "Excellent" & nugget_s.f$shareability_ci90_l <= 2.5,0,1))))


# Factorize reliability variable

package_s.f$reliable90a <- factor(package_s.f$reliable90a, levels = c(0,1), labels = c("Unreliable", "Reliable"))
test_s.f$reliable90a <- factor(test_s.f$reliable90a, levels = c(0,1), labels = c("Unreliable", "Reliable"))
nugget_s.f$reliable90a <- factor(nugget_s.f$reliable90a, levels = c(0,1), labels = c("Unreliable", "Reliable"))

package_s.f$reliable90b <- factor(package_s.f$reliable90b, levels = c(0,1), labels = c("Unreliable", "Reliable"))
test_s.f$reliable90b <- factor(test_s.f$reliable90b, levels = c(0,1), labels = c("Unreliable", "Reliable"))
nugget_s.f$reliable90b <- factor(nugget_s.f$reliable90b, levels = c(0,1), labels = c("Unreliable", "Reliable"))


# Run logit models

logit_package_90a <- glm(reliable90a ~ clicks, family = "binomial", data = package_s.f)
summary(logit_package_90a)

logit_test_90a <- glm(reliable90a ~ clicks, family = "binomial", data = test_s.f)
summary(logit_test_90a)

logit_nugget_90a <- glm(reliable90a ~ clicks, family = "binomial", data = nugget_s.f)
summary(logit_nugget_90a)


logit_package_90b <- glm(reliable90b ~ clicks, family = "binomial", data = package_s.f)
summary(logit_package_90b)

logit_test_90b <- glm(reliable90b ~ clicks, family = "binomial", data = test_s.f)
summary(logit_test_90b)

logit_nugget_90b <- glm(reliable90b ~ clicks, family = "binomial", data = nugget_s.f)
summary(logit_nugget_90b)



# Prepare for plotting

package_s.f$reliable90a_num <- revalue(package_s.f$reliable90a, c("Unreliable" = 0, "Reliable" = 1))
test_s.f$reliable90a_num <- revalue(test_s.f$reliable90a, c("Unreliable" = 0, "Reliable" = 1))
nugget_s.f$reliable90a_num <- revalue(nugget_s.f$reliable90a, c("Unreliable" = 0, "Reliable" = 1))


package_s.f$reliable90a_num <- as.numeric(as.character(package_s.f$reliable90a_num))
test_s.f$reliable90a_num <- as.numeric(as.character(test_s.f$reliable90a_num))
nugget_s.f$reliable90a_num <- as.numeric(as.character(nugget_s.f$reliable90a_num))





package_s.f$reliable90b_num <- revalue(package_s.f$reliable90b, c("Unreliable" = 0, "Reliable" = 1))
test_s.f$reliable90b_num <- revalue(test_s.f$reliable90b, c("Unreliable" = 0, "Reliable" = 1))
nugget_s.f$reliable90b_num <- revalue(nugget_s.f$reliable90b, c("Unreliable" = 0, "Reliable" = 1))


package_s.f$reliable90b_num <- as.numeric(as.character(package_s.f$reliable90b_num))
test_s.f$reliable90b_num <- as.numeric(as.character(test_s.f$reliable90b_num))
nugget_s.f$reliable90b_num <- as.numeric(as.character(nugget_s.f$reliable90b_num))


# Plot logit models

logit_package_90a <- ggplot(package_s.f, aes(x=clicks, y=reliable90a_num)) + geom_point() + stat_smooth(method="glm", family="binomial", se=F, aes(y=reliable90a_num)) + xlab("# Clicks") + ylab("Probability of Reliable Shareability Metric") + ggtitle("Probability of Reliable Shareability Metric as Function of # Clicks (Package-Level)")
logit_package_90a

logit_test_90a <- ggplot(test_s.f, aes(x=clicks, y=reliable90a_num)) + geom_point() + stat_smooth(method="glm", family="binomial", se=F, aes(y=reliable90a_num)) + xlab("# Clicks") + ylab("Probability of Reliable Shareability Metric") + ggtitle("Probability of Reliable Shareability Metric as Function of # Clicks (Test-Level)")
logit_test_90a

logit_nugget_90a <- ggplot(nugget_s.f, aes(x=clicks, y=reliable90a_num)) + geom_point() + stat_smooth(method="glm", family="binomial", se=F, aes(y=reliable90a_num)) + xlab("# Clicks") + ylab("Probability of Reliable Shareability Metric") + ggtitle("Probability of Reliable Shareability Metric as Function of # Clicks (Nugget-Level)")
logit_nugget_90a




logit_package_90b <- ggplot(package_s.f, aes(x=clicks, y=reliable90b_num)) + geom_point() + stat_smooth(method="glm", family="binomial", se=F, aes(y=reliable90b_num)) + xlab("# Clicks") + ylab("Probability of Reliable Shareability Metric") + ggtitle("Probability of Reliable Shareability Metric as Function of # Clicks (Package-Level)")
logit_package_90b

logit_test_90b <- ggplot(test_s.f, aes(x=clicks, y=reliable90b_num)) + geom_point() + stat_smooth(method="glm", family="binomial", se=F, aes(y=reliable90b_num)) + coord_cartesian(xlim=c(0,300)) + xlab("# Clicks") + ylab("Probability of Reliable Shareability Metric") + ggtitle("Probability of Reliable Shareability Metric as Function of # Clicks (Test-Level)")
logit_test_90b

logit_nugget_90b <- ggplot(nugget_s.f, aes(x=clicks, y=reliable90b_num)) + geom_point() + stat_smooth(method="glm", family="binomial", se=F, aes(y=reliable90b_num)) + coord_cartesian(xlim=c(0,300)) + xlab("# Clicks") + ylab("Probability of Reliable Shareability Metric") + ggtitle("Probability of Reliable Shareability Metric as Function of # Clicks (Nugget-Level)")
logit_nugget_90b

# Connected Scatterplot
# ln(y) = -3.027081 + .029007x

prob <- c(.10, .20, .30, .40, .50, .60, .70, .80, .90)
clicks <- c(29, 57, 75, 90, 104, 118, 134, 152, 180)

prob_clicks <- cbind(clicks, prob)
prob_clicks <- data.frame(prob_clicks)

at_x <- seq(from = 0, to = 200, by = 25)
at_y <- seq(from = 0, to = 1, by = 0.1)
plot(prob_clicks$clicks, prob_clicks$prob, xlab = "Number of Clicks", ylab = "Probability of Reliable Shareability Metric", axes = FALSE)
axis(side = 1, at = at_x)
axis(side = 2, at = at_y)
lines(prob_clicks$clicks, prob_clicks$prob)



# Follow-up Analysis

# First attempt: read data from SQL query

que <- "with nuggets_to_include as (
  SELECT
distinct nugget_id
FROM 
looker_scratch.LR$TWR1ZUVFN7CEQ2DGKTSKE_bcs_clickability
join clickability_tests ct on ct._id = clickability_test_id
WHERE
test_number = 1),
test_data as (
SELECT 
nuggets.nugget_id,
clickability_tests._id AS clickability_test_id,
bcs_clickability.test_number AS test_number,
(COALESCE(SUM(social_data_cumulative.package_shares),0)) AS test_shares,
COALESCE(SUM(click_package_summary.clicks),0) AS test_clicks
FROM click_packages
JOIN clickability_tests ON click_packages.clickability_test_id = clickability_tests._id
JOIN nuggets_to_include as nuggets ON clickability_tests.nugget_id = nuggets.nugget_id
LEFT JOIN looker_scratch.LR$TW2PP4VGUT67JWEMSV0UB_social_data_cumulative AS social_data_cumulative ON social_data_cumulative.click_package_id = click_packages._id
LEFT JOIN looker_scratch.LR$TWR1ZUVFN7CEQ2DGKTSKE_bcs_clickability AS bcs_clickability ON click_packages._id = bcs_clickability.click_package_id
LEFT JOIN looker_scratch.LR$TWOPZU2NP1M7EL4QYDL9_click_package_summary AS click_package_summary ON click_packages._id = click_package_summary.click_package_id
WHERE 
bcs_clickability.test_number >= 1
GROUP BY 1,2,3
)
SELECT
nugget_id,
clickability_test_id as test_id,
test_number as test_num,
test_shares as shares,
test_clicks as clicks,
100.0 * test_shares/NULLIF(test_clicks, 0) as shareability,
SUM(test_shares) OVER (partition by nugget_id order by test_number rows unbounded preceding) as cum_shares,
SUM(test_clicks) OVER (partition by nugget_id order by test_number rows unbounded preceding) as cum_clicks,
100.0 * SUM(test_shares) OVER (partition by nugget_id order by test_number rows unbounded preceding)/
NULLIF(SUM(test_clicks) OVER (partition by nugget_id order by test_number rows unbounded preceding), 0) as cum_shareability
FROM
test_data"

rs<-dbSendQuery(rsreadcon,que)
s_cat <- fetch(rs, n=-1)

# Filter tests with 0 clicks (cum)

s_cat <- filter(s_cat, cum_clicks >=1)

# Classify shareability

s_cat$s_level <- ifelse(s_cat$cum_shareability < 1, "Low", ifelse(s_cat$cum_shareability >= 1 & s_cat$cum_shareability < 2.5, "Moderate", ifelse(s_cat$cum_shareability >= 2.5 & s_cat$cum_shareability < 5, "Good", ifelse(s_cat$cum_shareability >= 5, "Excellent", NA))))

s_cat$s_level <- factor(s_cat$s_level, levels = c("Low", "Moderate", "Good", "Excellent"), labels = c("Low", "Moderate", "Good", "Excellent"))

s_cat <- filter(s_cat, !is.na(s_cat$s_level))

# Comoute 90% CIs

s_cat$ci_u <- round(((1/(1+((1/s_cat$cum_clicks)*1.64^2)))*((s_cat$cum_shareability/100)+(1.64^2/(2*s_cat$cum_clicks))+(1.64*sqrt((((s_cat$cum_shareability/100)*(1-(s_cat$cum_shareability/100)))/s_cat$cum_clicks)+(1.64^2/(4*s_cat$cum_clicks^2)))))*100),2)
s_cat$ci_l <- round(((1/(1+((1/s_cat$cum_clicks)*1.64^2)))*((s_cat$cum_shareability/100)+(1.64^2/(2*s_cat$cum_clicks))-(1.64*sqrt((((s_cat$cum_shareability/100)*(1-(s_cat$cum_shareability/100)))/s_cat$cum_clicks)+(1.64^2/(4*s_cat$cum_clicks^2)))))*100),2)

s_cat$overlap <- ifelse(s_cat$s_level == "Low" & s_cat$ci_u < 1, 1,
                        ifelse(s_cat$s_level == "Low" & s_cat$ci_u < 2.5, 2,
                               ifelse(s_cat$s_level == "Low" & s_cat$ci_u < 5, 3,
                                      ifelse(s_cat$s_level == "Low" & s_cat$ci_u >= 5, 4,
                                             ifelse(s_cat$s_level == "Excellent" & s_cat$ci_l >= 5, 1,
                                                    ifelse(s_cat$s_level == "Excellent" & s_cat$ci_l >= 2.5, 2,
                                                           ifelse(s_cat$s_level == "Excellent" & s_cat$ci_l >= 1, 3,
                                                                  ifelse(s_cat$s_level == "Excellent" & s_cat$ci_l < 1, 4,
                                                                         ifelse(s_cat$s_level == "Moderate" & s_cat$ci_l >= 1 & s_cat$ci_u < 2.5, 1,
                                                                                ifelse(s_cat$s_level == "Moderate" & s_cat$ci_l < 1 & s_cat$ci_u < 2.5, 2,
                                                                                       ifelse(s_cat$s_level == "Moderate" & s_cat$ci_l >= 1 & s_cat$ci_u >= 2.5 & s_cat$ci_u < 5, 2,
                                                                                              ifelse(s_cat$s_level == "Moderate" & s_cat$ci_l < 1 & s_cat$ci_u >= 2.5 & s_cat$ci_u < 5, 3,
                                                                                                     ifelse(s_cat$s_level == "Moderate" & s_cat$ci_l >= 1 & s_cat$ci_u >= 5, 3,
                                                                                                            ifelse(s_cat$s_level == "Moderate" & s_cat$ci_l < 1 & s_cat$ci_u >= 5, 4,
                                                                                                                   ifelse(s_cat$s_level == "Good" & s_cat$ci_l >= 2.5 & s_cat$ci_u < 5, 1,
                                                                                                                          ifelse(s_cat$s_level == "Good" & s_cat$ci_l >= 2.5 & s_cat$ci_u >= 5, 2,
                                                                                                                                 ifelse(s_cat$s_level == "Good" & s_cat$ci_l >= 1 & s_cat$ci_l < 2.5 & s_cat$ci_u < 5, 2,
                                                                                                                                        ifelse(s_cat$s_level == "Good" & s_cat$ci_l >= 1 & s_cat$ci_l < 2.5 & s_cat$ci_u >= 5, 3,
                                                                                                                                               ifelse(s_cat$s_level == "Good" & s_cat$ci_l < 1 & s_cat$ci_u < 5, 3,
                                                                                                                                                      ifelse(s_cat$s_level == "Good" & s_cat$ci_l < 1 & s_cat$ci_u >= 5, 4, NA))))))))))))))))))))
 
# Factorize variables

s_cat$test_num_c <- as.character(s_cat$test_num)
s_cat$test_num_c <- ifelse(s_cat$test_num > 4, "> 4", s_cat$test_num_c)

s_cat$test_num_f <- factor(s_cat$test_num_c, levels = c("1","2","3","4","> 4"), labels = c("1","2","3","4","> 4"))
s_cat$overlap_f <- factor(s_cat$overlap, levels = c("1","2","3","4"), labels = c("1","2","3","4"))
                                                                                                                                                                                                                                                                                                           ifelse(s_cat$s_level == "Good" & s_cat$ci_l < 1 & s_cat$ci_u >= 5, 4, NA))))))))))))))))))))
# Correlation

cor.test(s_cat$test_num, s_cat$overlap, use = "pairwise", method = "pearson")

# Stacked Bar Graph

stacked_bar <- ggplot(s_cat, aes(x = test_num_f, fill = overlap_f)) + geom_bar(position = "fill") + xlab("# of Tests") + ylab("Proportion of Nuggets") + ggtitle("# of Possible Shareability Categories as a Function of # Tests per Nugget")
stacked_bar + scale_fill_discrete(name = "# Categories")

# Crosstabs
library("gmodels")

CrossTable(s_cat$overlap_f, s_cat$test_num_f, chisq = TRUE)




# Second attempt: Read data from CSV file


s_cat_sc <- read.csv("tests_shareability.csv")
s_cat_sc <- data.frame(s_cat_sc)


# Name variables
names(s_cat_sc) <- c("nugget_id", "click_test_id", "test_num", "shares", "clicks", "shareability")

# Sort by nugget ID and test number

s_cat_sc <- s_cat_sc[order(s_cat_sc[1], s_cat_sc[3]),]

# Find first test number
s_cat_sc$min <- ave(s_cat_sc$test_num, s_cat_sc$nugget_id, FUN = function(x) x[1])

# Remove nuggets whose first test number > 1
s_cat_sc_f <- filter(s_cat_sc, min == 1)

# Remove last column
s_cat_sc_f <- subset(s_cat_sc_f, select = -c(7))

# Get cumulative shares and clicks

s_cat_sc_f$cum_shares <- ave(s_cat_sc_f$shares, s_cat_sc_f$nugget_id, FUN = function(x) {cumsum(x)})
s_cat_sc_f$cum_clicks <- ave(s_cat_sc_f$clicks, s_cat_sc_f$nugget_id, FUN = function(x) {cumsum(x)})

# Compute cumulative shareability

s_cat_sc_f$cum_shareability <- (s_cat_sc_f$cum_shares/s_cat_sc_f$cum_clicks)*100

# Filter tests with 0 clicks (cum)

s_cat_sc_f <- filter(s_cat_sc_f, cum_clicks >=1)

# Classify shareability

s_cat_sc_f$s_level <- ifelse(s_cat_sc_f$cum_shareability < 1, "Low", ifelse(s_cat_sc_f$cum_shareability >= 1 & s_cat_sc_f$cum_shareability < 2.5, "Moderate", ifelse(s_cat_sc_f$cum_shareability >= 2.5 & s_cat_sc_f$cum_shareability < 5, "Good", ifelse(s_cat_sc_f$cum_shareability >= 5, "Excellent", NA))))

s_cat_sc_f$s_level <- factor(s_cat_sc_f$s_level, levels = c("Low", "Moderate", "Good", "Excellent"), labels = c("Low", "Moderate", "Good", "Excellent"))

s_cat_sc_f <- filter(s_cat_sc_f, !is.na(s_cat_sc_f$s_level))

# Comoute 90% CIs

s_cat_sc_f$ci_u <- round(((1/(1+((1/s_cat_sc_f$cum_clicks)*1.64^2)))*((s_cat_sc_f$cum_shareability/100)+(1.64^2/(2*s_cat_sc_f$cum_clicks))+(1.64*sqrt((((s_cat_sc_f$cum_shareability/100)*(1-(s_cat_sc_f$cum_shareability/100)))/s_cat_sc_f$cum_clicks)+(1.64^2/(4*s_cat_sc_f$cum_clicks^2)))))*100),2)
s_cat_sc_f$ci_l <- round(((1/(1+((1/s_cat_sc_f$cum_clicks)*1.64^2)))*((s_cat_sc_f$cum_shareability/100)+(1.64^2/(2*s_cat_sc_f$cum_clicks))-(1.64*sqrt((((s_cat_sc_f$cum_shareability/100)*(1-(s_cat_sc_f$cum_shareability/100)))/s_cat_sc_f$cum_clicks)+(1.64^2/(4*s_cat_sc_f$cum_clicks^2)))))*100),2)

s_cat_sc_f$overlap <- ifelse(s_cat_sc_f$s_level == "Low" & s_cat_sc_f$ci_u < 1, 1,
                        ifelse(s_cat_sc_f$s_level == "Low" & s_cat_sc_f$ci_u < 2.5, 2,
                               ifelse(s_cat_sc_f$s_level == "Low" & s_cat_sc_f$ci_u < 5, 3,
                                      ifelse(s_cat_sc_f$s_level == "Low" & s_cat_sc_f$ci_u >= 5, 4,
                                             ifelse(s_cat_sc_f$s_level == "Excellent" & s_cat_sc_f$ci_l >= 5, 1,
                                                    ifelse(s_cat_sc_f$s_level == "Excellent" & s_cat_sc_f$ci_l >= 2.5, 2,
                                                           ifelse(s_cat_sc_f$s_level == "Excellent" & s_cat_sc_f$ci_l >= 1, 3,
                                                                  ifelse(s_cat_sc_f$s_level == "Excellent" & s_cat_sc_f$ci_l < 1, 4,
                                                                         ifelse(s_cat_sc_f$s_level == "Moderate" & s_cat_sc_f$ci_l >= 1 & s_cat_sc_f$ci_u < 2.5, 1,
                                                                                ifelse(s_cat_sc_f$s_level == "Moderate" & s_cat_sc_f$ci_l < 1 & s_cat_sc_f$ci_u < 2.5, 2,
                                                                                       ifelse(s_cat_sc_f$s_level == "Moderate" & s_cat_sc_f$ci_l >= 1 & s_cat_sc_f$ci_u >= 2.5 & s_cat_sc_f$ci_u < 5, 2,
                                                                                              ifelse(s_cat_sc_f$s_level == "Moderate" & s_cat_sc_f$ci_l < 1 & s_cat_sc_f$ci_u >= 2.5 & s_cat_sc_f$ci_u < 5, 3,
                                                                                                     ifelse(s_cat_sc_f$s_level == "Moderate" & s_cat_sc_f$ci_l >= 1 & s_cat_sc_f$ci_u >= 5, 3,
                                                                                                            ifelse(s_cat_sc_f$s_level == "Moderate" & s_cat_sc_f$ci_l < 1 & s_cat_sc_f$ci_u >= 5, 4,
                                                                                                                   ifelse(s_cat_sc_f$s_level == "Good" & s_cat_sc_f$ci_l >= 2.5 & s_cat_sc_f$ci_u < 5, 1,
                                                                                                                          ifelse(s_cat_sc_f$s_level == "Good" & s_cat_sc_f$ci_l >= 2.5 & s_cat_sc_f$ci_u >= 5, 2,
                                                                                                                                 ifelse(s_cat_sc_f$s_level == "Good" & s_cat_sc_f$ci_l >= 1 & s_cat_sc_f$ci_l < 2.5 & s_cat_sc_f$ci_u < 5, 2,
                                                                                                                                        ifelse(s_cat_sc_f$s_level == "Good" & s_cat_sc_f$ci_l >= 1 & s_cat_sc_f$ci_l < 2.5 & s_cat_sc_f$ci_u >= 5, 3,
                                                                                                                                               ifelse(s_cat_sc_f$s_level == "Good" & s_cat_sc_f$ci_l < 1 & s_cat_sc_f$ci_u < 5, 3,
                                                                                                                                                      ifelse(s_cat_sc_f$s_level == "Good" & s_cat_sc_f$ci_l < 1 & s_cat_sc_f$ci_u >= 5, 4, NA))))))))))))))))))))

# Factorize variables

s_cat_sc_f$test_num_c <- as.character(s_cat_sc_f$test_num)
s_cat_sc_f$test_num_c <- ifelse(s_cat_sc_f$test_num > 4, "> 4", s_cat_sc_f$test_num_c)

s_cat_sc_f$test_num_f <- factor(s_cat_sc_f$test_num_c, levels = c("1","2","3","4","> 4"), labels = c("1","2","3","4","> 4"))
s_cat_sc_f$overlap_f <- factor(s_cat_sc_f$overlap, levels = c("1","2","3","4"), labels = c("1","2","3","4"))


# Correlation

cor.test(s_cat_sc_f$test_num, s_cat_sc_f$overlap, use = "pairwise", method = "pearson")

# Stacked Bar Graph

stacked_bar <- ggplot(s_cat_sc_f, aes(x = test_num_f, fill = overlap_f)) + geom_bar(position = "fill") + xlab("# of Tests") + ylab("Proportion of Nuggets") + ggtitle("# of Possible Shareability Categories as a Function of # Tests per Nugget")
stacked_bar + scale_fill_discrete(name = "# Categories")

# Crosstabs
library("gmodels")

CrossTable(s_cat_sc_f$overlap_f, s_cat_sc_f$test_num_f, chisq = TRUE)
