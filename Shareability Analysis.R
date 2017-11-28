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