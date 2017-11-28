# share-rate-reliabilty-analysis
Measuring the Reliability of "Share-ability" Metric

There are 4 possible classifications for Shareability (# Shares/# Clicks): Low, Moderate, Good, Excellent. However, this metric is often not reliable, due to the fact that many articles (or click tests) receive an insufficient number of clicks to yield an acceptably small confidence interval. This analysis attempted to determine an appropriate threshold for classifying shareability values as reliable/unreliable, and consequently whether or not they should actually be reported. In addition, it measured the minimum number of clicks necessary to yield a reasonably high probability of obtaining a reliable shareability metric, which is used to determine which level(s) shareability should actually be reported for.

In order to determine whether a particular shareability value was reliable, Wilson’s score interval was used to calculate the confidence interval. If the confidence interval overlapped with 2 or more categories aside from the one in which the value was classified, it was considered unreliable. For example, if a shareability value was categorized as “Moderate”, but the upper confidence interval overlapped with the “Excellent” category, it was considered to be unreliable. 

A logit model revealed that at least 160 clicks were necessary in order for a shareability metric to have >= 50% probability of being considered reliable. Very few of the click tests in the analysis received a sufficient number of clicks. In contrast, more than 55% of nuggets (out of 487) received at least 160 clicks, so it is recommended that shareability be reported at the article level only, and only when the shareability metric is considered to be reliable.

