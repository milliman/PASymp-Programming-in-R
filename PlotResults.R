## We'll bucket the continuous variables in order to plot along them later.
ResultData <- ResultData %>%
  mutate(budget.buckets = cut(ModelData$budget.capped, breaks = quantile(ModelData$budget.capped, probs = seq(0, 1, 0.1)), include.lowest = T),
         facebook.buckets = cut(ModelData$facebook.capped, breaks = quantile(ModelData$facebook.capped, probs = seq(0, 1, 0.1))), include.lowest = T) %>%
  group_by(budget.buckets) %>%
  mutate(budget.mid = median(budget.capped)) %>%
  group_by(facebook.buckets) %>%
  mutate(facebook.mid = median(facebook.capped)) %>%
  ungroup()

## Choose whether to create plots on the training data or the holdout data
DataSubset <- "Training"
# DataSubset <- "Holdout"

## b1 gives record counts by budget bucket
b1 <- ResultData %>%
  filter(Sample == DataSubset) %>%
  group_by(budget.buckets, Audience) %>%
  summarize(Count = n()) %>%
  ggplot(aes(x = budget.buckets, y = Count, fill = Audience)) +
  geom_bar(stat = "identity")

## b2 plots the predicted success rates by budget, with observed rates on the same chart
b2 <- ResultData %>%
  filter(Sample == DataSubset) %>%
  group_by(budget.buckets, Audience) %>%
  summarize(Actual = mean(Success),
            Expected = mean(Prediction),
            budget.mid = median(budget.mid)) %>%
  ggplot(aes(x = budget.mid, colour = Audience)) +
  geom_line(aes(y = Actual), lty = 1, lwd = 1) +
  geom_line(aes(y = Expected), lty = 2, lwd = 1) +
  ylim(0, 1)

## b3 validates the predictions against the observations by plotting A/E ratios across budget 
b3 <- ResultData %>%
  filter(Sample == DataSubset) %>%
  group_by(budget.buckets, Audience) %>%
  summarize(Actual = mean(Success),
            Expected = mean(Prediction),
            budget.mid = median(budget.mid)) %>%
  mutate(AE = Actual/Expected) %>%
  ggplot(aes(x = budget.mid, colour = Audience)) +
  geom_line(aes(y = AE), lty = 1, lwd = 1) +
  geom_hline(aes(yintercept = 1)) +
  coord_cartesian(ylim = c(0, 2)) 

## The following set of charts are analagous to those three created above, with the facebook like count as
## the horizontal variable
f1 <- ResultData %>%
  filter(Sample == DataSubset) %>%
  group_by(facebook.buckets, Audience) %>%
  summarize(Count = n(),
            facebook.mid = median(facebook.mid)) %>%
  ggplot(aes(x = facebook.buckets, y = Count, fill = Audience)) +
  geom_bar(stat = "identity")

f2 <- ResultData %>%
  filter(Sample == DataSubset) %>%
  group_by(facebook.buckets, Audience) %>%
  summarize(Actual = mean(Success),
            Expected = mean(Prediction),
            facebook.mid = median(facebook.mid)) %>%
  ggplot(aes(x = facebook.mid, colour = Audience)) +
  geom_line(aes(y = Actual), lty = 1, lwd = 1) +
  geom_line(aes(y = Expected), lty = 2, lwd = 1) +
  ylim(0, 1)

f3 <- ResultData %>%
  filter(Sample == DataSubset) %>%
  group_by(facebook.buckets, Audience) %>%
  summarize(Actual = mean(Success),
            Expected = mean(Prediction),
            facebook.mid = median(facebook.mid)) %>%
  mutate(AE = Actual/Expected) %>%
  ggplot(aes(x = facebook.mid, colour = Audience)) +
  geom_line(aes(y = AE), lty = 1, lwd = 1) +
  geom_hline(aes(yintercept = 1)) +
  coord_cartesian(ylim = c(0, 2))