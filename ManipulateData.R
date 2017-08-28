## Load the IMDB data set that I got from Kaggle
MovieData <- read.csv("movie_metadata.csv")

## We'll look at the column names to see what the data set contains
names(MovieData)

## I'm interested in what levels content_rating includes.
table(MovieData$content_rating)

## We'll plan to model whether a film grossed more than their budget. Predictors will be budget, 
## total facebook likes, and who the target audience is.
ModelData <- MovieData %>%
  mutate(Success = ifelse(gross > budget, 1, 0),
         content_rating = trimws(content_rating),
         Audience = ifelse(content_rating %in% c("PG", "G", "TV-PG", "TV-G", "TV-Y", "TV-Y7", "GP"), "Ankle-biters",
                           ifelse(content_rating %in% c("PG-13", "TV-14"), "Whipper-snappers",
                                  ifelse(content_rating %in% c("R", "TV-MA", "M", "NC-17"), "Grown-ups",
                                         "Other")))) %>%
  filter(!is.na(Success),
         Audience != "Other")

## To set a tolerance for not using scientific notation: 
# options(scipen = 9999999)
budget.caps <- quantile(ModelData$budget, probs = c(0.025, 0.975))
facebook.caps <- quantile(ModelData$cast_total_facebook_likes, probs = c(0.025, 0.975))

ModelData <- ModelData %>%
  mutate(budget.capped = pmax(budget, budget.caps[1]),
         budget.capped = pmin(budget.capped, budget.caps[2]),
         facebook.capped = pmax(cast_total_facebook_likes, facebook.caps[1]),
         facebook.capped = pmin(facebook.capped, facebook.caps[2]))

## Since I use runif() to generate random numbers, I've set the seed to ensure I can replicate these partitions
set.seed(1337)
ModelData <- ModelData %>%
  mutate(Sample = ifelse(runif(nrow(ModelData)) < 0.7, "Training", "Holdout"))

## The following code is to confirm the previous mutate behaved as expected
table(ModelData$content_rating, ModelData$Audience, useNA = "ifany")
summary(ModelData$Success)
table(ModelData$Success)
table(ModelData$Sample)/nrow(ModelData)