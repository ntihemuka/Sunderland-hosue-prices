---
title: "Sunderland Price prediction"
output: html_notebook
---





WE begin by importing the data and set our working directory and doing a bit of data
cleaning

```{r}
library(tidyverse)

#if(!requireNamespace("devtools")) install.packages("devtools")
#devtools::install_github("dkahle/ggmap")

setwd("C:\\Users\\lydia\\OneDrive\\ASS2CETM47AGIAN")
train_raw <- read_csv("FinalMove.csv")

```

lets select only the columns we need for our analysis
```{r}
train_raw <- train_raw[c('address','propertyType','bedrooms','hasFloorPlan','location.lat','location.lng','details','displayPrice','dateSold','tenure','newBuild')]
```

lets see what we have
```{r}
head(train_raw)
```
                               DATA CLEANING
------------------------------------------------------------------------------


we use regular expressioins to remove weird characters from our detail columns
```{r}
train_raw$displayPrice <- gsub('£','',train_raw$displayPrice)
train_raw$displayPrice <- gsub(',','',train_raw$displayPrice)
train_raw$details <- gsub('<ul class="keyfeatures">', '', train_raw$details)
train_raw$details <- gsub('<li>', '', train_raw$details)
train_raw$details <- gsub('</li>', ' ', train_raw$details)
train_raw$details <- gsub('</ul>', ' ', train_raw$details)
```

we make change some columns to numeric, and factors others
```{r}
train_raw$displayPrice <- as.numeric(train_raw$displayPrice)

train_raw$tenure <- as.factor(train_raw$tenure)
train_raw$address <- as.factor(train_raw$address)
train_raw$dateSold <- gsub(' ', '', train_raw$dateSold)
train_raw$dateSold <- as.Date(train_raw$dateSold,"%d%b%Y")
```


we format the dates to a usable format
```{r}
train_raw$dateSold <- as.Date(train_raw$dateSold,"%d%b%Y" )
```

lets correct a mistake i made during scrapping
```{r}
library(naniar)

train_raw <- replace_with_na_all(train_raw,
                       condition = ~.x %in% c("Na"))
train_raw <- train_raw %>% drop_na() 
```



Lets create bins of our prices to visualise the data better
```{r}
df1 <- data.frame(train_raw$displayPrice,bin=cut(train_raw$displayPrice,c(50000,
                                                      100000,
                                                      150000,
                                                      250000,
                                                      400000,
                                                      700000,
                                                      1200000,
                                                      2000000
                                                      ),include.lowest=TRUE))

train_raw$bin <- df1$bin
```



                               EXPLORATORY ANALYSIS
------------------------------------------------------------------------------


so far our house prices seem to be spread well. with most of the houses being
in the 50 to 100 pound range. 

100 to 150 are also simliar to the lower group
wecan see that the closer you get to southshields and the beach the higher the prices

the 150 to 400 hundred thousand ones denoted in green and turquoise
seem to be consentrated in barnes area and toward roker

barns area etc.. while the
expensive 700 thousand plus seem to be mostly in south shields
```{r}
library(ggmap)

price_plot <- qmplot(location.lng, location.lat, data = train_raw, maptype = "toner-lite", color = bin)

price_plot
```



we can see the prices are definitley impacted by location, but also the type
of house. whether terraced or not.
```{r}
qmplot(location.lng, location.lat, data = train_raw, color = bin) + 
  facet_wrap(~ propertyType)
```
most houses seem to be old builds. and doesnt affect the price
```{r message=TRUE, warning=TRUE}

  qmplot(location.lng, location.lat, data = train_raw, color = bin) + 
  facet_wrap(~ newBuild)

```


Lets use our house descriptions to see if there is any correlation to price
```{r message=FALSE, warning=FALSE}
set.seed(345)
library(tidytext)
library(wordcloud)
library(RColorBrewer)

sunderland_tidy <- train_raw %>%

   unnest_tokens(word, details) %>% #tokenize words
   anti_join(get_stopwords()) #remove stop words

words <- sunderland_tidy  %>%
   count(word, sort = TRUE)


#create word cloud
set.seed(1234) 
 wordcloud(words = words$word, freq = words$n, min.n = 1,           
          max.words=200, random.order=FALSE, rot.per=0.35,            
          colors=brewer.pal(8, "Dark2"))
```


Now lets find out the top words used per price bin
```{r}
top_words <-
  sunderland_tidy %>%
  count(word, sort = TRUE) %>%
  filter(!word %in% as.character(1:5)) %>% #remove characters 1-5,they are already in the text
  slice_max(n, n = 100) %>% # select top 100
  pull(word)

#now lets count those words changing per bin 
word_freqs <-
  sunderland_tidy %>%
  count(word, displayPrice) %>%  # how mny times is each word used per price range
  complete(word, displayPrice, fill = list(n = 0)) %>% # if those words are missing just put zero
  group_by(displayPrice) %>%
  mutate(                      
    price_total = sum(n),#how many words are used total for each price range in a proportion
    proportion = n / price_total
  ) %>%
  ungroup() %>%
  filter(word %in% top_words)
word_freqs
```


lets train some linear models to find the words that are increasing with price and those that are decreasing with price

```{r}
word_mods <-
  word_freqs %>%
  nest(data = c(displayPrice, n, price_total, proportion)) %>%
  mutate(
    model = map(data, ~ glm(cbind(n, price_total) ~ displayPrice, ., family = "binomial")),
    #we want to know if the number of success depend on price range
    #we create multiple models over the data that we nested above
    model = map(model, tidy)#create dataframes out of the models instead of the models themselves
  ) %>%
  unnest(model) %>%
  filter(term == "displayPrice") %>%
  mutate(p.value = p.adjust(p.value)) %>% #adjust the p values since we trained a lot of models
  arrange(-estimate)# - sign is order of prices this is descending order
#estimate is the effect the words have on the slope(expensive or cheap)
word_mods
```


We can Create a visualization for this
```{r message=FALSE, warning=FALSE}
library(ggrepel)

word_mods %>%
   ggplot(aes(estimate, p.value)) +
   geom_vline(xintercept = 0, lty = 2, alpha = 0.7, color = "gray50") +
   geom_point(color ="brown", alpha = 0.8, size = 1.5) +
   scale_y_log10() +
   geom_text_repel(aes(label = word)) +
   labs(title = "WORD VALUE")


```


lets create different dataframes for the cheap and expensive words
```{r}
higher_words <-
  word_mods %>%
  filter(p.value < 0.05) %>%
  slice_max(estimate, n = 12) %>%
  pull(word)

lower_words <-
  word_mods %>%
  filter(p.value < 0.05) %>%
  slice_max(-estimate, n = 12) %>%
  pull(word)
```


We can observe how proportion of words change per price range
we have poportion of words on the Y axes and price range on the X axes


```{r message=FALSE, warning=FALSE}
word_freqs %>%
  filter(word %in% lower_words) %>%
  ggplot(aes(displayPrice, proportion, color = word)) +
  geom_line(size = 0.5, alpha = 0.7, show.legend = FALSE) +
  facet_wrap(vars(word), scales = "free_y") +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
  labs(x = NULL, y = "proportion of total words used for homes at that price") +
  theme_light(base_family = "IBMPlexSans")
```

before i model i want to see the distribution of my data to see what model i should use

```{r}
library(skimr)
skim(train_raw)
```
```{r}
hist(train_raw$displayPrice, breaks=12, col="red",
     main="House prices distribution",)
```


                                  MODELLING 
-------------------------------------------------------------------------------

we create our splits and cross validation folds
```{r}
library(tidymodels)

set.seed(123)
sunderland_split <- train_raw %>%
  mutate(details = str_to_lower(details)) %>%
  initial_split(strata = bin) #stratafied sampling

sunderland_train <- training(sunderland_split)
sunderland_test <- testing(sunderland_split)
sunderland_metrics <- metric_set(rmse,mae)


set.seed(234)
sunderland_folds <- vfold_cv(sunderland_train, v = 20, strata = bin)# 5 to reduce tuning time

higher_pat <- glue::glue_collapse(higher_words, sep = "|") # use the collape to make them usable with regex() 
lower_pat <- glue::glue_collapse(lower_words, sep = "|")
sunderland_folds
```

For feature engineering, let’s use basically everything in the dataset 
(aside from city, which was not a very useful variable) 
and create dummy or indicator variables using step_regex(). 
The idea here is that we will detect whether these words associated with 
low/high price are there and create a yes/no variable indicating their presence or absence.
```{r}
library(themis)

sunderland_rec <-
  recipe(displayPrice ~ ., data = sunderland_train) %>% 
  
  step_upsample(bin) %>%
  
  update_role(bin, new_role = "Range") %>% 
  update_role(address, new_role = "ID") %>% 
  # keep but dont us bin as predictor or outcome
  

  step_regex(details, pattern = higher_pat, result = "high_price_words") %>% 
  #we use this to make new variables out of our words
  step_regex(details, pattern = lower_pat, result = "low_price_words") %>%

  step_rm(details) %>%
  
  step_date(dateSold, features = c("month")) %>%
  #see if month has effects
  step_naomit(displayPrice) %>%
  update_role(dateSold, new_role = "dayofsale")%>%
  
  step_dummy(all_nominal_predictors()) %>%
  # this  codes the categorical with multiple factors into multiple variables
  
  step_nzv(all_predictors()) # removes highly spared and unbalanced data 


sunderland_rec %>% prep()%>%
  juice()
```


lets create our model specifications and our workflow
fmjfnb
```{r}
xgb_spec <-
  boost_tree(
    trees = 1000,
    tree_depth = tune(),
    min_n = tune(),
    mtry = tune(),
    sample_size = tune(),
    learn_rate = tune()
  ) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

xgb_word_wf <- workflow(sunderland_rec, xgb_spec)
```



lets Create a Grid that we will use to tune our work flow and models
```{r}
set.seed(123)
xgb_grid <-
  grid_max_entropy(
    tree_depth(c(5L, 10L)),
    min_n(c(10L, 40L)),
    mtry(c(5L, 10L)),
    sample_prop(c(0.5, 1.0)),
    learn_rate(c(-2, -1)),
    size = 20
  )
#this grid specifies how many parameters we want to try, 
#max entropy simply means try as much efficiently as possible without covering the whole grid
#bellow are all those parameters its trying


```


Now lets train and tune our models and work flows
tune grid tunes quickly by throwing away tuning folds that are unlikely to perform well based
on their similarity to other tuning instances that didn't go well
```{r}
library(finetune)
doParallel::registerDoParallel()

set.seed(234)
xgb_word_rs <-
  tune_grid(
    object = xgb_word_wf,
    resamples = sunderland_folds,
    grid = xgb_grid,
    metrics = sunderland_metrics,
    control = tune::control_grid(save_pred = TRUE)
  )

xgb_word_rs

```



                                 EVALUATION
--------------------------------------------------------------------------------

lets see which model performed best
```{r}
show_best(xgb_word_rs)
```


lets do final fit on best model
```{r}
set.seed(123)
xgb_last <-
  xgb_word_wf %>%
  finalize_workflow(select_best(xgb_word_rs,"rmse","rsq")) %>%
  last_fit(sunderland_split)
```


```{r}
xgb_last
```

```{r}
meta <- collect_predictions(xgb_last)
meta
```
```{r}
mets <- collect_metrics(xgb_last)
mets
```

lets find metrics
```{r}
library(caret)
mse1 = mean((meta$displayPrice - meta$.pred)^2)
mae1 = caret::MAE(meta$displayPrice, meta$.pred)
rmse1 = caret::RMSE(meta$displayPrice, meta$.pred)
mse1
mae1
rmse1
```
```{r}
maxim <- max(meta$displayPrice)
minim <- min(meta$displayPrice)

norm_rmse <- rmse1/(maxim - minim)
norm_rmse
```


```{r}
x = 1:length(meta$displayPrice)
plot(x, meta$displayPrice, col = "red", type = "l")
lines(x, meta$.pred, col = "blue", type = "l")
legend(x = 1, y = 38,  legend = c("original test_y", "predicted test_y"), 
       col = c("red", "blue"), box.lty = 1, cex = 0.8, lty = c(1, 1))
```



```{r}

ggplot(meta,                                     # Draw plot using ggplot2 package
       aes(x = .pred,
           y = displayPrice,fill= displayPrice)) +
  geom_point(alpha=0.5, size=2, shape=21) +
  scale_fill_viridis_c() +
  geom_smooth(method = "lm", alpha = .15, aes(fill = displayPrice)) 



```


we can  plot attribute importance
```{r}
library(vip)
 extract_workflow(xgb_last) %>%
  extract_fit_parsnip() %>%
  vip(geom = "point", num_features = 20, 
      mapping = aes_string(color = "Importance", size = 1)) +
      scale_fill_viridis_c() +
      scale_y_log10() +
      ggtitle("IMPORTANCE")
```

-The high price words are not really being used but interestingly the lower price words 
are being used. 

-maybe its because lower price words use more description in their listings as we saw in the volacno plot

-we can see that the model did better for echeaper houses than cheaper ones.
might be because they ar more in number and are clamped together more than expensive ones. 

-in the end the word descriptions did not matter as much as the ones in data frame.

-location latitude was most important. this is consistent with the map
because the further east you are the closer you are to the water.

                                     Discussion
from the regression line, we can see that the expensive houses are too outlying 
if we had removed them, we could have gotten higher rmse. the plot comfimrs it.
77% rsq means there are more attribute we could be missing.

```{r}
```{r}