---
  title: "Times Series with Revenue"
output: html_document
---
CMT Data Analysis
```{r}
setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/SustainWorkshop/CMT")
cmt = read.csv("member_list_9_10_19.csv", header = TRUE)
head(cmt)
```
Get descriptives
```{r}
cmt$Patient.ID = NULL
cmt_sub = cmt[,c(1,6:10)]
head(cmt_sub)
describe(cmt_sub)
```
Get some correlations
```{r}
cor(cmt_sub)
describe.factor(cmt_sub$ER.Count)
```
Model selection
```{r}
library(pscl)
head(cmt_sub)
model_pois_bin = hurdle(ER.Count ~ ., data = cmt_sub, dist = "poisson", zero.dist = "binomial")
summary(model_pois_bin)
model_neg_bin = hurdle(ER.Count ~ ., data = cmt_sub, dist = "negbin", zero.dist = "binomial")
summary(model_neg_bin)

model_pois = glm(ER.Count ~ ., data = cmt_sub, family = "poisson")
summary(model_pois)
library(DescTools)
PseudoR2(model_pois)
AIC(model_pois)
AIC(model_pois_bin)
AIC(model_neg_bin)
BIC(model_pois_bin)
BIC(model_neg_bin)

library(countreg)
rootogram(model_pois_bin)
rootogram(model_neg_bin)
```
Final model
```{r}
model_pois_bin = hurdle(ER.Count ~ ., data = cmt_sub, dist = "poisson", zero.dist = "binomial")
summary(model_pois_bin)
sum_model_pois_bin = summary(model_pois_bin)
exp(sum_model_pois_bin$coefficients$count[,1:2])
exp(sum_model_pois_bin$coefficients$zero[,1:2])

```



