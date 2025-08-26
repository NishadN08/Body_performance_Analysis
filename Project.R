
bodyp = read.csv("C:/Local Disk/R Assignments/Project/body_performance.csv")
head(bodyp)

model = lm(gripforce ~ . ,data = bodyp)
summary(model)

model_age = lm(gripforce ~ age,data = bodyp)
summary(model_age)

model_wgt = lm(gripforce ~ weight,data = bodyp)
summary(model_wgt)

model_fat = lm(gripforce ~ bodyfat,data = bodyp)
summary(model_fat)

model_snb = lm(gripforce ~ sitandbend,data = bodyp)
summary(model_snb)

model_su = lm(gripforce ~ situps,data = bodyp)
summary(model_su)

model_bj = lm(gripforce ~ broadjump,data = bodyp)
summary(model_bj)

model_act = lm(gripforce ~ diastolic + systolic + sitandbend + situps + broadjump,data = bodyp)
summary(model_act)

model_gen = lm(gripforce ~ gender + height + weight + bodyfat, data = bodyp)
summary(model_gen)


plot(
  bodyp$weight,
  #bodyp$sitandbend,
  #bodyp$situps,
  bodyp$gripforce,
  xlab = "gripforce",
  ylab = "body_weight",
  col = "dodgerblue",
  pch = 20,
  main = "Body Weight vs Gripforce",
)
abline(model_wgt,col = "maroon")



m_start = lm(gripforce ~ 1, data = bodyp)
m_forwaic = step(
  m_start,
  scope = gripforce ~ age + gender + height + weight + bodyfat + diastolic + systolic + sitandbend + situps + broadjump,
  direction = 'forward'
)

m_start2 = lm(gripforce ~ 1, data = bodyp)
n = nrow(bodyp)
m_forwbic = step(
  m_start,
  scope = gripforce ~ age + gender + height + weight + bodyfat + diastolic + systolic + sitandbend + situps + broadjump,
  direction = 'forward',
  k = log(n)
)

m_preds = lm(gripforce~., data = bodyp)
m_backaic = step(
  m_preds,
  direction = 'backward'
)
n1 = nrow(bodyp)
m_backbic = step(
  m_preds,
  direction = 'backward',
  k = log(n1)
)



m_start3 = lm(gripforce~1, data=bodyp)
m_stepaic = step(
  m_start3,
  scope = gripforce ~ age + gender + height + weight + bodyfat + diastolic + systolic + sitandbend + situps + broadjump,
  direction = 'both'
)
n3 = nrow(bodyp)
m_stepbic = step(
  m_start3,
  scope = gripforce ~ age + gender + height + weight + bodyfat + diastolic + systolic + sitandbend + situps + broadjump,
  direction = 'both',
  k = log(n3)
)






library(dplyr)
library(corrplot)
data("bodyp")
longl_preds = dplyr::select(bodyp, -gender)
round(cor(longl_preds), 3)
corrplot(cor(longl_preds),
         method = 'color', order = 'hclust', diag = FALSE,
         number.digits = 3, addCoef.col = 'black', tl.pos= 'd', cl.pos ='r')


model_1 = lm(gripforce ~  broadjump + weight + bodyfat + sitandbend + 
               situps + age + diastolic, data = bodyp)
summary(model_1)



final_model = lm(gripforce ~ age+gender+height+weight+broadjump+situps, data = bodyp)
summary(final_model)

install.packages("nortest")
library(nortest)
library(olsrr)
library(lmtest)


result = ad.test(resid(final_model))
result

ks.test(resid(final_model), "pnorm", mean = mean(resid(final_model)), sd = sd(resid(final_model)))


res <- residuals(final_model)
res <- res[is.finite(res)]
n <- length(res)
n

set.seed(1)
res_sub <- sample(res, 5000)
result <- shapiro.test(res_sub)
print(result$p.value)

