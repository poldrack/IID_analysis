# analyze IID data using brms
# R Poldrack, 12/9/2016

library(brms)

df=read.table('InhibitionInducedDevaluationData.csv',sep=',')

d=reshape(df,varying=c('V1','V2','V3','V4','V5','V6','V7','V8','V9'),
             direction='long',v.names='score'
             )
d=d[!is.na(d$score),]
d$id=seq(dim(d)[1])

# use an empirical prior 
prior_sd=sd(d$score)

# fit model with random effect of different datasets
capture.output(fit_rfx <- brm(formula = score ~ 0 + intercept + (1|time),
              data = d, 
              prior = c(set_prior(sprintf("normal(0.5,%f)",prior_sd), class = "b"),
                        set_prior("cauchy(0,2)", class = "sd")),
              warmup = 1000, iter = 2000, chains = 4,
              control = list(adapt_delta = 0.95),
              sample_prior = TRUE))
  
hyp_rfx <- hypothesis(fit_rfx, "intercept = 0.5")
print(fit_rfx)
print(hyp_rfx)

# fit model treating all as a single study
capture.output(fit_fx <- brm(formula = score ~ 0 + intercept,
            data = d, 
            prior = c(set_prior(sprintf("normal(0.5,%f)",prior_sd), class = "b")),
            warmup = 1000, iter = 2000, chains = 4,
            control = list(adapt_delta = 0.95),
            sample_prior = TRUE))

hyp_fx <- hypothesis(fit_fx, "intercept = 0.5")

print(fit_fx)
print(hyp_fx)
