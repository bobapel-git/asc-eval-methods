#########################
## EXPERIMENTAL DESIGN ##
#########################

setwd("C:/Users/lambc/Desktop/ASC Eval Methods/Data")

library(drgee)
library(effectsize)
library(ivreg)
library(lmtest)
library(modelsummary)
library(ritest)
library(sandwich)
library(scales)
library(tidyverse)

nsw <- read_csv("supported-work.csv")
names(nsw)
table(nsw$exp)
table(nsw$exp[nsw$out.earned != "NA"])

ggplot(nsw, aes(x = out.earned, fill = as.factor(exp), color = as.factor(exp))) + geom_histogram(aes(y = after_stat(density)), binwidth = 1000, alpha = 0.2, position = "identity") + labs(x = "Total Earnings", y = "Density", title = "Distribution of Earnings, by Treatment Assignment") + scale_fill_hue(name = "Treatment Group", labels = c("Control", "Experimental")) + scale_color_hue(name = "Treatment Group", labels = c("Control", "Experimental")) + theme_gray(base_size = 15) + theme(legend.position = c(0.8, 0.8)) + scale_x_continuous(label = dollar_format())
ggplot(subset(nsw, out.earned <= 12000), aes(x = out.earned, color = exp)) + stat_ecdf(geom = "step", pad = F, linewidth = 1) + labs(x = "Total Earnings", y = "Empirical CDF", title = "Cumulative Distribution of Earnings, by Treatment Assignment") + scale_color_hue(name = "Treatment Group", labels = c("Control", "Experimental")) + theme_gray(base_size = 15) + theme(legend.position = c(0.8, 0.2)) + scale_x_continuous(label = dollar_format()) + ylim(c(0.4, 1.0))
#ggsave("Fig.jpg", width = 13, height = 7.3)

fit.itt <- lm(out.earned ~ exp, data = nsw)
coeftest(fit.itt, vcov = vcovHC(fit.itt))

nsw <- nsw %>% mutate(trt.worked = if_else(trt.months > 0, 1, 0),
                      complier = case_when(exp == "exper" & trt.months > 0 ~ 1,
                                           exp == "contr" & trt.months == 0 ~ 1,
                                           TRUE ~ 0))
nsw %>% filter(trt.worked != "NA") %>% 
  group_by(exp) %>% summarize(n = n(),
                              pwork = mean(trt.worked, na.rm = T),
                              pcomp = mean(complier, na.rm = T))

fit.per <- lm(out.earned ~ exp, data = nsw, subset = complier == 1)
coeftest(fit.per, vcov = vcovHC(fit.per))

fit.trt <- lm(out.earned ~ trt.worked, data = nsw)
coeftest(fit.trt, vcov = vcovHC(fit.trt))

fit.ive <- ivreg(out.earned ~ 1 | trt.worked | exp, data = nsw)
coeftest(fit.ive, vcov = vcovHC(fit.ive))

modelsummary(list("ITT" = fit.itt, "PP" = fit.per, "AT" = fit.trt, "LATE" = fit.ive), vcov = vcovHC, gof_map = c("nobs"), estimate = "{estimate}{stars}", fmt = 1)

# effect size #

d <- cohens_d(out.earned ~ exp, pooled_sd = F, data = nsw)
d
interpret_cohens_d(d$Cohens_d, rules = "cohen1988")

coef(fit.itt)
coef(fit.ive)
d$Cohens_d
d$Cohens_d * (coef(fit.ive)[2] / coef(fit.itt)[2])

# randomization inference #

ri <- ritest(fit.itt, "expexper", reps = 1000, seed = 20231114)
ri

ri.coef <- data.frame(b = ri$betas)
ggplot(ri.coef, aes(x = b)) + geom_histogram(aes(y = after_stat(density)), binwidth = 20) + geom_function(fun = dnorm, args = list(mean = mean(ri.coef$b), sd = sd(ri.coef$b)), color = "blue", linetype = "dashed", linewidth = 1.2) + geom_vline(xintercept = coef(fit.itt)[2], linetype = "dashed") + labs(x = "Impact of Supported Work on Earnings", y = "Density", title = "Distribution of Placebo Intention-to-Treat Coefficients") + theme_gray(base_size = 15) + scale_x_continuous(label = dollar_format())

# regression adjustment #

nsw <- nsw %>% mutate(pre.worked = if_else(pre.months > 0, 1, 0),
                      pre.unemp.ln = log(pre.unemp),
                      pre.illeg.ln = log1p(pre.illeg),
                      pre.hours.ln = log1p(pre.hours),
                      pre.earned.ln = log1p(pre.earned))
covars <- data.frame(nsw$pre.age, nsw$pre.male, nsw$pre.race, nsw$pre.marr, nsw$pre.educ, nsw$pre.empl, nsw$pre.activ, nsw$pre.unemp.ln, nsw$pre.illeg.ln, nsw$pre.worked, nsw$pre.months, nsw$pre.hours.ln, nsw$pre.earned.ln)

bal <- matrix(NA, 13, 4)
rownames(bal) <- c("age", "male", "race", "marr", "educ", "empl", "activ", "unemp", "illeg", "worked", "months", "hours", "earned")
colnames(bal) <- c("F-stat", "df-num", "df-denom", "p-val")

for (i in 1:ncol(covars)) {
  est <- lm(as.numeric(as.factor(nsw$exp)) ~ covars[, i])
  fstat <- summary(est)$fstatistic
  bal[i, 1] <- round(fstat[1], 2)
  bal[i, 2] <- round(fstat[2], 0)
  bal[i, 3] <- round(fstat[3], 0)
  bal[i, 4] <- round(pf(fstat[1], fstat[2], fstat[3], lower.tail = F), 4)
}
bal

fit.itt.ra <- drgee(oformula = out.earned ~ stratum + pre.age + pre.male + pre.race + pre.marr + pre.educ + pre.empl + pre.unemp.ln + pre.illeg.ln + pre.worked + pre.months + pre.hours.ln + pre.earned.ln, exposure = "exp", estimation.method = "o", data = nsw)
summary(fit.itt.ra)$coefficients

# reweighting estimator #

nsw$insamp <- as.numeric(!is.na(nsw$out.earned))
logit <- glm(insamp ~ exp + stratum + pre.age + pre.male + pre.race + pre.marr + pre.educ + pre.empl + pre.unemp.ln + pre.illeg.ln + pre.worked + pre.months + pre.hours.ln + pre.earned.ln, family = binomial(link = "logit"), data = nsw, na.action = na.exclude)
nsw$phat <- predict(logit, type = "response")
nsw$invprob <- 1 / (nsw$insamp * nsw$phat + (1 - nsw$insamp) * (1 - nsw$phat))

fit.itt.ipw <- lm(out.earned ~ exp, weights = invprob, data = nsw)
coeftest(fit.itt.ipw, vcov = vcovHC(fit.itt.ipw))

fit.ive.ipw <- ivreg(out.earned ~ 1 | trt.worked | exp, weights = invprob, data = nsw)
coeftest(fit.ive.ipw, vcov = vcovHC(fit.ive.ipw))

modelsummary(list("ITT" = fit.itt, "ITT-IPW" = fit.itt.ipw, "LATE" = fit.ive, "LATE-IPW" = fit.ive.ipw), vcov = vcovHC, gof_map = c("nobs"), estimate = "{estimate}{stars}", fmt = 1)

# appendix: log-linear regression #

pois.itt <- glm(out.earned ~ exp, family = "quasipoisson", data = nsw)
coeftest(pois.itt, vcov = vcovHC(pois.itt))
100 * (exp(coef(pois.itt)[2])-1)

nsw$res.1st <- resid(lm(trt.worked ~ exp, data = nsw, subset = out.earned != "NA", na.action = na.exclude))
pois.ive <- glm(out.earned ~ trt.worked + res.1st, family = "quasipoisson", data = nsw)
coeftest(pois.ive, vcov = vcovBS(pois.ive, R = 250))
