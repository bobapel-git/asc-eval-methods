###############################
## QUASI-EXPERIMENTAL DESIGN ##
###############################

setwd("C:/Users/lambc/Desktop/ASC Eval Methods/Data")

library(broom)
library(did)
library(fixest)
library(ggiplot)
library(lmtest)
library(sandwich)
library(tidyverse)

nlsy <- read_csv("conviction-work.csv")
names(nlsy)

nlsy <- nlsy %>% filter(is.na(wave1convic) | wave1convic > 1)
dim(nlsy)
nlsy <- nlsy %>% mutate(treated = replace_na(if_else(wave1convic >= 1, 1, 0), 0))
table(nlsy$treated)
mean(nlsy$treated)

nlsy.long <- nlsy %>% pivot_longer(cols = age_1:convic_15, names_to = c(".value", "t"), names_sep = "_")
dim(nlsy.long)
names(nlsy.long)

nlsy.long <- nlsy.long %>% mutate(t = as.numeric(t),
                                  time1treated = replace_na(t - wave1convic, 0),
                                  group = case_when(treated == 0 ~ 0, 
                                                    treated == 1 & time1treated < 0 ~ 1, 
                                                    treated == 1 & time1treated >= 0 ~ 2))
ggplot(nlsy.long, aes(x = t, y = work, color = as.factor(group))) + geom_smooth(se = FALSE) + scale_color_discrete(name = "Treatment Group", labels = c("Never Convicted", "Ever Convicted, Pretest", "Ever Convicted, Posttest")) + theme_gray(base_size = 15) + theme(legend.position = c(0.85, 0.15)) + labs(x = "Interview Round", y = "Proportion Employed", title = "Distribution of Employment, by Time and Conviction Status")
#ggsave(file = "Fig.jpg", width = 13, height = 7.3)

# two-way fixed effects #

nlsy.long <- nlsy.long %>% mutate(tv.treated = treated == 1 & time1treated >= 0)
twfe <- feols(work ~ tv.treated | id + t, cluster = ~ id, data = nlsy.long)
tidy(twfe)

twfe.leads <- feols(work ~ f(tv.treated, 2:0) | id + t, panel.id = ~ id + t, cluster = ~ id, data = nlsy.long)
tidy(twfe.leads)
wald(twfe.leads, keep = "tv.treated", drop = "tv.treatedTRUE")

twfe.event <- feols(work ~ i(time1treated, treated, -1) | id + t, cluster = ~ id, data = nlsy.long)
print(tidy(twfe.event), n = Inf)

twfe.event.plot <- data.frame(relt = c(-14:-2, 0:13), b = twfe.event$coefficients, se = twfe.event$se)
twfe.event.plot <- rbind(twfe.event.plot, c(-1, 0, 0))
ggplot(twfe.event.plot, aes(x = relt, y = b)) + geom_vline(xintercept = 0, linetype = "dashed") + geom_hline(yintercept = 0, linetype = "dashed") + geom_pointrange(aes(ymin = b - 2 * se, ymax = b + 2 * se), linewidth = 0.7) + theme_gray(base_size = 15) + theme(legend.position = c(0.85, 0.8)) + labs(x = "Time to First Conviction", y = "Average Treatment Effect on the Treated", title = "Event Study Estimates of the Impact of Conviction on Employment")

# group-time estimator #

nlsy.long <- nlsy.long %>% mutate(wave1treated = replace_na(wave1convic, 0))
set.seed(20231114)
csdid <- att_gt(yname = "work", idname = "id", tname = "t", gname = "wave1treated", control_group = "nevertreated", allow_unbalanced_panel = T, data = nlsy.long)
csdid

csdid.avg <- aggte(csdid, "simple")
summary(csdid.avg)

# heterogeneous DID #

csdid.dyn <- aggte(csdid, "dynamic")
csdid.dyn.out <- data.frame(rel.t = csdid.dyn$egt, att = csdid.dyn$att.egt, se = csdid.dyn$se.egt)
ggplot(csdid.dyn.out, aes(x = rel.t, y = att)) + geom_vline(xintercept = 0, linetype = "dashed") + geom_hline(yintercept = 0, linetype = "dashed") + geom_pointrange(aes(ymin = att - 2 * se, ymax = att + 2 * se), linewidth = 0.7) + theme_gray(base_size = 15) + theme(legend.position = c(0.85, 0.8)) + labs(x = "Time to First Conviction", y = "Average Treatment Effect on the Treated", title = "Dynamic Impact of Conviction on Employment")

csdid.coh <- aggte(csdid, "group")
csdid.coh.out <- data.frame(t = csdid.coh$egt, att = csdid.coh$att.egt, se = csdid.coh$se.egt)
ggplot(csdid.coh.out, aes(x = t, y = att)) + geom_hline(yintercept = 0, linetype = "dashed") + geom_pointrange(aes(ymin = att - 2 * se, ymax = att + 2 * se), linewidth = 0.7) + theme_gray(base_size = 15) + theme(legend.position = c(0.85, 0.8)) + labs(x = "Interview Round of First Conviction", y = "Average Treatment Effect on the Treated", title = "Cohort-Specific Impact of Conviction on Employment")

csdid.cal <- aggte(csdid, "calendar")
csdid.cal.out <- data.frame(t = csdid.cal$egt, att = csdid.cal$att.egt, se = csdid.cal$se.egt)
ggplot(csdid.cal.out, aes(x = t, y = att)) + geom_hline(yintercept = 0, linetype = "dashed") + geom_pointrange(aes(ymin = att - 2 * se, ymax = att + 2 * se), linewidth = 0.7) + theme_gray(base_size = 15) + theme(legend.position = c(0.85, 0.8)) + labs(x = "Interview Round", y = "Average Treatment Effect on the Treated", title = "Calendar-Specific Impact of Conviction on Employment")

# adjustment for covariates and anticipation *

set.seed(20231114)
csdid.cov <- att_gt(yname = "work", idname = "id", tname = "t", gname = "wave1treated", control_group = "nevertreated", allow_unbalanced_panel = T, xformla = ~ (male + minrty) * cohort, data = nlsy.long)
csdid.cov$Wpval
csdid.cov.avg <- aggte(csdid.cov, type = "simple")
csdid.cov.avg

set.seed(20231114)
csdid.ant <- att_gt(yname = "work", idname = "id", tname = "t", gname = "wave1treated", control_group = "nevertreated", allow_unbalanced_panel = T, anticipation = 2, data = nlsy.long)
csdid.ant.avg <- aggte(csdid.ant, type = "simple")
csdid.ant.avg

