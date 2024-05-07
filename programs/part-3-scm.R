##############################
## SYNTHETIC CONTROL DESIGN ##
##############################

setwd("C:/Users/lambc/Desktop/ASC Eval Methods/Data")

library(broom)
library(cowplot)
library(fixest)
library(ggrepel)
library(tidysynth)
library(tidyverse)

pros <- read_csv("deprosecution-homicide.csv")
dim(pros)
names(pros)
length(unique(pros$agency))

pros <- pros %>% 
  mutate(murd.rate = murd / pop * 100000,
         post = if_else(year >= 2015, 1, 0),
         treated = if_else(agency == "philadelphia PA", 1, 0))
ggplot(pros, aes(x = year, y = murd.rate, group = agency)) + geom_line(color = "darkgray") + geom_line(data = subset(pros, treated == 1), mapping = aes(x = year, y = murd.rate), color = "black", linewidth = 1.5) + geom_vline(xintercept = 2015, linetype = "dashed") + theme_gray(base_size = 15) + labs(x = "Year", y = "Homicide Rate per 100,000", title = "Agency Trends in Homicide, 1990-2020")
#ggsave(file = "Fig.jpg", width = 13, height = 7.3)

pros <- pros %>% mutate(tv.treated = treated == 1 & year >= 2015,
                        year1treated = if_else(treated == 1, 2015, 0),
                        time1treated = case_when(treated == 1 ~ year - 2015,
                                                 TRUE ~ 0))
twfe <- feols(murd.rate ~ tv.treated | agency + year, cluster = ~ agency, data = pros)
twfe.leads <- feols(murd.rate ~ f(tv.treated, 3:0) | agency + year, panel.id = ~ agency + year, cluster = ~ agency, data = pros)

tidy(twfe)
tidy(twfe.leads)

twfe.event <- feols(murd.rate ~ i(time1treated, treated, -1) | agency + year, cluster = ~ agency, data = pros)
twfe.event.plot <- data.frame(relt = c(-25:-2, 0:5), b = twfe.event$coefficients, se = twfe.event$se)
twfe.event.plot <- rbind(twfe.event.plot, c(-1, 0, 0))
ggplot(twfe.event.plot, aes(x = relt, y = b)) + geom_vline(xintercept = 0, linetype = "dashed") + geom_hline(yintercept = 0, linetype = "dashed") + geom_pointrange(aes(ymin = b - 2 * se, ymax = b + 2 * se), linewidth = 0.7) + theme_gray(base_size = 15) + theme(legend.position = c(0.85, 0.8)) + labs(x = "Time to De-Prosecution Policy", y = "Estimate of the ATT", title = "Event Study Estimates of the Impact of De-Prosecution on Homicide")

synth <- pros %>% 
  synthetic_control(outcome = murd.rate, 
                    unit = agency, 
                    time = year, 
                    i_unit = "philadelphia PA", 
                    i_time = 2015, 
                    generate_placebos = T) %>%
  generate_predictor(time_window = 1990:2014, 
                     murder = mean(murd.rate)) %>%
  generate_weights(optimization_window = 1990:2014) %>%
  generate_control()

fit.synth <- grab_synthetic_control(synth)
fit.placebo <- grab_synthetic_control(synth, placebo = T)
fit.wgts <- grab_unit_weights(synth)
fit.mspe <- grab_significance(synth)

fit.mspe[fit.mspe$unit_name == "philadelphia PA", ]
sqrt(fit.mspe[fit.mspe$unit_name == "philadelphia PA", 3])

a1 <- fit.synth %>% pivot_longer(cols = c("real_y", "synth_y"), names_to = "group", values_to = "outcome") %>% ggplot(aes(x = time_unit, y = outcome, color = group)) + geom_line(linewidth = 1) + geom_vline(xintercept = 2015, linetype = "dashed") + scale_color_discrete(name = "Series", labels = c("Philadelphia", "Synthetic Philadelphia")) + theme_gray(base_size = 15) + theme(legend.position = "top") + labs(x = "Year", y = "Homicide Rate per 100,000", title = "Treated and Synthetic Homicide Series")
b1 <- fit.synth %>% ggplot(aes(x = time_unit, y = real_y - synth_y)) + geom_line(linewidth = 1) + geom_hline(yintercept = 0, linetype = "dashed") + geom_vline(xintercept = 2015, linetype = "dashed") + theme_gray(base_size = 15) + labs(x = "Year", y = "Difference in Homicide Rate per 100,000", title = "Treatment Effect Series")
plot_grid(a1, b1)

ggplot(fit.placebo, aes(x = time_unit, y = real_y - synth_y, group = .id)) + geom_line(color = "darkgray") + geom_line(data = subset(fit.placebo, .placebo == 0), mapping = aes(x = time_unit, y = real_y - synth_y), color = "black", linewidth = 1.5) + geom_hline(yintercept = 0, linetype = "dashed") + geom_vline(xintercept = 2015, linetype = "dashed") + theme_gray(base_size = 15) + labs(x = "Year", y = "Difference in Homicide Rate per 100,000", title = "Actual and Placebo Treatment Effect Series")

fit.wgts <- fit.wgts %>% mutate(rank = 148 - rank(weight)) %>% arrange(rank)
fit.wgts.lab <- fit.wgts$unit[35:1]
fit.mspe.lab <- fit.mspe$unit_name[35:1]
c1 <- ggplot(subset(fit.wgts, rank <= 35), aes(x = as.factor(rev(rank)), y = weight)) + geom_col() + coord_flip() + scale_x_discrete(labels = fit.wgts.lab) + theme_gray(base_size = 15) + labs(x = element_blank(), y = "Weight", title = "Unit Weights in Synthetic Control")
d1 <- ggplot(subset(fit.mspe, rank <= 35), aes(x = as.factor(rev(rank)), y = mspe_ratio)) + geom_col() + coord_flip() + scale_x_discrete(labels = fit.mspe.lab) + theme_gray(base_size = 15) + labs(x = element_blank(), y = "Post-Intervention MSPE / Pre-Intervention MSPE", title = "Mean Squared Prediction Error Ratios")
plot_grid(c1, d1)

synth2 <- pros %>% 
  synthetic_control(outcome = murd.rate, 
                    unit = agency, 
                    time = year, 
                    i_unit = "philadelphia PA", 
                    i_time = 2015, 
                    generate_placebos = T) %>%
  generate_predictor(time_window = 1990:2014, murd = mean(murd.rate)) %>%
  generate_predictor(time_window = 2006, murd06 = murd.rate) %>%
  generate_predictor(time_window = 2011:2013, murd11_13 = mean(murd.rate)) %>%
  generate_weights(optimization_window = 1990:2014) %>%
  generate_control()

fit2.mspe <- grab_significance(synth2)
sqrt(fit2.mspe[fit2.mspe$unit_name == "philadelphia PA", 3])
fit2.mspe[fit2.mspe$unit_name == "philadelphia PA", ]

fit2.synth <- grab_synthetic_control(synth2)
a2 <- fit2.synth %>% pivot_longer(cols = c("real_y", "synth_y"), names_to = "group", values_to = "outcome") %>% ggplot(aes(x = time_unit, y = outcome, color = group)) + geom_line(linewidth = 1) + geom_vline(xintercept = 2015, linetype = "dashed") + scale_color_discrete(name = "Series", labels = c("Philadelphia", "Synthetic Philadelphia")) + theme_gray(base_size = 15) + theme(legend.position = "top") + labs(x = "Year", y = "Homicide Rate per 100,000", title = "Treated and Synthetic Homicide Series")
b2 <- fit2.synth %>% ggplot(aes(x = time_unit, y = real_y - synth_y)) + geom_line(linewidth = 1) + geom_hline(yintercept = 0, linetype = "dashed") + geom_vline(xintercept = 2015, linetype = "dashed") + theme_gray(base_size = 15) + labs(x = "Year", y = "Difference in Homicide Rate per 100,000", title = "Treatment Effect Series")
plot_grid(a2, b2)

fit2.wgts <- grab_unit_weights(synth2)
fit2.wgts <- fit2.wgts %>% mutate(rank = 148 - rank(weight)) %>% arrange(rank)
fit2.wgts$weight[fit2.wgts$rank <= 10]
fit2.wgts$unit[fit2.wgts$rank <= 10]

labels <- pros %>% filter(agency == "philadelphia PA" | agency == "orlando FL" | agency == "richmond VA" | agency == "detroit MI" | agency == "inglewood CA" | agency == "flint MI") %>% group_by(agency) %>% summarize(pos = which.max(year), x = year[pos], y = murd.rate[pos])
ggplot(subset(pros, agency == "philadelphia PA" | agency == "orlando FL" | agency == "richmond VA" | agency == "detroit MI" | agency == "inglewood CA" | agency == "flint MI"), aes(x = year, y = murd.rate, group = agency, color = agency)) + geom_line(mapping = aes(size = treated)) + geom_vline(xintercept = 2015, linetype = "dashed") + geom_label_repel(data = labels, aes(x = x, y = y, label = agency), min.segment.length = 0, direction = "y", nudge_x = 5, hjust = 0) + scale_color_viridis_d() + scale_size(range = c(0.8, 2), guide = "none") + theme_gray(base_size = 15) + theme(legend.position = "none") + labs(x = "Year", y = "Homicide Rate per 100,000", title = "SCM-Selected Agency Trends in Homicide, 1990-2020")

fit2.att <- fit2.synth$real_y[fit2.synth$time_unit >= 2015] - fit2.synth$synth_y[fit2.synth$time_unit >= 2015]
round(fit2.att, 2)
mean(fit2.att)
sum(fit2.att)

