**So You Want to Do an Evaluation: Quantitative Tools for Program Evaluation**

This repository contains all the materials—slide decks, data files, R scripts—for a workshop on evaluation methods given at the American Society of Criminology meeting in November 2023. The workshop was designed in three parts, although there was only time to cover the first two. Although the workshop was taught in R and RStudio, the material also includes Stata do-files so participants who prefer that program would be able to perform the same procedures.

1. Experimental Design
   - Analysis of experimental data: Intention-to-treat analysis; per-protocol analysis; as-treated analysis; instrumental variable analysis
     - Compliance versus non-compliance
     - Estimands: Average treatment effect; average treatment effect on the treated; average treatment effect among compliers
   - Effect size estimation
   - Randomization inference
   - Incorporating pretest variables: Semi-parametric regression adjustment; reweighting estimator for missing outcome data
   - Empirical application: Impact of Supported Work on earnings
     - R packages: drgee, effectsize, ivreg, ritest

2. Difference in Differences Design
   - Analysis of quasi-experimental data: Two-way fixed effects; event study
     - Anticipation of treatment; non-parallel trends
   - Staggered difference in differences: Group-time estimator
   - Bootstrap inference
   - Empirical application: Impact of criminal conviction on employment
     - R packages: did, fixest

3. Synthetic Control Design
   - Analysis of comparative case study data: Two-way fixed effects; synthetic control
   - Permutation inference
   - Empirical application: Impact of de-prosecution on homicide in Philadelphia
     - R packages: tidysynth
