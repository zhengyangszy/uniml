# Method references

Every method in `uniml` wraps an established statistical or machine-learning
procedure. This file maps each layer of the package to the literature it
implements, so a study that uses the package can cite the underlying methods
directly. DOIs are given where the publisher assigns one.

## Framework and reproducibility

| Method | Reference | Used by |
| --- | --- | --- |
| mlr3 framework | Lang M, Binder M, Richter J, et al. (2019). mlr3: A modern object-oriented machine learning framework in R. *Journal of Open Source Software*, 4(44), 1903. doi:10.21105/joss.01903 | All modelling layers |
| Hyperparameter optimization | Bischl B, Binder M, Lang M, et al. (2023). Hyperparameter optimization: Foundations, algorithms, best practices, and open challenges. *WIREs Data Mining and Knowledge Discovery*, 13(2), e1484. doi:10.1002/widm.1484 | `tune_model()`, `set_tuning()` |
| Bayesian optimization | Snoek J, Larochelle H, Adams RP (2012). Practical Bayesian optimization of machine learning algorithms. *Advances in Neural Information Processing Systems*, 25. | `tune_model(tuner = "bayesian")` |
| Efficient global optimization | Jones DR, Schonlau M, Welch WJ (1998). Efficient global optimization of expensive black-box functions. *Journal of Global Optimization*, 13, 455-492. doi:10.1023/A:1008306431147 | `tune_model(tuner = "bayesian")` |

## Reporting and study design

| Method | Reference | Used by |
| --- | --- | --- |
| TRIPOD+AI reporting | Collins GS, Moons KGM, Dhiman P, et al. (2024). TRIPOD+AI statement: updated guidance for reporting clinical prediction models that use regression or machine learning methods. *BMJ*, 385, e078378. doi:10.1136/bmj-2023-078378 | `tripod_ai_check()`, `export_results()` |
| TRIPOD (original) | Collins GS, Reitsma JB, Altman DG, Moons KGM (2015). Transparent reporting of a multivariable prediction model for individual prognosis or diagnosis (TRIPOD): the TRIPOD statement. *Annals of Internal Medicine*, 162(1), 55-63. doi:10.7326/M14-0697 | `tripod_ai_check()` |
| TRIPOD explanation | Moons KGM, Altman DG, Reitsma JB, et al. (2015). Transparent reporting of a multivariable prediction model for individual prognosis or diagnosis (TRIPOD): explanation and elaboration. *Annals of Internal Medicine*, 162(1), W1-W73. doi:10.7326/M14-0698 | `tripod_ai_check()` |
| Internal validation | Steyerberg EW, Harrell FE, Borsboom GJ, et al. (2001). Internal validation of predictive models: efficiency of some procedures for logistic regression analysis. *Journal of Clinical Epidemiology*, 54(8), 774-781. doi:10.1016/S0895-4356(01)00341-9 | `make_split()`, `run_workflow()` |
| Sample size | Riley RD, Ensor J, Snell KIE, et al. (2020). Calculating the sample size required for developing a clinical prediction model. *BMJ*, 368, m441. doi:10.1136/bmj.m441 | `tripod_ai_check()` |
| Prediction model methodology | Steyerberg EW (2019). *Clinical Prediction Models: A Practical Approach to Development, Validation, and Updating*, 2nd edition. Springer. doi:10.1007/978-3-030-16399-0 | Whole package |

## Missing data and multiple imputation

| Method | Reference | Used by |
| --- | --- | --- |
| Multiple imputation | Rubin DB (1987). *Multiple Imputation for Nonresponse in Surveys*. Wiley. doi:10.1002/9780470316696 | `impute_data(method = "mice")`, `pool_mi()` |
| Rubin's rules, small-sample df | Barnard J, Rubin DB (1999). Small-sample degrees of freedom with multiple imputation. *Biometrika*, 86(4), 948-955. doi:10.1093/biomet/86.4.948 | `pool_mi()`, `calc_ci()` |
| MICE algorithm | van Buuren S, Groothuis-Oudshoorn K (2011). mice: Multivariate imputation by chained equations in R. *Journal of Statistical Software*, 45(3), 1-67. doi:10.18637/jss.v045.i03 | `impute_data(method = "mice")` |
| MICE practice guidance | White IR, Royston P, Wood AM (2011). Multiple imputation using chained equations: issues and guidance for practice. *Statistics in Medicine*, 30(4), 377-399. | `impute_data(method = "mice")` |
| Random-forest imputation | Stekhoven DJ, Buhlmann P (2012). MissForest: non-parametric missing value imputation for mixed-type data. *Bioinformatics*, 28(1), 112-118. doi:10.1093/bioinformatics/btr597 | `impute_data(method = "missForest")` |

## Preprocessing and class imbalance

| Method | Reference | Used by |
| --- | --- | --- |
| SMOTE | Chawla NV, Bowyer KW, Hall LO, Kegelmeyer WP (2002). SMOTE: synthetic minority over-sampling technique. *Journal of Artificial Intelligence Research*, 16, 321-357. doi:10.1613/jair.953 | `balance_classes(method = "smote")` |
| ROSE | Menardi G, Torelli N (2014). Training and assessing classification rules with imbalanced data. *Data Mining and Knowledge Discovery*, 28(1), 92-122. doi:10.1007/s10618-012-0295-5 | `balance_classes(method = "rose")` |
| Harm of imbalance correction | van den Goorbergh R, van Smeden M, Timmerman D, Van Calster B (2022). The harm of class imbalance corrections for risk prediction models: illustration and simulation using logistic regression. *Journal of the American Medical Informatics Association*, 29(9), 1525-1534. doi:10.1093/jamia/ocac093 | Why `balance` defaults to `"none"` |

## Feature selection

| Method | Reference | Used by |
| --- | --- | --- |
| LASSO | Tibshirani R (1996). Regression shrinkage and selection via the lasso. *Journal of the Royal Statistical Society: Series B*, 58(1), 267-288. doi:10.1111/j.2517-6161.1996.tb02080.x | `select_lasso()` |
| Elastic net / glmnet | Friedman J, Hastie T, Tibshirani R (2010). Regularization paths for generalized linear models via coordinate descent. *Journal of Statistical Software*, 33(1), 1-22. doi:10.18637/jss.v033.i01 | `select_lasso()`, `classif.cv_glmnet` |
| Boruta | Kursa MB, Rudnicki WR (2010). Feature selection with the Boruta package. *Journal of Statistical Software*, 36(11), 1-13. doi:10.18637/jss.v036.i11 | `select_boruta()` |
| Recursive feature elimination | Guyon I, Weston J, Barnhill S, Vapnik V (2002). Gene selection for cancer classification using support vector machines. *Machine Learning*, 46(1), 389-422. doi:10.1023/A:1012487302797 | `select_rfe()` |
| Random-forest importance | Breiman L (2001). Random forests. *Machine Learning*, 45(1), 5-32. doi:10.1023/A:1010933404324 | `select_rf_importance()` |
| Filter benchmark | Bommert A, Sun X, Bischl B, Rahnenfuhrer J, Lang M (2020). Benchmark for filter methods for feature selection in high-dimensional classification data. *Computational Statistics & Data Analysis*, 143, 106839. doi:10.1016/j.csda.2019.106839 | `select_univariate()`, `select_multivariate()` |

## Learners

| Method | Reference | Used by |
| --- | --- | --- |
| Random forest (ranger) | Wright MN, Ziegler A (2017). ranger: a fast implementation of random forests for high dimensional data in C++ and R. *Journal of Statistical Software*, 77(1), 1-17. doi:10.18637/jss.v077.i01 | `rf` |
| XGBoost | Chen T, Guestrin C (2016). XGBoost: a scalable tree boosting system. *Proceedings of the 22nd ACM SIGKDD*, 785-794. doi:10.1145/2939672.2939785 | `xgb` |
| LightGBM | Ke G, Meng Q, Finley T, et al. (2017). LightGBM: a highly efficient gradient boosting decision tree. *Advances in Neural Information Processing Systems*, 30. | `lgb` |
| Elastic net | Zou H, Hastie T (2005). Regularization and variable selection via the elastic net. *Journal of the Royal Statistical Society: Series B*, 67(2), 301-320. doi:10.1111/j.1467-9868.2005.00503.x | `lr` |
| Modern applied statistics | Venables WN, Ripley BD (2002). *Modern Applied Statistics with S*, 4th edition. Springer. doi:10.1007/978-0-387-21706-2 | `nnet`, `knn`, `lda`, `qda`, `nb` |

## Discrimination, calibration, and clinical utility

| Method | Reference | Used by |
| --- | --- | --- |
| DeLong test | DeLong ER, DeLong DM, Clarke-Pearson DL (1988). Comparing the areas under two or more correlated receiver operating characteristic curves: a nonparametric approach. *Biometrics*, 44(3), 837-845. doi:10.2307/2531595 | `delong_test()` |
| Hosmer-Lemeshow test | Hosmer DW, Lemeshow S (1980). A goodness-of-fit test for the multiple logistic regression model. *Communications in Statistics - Theory and Methods*, 10, 1043-1069. doi:10.1080/03610928008827941 | `hl_test()` |
| Bootstrap intervals | Efron B, Tibshirani RJ (1993). *An Introduction to the Bootstrap*. Chapman & Hall. doi:10.1201/9780429246593 | `calc_ci()` |
| ROC analysis (pROC) | Robin X, Turck N, Hainard A, et al. (2011). pROC: an open-source package for R and S+ to analyze and compare ROC curves. *BMC Bioinformatics*, 12, 77. doi:10.1186/1471-2105-12-77 | `plot_roc()`, `calc_metrics()` |
| Calibration | Van Calster B, McLernon DJ, van Smeden M, et al. (2019). Calibration: the Achilles heel of predictive analytics. *BMC Medicine*, 17, 230. doi:10.1186/s12916-019-1466-7 | `plot_calibration()`, `calibrate_probabilities()` |
| Platt scaling | Platt J (1999). Probabilistic outputs for support vector machines and comparisons to regularized likelihood methods. In: *Advances in Large Margin Classifiers*. MIT Press. | `calibration = "platt"` |
| Isotonic calibration | Zadrozny B, Elkan C (2002). Transforming classifier scores into accurate multiclass probability estimates. *Proceedings of the 8th ACM SIGKDD*, 694-699. doi:10.1145/775047.775109 | `calibration = "isotonic"` |
| Calibration and model selection | Niculescu-Mizil A, Caruana R (2005). Predicting good probabilities with supervised learning. *Proceedings of the 22nd ICML*, 625-632. doi:10.1145/1102351.1102430 | `calibrate_probabilities()` |
| Decision curve analysis | Vickers AJ, Elkin EB (2006). Decision curve analysis: a novel method for evaluating prediction models. *Medical Decision Making*, 26(6), 565-574. doi:10.1177/0272989X06295361 | `dca()` |
| Net benefit | Vickers AJ, van Calster B, Steyerberg EW (2016). Net benefit approaches to the evaluation of prediction models, molecular markers, and diagnostic tests. *BMJ*, 352, i6. doi:10.1136/bmj.i6 | `dca()`, `plot_dca()` |
| NRI | Pencina MJ, D'Agostino RB Sr, D'Agostino RB Jr, Vasan RS (2008). Evaluating the added predictive ability of a new marker: from area under the ROC curve to reclassification and beyond. *Statistics in Medicine*, 27(2), 157-172. doi:10.1002/sim.2929 | `calc_nri_idi()` |
| IDI and continuous NRI | Pencina MJ, D'Agostino RB Sr, Steyerberg EW (2011). Extensions of net reclassification improvement calculations to measure usefulness of new biomarkers. *Statistics in Medicine*, 30(1), 11-21. doi:10.1002/sim.4067 | `calc_nri_idi()` |
| Youden index | Youden WJ (1950). Index for rating diagnostic tests. *Cancer*, 3(1), 32-35. | `find_threshold(method = "youden")` |
| Brier score | Brier GW (1950). Verification of forecasts expressed in terms of probability. *Monthly Weather Review*, 78(1), 1-3. | `calc_metrics()` |
| Cohen's kappa | Landis JR, Koch GG (1977). The measurement of observer agreement for categorical data. *Biometrics*, 33(1), 159-174. doi:10.2307/2529310 | `calc_metrics()` |

## Explainability

| Method | Reference | Used by |
| --- | --- | --- |
| SHAP | Lundberg SM, Lee SI (2017). A unified approach to interpreting model predictions. *Advances in Neural Information Processing Systems*, 30. | `explain_shap()` |
| TreeSHAP | Lundberg SM, Erion G, Chen H, et al. (2020). From local explanations to global understanding with explainable AI for trees. *Nature Machine Intelligence*, 2(1), 56-67. doi:10.1038/s42256-019-0138-9 | Native xgboost/lightgbm path |
| Kernel SHAP | Covert I, Lee SI (2021). Improving KernelSHAP: practical Shapley value estimation using linear regression. *Proceedings of AISTATS*, PMLR 130, 3457-3465. | `explain_shap()` model-agnostic path |
| Interpretable ML | Molnar C (2022). *Interpretable Machine Learning*, 2nd edition. https://christophm.github.io/interpretable-ml-book/ | `plot_shap_*()` |

## Graphics

| Method | Reference | Used by |
| --- | --- | --- |
| ggplot2 | Wickham H (2016). *ggplot2: Elegant Graphics for Data Analysis*, 2nd edition. Springer. doi:10.1007/978-3-319-24277-4 | All plots |
| Graphical presentation of prediction models | Harrell FE (2015). *Regression Modeling Strategies*, 2nd edition. Springer. doi:10.1007/978-3-319-19425-7 | `plot_roc()`, `plot_calibration()`, `plot_dca()` |

## How to cite a study that used this package

Report the methods you actually used, for example: "Models were developed with
the `uniml` R package (version x.y.z), which implements the mlr3 framework (Lang
et al., 2019). Discrimination was assessed with the C-statistic and 95%
bootstrap confidence intervals (Efron & Tibshirani, 1993); calibration was
assessed with the calibration curve and Brier score (Van Calster et al., 2019);
clinical utility was assessed with decision curve analysis (Vickers & Elkin,
2006). Reporting followed TRIPOD+AI (Collins et al., 2024)."
