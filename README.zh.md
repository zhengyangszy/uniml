# uniml

[English](README.md) | **简体中文**

[![R-CMD-check](https://github.com/zhengyangszy/uniml/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/zhengyangszy/uniml/actions/workflows/R-CMD-check.yaml)

`uniml` 是一个面向**二分类预测模型**的统一建模工具包。它把数据切分、缺失值填补、
特征筛选、模型训练与调参、性能评价、可视化、SHAP 可解释性串成一条流水线，同时强制
区分「用来拟合模型的数据」和「用来评价模型的数据」，避免信息泄漏。

整个包按层组织（L0–L9）：每一层都能单独使用，`run_workflow()` 把它们串起来。

| 层 | 主要函数 | 作用 |
| --- | --- | --- |
| L0 数据 | `uniml_spec()`、`as_uniml_data()`、`make_split()` | 声明任务、变量角色、数据切分 |
| L1 任务类型 | `list_task_types()`、`task_type_spec()` | 可扩展的任务类型体系 |
| L2 预处理 | `impute_data()`、`scale_numeric()`、`encode_categorical()`、`balance_classes()` | 缺失值填补、标准化、哑变量、类别平衡 |
| L3 特征筛选 | `select_univariate()`、`select_lasso()`、`select_boruta()`、`select_rfe()`、`remove_collinear()` | 单因素/多因素/机器学习筛选与共线性处理 |
| L4 建模 | `make_task()`、`resolve_learners()`、`train_model()`、`tune_model()` | 基于 mlr3 的学习器、调参、交叉验证 |
| L5 统计 | `calc_metrics()`、`calc_ci()`、`calibrate_probabilities()`、`delong_test()`、`hl_test()`、`calc_nri_idi()`、`dca()`、`find_threshold()` | 指标与置信区间、校准、统计检验、决策曲线 |
| L6 结果 | `assemble_result()`、`rank_models()`、`export_results()`、`tripod_ai_check()` | 模型排序、结果导出、TRIPOD+AI 自检 |
| L7 绘图 | `plot_roc()`、`plot_calibration()`、`plot_dca()`、`plot_confusion()`、`theme_uniml()` | 论文级图形 |
| L8 可解释性 | `explain_shap()`、`plot_shap_*()`、`extract_thresholds()` | SHAP 重要性、蜂群、依赖、瀑布图 |
| L9 工作流 | `run_workflow()`、`predict_uniml()` | 一键跑通与复用已拟合状态 |

## 安装

包还没有上 CRAN，从 GitHub 安装：

```r
# install.packages("remotes")
remotes::install_github("zhengyangszy/uniml")
```

`lgb`（LightGBM）需要额外安装 `mlr3extralearners`（不在 CRAN 上）：

```r
remotes::install_github("mlr-org/mlr3extralearners")
```

## 快速上手

```r
library(uniml)

development <- read.csv(
  system.file("extdata", "uniml-cvd-mortality.csv", package = "uniml")
)

spec <- uniml_spec(
  task_type = "binary",
  outcome = "died",
  positive = "yes",
  id = "id"
)
spec <- set_preprocess(spec, impute = "median_mode", scale = "zscore", encode = "onehot")
spec <- set_feature_selection(spec, methods = c("univariate", "lasso"), combine = "intersect")
spec <- set_learners(spec, c("lr", "cart"), tune = FALSE)
spec <- set_evaluation(
  spec,
  ci = "bootstrap",
  calibration = "platt",
  select_on = "validation"
)

split <- make_split(as_uniml_data(development, spec), prop = c(0.8, 0.2), stratify = TRUE, seed = 123)

result <- run_workflow(
  spec,
  train = split$train$data,
  validation = split$validation$data,
  ci_R = 200
)

result$metrics
result$ranking
plot_roc(result)
plot_calibration(result)
plot_dca(result)
```

## 数据角色（重要）

`uniml` 完全建立在 mlr3 之上：每个模型都是 mlr3 `Learner`，每份数据都变成 mlr3 `Task`，
调参与重采样直接用 mlr3 对象。包只做一层薄封装，不把 mlr3 藏起来：

- `make_task()` 把数据框 + 规格变成 `TaskClassif`；
- `resolve_learners()` 返回 mlr3 学习器及其搜索空间，你也可以自己用 `mlr3tuning` 调；
- `run_workflow()` 把拟合好的学习器存在 `result$models[[key]]$learner`；
- `as_uniml_binary()` 把 mlr3 的 `PredictionClassif` 转成评价/绘图函数接受的对象，
  `plot_roc()`、`plot_calibration()`、`plot_confusion()` 也能直接吃 mlr3 预测。

| 数据 | 用途 | 是否参与模型选择 |
| --- | --- | --- |
| `train` | 拟合预处理、特征筛选、模型、校准、阈值 | 是（内部交叉验证） |
| `validation` | 模型比较与选择（默认 `select_on = "validation"`） | 是 |
| `test` | 只报告性能，不参与选择 | 否 |
| `external` | 外部验证，只报告泛化性能 | 否 |

`select_on = "test"` 仍然可选，但会警告（会产生乐观偏倚）。`external` 永远不会被用于
选择。

## 方法学约定

- **多重插补**用严格 Rubin 规则池化：`T = U + (1 + 1/m)B`，Barnard–Rubin 自由度，
  t 分布区间，并把区间裁剪到指标本身的取值范围内（如 AUC ∈ [0, 1]）。
- **校准与阈值**都在训练集的**折外（OOF）预测**上拟合，保证不使用验证集/测试集信息。
- **类别不平衡校正默认关闭**（`balance = "none"`），因为对风险预测模型做重采样会
  改变预测概率的含义（van den Goorbergh et al., 2022）。
- **失败可见**：任何模型/绘图/阈值计算失败都会记录在 `result$meta$failures`，并在
  结束时给出 warning，不会被静默丢弃。
- **导出可复用**：`export_results()` 写出 CSV/RDS/图片，可直接用 Excel、Python 或
  其他 R 脚本二次作图。

## 报告规范

包内置 `tripod_ai_check()`，按 TRIPOD+AI 的条目域检查结果对象里有哪些信息、缺哪些、
哪些需要作者自己撰写（例如研究设计、亚组公平性、代码可得性）。

```r
checklist <- tripod_ai_check(result)
table(checklist$status)
```

函数的完整方法学参考文献见 `system.file("REFERENCES.md", package = "uniml")`。

## 文档

- `vignette("uniml-workflow")`：完整工作流（含外部验证、NRI/IDI、阈值、TRIPOD 自检）
- `vignette("uniml-shap")`：SHAP 解释与可视化
- `vignette("uniml-atomic")`：逐层单独使用各函数

## 许可

MIT。
