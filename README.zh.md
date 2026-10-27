# uniml

[English](README.md) | **简体中文**

[![R-CMD-check](https://github.com/zhengyangszy/uniml/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/zhengyangszy/uniml/actions/workflows/R-CMD-check.yaml)

`uniml` 是一个面向**二分类与多分类预测模型**的统一建模工具包。它把数据切分、缺失值
填补、特征筛选、模型训练与调参、性能评价、可视化、SHAP 可解释性串成一条流水线，同时
强制区分「用来拟合模型的数据」和「用来评价模型的数据」，避免信息泄漏。

整个包按层组织（L0–L9）：每一层都能单独使用，`run_workflow()` 把它们串起来。

| 层 | 主要函数 | 作用 |
| --- | --- | --- |
| L0 数据 | `uniml_spec()`、`as_uniml_data()`、`make_split()` | 声明任务、变量角色、数据切分 |
| L1 任务类型 | `list_task_types()`、`task_type_spec()` | 可扩展的任务类型体系 |
| L2 预处理 | `impute_data()`、`scale_numeric()`、`encode_categorical()`、`balance_classes()` | 缺失值填补、标准化、哑变量、类别平衡 |
| L3 特征筛选 | `select_univariate()`、`select_lasso()`、`select_boruta()`、`select_rfe()`、`remove_collinear()` | 单因素/多因素/机器学习筛选与共线性处理 |
| L4 建模 | `make_task()`、`resolve_learners()`、`train_model()`、`tune_model()` | 基于 mlr3 的学习器、调参、交叉验证 |
| L5 统计 | `calc_metrics()`、`calc_ci()`、`calibration_metrics()`、`multiclass_calibration()`、`calibrate_probabilities()`、`delong_test()`、`hl_test()`、`calc_nri_idi()`、`dca()`、`find_threshold()` | 指标与置信区间、校准（含多分类）、统计检验、决策曲线 |
| L6 结果 | `assemble_result()`、`rank_models()`、`export_results()`、`save_result()`、`read_result()`、`tripod_ai_check()` | 模型排序、结果导出、模型交接、TRIPOD+AI 自检 |
| L7 绘图 | `plot_roc()`、`plot_pr_curve()`、`plot_calibration()`、`plot_dca()`、`plot_confusion()`、`theme_uniml()` | 论文级图形 |
| L8 可解释性 | `explain_shap()`、`plot_shap_*()`、`extract_thresholds()` | SHAP 重要性、蜂群、依赖、瀑布图 |
| L9 工作流 | `run_workflow()`、`predict_uniml()` | 一键跑通与复用已拟合状态 |

## 为什么用 uniml

`uniml` 不取代 mlr3，也不取代 tidymodels。它只针对一件事给出有主见的编排：**产出一个
二分类风险模型，外加一份你答辩得住的报告**。

**和直接用 mlr3 比。** mlr3 提供学习器、任务、重采样、调参，并按你给的顺序执行。出问题
的恰恰是「顺序」：只要在整份数据上拟合了标准化或特征筛选，之后所有交叉验证数字都会偏
乐观。`uniml` 把这套顺序固定下来——只在训练集上拟合、只把参数套到留出集、绝不回头——
并把剩下的选择变成显式的 `leakage_policy` 对象，而不是脚本写法带来的副作用；同时补上
mlr3 留给你自己写的报告层：bootstrap 置信区间、折外预测拟合的校准、DeLong 与
Hosmer–Lemeshow 检验、NRI/IDI、决策曲线、多重插补的 Rubin 池化、TRIPOD+AI 自检。

**和 tidymodels 比。** `recipes` / `workflows` / `tune` 把建模机制做得很好。`uniml`
瞄准的是**临床预测模型的报告路径**——统计量与审计轨迹——并建立在 mlr3 学习器生态上，
所以里面每个模型都是你可以直接拿去用的 mlr3 学习器。

**两边都不直接给的东西。** 严格 Rubin 规则作用在整条流水线上（而不是只针对一个指标）、
能把每张图在 R 之外重建出来的导出目录、以及失败被记进 `result$meta$failures` 而不是
悄悄消失。

**什么情况下别用它。** 只要一个模型和一个 AUC，直接上 mlr3 更简单；要多分类、生存分析
或深度学习，这个包目前还不行。

## 适用范围与限制

- **两种任务类型：二分类与多分类。** `run_workflow()` **只支持二分类**：它的预测表、
  阈值、校准器、事后检验都建立在「单个概率列」上，所以多分类规格会直接报错并指向
  `vignette("uniml-multiclass")`。多分类在**逐层使用**的意义上完整可用——规格、数据、
  任务、学习器、调参、指标、校准、绘图都支持。
- **部分二分类方法没有多分类对应物。** 决策曲线、NRI/IDI、DeLong、
  Hosmer–Lemeshow 都需要一个阳性类；多分类下明确拒绝，而不是替你选一个约定。
  校准**有**多分类版本，但把约定写在明处：top-label 的 ECE/MCE 与 class-wise ECE
  （`multiclass_calibration()`）。多分类的**重新校准**没有：Platt 与 isotonic 本质是
  一对其余，把 K 个拼起来并不会得到一个校准好的多项分布。
- **生存分析不在本包范围内。** 它是独立包（数据模型不同：时间 + 状态 + 删失）。
- **示例数据是模拟的。** `inst/extdata` 两个文件都由
  `data-raw/simulate-cvd-mortality.R` 生成，6.5% 的事件率是模拟参数。它们的作用是让
  示例可运行、可复现；**不是真实患者队列**，在它们身上算出来的数字不能说明临床性能，
  也不能说明这些方法在真实数据上的表现。
- **只支持表格数据框。** 数值、因子、字符、逻辑四类列。不支持图像、文本、纵向/时变
  协变量、删失、竞争风险。
- **缺失机制假定为随机缺失（MAR）。** 填补方式为 `median_mode`、`mice`、`missForest`
  或 `missRanger`；缺失依赖于未观测值的机制没有建模。
- **不做公平性/亚组分析。** `tripod_ai_check()` 只会把这几条标成「缺失」，写不写是你
  的事。
- **固定学习器清单 + 你自己注入的。** 7 个默认、4 个可选；其他 mlr3 学习器也能用，但
  不提供搜索空间时不会被调参。
- **不是部署服务。** `predict_uniml()` 是在 R 里给数据框打分，没有 REST 接口、没有模型
  仓库、没有漂移监控。

## 安装

包还没有上 CRAN，从 GitHub 安装：

```r
# install.packages("remotes")
remotes::install_github("zhengyangszy/uniml")
```

内置 7 个默认模型和 4 个可选模型，详见 [可用模型](#可用模型)。**任何 mlr3 分类学习器
都能直接用**，包括不在 CRAN 上的扩展包，先 `library()` 再按学习器名传入即可：

```r
# remotes::install_github("mlr-org/mlr3extralearners")
library(mlr3extralearners)
spec <- set_learners(spec, "classif.lightgbm", tune = FALSE)
```

（旧版的 `lgb` / `randomForest` / `gbm` 快捷键已移除，现在用它们会提示你改用
对应的 mlr3 学习器名。）

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

## 示例数据

`inst/extdata` 里是两份**模拟**的心血管队列：

| 文件 | 行数 | 用途 |
| --- | ---: | --- |
| `uniml-cvd-mortality.csv` | 2000 | 开发队列，切分为训练集与验证集 |
| `uniml-cvd-mortality-new.csv` | 2000 | 独立的新入院队列，作为外部测试集 |

两份数据的目标院内死亡率都设为 6.5%，实际事件率会有小幅差异（结局是抽样得到的）。
新入院队列与开发队列独立生成，标识符不重叠。

## 可用模型

每个模型都是 mlr3 `Learner`，每份数据都变成 mlr3 `Task`，调参与重采样直接用 mlr3
对象。默认提供 7 个稳定学习器：

| 键 | mlr3 学习器 |
| --- | --- |
| `lr` | `classif.cv_glmnet` |
| `rf` | `classif.ranger` |
| `xgb` | `classif.xgboost` |
| `nnet` | `classif.nnet` |
| `knn` | `classif.kknn` |
| `cart` | `classif.rpart` |
| `nb` | `classif.naive_bayes` |

另有 4 个可选学习器，可以按名字直接用，但既不会默认安装、也不提供搜索空间（使用 mlr3
默认参数）：

| 键 | mlr3 学习器 | 需要的包 |
| --- | --- | --- |
| `log_reg` | `classif.log_reg` | stats |
| `svm` | `classif.svm` | e1071 |
| `lda` | `classif.lda` | MASS |
| `qda` | `classif.qda` | MASS |

不限于这些键：任何已安装的 mlr3 分类学习器都能传给 `set_learners()`，此时使用 mlr3
默认参数，除非你提供搜索空间。

```r
spec <- set_learners(spec, c("lr", "classif.log_reg", "classif.svm"), tune = TRUE)
```

自定义搜索空间：

```r
register_learner(
  "svm_custom",
  mlr3::lrn("classif.svm", predict_type = "prob", probability = TRUE),
  paradox::ps(cost = paradox::p_dbl(0.1, 10), gamma = paradox::p_dbl(0.001, 1))
)
spec <- set_learners(spec, "svm_custom", tune = TRUE)
```

## 步骤参数

每个阶段都接受显式参数列表，**写错名字会直接报错**，不会被静默忽略：

```r
spec <- set_preprocess(
  spec,
  impute = "mice",
  impute_args = list(m = 5, max_iter = 10),
  scale_args = list(vars = c("age", "bmi")),
  encode_args = list(max_levels = 20)
)
spec <- set_feature_selection(
  spec,
  methods = c("lasso", "boruta"),
  args = list(lasso = list(nfolds = 5, alpha = 0.8))
)
spec <- set_tuning(spec, folds = 5, repeats = 5, terminator = "seconds", seconds = 600)
spec <- set_evaluation(
  spec,
  calibration = "platt",
  calibration_args = list(folds = 5, bins = 10, fit = "out_of_fold")
)
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

## 泄漏策略

`set_leakage_policy()` 决定哪些步骤在**交叉验证的每一折里重做**：

| 模式 | 每折重做的步骤 | 什么时候用 |
| --- | --- | --- |
| `strict_in` | 填补、标准化、编码、特征筛选、平衡、调参 | 想要最不乐观的交叉验证估计，且跑得动 |
| `strict_out` | 无 | 步骤很贵（Boruta、RFE、调参）且数据量大 |
| `custom` | 特征筛选、平衡、调参 | 想自己指定折中方案 |

```r
spec <- set_leakage_policy(spec, mode = "custom", in_cv = c("selection", "balance"))
```

两点提醒：

- `strict_out` 会在**训练集全量上拟合一次**筛选、平衡、调参。这些步骤因此见过每一行
  训练数据，折内得分对它们来说是偏乐观的。交叉验证仍然适合**比较模型**，但不要当成
  无偏的绝对性能报告。
- 无论哪种模式，`validation`、`test`、`external` 都不参与拟合、调参、校准、阈值选择。
  `strict_out` 放宽的是**训练集内部**的做法，不是外部。

## 交叉验证与校准

`run_workflow()` 会输出每折的交叉验证结果和汇总；`repeats > 1` 启用重复交叉验证，
汇总时先在每个重复内对折取平均，再跨重复汇总。校准默认关闭（`calibration = "none"`），
开启后在训练集的**折外预测**上拟合校准器，并同时保留原始与校准后的概率；分类阈值同样
来自折外预测。任何被降级或失败的步骤都会写进 `result$meta$failures`，不会静默丢弃。

## 导出与复用

`export_results()` 会把所有需要的东西写成纯 CSV 加一个 RDS：
`predictions/predictions.csv`（模型、数据集、原始与校准概率、真实标签）足以重建
ROC/校准/DCA/混淆矩阵；`performance/thresholds.csv` 存每个模型的两套阈值；
`shap/values_matrix_*.csv` 与 `shap/feature_values_*.csv` 可以自己画 SHAP 图；
`result.rds` 是完整拟合状态，供 `predict_uniml()` 给新数据打分。

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
- `vignette("uniml-realdata")`：**真实数据**——用 `pROC::aSAH`（113 例蛛网膜下腔出血
  的公开队列）跑完整流程。演示 `gos6` 必须排除（它是结局的原始五分类）、bootstrap
  分布退化时比值的区间给 `NA` 而不是编数、以及 Hosmer–Lemeshow 统计量无法定义时
  返回 `not_estimable` 而不是 `NaN`。
- `vignette("uniml-multiclass")`：**多分类**——指标约定、k×k 混淆矩阵、一对多 ROC，
  以及哪些方法在多分类下刻意没有实现、为什么。

## 开发状态

`uniml` 当前版本 `0.2.0`，实现了二分类与多分类任务类型（`survival` 仍是占位）。
GitHub Actions 在 Windows、macOS、Linux 三平台做 `R CMD check`；
`tests/testthat/test-consistency.R` 把二分类指标层钉在独立实现上（`mlr3measures`、
`pROC`、`PRROC`、`stats::glm()`、`stats::isoreg()`、`ResourceSelection`），
`test-multiclass.R` 把多分类指标钉在 `mlr3measures` 与手算上。变更见 `NEWS.md`。

## 许可

MIT。
