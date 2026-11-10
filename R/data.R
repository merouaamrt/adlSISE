#' Wine recognition data
#'
#' Results of a chemical analysis of 178 Italian wines grown in the same
#' region but derived from three different cultivars. The 13 measured
#' constituents are all numeric, which makes the data a classical example
#' for linear discriminant analysis.
#'
#' @format A data frame with 178 rows and 14 columns:
#' \describe{
#'   \item{alcohol}{Alcohol.}
#'   \item{malic_acid}{Malic acid.}
#'   \item{ash}{Ash.}
#'   \item{alcalinity}{Alcalinity of ash.}
#'   \item{magnesium}{Magnesium.}
#'   \item{total_phenols}{Total phenols.}
#'   \item{flavanoids}{Flavanoids.}
#'   \item{nonflavanoid_phenols}{Nonflavanoid phenols.}
#'   \item{proanthocyanins}{Proanthocyanins.}
#'   \item{color_intensity}{Color intensity.}
#'   \item{hue}{Hue.}
#'   \item{od280_od315}{OD280/OD315 of diluted wines.}
#'   \item{proline}{Proline.}
#'   \item{class}{Factor with 3 levels (`"1"`, `"2"`, `"3"`): the cultivar.
#'     There are 59, 71 and 48 wines in the three classes.}
#' }
#' There are no missing values.
#'
#' @source Aeberhard, S. and Forina, M. (1992). Wine. UCI Machine Learning
#'   Repository. \doi{10.24432/C5PC7J}.
#'   <https://archive.ics.uci.edu/dataset/109/wine>.
#'   Licensed under
#'   [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).
#'   The data are reproduced here without modification apart from the
#'   column names and the order of the columns (the class is the last
#'   column); see `data-raw/wine.R`.
#'
#' @examples
#' model <- ADL$new(selection = "forward")$fit(wine[, 1:13], wine$class)
#' model$selected_variables
#' model$confusion
"wine"

#' Breast Cancer Wisconsin (Original) data
#'
#' Clinical samples of breast tumours collected by Dr. W. H. Wolberg at the
#' University of Wisconsin Hospitals. Nine cytological characteristics, each
#' scored from 1 to 10, describe a sample, which is either benign or
#' malignant. The sample identifier of the original file has been removed.
#'
#' @format A data frame with 699 rows and 10 columns:
#' \describe{
#'   \item{clump_thickness}{Clump thickness (1 to 10).}
#'   \item{cell_size_uniformity}{Uniformity of cell size (1 to 10).}
#'   \item{cell_shape_uniformity}{Uniformity of cell shape (1 to 10).}
#'   \item{marginal_adhesion}{Marginal adhesion (1 to 10).}
#'   \item{epithelial_cell_size}{Single epithelial cell size (1 to 10).}
#'   \item{bare_nuclei}{Bare nuclei (1 to 10); 16 missing values.}
#'   \item{bland_chromatin}{Bland chromatin (1 to 10).}
#'   \item{normal_nucleoli}{Normal nucleoli (1 to 10).}
#'   \item{mitoses}{Mitoses (1 to 10).}
#'   \item{class}{Factor with 2 levels (`"benign"`, `"malignant"`): 458
#'     benign and 241 malignant samples.}
#' }
#' The 16 missing values (coded `"?"` in the original file) are all in
#' `bare_nuclei` and are kept as `NA`: remove the incomplete rows, for
#' example with [stats::na.omit()], before calling [ADL].
#'
#' @source Wolberg, W. (1990). Breast Cancer Wisconsin (Original). UCI
#'   Machine Learning Repository. \doi{10.24432/C5HP4Z}.
#'   <https://archive.ics.uci.edu/dataset/15/breast+cancer+wisconsin+original>.
#'   Licensed under
#'   [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).
#'   The sample identifier was dropped and the columns were renamed; see
#'   `data-raw/breast_cancer.R`.
#'
#' @examples
#' complete <- na.omit(breast_cancer)
#' model <- ADL$new()$fit(complete[, 1:9], complete$class)
#' model$confusion
"breast_cancer"

#' Heart Disease data (Cleveland database)
#'
#' Data of 303 patients of the Cleveland Clinic Foundation who underwent a
#' coronary angiography. Thirteen clinical variables, numeric and
#' qualitative, are used to predict the presence of a heart disease. This
#' is the only one of the four original heart disease databases that is
#' commonly used; the original 0 to 4 diagnosis was reduced to a binary
#' target.
#'
#' @format A data frame with 303 rows and 14 columns:
#' \describe{
#'   \item{age}{Age in years.}
#'   \item{sex}{Factor: `"female"`, `"male"`.}
#'   \item{cp}{Chest pain type, factor: `"typical_angina"`,
#'     `"atypical_angina"`, `"non_anginal"`, `"asymptomatic"`.}
#'   \item{trestbps}{Resting blood pressure on admission (mm Hg).}
#'   \item{chol}{Serum cholesterol (mg/dl).}
#'   \item{fbs}{Fasting blood sugar above 120 mg/dl, factor: `"no"`,
#'     `"yes"`.}
#'   \item{restecg}{Resting electrocardiographic results, factor:
#'     `"normal"`, `"st_t_abnormality"`, `"lv_hypertrophy"`.}
#'   \item{thalach}{Maximum heart rate achieved.}
#'   \item{exang}{Exercise induced angina, factor: `"no"`, `"yes"`.}
#'   \item{oldpeak}{ST depression induced by exercise relative to rest.}
#'   \item{slope}{Slope of the peak exercise ST segment, factor:
#'     `"upsloping"`, `"flat"`, `"downsloping"`.}
#'   \item{ca}{Number of major vessels (0 to 3) coloured by fluoroscopy;
#'     4 missing values.}
#'   \item{thal}{Thallium stress test result, factor: `"normal"`,
#'     `"fixed_defect"`, `"reversible_defect"`; 2 missing values.}
#'   \item{disease}{Factor with 2 levels (`"no"`, `"yes"`): presence of a
#'     heart disease (diagnosis 1 to 4 in the original data). There are
#'     164 patients without and 139 with a disease.}
#' }
#' The 6 missing values (coded `"?"` in the original file) are kept as
#' `NA`: remove the incomplete rows, for example with [stats::na.omit()],
#' before calling [ADL].
#'
#' @source Janosi, A., Steinbrunn, W., Pfisterer, M. and Detrano, R.
#'   (1989). Heart Disease. UCI Machine Learning Repository.
#'   \doi{10.24432/C52P4X}.
#'   <https://archive.ics.uci.edu/dataset/45/heart+disease>.
#'   Licensed under
#'   [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).
#'   Qualitative variables were recoded as factors with explicit labels and
#'   the target was made binary; see `data-raw/heart_disease.R`.
#'
#' @examples
#' complete <- na.omit(heart_disease)
#' model <- ADL$new()$fit(complete[, 1:13], complete$disease)
#' model$confusion
"heart_disease"
