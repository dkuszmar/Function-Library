
# FOR PRINCIPLE COMPONENT ANALYSIS
principle_component_analyzer <- function(dataset, columns, center = TRUE, scale = TRUE) {
  dataset.pca <- prcomp(dataset[,c(columns)])
  ggbiplot(dataset.pca, labels = colnames(dataset.pca))
}
 # GENERATES SCREE PLOTS BASED ON PCA
scree_plotter <- function (dataset, num_pcs, title = "SCREE PLOT") {
  temp_scree <- dataset$sdev^2 / sum(dataset$sdev^2)
  qplot(c(num_pcs), temp_scree) +
    geom_line() +
    xlab("Principal Components") +
    ylab("Variance Explained") +
    ggtitle(title) +
    ylim(0,1)
}

# RUNS CHI FIT TEST BETWEEN TWO VARIABLES
chi_fit_tester <- function(var1, var2){
  chi_data <- data.frame(var1, var2)
  chi_data <- as.matrix(chi_data)
  chi_data <- as.table(chi_data)
  chisq.test(chi_data)
}

# RUNS PROBABILITY TESTING FOR DISCRETE UNIFORM DISTRIBUTIONS
fair_probability_tester <- function(possible_event_outcomes, total_possible_outcomes){
  possible_event_outcomes / total_possible_outcomes
}

# CALCULATES CONTINUOUS PROBABILITY DISTRIBUTION
continuous_probability_tester <- function(low_limit, high_limit, total_possible_outcomes){
  fractional_outcomes <- 1/total_possible_outcomes
  fractional_outcomes * (high_limit - low_limit)
}