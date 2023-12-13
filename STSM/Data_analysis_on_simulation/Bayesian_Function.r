# MARCO BALDO 01-12-2023 R-SCRIPT


'Computed from data “Bdv_predictors_table_for_stat_analysis.xlsx”.
Variable selection based on weighted version of horshoe priors, using also priors on variable signs.
Variables appearing in both linear and quadratic terms are mean-standardized (both for linear and quadratic terms).'


# CREATE DATAFRAMES FROM THE BAYESIAN STAT MODEL FOR BDV PREDICTORS


# BRYOPHYTES = [beta0 + beta1*"age"  + beta2*"volume_dw" ]
bryophytes_bayesian <- data.frame(
  Parameter = c("beta0", "beta[1]", "beta[2]", "sigma"),
  mean = c(9.29610, 0.02951, 0.07511, 4.36689),
  se_mean = c(0.02323, 0.00022, 0.00072, 0.00649),
  sd = c(0.93454, 0.00819, 0.02974, 0.32123),
  `2.5%` = c(7.49163, 0.01300, 0.01863, 3.76986),
  `25%` = c(8.64749, 0.02415, 0.05458, 4.15135),
  `50%` = c(9.29629, 0.02968, 0.07370, 4.35403),
  `75%` = c(9.92571, 0.03502, 0.09488, 4.57302),
  `97.5%` = c(11.13348, 0.04492, 0.13528, 5.04368),
  n_eff = c(1618, 1353, 1684, 2449),
  Rhat = c(1.00092, 1.00175, 1.00177, 1.00215)
)

# Print the data frame
print(bryophytes_bayesian)

R2_bryophytes_bayesian <- 0.3138344


# To select a single position of the list can use this command:
bryophytes_bayesian[1, "X2.5."] # for example for beta1 bryophytes 2.5% 


# LICHENS = [beta0 + beta1*"age" + beta2*"volume_dw"   + beta3*"lai_sim_1"   + beta4*"broadl_40_1" + beta5*"broadl_40_2"

# Creating a data frame
lichens_bayesian <-  data.frame(
    row.names = c("beta0", "beta[1]", "beta[2]", "beta[3]", "beta[4]", "beta[5]", "sigma"),
    mean = c(13.08087, 0.03269, 0.14800, -1.45191, 0.28999, -0.00783, 9.40341),
    se_mean = c(0.06977, 0.00051, 0.00155, 0.01512, 0.00375, 0.00014, 0.01410),
    sd = c(2.41656, 0.01752, 0.06139, 0.68097, 0.12792, 0.00531, 0.69372),
    `2.5%` = c(8.28928, 0.00352, 0.03030, -2.86803, 0.05974, -0.02065, 8.15239),
    `25%` = c(11.45072, 0.01948, 0.10549, -1.89524, 0.19930, -0.01108, 8.91221),
    `50%` = c(13.09615, 0.03147, 0.14759, -1.42724, 0.28198, -0.00713, 9.37454),
    `75%` = c(14.75087, 0.04431, 0.18820, -0.96457, 0.37278, -0.00357, 9.84019),
    `97.5%` = c(17.75719, 0.06976, 0.27118, -0.20916, 0.56473, -0.00052, 10.89957),
    n_eff = c(1200, 1188, 1568, 2028, 1166, 1496, 2421),
    Rhat = c(1.00013, 0.99996, 1.00129, 1.00007, 1.00061, 1.00048, 1.00043)
  )


# Display the data frame
print(lichens_bayesian)

R2_lichens_bayesian <- 0.3262373

# macrofungi = [beta0+ beta1*age"   + beta2*"volume_dw"   +  beta3*"ba_broadl"  + beta4*"trees_10_40_1"


# Creating a data frame
macrofungi_bayesian <- data.frame(
  row.names = c("beta0", "beta[1]", "beta[2]", "beta[3]", "beta[4]", "sigma"),
  mean = c(86.74255, 0.22843, 1.10102, 2.96326, 0.07011, 36.38183),
  se_mean = c(0.20057, 0.00232, 0.00616, 0.02473, 0.00083, 0.04850),
  sd = c(8.23929, 0.08326, 0.26077, 1.09449, 0.03764, 2.61388),
  `2.5%` = c(70.10431, 0.06419, 0.57899, 0.80328, 0.00816, 31.86734),
  `25%` = c(81.37735, 0.17372, 0.92817, 2.23056, 0.04189, 34.56523),
  `50%` = c(86.91990, 0.22621, 1.10588, 2.94625, 0.06710, 36.19260),
  `75%` = c(92.45331, 0.28308, 1.27645, 3.69042, 0.09516, 37.96503),
  `97.5%` = c(102.13053, 0.39827, 1.60687, 5.14782, 0.14964, 41.94464),
  n_eff = c(1687, 1287, 1790, 1959, 2039, 2904),
  Rhat = c(1.00364, 1.00804, 1.00212, 1.00047, 1.00233, 0.99940)
)

# Display the data frame
print(macrofungi_bayesian)

R2_macrofungi_bayesian  <- 0.5044866


# macrofungi_redlist = [beta0+ beta1*age"   + beta2*"volume_dw"   +  beta3*"ba_broadl"


# Creating a data frame
macrofungi_redlist_bayesian <- data.frame(
  row.names = c("beta0", "beta[1]", "beta[2]", "beta[3]", "sigma"),
  mean = c(100.44265, 0.28830, 2.54245, 4.11181, 56.08839),
  se_mean = c(0.26850, 0.00344, 0.00827, 0.04664, 0.07328),
  sd = c(11.54260, 0.11378, 0.37356, 1.66677, 4.08103),
  `2.5%` = c(77.44204, 0.05421, 1.78682, 0.84145, 48.91273),
  `25%` = c(92.88073, 0.21397, 2.29442, 2.97206, 53.18311),
  `50%` = c(100.38539, 0.28783, 2.55255, 4.09670, 55.80185),
  `75%` = c(108.28868, 0.36374, 2.79848, 5.28199, 58.76238),
  `97.5%` = c(122.75816, 0.51149, 3.25139, 7.43658, 64.59040),
  n_eff = c(1848, 1096, 2042, 1277, 3101),
  Rhat = c(1.00033, 1.00061, 0.99954, 1.00032, 0.99975)
)

# Display the data frame
print(macrofungi_redlist_bayesian)

R2_macrofungi_redlist_bayesian  <- 0.5911845


