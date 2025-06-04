# MARCO BALDO 01-12-2023 R-SCRIPT


'Bayesian_model_results_V1: Computed from data "C:\iLand\2023\20230901_Bottoms_Up\Sources_bottoms_up\Jenik\final_table_imp\tables_for_stat\Bdv_predictors_table_BayesianMod_results_track\01_Bdv_predictors_table_BayesianMod_results.xlsx”.
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



#------------------------------------------------------------------------------- 2nd step
# MARCO BALDO 10-11-2024 R-SCRIPT


'Bayesian_model_results_V3: Computed from data "C:\iLand\2023\20230901_Bottoms_Up\Sources_bottoms_up\Jenik\final_table_imp\tables_for_stat\Bdv_predictors_table_BayesianMod_results_track\20_Bdv_predictors_table_BayesianMod_results_th_with_elevation_mng_DWC_GAMage_snags_tot_deadwood.xlsx”.
Variable selection based on weighted version of horshoe priors, using also priors on variable signs.
Variables appearing in both linear and quadratic terms are mean-standardized (both for linear and quadratic terms).'


# CREATE DATAFRAMES FROM THE BAYESIAN STAT MODEL FOR BDV PREDICTORS

# BRYOPHYTES = [beta0 + beta1*"age"  + beta2*"deadwood" ]
bryophytes_bayesian_updated <- data.frame(
  Parameter = c("beta0", "beta[1]", "beta[2]", "sigma"),
  mean = c(9.66445, 0.01634, 0.00004, 4.64611),
  se_mean = c(0.03181, 0.00023, 0.00000, 0.00659),
  sd = c(1.07307, 0.00752, 0.00001, 0.32912),
  `2.5%` = c(7.49491, 0.00269, 0.00001, 4.06785),
  `25%` = c(8.92257, 0.01101, 0.00003, 4.40523),
  `50%` = c(9.71337, 0.01584, 0.00004, 4.63247),
  `75%` = c(10.43265, 0.02123, 0.00004, 4.86033),
  `97.5%` = c(11.67784, 0.03236, 0.00006, 5.33883),
  n_eff = c(1138, 1077, 1867, 2496),
  Rhat = c(1.00033, 1.00103, 0.99992, 1.00034)
)

# Print the updated data frame
print(bryophytes_bayesian_updated)

# Updated R-squared
R2_bryophytes_bayesian_updated <- 0.2157045

# To select a single position of the list can use this command:
bryophytes_bayesian[1, "X2.5."] # for example for beta1 bryophytes 2.5% 


# LICHENS = [beta0 + beta1*"age" + beta2*"lai_sim_1"  + beta3*"broadl_40_1"

# Creating a data frame
lichens_bayesian <- data.frame(
  Parameter = c("beta0", "beta[1]", "beta[2]", "beta[3]", "sigma"),
  mean = c(9.82038, 0.04728, -1.65763, 0.12329, 8.85680),
  se_mean = c(0.06606, 0.00043, 0.01700, 0.00178, 0.01327),
  sd = c(2.38061, 0.01488, 0.65896, 0.07304, 0.64741),
  `2.5%` = c(5.17480, 0.01805, -2.98086, 0.01149, 7.69310),
  `25%` = c(8.18501, 0.03711, -2.10063, 0.06623, 8.40647),
  `50%` = c(9.82144, 0.04723, -1.64795, 0.11608, 8.82772),
  `75%` = c(11.40871, 0.05726, -1.20163, 0.17120, 9.26695),
  `97.5%` = c(14.39949, 0.07614, -0.37808, 0.28592, 10.19884),
  n_eff = c(1299, 1226, 1502, 1684, 2382),
  Rhat = c(1.00157, 1.00139, 1.00245, 1.00040, 1.00109)
)

# Display the data frame
print(lichens_bayesian)

R2_lichens_bayesian <- 0.2518927

# macrofungi = [beta0+ beta1*age"   + beta2*"deadwood"   +  beta3*"ba_broadl"  + beta4*"trees_10_40_1"


# Creating a data frame
macrofungi_bayesian <- data.frame(
  Parameter = c("beta0", "beta[1]", "beta[2]", "beta[3]", "beta[4]", "sigma"),
  mean = c(83.13983, 0.17747, 0.00034, 3.27144, 0.08445, 39.40222),
  se_mean = c(0.26571, 0.00256, 0.00000, 0.03474, 0.00120, 0.06228),
  sd = c(10.41423, 0.08382, 0.00010, 1.22521, 0.04394, 2.91078),
  `2.5%` = c(62.10863, 0.03120, 0.00014, 0.82859, 0.01157, 34.14599),
  `25%` = c(76.18244, 0.11605, 0.00028, 2.43093, 0.05172, 37.33985),
  `50%` = c(83.52201, 0.17438, 0.00035, 3.29810, 0.08110, 39.20798),
  `75%` = c(90.21518, 0.23333, 0.00041, 4.10014, 0.11222, 41.30646),
  `97.5%` = c(103.14266, 0.34920, 0.00054, 5.67190, 0.17916, 45.47460),
  n_eff = c(1536, 1069, 1395, 1244, 1351, 2184),
  Rhat = c(1.00399, 1.00586, 1.00170, 1.00433, 1.00616, 1.00066)
)


# Display the data frame
print(macrofungi_bayesian)

R2_macrofungi_bayesian  <- 0.3958684


# macrofungi_redlist = [beta0+ beta1*deadwood"   + beta2*"ba_broadl"

# Creating a data frame
macrofungi_redlist_bayesian <- data.frame(
  Parameter = c("beta0", "beta[1]", "beta[2]", "sigma"),
  mean = c(115.78055, 0.00088, 6.20003, 62.40061),
  se_mean = c(0.19781, 0.00000, 0.03210, 0.08496),
  sd = c(9.61528, 0.00014, 1.64104, 4.58304),
  `2.5%` = c(97.41300, 0.00061, 3.06694, 53.90684),
  `25%` = c(109.36434, 0.00079, 5.06637, 59.13968),
  `50%` = c(115.57863, 0.00088, 6.18950, 62.22819),
  `75%` = c(122.03234, 0.00097, 7.29332, 65.36952),
  `97.5%` = c(134.47920, 0.00116, 9.44456, 72.11250),
  n_eff = c(2363, 2752, 2613, 2910),
  Rhat = c(1.00004, 1.00059, 1.00033, 1.00031)
)

# Display the data frame
print(macrofungi_redlist_bayesian)

R2_macrofungi_redlist_bayesian  <- 0.4399397


# beetles = [beta0+ beta1*deadwood"   + beta2*"ba_broadl"

# Creating a data frame
beetles_bayesian <- data.frame(
  Parameter = c("beta0", "beta[1]", "beta[2]", "sigma"),
  mean = c(8.55345, 0.00002, 0.11991, 2.77078),
  se_mean = c(0.00887, 0.00000, 0.00132, 0.00419),
  sd = c(0.40048, 0.00001, 0.06329, 0.19871),
  `2.5%` = c(7.78440, 0.00000, 0.01439, 2.40869),
  `25%` = c(8.27737, 0.00001, 0.07218, 2.63044),
  `50%` = c(8.55743, 0.00002, 0.11495, 2.76068),
  `75%` = c(8.82788, 0.00002, 0.16069, 2.89364),
  `97.5%` = c(9.32954, 0.00003, 0.25618, 3.18986),
  n_eff = c(2038, 2271, 2283, 2254),
  Rhat = c(1.00325, 1.00176, 1.00237, 1.00049)
)

# Display the data frame
print(beetles_bayesian)

R2_beetles_bayesian  <- 0.113578


# moths = [beta0+ beta1*trees_10_40_2"   + beta2*"broadl_40_1"

# Creating a data frame
moths_bayesian <- data.frame(
  Parameter = c("beta0", "beta[1]", "beta[2]", "sigma"),
  mean = c(61.45297, -0.00018, 0.62700, 16.38500),
  se_mean = c(0.03572, 0.00000, 0.00250, 0.02158),
  sd = c(1.80969, 0.00007, 0.13527, 1.19052),
  `2.5%` = c(57.88974, -0.00033, 0.35542, 14.22023),
  `25%` = c(60.23248, -0.00023, 0.53730, 15.54480),
  `50%` = c(61.44840, -0.00018, 0.62675, 16.32075),
  `75%` = c(62.68020, -0.00014, 0.71741, 17.16020),
  `97.5%` = c(64.98348, -0.00004, 0.89673, 18.86794),
  n_eff = c(2567, 1974, 2935, 3044),
  Rhat = c(1.00007, 1.00090, 1.00051, 1.00040)
)


# Display the data frame
print(moths_bayesian)

R2_moths_bayesian  <- 0.2670846


# moths_redlist = [beta0+ beta1*trees_10_40_2"   + beta2*"broadl_40_1"

# Creating a data frame
moths_redlist_bayesian <- data.frame(
  Parameter = c("beta0", "beta[1]", "beta[2]", "sigma"),
  mean = c(62.49856, -0.00019, 0.65900, 17.28056),
  se_mean = c(0.03795, 0.00000, 0.00278, 0.02306),
  sd = c(1.93812, 0.00008, 0.14067, 1.26462),
  `2.5%` = c(58.73980, -0.00035, 0.38171, 15.06208),
  `25%` = c(61.20624, -0.00024, 0.56074, 16.38521),
  `50%` = c(62.49055, -0.00019, 0.65890, 17.19335),
  `75%` = c(63.76575, -0.00014, 0.75374, 18.11776),
  `97.5%` = c(66.44195, -0.00005, 0.94178, 19.96064),
  n_eff = c(2608, 2005, 2568, 3009),
  Rhat = c(0.99968, 0.99965, 1.00044, 1.00147)
)
# Display the data frame
print(moths_redlist_bayesian)

R2_moths_redlist_bayesian  <- 0.2660895




