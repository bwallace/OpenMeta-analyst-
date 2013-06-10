# Test for diagnost hsroc for sens/spec

library("openmetar")
CONF.LEVEL.GLOBAL <- 95

# SET_UP PARAMETERS

tmp_obj <- new('DiagnosticData', TP=c(19.0, 8.0, 41.0, 5.0, 45.0, 8.0, 5.0,
15.0, 16.0, 4.0, 8.0, 10.0, 2.0, 7.0, 44.0, 8.0, 4.0), FN=c(10.0, 2.0, 12.0,
2.0, 32.0, 2.0, 1.0, 11.0, 8.0, 2.0, 10.0, 4.0, 6.0, 7.0, 12.0, 1.0, 0.0),
TN=c(81.0, 13.0, 49.0, 18.0, 165.0, 32.0, 7.0, 52.0, 24.0, 25.0, 70.0, 55.0,
23.0, 30.0, 135.0, 37.0, 14.0), FP=c(1.0, 9.0, 1.0, 1.0, 58.0, 6.0, 8.0, 17.0,
11.0, 8.0, 12.0, 4.0, 5.0, 10.0, 50.0, 3.0, 3.0), y=c(0.641853886172,
1.38629436112, 1.22866541692, 0.916290731874, 0.340926586971, 1.38629436112,
1.60943791243, 0.310154928304, 0.69314718056, 0.69314718056, -0.223143551314,
0.916290731874, -1.09861228867, 0.0, 1.29928298413, 2.07944154168,
2.19722457734), SE=c(0.39068091705, 0.790569415042, 0.32821270121,
0.836660026534, 0.231240615425, 0.790569415042, 1.09544511501,
0.396958130759, 0.433012701892, 0.866025403784, 0.474341649025, 0.59160797831,
0.816496580928, 0.534522483825, 0.325669473639, 1.06066017178, 1.490711985),
study.names=c('Kinderman', 'Lecart Lenfant', 'Piver', 'Piver Barlow', 'Kolbenstvedt',
'Lehman', 'Brown', 'Lagasse', 'Kjorstad', 'Ashraf', 'De Muylder', 'Smales', 'Feigen',
'Swart', 'Heller', 'La Fianza', 'Stellato'), years=c(as.integer(1970), as.integer(1971),
as.integer(1971), as.integer(1973), as.integer(1975), as.integer(1975), as.integer(1979),
as.integer(1979), as.integer(1980), as.integer(1982), as.integer(1984), as.integer(1986),
as.integer(1987), as.integer(1989), as.integer(1990), as.integer(1990), as.integer(1992)),
covariates=list())


list.of.params <- list(list('conf.level'=95.0,'burn.in'=1000.0,'fp_xticks'='[default]','fp_show_col4'=FALSE,'fp_show_col3'=TRUE,'fp_show_col2'=TRUE,'fp_show_col1'=TRUE,'fp_plot_ub'='[default]','fp_col1_str'='Studies','measure'='Sens','theta.lower'=-2.0,'lambda.lower'=-2.0,'fp_xlabel'='[default]','fp_col3_str'='[default]','lambda.upper'=2.0,'fp_show_summary_line'=TRUE,'to'='only0','num.iters'=5000.0,'theta.upper'=2.0,'fp_plot_lb'='[default]','fp_outpath'='./r_tmp/forest_sens.png','fp_col2_str'='[default]','fp_col4_str'='Ev/Ctrl','num.chains'=3.0,'adjust'=0.5,'thin'=2.0),list('conf.level'=95.0,'burn.in'=1000.0,'fp_xticks'='[default]','fp_show_col4'=FALSE,'fp_show_col3'=TRUE,'fp_show_col2'=TRUE,'fp_show_col1'=TRUE,'fp_plot_ub'='[default]','fp_col1_str'='Studies','measure'='Spec','theta.lower'=-2.0,'lambda.lower'=-2.0,'fp_xlabel'='[default]','fp_col3_str'='[default]','lambda.upper'=2.0,'fp_show_summary_line'=TRUE,'to'='only0','num.iters'=5000.0,'theta.upper'=2.0,'fp_plot_lb'='[default]','fp_outpath'='./r_tmp/forest_spec.png','fp_col2_str'='[default]','fp_col4_str'='Ev/Ctrl','num.chains'=3.0,'adjust'=0.5,'thin'=2.0))

f.names <- c('diagnostic.hsroc','diagnostic.hsroc')


# The call
#multiple.diagnostic(f.names, list.of.params, tmp_obj)