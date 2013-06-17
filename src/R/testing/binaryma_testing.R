# test for binary meta analysis function

library("openmetar")

CONF.LEVEL.GLOBAL <- 95

tmp_obj <- new('BinaryData',
               g1O1=c(6.0,  3.0,  0.0,  19.0, 0.0, 26.0, 8.0, 1.0, 2.0, 6.0, 0.0, 3.0, 16.0, 10.0, 0.0, 0.0, 11.0, 0.0, 0.0),
               g1O2=c(21.0, 56.0, 30.0, 145.0, 36.0, 129.0, 24.0, 73.0, 26.0, 74.0, 92.0, 26.0, 55.0, 59.0, 19.0, 30.0, 14.0, 19.0, 58.0),
               g2O1=c(9.0,  7.0,  5.0,  13.0, 2.0, 49.0, 6.0, 0.0, 11.0, 3.0, 6.0, 4.0, 16.0, 8.0, 0.0, 2.0, 3.0, 0.0, 1.0),
               g2O2=c(18.0, 57.0, 25.0, 139.0, 34.0, 96.0, 29.0, 70.0, 20.0, 73.0, 87.0, 23.0, 56.0, 64.0, 19.0, 28.0, 22.0, 20.0, 55.0),                             y=c(-0.559615787935, -0.829598283288, -2.57694350425, 0.337229812415, -1.66579084899, -0.929187972983, 0.47692407209, 1.05693959227, -1.96711235671, 0.679541528504, -2.62051920862, -0.410284394544, 0.0180185055027, 0.304489190768, 0.0, -1.67726050877, 1.75126810787, 0.0500104205747, -1.15125602215), SE=c(0.617213399848, 0.715256232896, 1.50127304423, 0.379005873568, 1.56728515833, 0.27755775323, 0.606478434863, 1.64147997429, 0.824239424785, 0.726093756791, 1.47515610567, 0.815642772704, 0.401296599835, 0.507517637666, 2.02547873417, 1.57094704066, 0.735612357921, 2.024861116, 1.64370882512), study.names=c('Gonzalez', 'Prins', 'Giamarellou', 'Maller', 'Sturm', 'Marik', 'Muijsken', 'Vigano', 'Hansen', 'De Vries', 'Mauracher', 'Nordstrom', 'Rozdzinski', 'Ter Braak', 'Tulkens', 'Van der Auwera', 'Klastersky', 'Vanhaeverbeek', 'Hollender'), years=c(as.integer(1993), as.integer(1993), as.integer(1991), as.integer(1993), as.integer(1989), as.integer(1991), as.integer(1988), as.integer(1992), as.integer(1988), as.integer(1990), as.integer(1989), as.integer(1990), as.integer(1993), as.integer(1990), as.integer(1988), as.integer(1991), as.integer(1977), as.integer(1993), as.integer(1989)), covariates=list())


params_df <- data.frame(
'conf.level'= 95.0,
'digits'= 3.0,
'fp_col2_str'= '[default]',
'fp_show_col4'= TRUE,
'to'= 'all',
'fp_col4_str'= 'Ev/Ctrl',
'fp_xticks'= '[default]',
'fp_col3_str'= 'Ev/Trt',
'fp_show_col3'= TRUE,
'fp_show_col2'= TRUE,
'fp_show_col1'= TRUE,
'fp_plot_lb'= '[default]',
'fp_outpath'= './r_tmp/forest.png',
'rm.method'= 'DL',
'adjust'= 0.5,
'fp_plot_ub'= '[default]',
'fp_col1_str'= 'Studies',
'measure'= 'OR',
'fp_xlabel'= '[default]',
'fp_show_summary_line'= TRUE)

#result<-binary.fixed.peto(tmp_obj, params_df)