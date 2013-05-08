'''
MetaAnalyst
Tufts Medical Center 2008
bugs_generator.py aka pyBugs
IronPython (ipy)

*** DO NOT REMOVE OR ALTER THIS FILE ***

This module contains static methods for talking to BRUGs via buffer files. It is a convienent 
glue between MetaAnalyst (.NET) and the BRUGS.dll, and includes methods
both for generating data input files for BRUGS and for collecting the results back up. 

It is called by MetaAnalyst prior to Bayesian analyses to generate the files necessary to talk to BRUGS,
as well as afterwards to collect and package the results distributed in their respective buffer text files.
'''

import System
import System.Collections.ArrayList as arraylist
import clr 
import math

#																						    
#				BRUGS model strings. 											
#																						    


#########################################################
#				   Continuous string(s)						                                                                                    #
#########################################################

#  (linear) control rate, with no covariates
cont_bayes_two_group_linear_cr_str = """
model {
    for( i in 1 : N ) { 
        # N is number of studies
        # the precisions for the mean responses 
        # which are stochastic nodes!
    
        prec.t[i] <- 1/((sd.t[i]*sd.t[i])/n.t[i])		
        prec.c[i] <- 1/((sd.c[i]*sd.c[i])/n.c[i])
        
        y.t[i] ~dnorm(theta.t[i], prec.t[i])
        y.c[i] ~dnorm(theta.c[i], prec.c[i])
                        
        theta.t[i] <- delta.star[i] + theta.c[i]
        theta.c[i] ~ dnorm(muc,  inv.tauc2) 													
                    
        delta.star[i] ~ dnorm(mu[i], inv.tau2 )
                
        mu[i] <- delta + beta0 *(theta.c[i] - muc)		
    }
    muc ~ dnorm(0.0,precnorm)
            
    delta ~ dnorm(0.0,precnorm)
    beta0 ~ dnorm(0.0,precnorm)
    
    inv.tau2 ~ dgamma(gamma.a,gamma.b)
    tau2 <- 1/inv.tau2
    tau <- sqrt(tau2)			

    inv.tauc2 ~ dgamma(gamma.c.a, gamma.c.b)
    tauc2 <- 1/inv.tauc2
    tauc <- sqrt(tauc2)			

    
"""


cont_bayes_str_one_group = """
model {
    for( i in 1 : N ) { # N is number of studies
        # the precisions for the mean responses 
        # which are stochastic nodes!
        y.t[i] ~ dnorm(theta[i], prec.y[i])
        prec.y[i] <- 1/var.y[i]
        var.y[i] <-  (sd.t[i]*sd.t[i])/n.t[i] 
        
        theta[i] ~ dnorm(mu[i], inv.tau2)
        mu[i] <- Ysum 
        
    }
    Ysum ~ dnorm(0.0, precnorm)
    inv.tau2 ~ dgamma(gamma.a, gamma.b)
    tau2 <- 1/inv.tau2
    tau <- sqrt(tau2)		
    

"""

cont_bayes_str_generic = """
model {
    for( i in 1 : N ) { # N is number of studies
        # the precisions for the mean responses 
        # which are stochastic nodes!
        y.t[i] ~ dnorm(theta[i], prec.y[i])
        prec.y[i] <- 1/var.y[i]
    
        theta[i] ~ dnorm(mu[i], inv.tau2)
        mu[i] <- Ysum 
    }
    Ysum ~ dnorm(0.0, precnorm)
    inv.tau2 <- 1/tau2
    tau2 <- tau*tau
    tau ~ dunif(0, 100)		
"""




########################################################
#					Binary string(s)						      #
########################################################

# this is the meta-analysis of logOR 
# without covariates
binary_bayes_or_model_str = """
model {
    for (i in 1:N) {
        rc[i] ~ dbin(pc[i],nc[i])
        rt[i] ~ dbin(pt[i],nt[i])
        logit(pc[i]) <- mu[i]
        logit(pt[i]) <- mu[i] + d[i]

        d[i] ~ dnorm(delta,invtau2)
        mu[i] ~ dnorm(mu.bar, invtauc2)

        d.exp[i] <- exp(d[i])
    }

    delta ~ dnorm(0.0, precnorm)
    delta.exp <- exp(delta)	
    
    beta ~ dnorm(0.0, precnorm)
    beta1 ~ dnorm(0.0, precnorm)
    
    mu.bar ~ dnorm(0.0, precnorm)
    invtau2 ~ dgamma(gamma.a, gamma.b)
    invtauc2 ~ dgamma(gamma.c.a, gamma.c.b)
    tau2 <- 1/invtau2	
    tauc2 <- 1/invtauc2	
    cr.bar <- exp(mu.bar) / (exp(mu.bar) + 1)
    
"""

binary_bayes_one_group_str = """
model{
    for (i in 1:N) {
        r[i] ~ dbin(p[i],n[i])
        logit(p[i]) <- mu[i]
        
        mu[i] ~ dnorm(mu.bar, invtau2)
        
    }

    mu.bar ~ dnorm(0.0, precnorm)
    invtau2 ~ dgamma(gamma.a, gamma.b)
    tau2 <- 1/invtau2	
    psum <- exp(mu.bar) / (exp(mu.bar) + 1)
    
"""

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#																							   #
#								End BRUGS model strings. 					   #
#																							   #
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''


def generate_model_file_binary(out_path, metric="or", cr="none", num_covs=0, is_two_group=  False):
    if not is_two_group:
        out_str = binary_bayes_one_group_str 
    if num_covs:
        cov_str = build_reg_str_for_covs(num_covs)  
        out_str = out_str.replace("mu[i] ~ dnorm(mu.bar, invtau2)", "mu[i] ~ dnorm(mu.bar.star[i], invtau2)\n\t\tmu.bar.star[i]<-mu.bar + " + cov_str)
        out_str += build_assignments_for_covs(num_covs)
        out_str += "\n}"
    f = file(out_path, 'w')
    f.write(out_str)
    f.close()
    return 
    
    out_str = binary_bayes_or_model_str
    #
    # here we modify the  model for risk ratio
    #
    if metric == "rr":
        out_str = out_str.replace("logit(pt[i]) <- mu[i] + d[i]",  "log(pt[i]) <- mu[i] - log(1 + exp(mu[i])) + d[i]");
        out_str = out_str.replace("cr.bar <- exp(mu.bar) / (exp(mu.bar) + 1)",  "cr.bar <- exp(mu.bar)")
       
    #
    # build string for covariate regression
    #
    cov_str = ""
    if num_covs:
        cov_str = " + " + build_reg_str_for_covs(num_covs)
    
    if cr == "none":
        out_str = out_str.replace("beta ~ dnorm(0.0, precnorm)", "")
        out_str = out_str.replace("beta1 ~ dnorm(0.0, precnorm)", "")
    if num_covs:
        out_str = out_str.replace("d[i] ~ dnorm(delta,invtau2)",  "d[i] <- d.star[i] " +  cov_str + "\n\t\td.star[i] ~ dnorm(delta,invtau2)")
            
    elif cr == "linear":
    out_str = out_str.replace("beta1 ~ dnorm(0.0, precnorm)", "")
        out_str = out_str.replace("d[i] ~ dnorm(delta,invtau2)", "d[i] <- d.star[i] + beta*(mu[i] - mu.bar)" + cov_str + "\n\t\td.star[i] ~ dnorm(delta,invtau2)")
    else:
    # quadratic
        out_str = out_str.replace("d[i] ~ dnorm(delta,invtau2)", "d[i] <- d.star[i] + beta*(mu[i] - mu.bar) + beta1*((mu[i] - mu.bar) * (mu[i] - mu.bar)) " + cov_str + "\n\t\td.star[i] ~ dnorm(delta,invtau2)")
    
    if num_covs:
        out_str += build_assignments_for_covs(num_covs)
 
    out_str += "\n}"

    f = file(out_path, 'w')
    f.write(out_str)
    f.close()
   
def build_reg_str_for_covs(num_covs):
    return "+".join(["cov%s * (x%s[i] - mean(x%s[]))" % (i, i, i) for i in range(num_covs)])
       
def build_assignments_for_covs(num_covs):
    return "\n".join(["\tcov%s ~ dnorm(0.0, precnorm)" % i for i in range(num_covs)])	

def generate_data_file_binary_one_group(out_path, num_studies, r, n, prior_on_q, prior_on_lam, covariates, precnorm=.000001):
    '''
    r -- numerator (events)
    n -- denominator (sample size)
    '''
    precnorm = "%15.10f" % precnorm
    bin_data = []
    f = file(out_path, 'w')
    r = build_brugs_list_from_vector("r", r)
    n = build_brugs_list_from_vector("n", n)
    f.write("list(")
    f.write(", ".join([r, n]))

    all_covs = []
    for cov_index in range(covariates.GetLength(1) - 1):
        all_covs.append(build_brugs_list_from_vector("x%s"%cov_index, get_col(covariates, cov_index+1)))

    if (covariates.GetLength(1) - 1):
        f.write(", ")
        f.write(", ".join(all_covs))

    f.write(", precnorm=%s, gamma.a=%s, gamma.b=%s, N=%s)" % 
    (precnorm, prior_on_q, prior_on_lam, num_studies))
    f.close()


def generate_all_data_files_diagnostic(out_dir, num_studies, tps, fps, fns, tns, q_priors, lam_priors, xt):
    num_diag_outcomes = 8
    metric_functions_one_grp = ["specificity(tn, fp)", "sensitivity(tp, fn)", "ppv(tp, fp)", "npv(tn, fn)", "accuracy(tp, fp, fn, tn)"]
    
    for metric_index in range(len(metric_functions_one_grp)):
    metric_f = metric_functions_one_grp[metric_index]
    r, n = [], []
        for i in range(num_studies):
            tp, fp, fn, tn = tps[i], fps[i], fns[i], tns[i]
        r_i, n_i = eval(metric_f)
        r.append(r_i) # outcome metric for study
        n.append(n_i) # study size
    generate_data_file_binary_one_group(out_dir + metric_f.split("(")[0] + "//data.txt", num_studies, r, n, q_priors[metric_index], lam_priors[metric_index], xt)
        
    metric_functions_two_grp = ["dor(tp, fp, fn, tn)",  "lrp(tp, fp, fn, tn)", "lrn(tp, fp, fn, tn)"]
    for metric_index in range(len(metric_functions_two_grp)):
        metric_f = metric_functions_two_grp[metric_index]
    rt, nt, rc, nc = [], [], [], []
    for i in range(num_studies):
        tp, fp, fn, tn = tps[i], fps[i], fns[i], tns[i]
            rt_i, nt_i, rc_i, nc_i = eval(metric_f)
        rt.append(rt_i)
        nt.append(nt_i)
        rc.append(rc_i)
        nc.append(nc_i)
    q_tuple = [q_priors[metric_index] for i in range(2)]
    lam_tuple = [lam_priors[metric_index] for i in range(2)]
    generate_data_file_binary(out_dir + metric_f.split("(")[0] + "//data.txt", num_studies, rt, nt, rc, nc, q_tuple , lam_tuple , xt)
    
    
# 
# These return a tuple; the first entry is the numerator (r) the second, the denominator (n)
#
def sensitivity(tp, fn):
    return (tp , (tp + fn))
    
def specificity(tn, fp):
    return (tn , (fp + tn))
    
def ppv(tp, fp):
    return (tp , (tp + fp))
    
def npv(tn, fn):
    return (tn , (fn + tn))
    
def accuracy(tp, fp, fn, tn):
    return ((tp + tn) , (fp + tp + fn + tn))
    
#
#  Thesee return a fourple; rt, nt, rc, nc
#
def dor(tp, fp, fn, tn):
    #return ((tp * tn) , (fp * fn))
    return (tp, tp + fn, fp, fp + tn)
    
def lrp(tp, fp, fn, tn):
    #return (sensitivity(tp, fn), (1-specificity(tn, fp)))
    return (tp, tp + fn, fp, fp + tn)
    
def lrn(tp, fp, fn, tn):
    #return ((1 - sensitivity(tp, fn)), specificity(tn, fp))
    return (fn, fn + tp, tn, tn + fp)

def generate_data_file_binary(out_path, num_studies, rt, Nt, rc, Nc, priors_on_q, priors_on_lam, covariates, precnorm=.000001):
    '''
    rt is numerator in treatment (number of events in treated group), Nt is the total number of subjects in the treated group; rc is numerator in control
    Nc is the total number of subjects in the control group.
    '''
    precnorm = "%15.10f" % precnorm
    bin_data = []
    f = file(out_path, 'w')
    f.write("list(")
    rt = build_brugs_list_from_vector("rt", rt)
    nt = build_brugs_list_from_vector("nt", Nt)
    rc = build_brugs_list_from_vector("rc", rc)
    nc = build_brugs_list_from_vector("nc", Nc)
    
    # here we deal with covariates
    all_covs = []
    # the covariates (or, xi) double[,] matrix has a 1s column at index 0. we're not interested in this.
    for cov_index in range(covariates.GetLength(1) - 1):
        all_covs.append(build_brugs_list_from_vector("x%s"%cov_index, get_col(covariates, cov_index+1)))
    vecs_for_brugs =[rt, nt, rc, nc]
    vecs_for_brugs.extend(all_covs)
    
    f.write(", ".join(vecs_for_brugs))
    f.write(", precnorm=%s, gamma.a=%s, gamma.b=%s, gamma.c.a=%s, gamma.c.b=%s, N=%s)" % 
    (precnorm, priors_on_q[0], priors_on_lam[0], priors_on_q[1], priors_on_lam[1], num_studies))
    f.close()
  

# 
# Use generate_all_inits_using_values (getting init values via Chris' method) instead!
#
def generate_all_inits_for_binary(outdir, N,  num_chains=3):
    # get bugsy style list for mu and d inits (these need a list of values)
    list_chains = generate_inits_for_vars_that_need_list(num_chains, ["mu", "d"], N)
    
    # now generate initial values for variables that only need a scalar
    scalar_chains = generate_inits_for_vars_that_need_scalars(num_chains, ["delta", "mu.bar", "invtau2", "invtauc2"], [0.0, 0.0, 0.1, .05])
    # now comine them
    for list_inits, scalar_inits in zip(list_chains, scalar_chains):
        list_inits.append(scalar_inits)
    write_out_init_chains(outdir, list_inits)
   
    
def generate_all_inits_for_cont(outdir, N, cr, num_covs, is_two_group, generic=False):
    num_chains=3
    scalar_vars, scalar_vals = None, None
    list_chains = None
    if is_two_group:
    list_chains = generate_inits_for_vars_that_need_list(num_chains, ["delta.star", "theta.c"], N)
        scalar_vars = ["delta", "muc", "inv.tau2", "inv.tauc2"]
    scalar_vals = [0.0, 0.0, 0.1, 0.05]
    if cr == "linear":
        scalar_vars.append("beta0")
        scalar_vals.append(0.0)
    else:
    list_chains = generate_inits_for_vars_that_need_list(num_chains, ["theta"], N)
        if not generic:	
            scalar_vars = ["Ysum", "inv.tau2"]
            scalar_vals = [0.0, 0.1]
        else:
            scalar_vars = ["Ysum", "tau"]
            scalar_vals = [0.0, 1.0]

    for cov in range(num_covs):
        scalar_vars.append("cov%s"%cov)
    scalar_vals.append(0.0)
    scalar_chains = generate_inits_for_vars_that_need_scalars(num_chains,  scalar_vars, scalar_vals)
    for list_inits, scalar_inits in zip(list_chains, scalar_chains):
        list_inits.append(scalar_inits)
    write_out_init_chains(outdir, list_chains)


def generate_all_inits_using_values(phi_mat, xi_mat, outdir, num_chains, num_covs, cr, is_two_group):
    all_chains = massage_inits_to_bugs_format_binary(num_chains, phi_mat, xi_mat, num_covs, cr, is_two_group)
    write_out_init_chains(outdir, all_chains)


def write_out_init_chains(outdir, list_of_chains):
    for chain_index in range(len(list_of_chains)):
        f = file(outdir + "//inits%s.txt" % (chain_index+1), 'w')
    f.write("list(" + ",".join(list_of_chains[chain_index]) + ")")
    f.close()

def massage_inits_to_bugs_format_binary(num_chains, phi_mat, xi_mat, num_covs, cr, is_two_group):
    '''
    This method takes the output from Chris' two intialization generating routines and outputs them to BUGS
    friendly files. 
    
    phi_mat - these are the scalar (I think of them, perhaps incorrectly, as initial values for 'summary' values)
                  phi_mat is indexed by x,y where x is the parameter index and y is the chain index 
    xi_mat - these are arrays, i.e., study level parameters. however this gets passed in as a three-dimensional
            matrix, indexed by study, parameter, and chain, respectively. that is, xi_mat[x,y,z] gives the initial
            value for study x, parameter y, for chain z. 
    both of the above parameters are assumed to have three chains. this is hard-coded into Chris' code. from
    these chains we can build additional chains.
    '''
    list_of_chains = []
    number_of_studies = xi_mat.GetLength(0)
    xi_parameter_names = ["mu"]
    if is_two_group:
        xi_parameter_names.append("d") # this will probably need to be changed for other (non-binary) datatypes

    phi_mat_parameter_names = None
    
    if is_two_group:
        phi_mat_parameter_names = ["mu.bar", "delta"]
    else:
        phi_mat_parameter_names = ["mu.bar"]
    
    for cov_num in range(num_covs):
        phi_mat_parameter_names.append("cov%s" % cov_num)
    
    if cr > 0:
        # we need beta!
    phi_mat_parameter_names.append("beta")
    
    if cr == 2:
    phi_mat_parameter_names.append("beta1")

    if is_two_group:
        phi_mat_parameter_names.extend(["invtauc2", "invtau2"])
    else:
    phi_mat_parameter_names.append("invtau2")

    for chain in range(xi_mat.GetLength(2)):
    # generate the whole chain at once
    chain_str = []
        for param in range(xi_mat.GetLength(1)):
        # first generate the inits for the values in the xi_mat; i.e., study-level initial values
            cur_param_vector= [xi_mat[study, param, chain] for study in range(number_of_studies)]
            chain_str.append(build_brugs_list_from_vector(xi_parameter_names[param], cur_param_vector))
    phi_vals_for_cur_chain = [phi_mat[param, chain] for param in range(phi_mat.GetLength(0))]
    chain_str.append(", ".join(["%s = %15.10f" % (parameter, val) for parameter, val in zip(phi_mat_parameter_names, phi_vals_for_cur_chain)]))
    list_of_chains.append(chain_str)
    return list_of_chains
    

def generate_inits_for_vars_that_need_scalars(num_chains, parameters, init_vals):
    all_chains = []
    for chain in range(num_chains):
        all_chains.append(", ".join(["%s = %15.10f" % (parameter, val) for parameter, val in zip(parameters, init_vals)]))
    return all_chains
    

        
def generate_inits_for_vars_that_need_list(num_chains, parameters, num_vals_needed):
    rand = System.Random()
    first_three_vals = [0.0, -.1, .1]
    all_chains = []
    for chain_i in range(num_chains):
    chain_str = []
        for p in parameters:
        if chain_i < 3:
            val = first_three_vals[chain_i]
        else:
        val = .1 * rand.NextDouble()
        if (rand.NexDouble > .5):
            val = -1 * val
        chain_str.append(build_brugs_list_from_vector(p, ["%15.10f" % val for i in range(num_vals_needed)]))
        all_chains.append(chain_str)
    return all_chains


def build_results_list_for_cont_bayes(results_dir, N, num_covs, nphi, cr, is_two_group):
    results = arraylist()
    
    overall_effects = System.Array.CreateInstance(float, nphi, 6)
    vars_for_overall_table = None
    if is_two_group:
        vars_for_overall_table = ["muc", "delta"]
    else:
        vars_for_overall_table = ["Ysum"]
    
    for c in range(num_covs):
        vars_for_overall_table.append("cov%s" % c)   
    
    if cr > 0:
        vars_for_overall_table.append("beta0")
    
    add_tau = ["tau2"]
    if is_two_group:
        add_tau.insert(0, "tauc2")
    vars_for_overall_table.extend(add_tau)
    
    for i in range(len(vars_for_overall_table)):
        insert_arr_into_mat(overall_effects, i, ma_format_arr(get_first_line_arr(results_dir + "//%s//buffer.txt" % vars_for_overall_table[i])))
    results.Add(overall_effects)

    # build the treatment effects table -- this is delta.star
    treat_matrix = System.Array.CreateInstance(float, N, 6)
    treat_rows = None
    if is_two_group:
        treat_rows = build_arrs_from_file(results_dir + "//delta.star//buffer.txt")
    else:
        treat_rows = build_arrs_from_file(results_dir + "//theta//buffer.txt")
    for i in range(len(treat_rows)):
        insert_arr_into_mat(treat_matrix, i, treat_rows[i])
    results.Add(treat_matrix)
    
    # now build the posterior control group table -- this is 'pc'
    if is_two_group:
        pc_matrix = System.Array.CreateInstance(float, N, 6)
        pc_rows = build_arrs_from_file(results_dir + "//theta.c//buffer.txt")
        for i in range(len(pc_rows)):
            insert_arr_into_mat(pc_matrix, i, pc_rows[i])
        results.Add(pc_matrix)

    return results

    
def build_overall_table(results_dir, nphi, num_covs, cr, is_two_group):
    # overall effects matrix
    overall_effects = System.Array.CreateInstance(float, nphi, 6)
    vars_for_overall_table = None
    
    if is_two_group:
        vars_for_overall_table = ["cr.bar", "delta.exp"]
        if cr > 0:
            vars_for_overall_table.append("beta")
    
        if cr == 2:
            vars_for_overall_table.append("beta1")
    else:
        vars_for_overall_table = ["psum", "mu.bar"]

    # now add the covariates
    for c in range(num_covs):
        vars_for_overall_table.append("cov%s" % c)   
    
    add_tau = ["tau2"]
    if is_two_group:
        add_tau.insert(0, "tauc2")
    vars_for_overall_table.extend(add_tau)
    
    for i in range(len(vars_for_overall_table)):
        insert_arr_into_mat(overall_effects, i, ma_format_arr(get_first_line_arr(results_dir + "//%s//buffer.txt" % vars_for_overall_table[i])))

    return overall_effects
    
    
def build_results_list_for_binary_bayes(results_dir, N, num_covs, nphi, cr, is_two_group):
    '''
    Builds and returns a results ArrayList from the BRUGs output (the top level is assumed to be the results_dir parameter).
    This list constructed so as to be ready to be passed off to the ReportGenerator on the C# side. 
    '''
    results = arraylist()
    results.Add(build_overall_table(results_dir, nphi, num_covs, cr, is_two_group))
    
    # now build the posterior control group table -- this is 'pc'
    if is_two_group:
        pc_matrix = System.Array.CreateInstance(float, N, 6)
        pc_rows = build_arrs_from_file(results_dir + "//pc//buffer.txt")
        for i in range(len(pc_rows)):
            insert_arr_into_mat(pc_matrix, i, pc_rows[i])
        results.Add(pc_matrix)
    
    # finally, build the treatment effects table -- this is d.exp
    treat_matrix = System.Array.CreateInstance(float, N, 6)
    treat_rows = None
    mstr = "or"
    if is_two_group and mstr != "rd":
        treat_rows = build_arrs_from_file(results_dir + "//d.exp//buffer.txt")
    else:
        treat_rows = build_arrs_from_file(results_dir + "//p//buffer.txt")
    for i in range(len(treat_rows)):
        insert_arr_into_mat(treat_matrix, i, treat_rows[i])
    results.Add(treat_matrix)
    return results
    
def invlogit(x):
    return math.exp(x) / (1 + math.exp(x))
    
def build_diagnostic_bayes_outcome_table(results_dir, outcome, N, cr, num_covs, two_group):
    # intercept + covariates + tau2
    total_rows = N + 2 + num_covs 
    
    vars_for_overall_table = None
    if two_group:
        vars_for_overall_table = ["d.exp"]
    else:
    vars_for_overall_table = ["mu.bar"]
    
    if cr == "linear":
        total_rows += 1
        vars_for_overall_table.append("beta")

    vars_for_overall_table.append("tau2")
    pstr = "p"
    if two_group:
    pstr = "d.exp"
        vars_for_overall_table.append("tau2")
    total_rows += 1
     
    outcome_table = System.Array.CreateInstance(float, total_rows, 6)
     
    for cov in range(num_covs):
        vars_for_overall_table.append("cov%s" % cov)
    

    # first insert the study data
    file = open(results_dir +  outcome + "//%s//buffer.txt" % pstr, 'r')
    lines = file.readlines()[1:] # drop the headers
    file.close()
    
    for row_index in range(len(lines)):
    row = lines[row_index]
        insert_arr_into_mat(outcome_table, row_index, ma_format_arr(parse_numbers_from_line(row)))

    # add the 'overall' variables
    for i in range(len(vars_for_overall_table)):
        insert_arr_into_mat(outcome_table, N+i, ma_format_arr(get_first_line_arr(results_dir + outcome + "//%s//buffer.txt" % vars_for_overall_table[i])))
    
    return outcome_table

    
def build_results_list_for_diagnostic_bayes(results_dir, metrics, N, num_covs, nphis, cr):
    all_results = arraylist()
    results = arraylist()
    forest_plot_stuff = arraylist()
    outcome_index = 0
    for outcome in metrics:
    is_two_group = False
    if outcome in ["dor", "lrp", "lrn"]:
        is_two_group = True
    cur_outcome_table = build_diagnostic_bayes_outcome_table(results_dir, outcome, N, cr, num_covs, is_two_group)
    results.Add(cur_outcome_table)
    
    #
    # now deal with forest plot stuff
    #
    # this will include: [lowers, means, uppers, overalls]
    stuff_for_this_forest_plot = arraylist() 
        # here we build double[] arrays to hand off for the forest plot. we need lowers, means and uppers, as well
        # as overalls, for each metric. first we add results for each study.
    lowers, means, uppers = System.Array.CreateInstance(float, N), System.Array.CreateInstance(float, N), System.Array.CreateInstance(float, N)
    lower_index, mean_index, upper_index = 0, 2, 4
    for study_i in range(N):
        lowers[study_i] = cur_outcome_table[study_i, lower_index]
        means[study_i] = cur_outcome_table[study_i, mean_index]
        uppers[study_i] = cur_outcome_table[study_i, upper_index]
        
    
    for double_arr in [lowers, means, uppers]:
        stuff_for_this_forest_plot.Add(double_arr)
        
    # now add the overall estimate
    overall = System.Array.CreateInstance(float, 3)
    # the summary statistic is psum if 'one group' model; delta.exp otherwise
    overall_var = "psum"
    if is_two_group:
        overall_var = "delta.exp"
        #overall_var = "delta"
    overall_arr = ma_format_arr(get_first_line_arr(results_dir + outcome + "//%s//buffer.txt" % overall_var))

    overall[0] = overall_arr[2]
    overall[1] = overall_arr[0]
    overall[2] = overall_arr[4]
    stuff_for_this_forest_plot.Add(overall)
    forest_plot_stuff.Add(stuff_for_this_forest_plot)
    outcome_index += 1
    all_results.Add(results)
    all_results.Add(forest_plot_stuff)
    return all_results



def generate_model_file_cont(out_path, cr, num_covs, is_two_group, generic):
    out_str = None
    # build string for covariate regression
    cov_str = ""
    if num_covs:
        cov_str = " + " + build_reg_str_for_covs(num_covs)
        
    if is_two_group:
    out_str = cont_bayes_two_group_linear_cr_str
        if cr == "none":
            out_str = out_str.replace("beta0 ~ dnorm(0.0,precnorm)", "")
        if num_covs:
            out_str = out_str.replace("mu[i] <- delta + beta0 *(theta.c[i] - muc)",  "mu[i] <- delta" +  cov_str)
            out_str +=  build_assignments_for_covs(num_covs)
        else:
            # no control rate, no covariates. 
        out_str = out_str.replace("delta.star[i] ~ dnorm(mu[i], inv.tau2 )", "delta.star[i] ~ dnorm(delta, inv.tau2 )")
        out_str = out_str.replace("mu[i] <- delta + beta0 *(theta.c[i] - muc)", "mu[i] <- delta")
        else:
        if num_covs:
            out_str = out_str.replace("mu[i] <- delta + beta0 *(theta.c[i] - muc)", "mu[i] <- delta + beta0 *(theta.c[i] - muc)" + cov_str)
            out_str += build_assignments_for_covs(num_covs)
        
    else:
    # one group
        out_str = cont_bayes_str_one_group
        if generic:
            out_str = cont_bayes_str_generic
    if num_covs:
        out_str = out_str.replace("mu[i] <- Ysum ", "mu[i] <- Ysum " + cov_str)
        out_str += build_assignments_for_covs(num_covs)
        
    out_str += "\n}"
    f = open(out_path, 'w')
    f.write(out_str)
    f.close()
    
        

def generate_data_file_cont(out_path, num_studies, yt_vec, yc_vec, sdt_vec, sdc_vec, nt_vec, nc_vec, priors_on_q, priors_on_lam, covariates, precnorm=.000001):
    '''
    out_path -- data file will be written here
    num_studies -- number of studies total (equal to len(*_vec), or, N)
    yt_vec -- vector of means in treatment groups over all N studies
    yc_vec -- ditto, for control
    sdt_vec -- vector of standard deviations in treatment groups over all N studies
    sdc_vec -- ditto, for control
    nt_vec -- number of subjects in treated groups over all N studies
    nc_vec -- ditto, for control
    covs -- matrix of covariates
    '''
    precnorm = "%15.10f" % precnorm
    cont_data = []
    f = file(out_path, 'w')
    f.write("list(")
    yt = build_brugs_list_from_vector("y.t", yt_vec)
    yc = build_brugs_list_from_vector("y.c", yc_vec)
    sdt = build_brugs_list_from_vector("sd.t", sdt_vec)
    sdc = build_brugs_list_from_vector("sd.c", sdc_vec)
    nt = build_brugs_list_from_vector("n.t", nt_vec)
    nc = build_brugs_list_from_vector("n.c", nc_vec)
    
    # here we deal with covariates
    all_covs = []
    # the covariates (or, xi) double[,] matrix has a 1s column at index 0. we're not interested in this.
    for cov_index in range(covariates.GetLength(1) - 1):
        all_covs.append(build_brugs_list_from_vector("x%s"%cov_index, get_col(covariates, cov_index+1)))
    vecs_for_brugs =[yt, yc, sdt, sdc, nt, nc]
    vecs_for_brugs.extend(all_covs)
    
    f.write(", ".join(vecs_for_brugs))
    f.write(", precnorm=%s, gamma.a=%s, gamma.b=%s, gamma.c.a=%s, gamma.c.b=%s, N=%s)" % 
    (precnorm, priors_on_q[0], priors_on_lam[0], priors_on_q[1], priors_on_lam[1], num_studies))
    f.close()
    

def generate_data_file_cont_generic(out_path, num_studies, yt_vec,  yt_var_vec, prior_on_q, prior_on_lam, covariates, precnorm=.000001):
    '''
    out_path -- data file will be written here
    num_studies -- number of studies total (equal to len(*_vec), or, N)
    yt_vec -- vector of means in treatment groups over all N studies

    covs -- matrix of covariates
    '''	
    precnorm = "%15.10f" % precnorm
    cont_data = []
    f = file(out_path, 'w')
    f.write("list(")
    yt = build_brugs_list_from_vector("y.t", yt_vec)
    yt_var= build_brugs_list_from_vector("var.y", yt_var_vec)
    
    # here we deal with covariates
    all_covs = []
    # the covariates (or, xi) double[,] matrix has a 1s column at index 0. we're not interested in this.
    for cov_index in range(covariates.GetLength(1) - 1):
        all_covs.append(build_brugs_list_from_vector("x%s"%cov_index, get_col(covariates, cov_index+1)))
    vecs_for_brugs =[yt, yt_var]
    vecs_for_brugs.extend(all_covs)
    
    f.write(", ".join(vecs_for_brugs))
    #f.write(", precnorm=%s, gamma.a=%s, gamma.b=%s, N=%s)" % 
    #(precnorm, prior_on_q, prior_on_lam, num_studies))
    f.write(", precnorm=%s, N=%s)" % (precnorm, num_studies))
    f.close()
    
    
def generate_data_file_cont_one_group(out_path, num_studies, yt_vec, sdt_vec, nt_vec, prior_on_q, prior_on_lam, covariates, precnorm=.000001):
    '''
    out_path -- data file will be written here
    num_studies -- number of studies total (equal to len(*_vec), or, N)
    yt_vec -- vector of means in treatment groups over all N studies
    sdt_vec -- vector of standard deviations in treatment groups over all N studies
    nt_vec -- number of subjects in treated groups over all N studies
    covs -- matrix of covariates
    '''	
    precnorm = "%15.10f" % precnorm
    cont_data = []
    f = file(out_path, 'w')
    f.write("list(")
    yt = build_brugs_list_from_vector("y.t", yt_vec)
    sdt = build_brugs_list_from_vector("sd.t", sdt_vec)
    nt = build_brugs_list_from_vector("n.t", nt_vec)
    
    # here we deal with covariates
    all_covs = []
    # the covariates (or, xi) double[,] matrix has a 1s column at index 0. we're not interested in this.
    for cov_index in range(covariates.GetLength(1) - 1):
        all_covs.append(build_brugs_list_from_vector("x%s"%cov_index, get_col(covariates, cov_index+1)))
    vecs_for_brugs =[yt, sdt, nt]
    vecs_for_brugs.extend(all_covs)
    
    f.write(", ".join(vecs_for_brugs))
    f.write(", precnorm=%s, gamma.a=%s, gamma.b=%s, N=%s)" % 
    (precnorm, prior_on_q, prior_on_lam, num_studies))
    f.close()
    
    

def get_col(d, col_index):
    return [d[i, col_index] for i in range(d.GetLength(0))]
    
    
def bugs_cmd(s, brugs_wrapper_path):
    clr.AddReferenceToFileAndPath(brugs_wrapper_path)
    from BRUGSWrapper import *
    b = BRUGS()
    return b.ExecuteCmdBRUGS(s)
    
    
def build_arrs_from_file(f_path):
    file = open(f_path, 'r')
    lines = file.readlines()[1:] # drop the headers
    file.close()
    
    pc_rows = []
    for row in lines:
        pc_rows.append(ma_format_arr(parse_numbers_from_line(row)))
    
    return pc_rows
        

def insert_arr_into_mat(mat, row, arr):
    for col in range(len(arr)):
        mat[row, col] = arr[col]
 
 
def ma_format_arr(arr):
    # mean, sd, 2.5%, median, 97.5%, mc_error
    ma_arr = arr[:2]
    ma_arr.extend(arr[3:6])
    ma_arr.append(arr[2])
    return ma_arr
    
def get_first_line_arr(f_path):
    file = open(f_path, 'r')
    l = parse_numbers_from_line(file.readlines()[1])
    file.close()
    return l
    
    
def parse_numbers_from_line(line):
    nums = [x for x in line.split(" ") if x!= ''][1:] # get rid of variable name
    return [eval(num) for num in nums]
    

def build_brugs_list_from_vector(name, v):
    return name + "=c(" + ",".join([str(x) for x in v])+ ")"