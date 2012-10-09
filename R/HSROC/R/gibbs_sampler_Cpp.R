gibbs_sampler_Cpp <-
function (iter.num, Gold_Std, Gold_se, Gold_sp, Total, t1, t2, 
    init.PI, init.S1, init.S2, init.C1, init.C2, n, N, alpha.PI, 
    beta.PI, n.refstd, n_REFSTD, Sens2.alpha, Sens2.beta, Spec2.alpha, 
    Spec2.beta, init.alpha, init.theta, init.beta, low.rj, up.rj, 
    init.THETA, init.sigma.theta, init.sigma.alpha, init.LAMBDA, 
    prior.LAMBDA.lower, prior.LAMBDA.upper, beta.a, beta.b, prior.THETA.lower, 
    prior.THETA.upper, low.disp.alpha, up.disp.alpha, low.disp.theta, 
    up.disp.theta, prior_sig_alpha, prior_sig_theta, refresh) 
{
    test = .C("HSROC", iter = as.integer(iter.num), gold_std = as.integer(Gold_Std), 
        gold_se = as.integer(Gold_se), gold_sp = as.integer(Gold_sp), 
        total = as.integer(Total), t1 = as.integer(t1), t2 = as.integer(t2), 
        vec_pi = as.double(init.PI), vec_S1 = as.double(init.S1), 
        vec_S2 = as.double(init.S2), vec_C1 = as.double(init.C1), 
        vec_C2 = as.double(init.C2), study_samplesize = as.integer(n), 
        n_studies = as.integer(N), alpha_pi = as.double(alpha.PI), 
        beta_pi = as.double(beta.PI), refstd = as.integer(n.refstd), 
        numb_refstd = as.integer(n_REFSTD), sens2_alpha = as.double(Sens2.alpha), 
        sens2_beta = as.double(Sens2.beta), spec2_alpha = as.double(Spec2.alpha), 
        spec2_beta = as.double(Spec2.beta), vec_alpha = as.double(init.alpha), 
        vec_theta = as.double(init.theta), vec_beta = as.double(init.beta), 
        low_rij = as.double(low.rj), up_rij = as.double(up.rj), 
        vec_CTHETA = as.double(init.THETA), vec_sigma_theta = as.double(init.sigma.theta), 
        vec_sigma_alpha = as.double(init.sigma.alpha), vec_LAMBDA = as.double(init.LAMBDA), 
        LAMBDA_lower = as.double(prior.LAMBDA.lower), LAMBDA_upper = as.double(prior.LAMBDA.upper), 
        beta_a = as.double(beta.a), beta_b = as.double(beta.b), 
        CTHETA_lower = as.double(prior.THETA.lower), CTHETA_upper = as.double(prior.THETA.upper), 
        low_sd_alpha = as.double(low.disp.alpha), up_sd_alpha = as.double(up.disp.alpha), 
        low_sd_theta = as.double(low.disp.theta), up_sd_theta = as.double(up.disp.theta), 
        prior_sd_alpha = as.integer(prior_sig_alpha), prior_sd_theta = as.integer(prior_sig_theta), 
        refresh = as.integer(refresh), breaking_point = as.integer(0))
    return(test$breaking_point)
}
