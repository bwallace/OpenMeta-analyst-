#include <iostream>
#include <fstream>
#include <math.h>
#include <R.h>
#include <Rmath.h>
#include <stdio.h>  
 
using namespace std;

extern "C"{ 

    double Prob_cpp (int t1, int t2, double pi, double S1, double S2, double C1, double C2)
	{
        double res;
        res =  pi*pow(S1,t1)*pow(1.0-S1,1-t1)*pow(S2,t2)*pow(1.0-S2,1-t2)/(pi*pow(S1,t1)*pow(1.0-S1,1-t1)*pow(S2,t2)*pow(1.0-S2,1-t2) + (1.0-pi)*pow(C1,1-t1)*pow(1.0-C1,t1)*pow(C2,1-t2)*pow(1.0-C2,t2) ) ;
        return(res);
	}

    double REFSTD_2 (double likelihood_1, double likelihood_2, double prior)
	{
        double res=0.0;
        res = likelihood_1 + likelihood_2 + prior ;
        return(res);
	}

    double Truncnorm (int t1, double mu, double sd, double theta, double low_r, double up_r )
	{
        // t1 = outcome of subject to test under evaluation
        // mu defined as : alpha.rep*(Y.ij/2) - alpha.rep*((1-Y.ij)/2)
        // sd defined as : exp(vec.beta*(Y.ij - 0.5))
        // theta
        // low_r : lower bound of latent variable rij
        // up_r : upper bound of latent variable rij

        double low =0.0;
        double up =0.0;
        double low_trunc;
        double up_trunc ;
        double x;
        double res;

      //  GetRNGstate();
        if(t1 == 1)
        {
            low = theta;
            up = up_r;
        }
        else if (t1 == 0)
        {
            low = low_r;
            up = theta;
        }
        low_trunc = pnorm( (low-mu)/sd, 0.0, 1.0, 1, 0 ) ;
        up_trunc = pnorm( (up-mu)/sd, 0.0, 1.0, 1, 0 ) ;
        x = runif(low_trunc, up_trunc) ;
        res = qnorm(x, 0.0, 1.0, 1, 0)*sd + mu ;
        // PutRNGstate();
        return(res);
	}

    double Truncnorm2 (double mu, double sd, double low, double up )
	{
        // mu defined as : alpha.rep*(Y.ij/2) - alpha.rep*((1-Y.ij)/2)
        // sd defined as : exp(vec.beta*(Y.ij - 0.5))
        // low : lower bound
        // up : upper bound

        double low_trunc;
        double up_trunc ;
        double x;
        double res;

        //GetRNGstate();
        low_trunc = pnorm( (low-mu)/sd, 0.0, 1.0, 1, 0 ) ;
        up_trunc = pnorm( (up-mu)/sd, 0.0, 1.0, 1, 0 ) ;
        x = runif(low_trunc, up_trunc) ;
        res = qnorm(x, 0.0, 1.0, 1, 0)*sd + mu ;
        //PutRNGstate();
        return(res);
	}


	double mean (double size_x, double *x )
	{

        // size_x : size of the vector to average
        // x : vector to average)

        int i;
        double partial_res = 0.0;
        double res = 0.0;
        double n = size_x;
        for(i=0; i< n; i++)
        {
            partial_res += x[i];
        }
        res = partial_res/n;
        return(res);
	}













	   double truncgamma_cpp(double shape, double scale, double low, double up)
	   {
        // shape = shape parameter
        // scale = scale parameter
        // theta
        // low : lower bound (truncation)
        // up : upper bound (truncation)


        double low_trunc;
        double up_trunc;
        double x;
        double res;
        double x_c;

        //GetRNGstate();
        low_trunc = pgamma( low, shape, scale, 1, 0 ) ;
        up_trunc = pgamma( up, shape, scale, 1, 0 ) ;
        x = runif(low_trunc, up_trunc) ;
        if(x == 0.0)
        {
            x_c = low;
        }
        else if(x == 1.0)
        {
            x_c = up;
        }
        else x_c = qgamma(x, shape, scale, 1, 0);
        res = x_c;
        //PutRNGstate();
        return(res);
	}


    double MH_algo_cpp (int t, double r[], double Y[], double alpha[], double low, double up, double x )

	{

        // t : Total number of subjects in all studies
        // r : latent variabel rij
        // Y : Yij
        // alpha
        // low : lower bound of prior of beta
        // up : upper bound of prior of beta
        // x : previous state value

        //

        //double acc_rate = 0.0;
        double alpha_G = 0.0;
        double beta_G = 0.0;
        double alpha_IG = 0.0;
        double beta_IG = 0.0;
        double aprob;
        double u;
        double previous = x;

        // PROPOSAL DISTRIBUTION IS AN INVERSE GAMMA involving the part where Yij = 1
        for(int i=0; i < t; i++)
        {
            alpha_G += (1.0-Y[i]);
            beta_G += (1.0-Y[i])*( (r[i] + 0.5*alpha[i])*(r[i] + 0.5*alpha[i]) );
            alpha_IG += Y[i];
            beta_IG += Y[i]*( (r[i] - 0.5*alpha[i])*(r[i] - 0.5*alpha[i]) );
        }
        double alpha_G_param = 0.5*alpha_G;
        double beta_G_param = 0.5*beta_G;
        double alpha_IG_param = 0.5*alpha_IG ;
        double beta_IG_param = 0.5*beta_IG;



        //GetRNGstate();
        double can = 1/truncgamma_cpp(alpha_IG_param, 1.0/beta_IG_param, 1.0/up, 1.0/low);
        double numerator = dgamma(can, alpha_G_param, 1.0/(beta_G_param), 0);
        double denominator = dgamma(x, alpha_G_param, 1.0/(beta_G_param), 0);
        double ratio = numerator/(denominator);
        if(1.0 < ratio) aprob = 1.0;
        else aprob = ratio ;
        u = runif(0.0,1.0);
    	if(u < aprob)
        {
            previous = can;
        //    acc_rate = 1;
        }
        //else  previous = previous;
        double res = previous;
        return(res);
        //PutRNGstate();
	}

    double summation (int t, double x[], double y)
    {
        double res = 0.0;
        double partial_res = 0.0;
        for(int i=0; i<t; i++)
        {
            partial_res += (x[i] - y)*(x[i] - y);
        }
        res = 1.0/( 0.5*partial_res );
        return(res);
    }

    double summation2 (int t, double x[], double y, double z)
    {
        double res = 0.0;
        double partial_res = 0.0;
        for(int i=0; i<t; i++)
        {
            partial_res += (x[i] - y)*(x[i] - y);
        }
        res = 1.0/( z + 0.5*partial_res );
        return(res);
    }




/*	//PROGRESSION BAR
    int main (N1 )
    {
        int counter ;
        for (counter = 0; counter < (N1+1); ++counter) {
		  if ((counter / N2) == 2 && (counter % N2) == 0) {
			 cout << "[====                ]20%\r";
		  }
		  else if ((counter / N2) == 4 && (counter % N2) == 0) {
			 cout << "[========            ]40%\r";
		  }
		  else if ((counter / N2) == 6 && (counter % N2) == 0) {
			 cout << "[============        ]60%\r";
	   	   }
		  else if ((counter / N2) == 8 && (counter % N2) == 0) {
			 cout << "[================    ]80%\r";
		  }
		  else if ((counter / N2) == 10 && (counter % N2) == 0) {
			 cout << "[====================]100%" << endl;
		  }
	   }
    }
*/

    double function1 (int gs, double x )
    {
        double a = 0.0;
        if(gs == 0)
        {
            a = x;
        }
        else if(gs == 1)
        {
            a = 1.0;
        }
        return(a);
    }

    double function2 (int gs, double a, double b)
    {
        double res = 0.0;
        if(gs == 0)
        {
            res = rbeta(a, b);
        }
        else if(gs == 1)
        {
            res = 1;
        }
        return(res);
    }
    
    double size_alloc_between_study (int iteration, int max_size)
    {
        int res = 0;
        if(iteration > max_size) { res = max_size; }
        else if(iteration <= max_size) { res = iteration; }
        return(res) ;        
    }


    void HSROC (int *iter, int *gold_std, int *gold_se, int *gold_sp, int *total, int *t1, int *t2, double *vec_pi, double *vec_S1,
                    double *vec_S2, double *vec_C1, double *vec_C2, int *study_samplesize, int *n_studies, double *alpha_pi, double *beta_pi,
                    int *refstd, int *numb_refstd, double *sens2_alpha, double *sens2_beta, double *spec2_alpha, double *spec2_beta, double *vec_alpha,
                    double *vec_theta, double *vec_beta, double *low_rij, double *up_rij, double *vec_CTHETA, double *vec_sigma_theta, double *vec_sigma_alpha,
                    double *vec_LAMBDA, double *LAMBDA_lower, double *LAMBDA_upper, double *beta_a, double *beta_b, double *CTHETA_lower,
                    double *CTHETA_upper, double *low_sd_alpha, double *up_sd_alpha, double *low_sd_theta, double *up_sd_theta,
                    int *prior_sd_alpha, int *prior_sd_theta, int *refresh, int *break_point

                     )
                     
                     
                     /*  (int *iter, int *gold_std, int *gold_se, int *gold_sp, int *total, int *t1, int *t2, double *vec_pi, double *vec_S1,
                    double *vec_S2, double *vec_C1, double *vec_C2,
                    int *study_samplesize, int *n_studies, double *alpha_pi, double *beta_pi, int *refstd, int *numb_refstd,
                    double *sens2_alpha, double *sens2_beta, double *spec2_alpha, double *spec2_beta, double *vec_alpha, double *vec_theta,
                    double *vec_beta, double *low_rij, double *up_rij
                     , int *refresh  ) */
                    

	{
        //int single_print = 200000;
        
/*        if( (*iter)*(*n_studies) > single_print)
        {
            int howmany = floor( (*iter)*(*n_studies) ) ;
            int remains = fmod( (*iter)*(*n_studies), single_point);  
            double matrix_PI[howmany];
        }
        else if( (*iter)*(*n_studies) <= single_print )
        {
             double matrix_PI[(*iter)*(*n_studies)];  
        }
*/
        FILE *PI_file;
        PI_file = fopen("PI.txt", "wb");
        double *matrix_PI = new double [(*iter)*(*n_studies)];
        
        
        FILE *S2_file;
        S2_file = fopen("Sens2.txt", "wb");
        double *matrix_S2 = new double [(*iter)*(*refstd)];
        
        FILE *C2_file;
        C2_file = fopen("Spec2.txt", "wb");
        double *matrix_C2 = new double [(*iter)*(*refstd)];    
        
        FILE *theta_file;
        theta_file = fopen("theta.txt", "wb");
        double *matrix_theta = new double [(*iter)*(*n_studies)];

        FILE *alpha_file;
        alpha_file = fopen("alpha.txt", "wb");
        double *matrix_alpha = new double [(*iter)*(*n_studies)];
        
        FILE *LAMBDA_file;
        LAMBDA_file = fopen("LAMBDA.txt", "wb");
        double *matrix_LAMBDA = new double [(*iter)*(*n_studies)];
        
        FILE *beta_file;
        beta_file = fopen("beta.txt", "wb");
        double *matrix_beta = new double [*iter];
        
        FILE *CTHETA_file;
        CTHETA_file = fopen("capital_THETA.txt", "wb");
        double *matrix_CTHETA = new double [*iter];
        
        FILE *sd_alpha_file;
        sd_alpha_file = fopen("sigma.alpha.txt", "wb");
        double *matrix_sd_alpha = new double [*iter];
        
        FILE *sd_theta_file;
        sd_theta_file = fopen("sigma.theta.txt", "wb");
        double *matrix_sd_theta = new double [*iter];
        
        FILE *S1_file;
        S1_file = fopen("Sens1.txt", "wb");
        double *matrix_S1 = new double [(*iter)*(*n_studies)];

        FILE *C1_file;
        C1_file = fopen("Spec1.txt", "wb");
        double *matrix_C1 = new double [(*iter)*(*n_studies)];
       
        FILE *pool_S_file;
        pool_S_file = fopen("S_overall.txt", "wb");
        double *matrix_pool_S = new double [*iter];
        
        FILE *pool_C_file;
        pool_C_file = fopen("C_overall.txt", "wb");
        double *matrix_pool_C = new double [*iter];
        
        FILE *SNEW_file;
        SNEW_file = fopen("Sens1_new.txt", "wb");
        double *matrix_SNEW = new double [*iter];

        FILE *CNEW_file;
        CNEW_file = fopen("Spec1_new.txt", "wb");
        double *matrix_CNEW = new double [*iter];
        
        
//        int t1_0[*total];
//        int t2_0[*total];
//        for(int i=0; i<*total; i++)
//        {
//            t1_0[i] = 1 - t1[i];
//            t2_0[i] = 1 - t2[i];   
//        }

        int condition = 1;  // Variable to break the outer loop if undefined real results occurs inside an inner loop, i.e. inisde the inner loop of prevalence, or alpha, or theta, ...
        int loop_count = 1;
        Rprintf("Starting the Gibbs sampler for %d iterations ... \n", *iter);
        for(int big_loop=0; big_loop<*iter; big_loop++)
        {
            if(big_loop == (*refresh)*loop_count) {
	           Rprintf(" %d iterations completed out of %d ... \n", (*refresh)*loop_count, *iter );
	           loop_count = loop_count + 1;
            }
            
            
            //*****************************************************
            //  Creation of "Restore" files.  The following files will include the last posterior estimates before the program crashes (if it ever crashes)
            //*****************************************************
/*            ofstream Restore_file;
            Restore_file.open("Restore.txt");
            for(int ii1=0; ii1<*n_studies; ii1++)
            {
                Restore_file << vec_alpha[ii1] << " ";
            }
            Restore_file << endl;
            for(int ii2=0; ii2<*n_studies; ii2++)
            {
                Restore_file << vec_theta[ii2] << " ";
            }
            Restore_file << endl;
            for(int ii3=0; ii3<*n_studies; ii3++)
            {
                Restore_file << vec_S1[ii3] << " ";
            }
            Restore_file << endl;
            for(int ii4=0; ii4<*n_studies; ii4++)
            {
                Restore_file << vec_C1[ii4] << " ";
            }
            Restore_file << endl;
            for(int ii5=0; ii5<*n_studies; ii5++)
            {
                Restore_file << vec_pi[ii5] << " ";
            }
            Restore_file << endl;

            Restore_file.close();

            ofstream Restore2_file;
            Restore2_file.open("Restore2.txt");
            Restore2_file << *vec_LAMBDA << " " << *vec_sigma_alpha << " " << *vec_CTHETA << " " << *vec_sigma_theta << " " << *vec_beta ;
            Restore2_file << endl;
            Restore2_file.close();
            
            if(*gold_std == 0)
            {
                ofstream Restore3_file;
                Restore3_file.open("Restore3.txt");
                for(int ii6=0; ii6<*refstd; ii6++)
                {
                    Restore3_file << vec_S2[ii6] << " ";
                }
                Restore3_file << endl;   
                for(int ii7=0; ii7<*refstd; ii7++)
                {
                    Restore3_file << vec_C2[ii7] << " ";
                }
                Restore3_file << endl;
                Restore3_file.close();
            }            
*/            //*****************************************************

        //*****************************************************
        // REPEAT THE PI, S1 and C1 VECTORS
        //*****************************************************
        int count = 0;
        double pi_rep[*total];
        double S1_rep[*total];
        double C1_rep[*total];
        for(int i=0; i<*n_studies ; i++)
        {
            int N = study_samplesize[i];
            for(int j=0; j<N ; j++)
            {
                pi_rep[count] = vec_pi[i];
                S1_rep[count] = vec_S1[i];
                C1_rep[count] = vec_C1[i];

                count++;
            }
        }
        int count2 = 0;
        double S2_rep[*total];
        double C2_rep[*total];
        for(int i2=0; i2<*refstd ; i2++)
        {
            int N2 = numb_refstd[i2];
            for(int j2=0; j2<N2 ; j2++)
            {
                S2_rep[count2] = function1(*gold_std, vec_S2[i2]);
                C2_rep[count2] = function1(*gold_std, vec_C2[i2]);
                count2++;
            }
        }        
        //*****************************************************


        //*****************************************************
        // Calculation of prob.Yj
        //*****************************************************
        double res_probYij[*total];
        for(int i3=0; i3<*total; i3++)
		{
            res_probYij[i3] = Prob_cpp(t1[i3], t2[i3], pi_rep[i3], S1_rep[i3], S2_rep[i3], C1_rep[i3], C2_rep[i3]);
        }
        //*****************************************************


        //*****************************************************
        // Calculation of Y.ij
        //*****************************************************
        double res_Yij[*total];
        GetRNGstate();
        for(int i4=0; i4<*total; i4++)
        {
           res_Yij[i4] = rbinom(1,res_probYij[i4]);
        }
        PutRNGstate();
        //*****************************************************


        //*****************************************************
        // Calculation of Y1, Y2, Y3 and Y4 AND
        //*****************************************************
        // CONDITIONAL DISTRIBUTION OF PI
        int count5=0;
        double resY1[*n_studies];
        double resY2[*n_studies];
        double resY3[*n_studies];
        double resY4[*n_studies];
        double resPI_a[*n_studies];
        double resPI_b[*n_studies];
        
        //resY1[i5] = resY2[i5] = resY3[i5] = resY4[i5] = resPI_a[i5] = 0.0;
        for(int i5=0; i5<*n_studies; i5++)
        {
            int N5 = study_samplesize[i5] ;
            double sum_y = 0.0;
            for(int j5=0; j5<N5; j5++)
            {
                resY1[i5] += t1[j5+count5]*t2[j5+count5]*res_Yij[j5+count5];
                resY2[i5] += (1-t1[j5+count5])*(1-t2[j5+count5])*res_Yij[j5+count5] ;
                resY3[i5] += t1[j5+count5]*(1-t2[j5+count5])*res_Yij[j5+count5] ;
                resY4[i5] += (1-t1[j5+count5])*t2[j5+count5]*res_Yij[j5+count5] ;
                //resPI_a[i5] += res_Yij[j5+count5];
                sum_y += res_Yij[j5+count5];
            }
            resPI_a[i5] = sum_y + *alpha_pi ;
            resPI_b[i5] = N5 - sum_y + *beta_pi ;
            count5 = count5 + N5  ;
        }
        //double pi_iter[*n_studies];
        GetRNGstate();
        for(int i6=0; i6<*n_studies; i6++)
        {
            vec_pi[i6] = rbeta( resPI_a[i6],  resPI_b[i6]);                 
            matrix_PI[(big_loop*(*n_studies)) + i6] = vec_pi[i6];
            //PI_file << vec_pi[i6] << " ";
            if(!finite(vec_pi[i6]))
            {
                Rprintf("Undefined real result. \n");
                Rprintf("Please set 'first.run' argument to 'FALSE' and call HSROC() again.\n");
                condition = 0;
                *break_point = 5;
                break;
            }
        }    
        if(condition == 0)
        {
            break;   
        }       
        //PI_file << endl;
        PutRNGstate();
        //*****************************************************





        // *****************************************************
        // CONDITIONAL DISTRIBUTION OF S2 & C2.  WE WILL ASSUME A NON GOLD STARDARD WITH MULTIPLE REFERENCE STADARDS
        // *****************************************************
        double res_Xij[*total];            
        for(int i2a=0; i2a<*total; i2a++)
        {
            res_Xij[i2a] = (1.0 - res_Yij[i2a]);
                //Xij_file << res_Xij[i2a] << " ";
        }
            //Xij_file << endl;

        int count3=0;
        double resX1[*refstd];
        double resX2[*refstd];
        double resX3[*refstd];
        double resX4[*refstd];
        double resYY1[*refstd];
        double resYY2[*refstd];
        double resYY3[*refstd];
        double resYY4[*refstd];
        for(int i6=0; i6<*refstd; i6++)
        {
            int rssize2 = numb_refstd[i6];
            for(int j3=0; j3<rssize2; j3++)
            {
                resX1[i6] += t1[j3+count3]*t2[j3+count3]*res_Xij[j3+count3];
                resX2[i6] += (1-t1[j3+count3])*(1-t2[j3+count3])*res_Xij[j3+count3];
                resX3[i6] += t1[j3+count3]*(1-t2[j3+count3])*res_Xij[j3+count3];
                resX4[i6] += (1-t1[j3+count3])*t2[j3+count3]*res_Xij[j3+count3];
                resYY1[i6] += t1[j3+count3]*t2[j3+count3]*res_Yij[j3+count3];
                resYY2[i6] += (1-t1[j3+count3])*(1-t2[j3+count3])*res_Yij[j3+count3];
                resYY3[i6] += t1[j3+count3]*(1-t2[j3+count3])*res_Yij[j3+count3];
                resYY4[i6] += (1-t1[j3+count3])*t2[j3+count3]*res_Yij[j3+count3];
            }
                //X1_file << resX1[i6] << " ";
            count3 = count3 + rssize2  ;
        }
            //X1_file << endl;


/*        if(*gold_se == 1)
        {
            double a_sp2[*refstd];
            double b_sp2[*refstd];
            for(int i9=0; i9<*refstd; i9++)
            {
                a_sp2[i9] = resX2[i9] + resX3[i9] + spec2_alpha[i9];
                b_sp2[i9] = resX1[i9] + resX4[i9] + spec2_beta[i9];
            }
            S2_file << endl;
            GetRNGstate();
            for(int i9a=0; i9a<*refstd; i9a++)
            {
                vec_C2[i9a] = rbeta(a_sp2[i9a], b_sp2[i9a]);
                C2_file << vec_C2[i9a] << " ";
            }                
            C2_file << endl;
            PutRNGstate();
            *vec_S1 = 1;
            for(int reinit_3=0; reinit_3<*refstd; reinit_3++)
            {
                a_sp2[reinit_3] = 0;
                b_sp2[reinit_3] = 0;
            }
        }
        else if(*gold_sp == 1)
        {
            double a_se2[*refstd];
            double b_se2[*refstd];
            for(int i9=0; i9<*refstd; i9++)
            {
                a_se2[i9] = resYY1[i9] + resYY4[i9] + sens2_alpha[i9];
                b_se2[i9] = resYY2[i9] + resYY3[i9] + sens2_beta[i9];
            }
            GetRNGstate();
            for(int i9a=0; i9a<*refstd; i9a++)
            {
                vec_S2[i9a] = rbeta(a_se2[i9a], b_se2[i9a]);
                S2_file << vec_S2[i9a] << " ";
            }
            *vec_C1 = 1;
            PutRNGstate();
            for(int reinit_3=0; reinit_3<*refstd; reinit_3++)
            {
                a_se2[reinit_3] = 0;
                b_se2[reinit_3] = 0;
            }
        }
        else
        {
*/     

        double a_se2[*refstd];
        double b_se2[*refstd];
        double a_sp2[*refstd];
        double b_sp2[*refstd];
        for(int i9=0; i9<*refstd; i9++)
        {
            a_se2[i9] = resYY1[i9] + resYY4[i9] + sens2_alpha[i9];
            b_se2[i9] = resYY2[i9] + resYY3[i9] + sens2_beta[i9];
            a_sp2[i9] = resX2[i9] + resX3[i9] + spec2_alpha[i9];
            b_sp2[i9] = resX1[i9] + resX4[i9] + spec2_beta[i9];
        }
        GetRNGstate();
        for(int i9a=0; i9a<*refstd; i9a++)
        {
            vec_S2[i9a] = function2(*gold_std, a_se2[i9a], b_se2[i9a] );
            matrix_S2[(big_loop*(*refstd)) + i9a] = vec_S2[i9a];
            //S2_file << vec_S2[i9a] << " ";
            if(!finite(vec_S2[i9a]))
            {
                Rprintf("Undefined real result. \n");
                Rprintf("Please set 'first.run' argument to 'FALSE' and call HSROC() again.\n");
                condition = 0;
                *break_point = 11;
                break;
                
            }
        }
        if(condition == 0)
        {
            break;
        }
        //S2_file << endl;
        PutRNGstate();
        GetRNGstate();
        for(int i9b=0; i9b<*refstd; i9b++)
        {
            vec_C2[i9b] = function2(*gold_std, a_sp2[i9b], b_sp2[i9b] );
            matrix_C2[(big_loop*(*refstd)) + i9b] = vec_C2[i9b];
            //C2_file << vec_C2[i9b] << " ";
            if(!finite(vec_C2[i9b]))
            {
                Rprintf("Undefined real result. \n");
                Rprintf("Please set 'first.run' argument to 'FALSE' and call HSROC() again.\n");
                condition = 0;
                *break_point = 12;
                break;
            }
        }
        if(condition == 0) {break;}  //break the outer loop because of undefined real results
        //C2_file << endl;
        PutRNGstate();

        for(int reinit_3=0; reinit_3<*refstd; reinit_3++)
        {
            resX1[reinit_3] = 0.0;
            resX2[reinit_3] = 0.0;
            resX3[reinit_3] = 0.0;
            resX4[reinit_3] = 0.0;
            resYY1[reinit_3] = 0.0;
            resYY2[reinit_3] = 0.0;
            resYY3[reinit_3] = 0.0;
            resYY4[reinit_3] = 0.0;
        }
        

        // *****************************************************

        // *****************************************************
        // CONDITIONAL DISTRIBUTION OF rij
        // *****************************************************
        // REPEAT THE alpha AND theta VECTORS
        int counter_iv = 0;
        double alpha_rep[*total];
        double theta_rep[*total];
        double mu[*total];
        double sd[*total];
        double rij[*total];
        GetRNGstate();
        for(int iv=0; iv<*n_studies ; iv++)
        {
            int times_iv = study_samplesize[iv];
            for(int jv=0; jv<times_iv ; jv++)
            {
                alpha_rep[counter_iv] = vec_alpha[iv];
                theta_rep[counter_iv] = vec_theta[iv];
                mu[counter_iv] = alpha_rep[counter_iv]*(res_Yij[counter_iv]/2.0) - alpha_rep[counter_iv]*((1.0-res_Yij[counter_iv])/2.0);
                sd[counter_iv] = exp(*vec_beta*(res_Yij[counter_iv] - 0.5));
                rij[counter_iv] = Truncnorm(t1[counter_iv], mu[counter_iv], sd[counter_iv], theta_rep[counter_iv], *low_rij, *up_rij);
                counter_iv++;
            }
        }
        PutRNGstate();
        // *****************************************************

//
        // *****************************************************
        // CONDITIONAL DISTRIBUTION OF theta
        // *****************************************************
        int count10 = 0;
        double lower_t[*n_studies];
        double upper_t[*n_studies];
        for(int i10 = 0; i10<*n_studies; i10++)
        {
            int size_10 = study_samplesize[i10] ;
            double max = *low_rij;
            double min = *up_rij;
            for(int j10 = 0; j10<size_10; j10++)
            {
                if(t1[j10+count10] == 0)
                {
                    if (max < rij[j10+count10]) max = rij[j10+count10];
                }
                else if(t1[j10+count10] == 1)
                {
                    if (min > rij[j10+count10]) min = rij[j10+count10];
                    else min = min;
                }
            }
            lower_t[i10] = max ;
            upper_t[i10] = min ;
            count10 = count10 + size_10  ;
        }
        GetRNGstate();
        for(int i11=0; i11<*n_studies; i11++)
        {
            vec_theta[i11] = Truncnorm2(*vec_CTHETA, *vec_sigma_theta, lower_t[i11], upper_t[i11] )   ;
            matrix_theta[(big_loop*(*n_studies)) + i11] = vec_theta[i11];
            //theta_file << vec_theta[i11] << " ";
            if(!finite(vec_theta[i11]))
            {
                Rprintf("Undefined real result. \n");
                Rprintf("Please set 'first.run' argument to 'FALSE' and call HSROC() again.\n");
                condition = 0;
                *break_point = 2;
                break;
            }
        }
        if(condition == 0) {break;}  //break the outer loop because of undefined real results
        //theta_file << endl;
        PutRNGstate();
        // *****************************************************


        // *****************************************************
        // CONDITIONAL DISTRIBUTION OF alpha
        // *****************************************************
        int count12 = 0;
        double A[*n_studies];
        double B[*n_studies];
        for (int i12 = 0; i12 < *n_studies; i12++)
        {
            double partial_A = 0.0;
            double partial_B = 0.0;
            int size_12 = study_samplesize[i12] ;
            for(int j12=0 ; j12<size_12; j12++)
            {
                partial_A +=  (res_Yij[j12+count12]*res_Yij[j12+count12] + (1.0-res_Yij[j12+count12])*(1.0-res_Yij[j12+count12]))/exp(*vec_beta*2.0*(res_Yij[j12+count12] - 0.5));
                partial_B +=  (rij[j12+count12]*(2.0*res_Yij[j12+count12] - 1.0) )/exp(*vec_beta*2.0*(res_Yij[j12+count12] - 0.5));
            }
            count12 = count12 + size_12  ;
            A[i12] = 0.25*partial_A + 1.0/( (*vec_sigma_alpha)*(*vec_sigma_alpha) );
            B[i12] = partial_B + (*vec_LAMBDA*2.0)/( (*vec_sigma_alpha)*(*vec_sigma_alpha) );
            //vec_alpha[i12] = rnorm(B[i12]/(2.0*A[i12]), 1.0/sqrt(A[i12]));
            //alpha_file << vec_alpha[i12] << " ";
        }
        GetRNGstate();
        for(int i14 = 0; i14 < *n_studies; i14++)
        {
            vec_alpha[i14] = rnorm(B[i14]/(2.0*A[i14]), 1.0/sqrt(A[i14]));
            matrix_alpha[(big_loop*(*n_studies)) + i14] = vec_alpha[i14];
            //alpha_file << vec_alpha[i14] << " ";
            if(!finite(vec_alpha[i14]))
            {
                Rprintf("Undefined real result. \n");
                Rprintf("Please set 'first.run' argument to 'FALSE' and call HSROC() again.\n");
                condition = 0;
                *break_point = 1;
                break;
            }
        }
        if(condition == 0) {break;}  //break the outer loop because of undefined real results
        //alpha_file << endl;
        PutRNGstate();
        // *****************************************************

        // *****************************************************
        // CONDITIONAL DISTRIBUTION OF LAMBDA
        // *****************************************************
        GetRNGstate();
        *vec_LAMBDA = Truncnorm2(mean(*n_studies,vec_alpha),  *vec_sigma_alpha/sqrt(*n_studies), *LAMBDA_lower, *LAMBDA_upper )   ;
        matrix_LAMBDA[ big_loop ] = *vec_LAMBDA;
        //LAMBDA_file << *vec_LAMBDA << " ";
        //LAMBDA_file << endl;
        PutRNGstate();
        if(!finite(*vec_LAMBDA))
        {
            Rprintf("Undefined real result. \n");
            Rprintf("Please set 'first.run' argument to 'FALSE' and call HSROC() again.\n");
            *break_point = 6;
            break;
        }
        // *****************************************************



        // *****************************************************
        // CONDITIONAL DISTRIBUTION OF beta
        // *****************************************************
        double vec_exp_beta = 0.0;
        GetRNGstate();
        vec_exp_beta = MH_algo_cpp(*total, rij, res_Yij, alpha_rep, exp(*beta_a), exp(*beta_b), exp(*vec_beta) );
        *vec_beta = log(vec_exp_beta) ;
        matrix_beta[ big_loop ] = *vec_beta;
        //beta_file << *vec_beta << " ";
        //beta_file << endl;
        PutRNGstate();
        if(!finite(*vec_beta))
        {
            Rprintf("Undefined real result. \n");
            Rprintf("Please set 'first.run' argument to 'FALSE' and call HSROC() again.\n");
            *break_point = 10;
            break;
        }        
        // *****************************************************


        // *****************************************************
        // CONDITIONAL DISTRIBUTION OF CAPITAL_THETA
        // *****************************************************
        GetRNGstate();
        *vec_CTHETA = Truncnorm2(mean(*n_studies,vec_theta),  *vec_sigma_theta/sqrt(*n_studies), *CTHETA_lower, *CTHETA_upper )   ;
        matrix_CTHETA[ big_loop ] = *vec_CTHETA;
        //CTHETA_file << *vec_CTHETA << " ";
        //CTHETA_file << endl;
        PutRNGstate();
        if(!finite(*vec_CTHETA))
        {
            Rprintf("Undefined real result. \n");
            Rprintf("Please set 'first.run' argument to 'FALSE' and call HSROC() again.\n");
            *break_point = 8;
            break;
        }
        // *****************************************************



        // *****************************************************
        // CONDITIONAL DISTRIBUTION OF sigma_alpha
        // *****************************************************
        if(*prior_sd_alpha == 1)        // 1 = "sd"
        {
            double prec_alpha_shape = 0.0;
            double prec_alpha_scale = 0.0;
            double prec_alpha = 0.0;

            prec_alpha_shape = (*n_studies/2.0) - 0.5;
            prec_alpha_scale = summation(*n_studies, vec_alpha, *vec_LAMBDA);
            GetRNGstate();
            prec_alpha = truncgamma_cpp(prec_alpha_shape, prec_alpha_scale, *low_sd_alpha, *up_sd_alpha);
            PutRNGstate();
            *vec_sigma_alpha = sqrt(1.0/prec_alpha);
            matrix_sd_alpha[ big_loop ] = *vec_sigma_alpha;
            //sd_alpha_file << *vec_sigma_alpha << " ";
            //sd_alpha_file << endl;
            if(!finite(*vec_sigma_alpha))
            {
                Rprintf("Undefined real result. \n");
                Rprintf("Please set 'first.run' argument to 'FALSE' and call HSROC() again.\n");
                *break_point = 7;
                break;
            }
        }
        else if(*prior_sd_alpha == 2)   // 2 = "v"
        {
            double prec_alpha_shape = 0.0;
            double prec_alpha_scale = 0.0;
            double prec_alpha = 0.0;

            prec_alpha_shape = (*n_studies/2.0) - 1.0;
            prec_alpha_scale = summation(*n_studies, vec_alpha, *vec_LAMBDA);
            GetRNGstate();
            prec_alpha = truncgamma_cpp(prec_alpha_shape, prec_alpha_scale, *low_sd_alpha, *up_sd_alpha);
            PutRNGstate();
            *vec_sigma_alpha = sqrt(1.0/prec_alpha);
            matrix_sd_alpha[ big_loop ] = *vec_sigma_alpha;
            //sd_alpha_file << *vec_sigma_alpha << " ";
            //sd_alpha_file << endl;
            if(!finite(*vec_sigma_alpha))
            {
                Rprintf("Undefined real result. \n");
                Rprintf("Please set 'first.run' argument to 'FALSE' and call HSROC() again.\n");
                *break_point = 7;
                break;
            }
        }
        else if(*prior_sd_alpha == 3)   // 3 = "p"
        {
            double prec_alpha_shape = 0.0;
            double prec_alpha_scale = 0.0;
            double prec_alpha = 0.0;

            prec_alpha_shape = (*n_studies/2.0) + *low_sd_alpha;
            prec_alpha_scale = summation2(*n_studies, vec_alpha, *vec_LAMBDA, *up_sd_alpha);
            GetRNGstate();
            prec_alpha = rgamma( prec_alpha_shape, prec_alpha_scale ) ;
            PutRNGstate();
            *vec_sigma_alpha = sqrt(1.0/prec_alpha);
            matrix_sd_alpha[ big_loop ] = *vec_sigma_alpha;
            //sd_alpha_file << *vec_sigma_alpha << " ";
            //sd_alpha_file << endl;
            if(!finite(*vec_sigma_alpha))
            {
                Rprintf("Undefined real result. \n");
                Rprintf("Please set 'first.run' argument to 'FALSE' and call HSROC() again.\n");
                *break_point = 7;
                break;
            }
        }

        // *****************************************************






        // *****************************************************
        // CONDITIONAL DISTRIBUTION OF sigma_theta
        // *****************************************************
        if(*prior_sd_theta == 1)        // 1 = "sd"
        {
            double prec_theta_shape = 0.0;
            double prec_theta_scale = 0.0;
            double prec_theta = 0.0;

            prec_theta_shape = (*n_studies/2.0) - 0.5;
            prec_theta_scale = summation(*n_studies, vec_theta, *vec_CTHETA);
            GetRNGstate();
            prec_theta = truncgamma_cpp(prec_theta_shape, prec_theta_scale, *low_sd_theta, *up_sd_theta);
            PutRNGstate();
            *vec_sigma_theta = sqrt(1.0/prec_theta);
            matrix_sd_theta[ big_loop ] = *vec_sigma_theta;
            //sd_theta_file << *vec_sigma_theta << " ";
            //sd_theta_file << endl;
            if(!finite(*vec_sigma_theta))
            {
                Rprintf("Undefined real result. \n");
                Rprintf("Please set 'first.run' argument to 'FALSE' and call HSROC() again.\n");
                *break_point = 9;
                break;
            }
        }
        if(*prior_sd_theta == 2)        // 2 = "v"
        {
            double prec_theta_shape = 0.0;
            double prec_theta_scale = 0.0;
            double prec_theta = 0.0;

            prec_theta_shape = (*n_studies/2.0) - 1.0;
            prec_theta_scale = summation(*n_studies, vec_theta, *vec_CTHETA);
            GetRNGstate();
            prec_theta = truncgamma_cpp(prec_theta_shape, prec_theta_scale, *low_sd_theta, *up_sd_theta);
            PutRNGstate();
            *vec_sigma_theta = sqrt(1.0/prec_theta);
            matrix_sd_theta[ big_loop ] = *vec_sigma_theta;
            //sd_theta_file << *vec_sigma_theta << " ";
            //sd_theta_file << endl;
            if(!finite(*vec_sigma_theta))
            {
                Rprintf("Undefined real result. \n");
                Rprintf("Please set 'first.run' argument to 'FALSE' and call HSROC() again.\n");
                *break_point = 9;
                break;
            }
        }
        if(*prior_sd_theta == 3)        // 3 = "p"
        {
            double prec_theta_shape = 0.0;
            double prec_theta_scale = 0.0;
            double prec_theta = 0.0;

            prec_theta_shape = (*n_studies/2.0) + *low_sd_theta;
            prec_theta_scale = summation2(*n_studies, vec_theta, *vec_CTHETA, *up_sd_theta );
            GetRNGstate();
            prec_theta = rgamma( prec_theta_shape, prec_theta_scale ) ;
            PutRNGstate();
            *vec_sigma_theta = sqrt(1.0/prec_theta);
            matrix_sd_theta[ big_loop ] = *vec_sigma_theta;
            //sd_theta_file << *vec_sigma_theta << " ";
            //sd_theta_file << endl;
            if(!finite(*vec_sigma_theta))
            {
                Rprintf("Undefined real result. \n");
                Rprintf("Please set 'first.run' argument to 'FALSE' and call HSROC() again.\n");
                *break_point = 9;
                break;
            }
        }
        // *****************************************************



        // *****************************************************
        // CONDITIONAL DISTRIBUTION OF S1 AND C1 AND Pooled S and Pooled C
        // *****************************************************
        GetRNGstate();
        for(int i15=0; i15<*n_studies; i15++)
        {
            vec_S1[i15] = 1.0-pnorm( exp((-(*vec_beta))/2.0)*(vec_theta[i15] - (vec_alpha[i15]/2.0)), 0, 1, 1, 0 );
            matrix_S1[(big_loop*(*n_studies)) + i15] = vec_S1[i15];
            //S1_file << vec_S1[i15] << " ";
            if(!finite(vec_S1[i15]))
            {
                Rprintf("Undefined real result. \n");
                Rprintf("Please set 'first.run' argument to 'FALSE' and call HSROC() again.\n");
                condition = 0;
                *break_point = 3;
                break;
            }
        }        
        if(condition == 0) {break;}  //break the outer loop because of undefined real results
        //S1_file << endl;
        PutRNGstate();
        GetRNGstate();
        for(int i16=0; i16<*n_studies; i16++)
        {
            vec_C1[i16] = pnorm( exp(*vec_beta/2.0)*(vec_theta[i16] + (vec_alpha[i16]/2.0)), 0, 1, 1, 0 );
            matrix_C1[(big_loop*(*n_studies)) + i16] = vec_C1[i16];
            //C1_file << vec_C1[i16] << " ";
            if(!finite(vec_C1[i16]))
            {
                Rprintf("Undefined real result. \n");
                Rprintf("Please set 'first.run' argument to 'FALSE' and call HSROC() again.\n");
                condition = 0;
                *break_point = 4;
                break;
            }

        }
        if(condition == 0) {break;}  //break the outer loop because of undefined real results
        //C1_file << endl;
        PutRNGstate();
        GetRNGstate();
        double C_pool = pnorm( exp(*vec_beta/2.0)*(*vec_CTHETA + (*vec_LAMBDA/2.0)), 0, 1, 1, 0 );
        matrix_pool_C[ big_loop ] = C_pool;
        PutRNGstate();
        //pool_C_file << C_pool << " ";
        //pool_C_file << endl;
        GetRNGstate();
        double S_pool = 1.0-pnorm( exp(-(*vec_beta)/2.0)*(*vec_CTHETA - (*vec_LAMBDA/2.0)), 0, 1, 1, 0 );
        matrix_pool_S[ big_loop ] = S_pool;
        PutRNGstate();
        //pool_S_file << S_pool << " ";
        //pool_S_file << endl;
        // *****************************************************


        // *****************************************************
        // CALCULATION OF POSTERIOR PREDICTIVE VALUES FOR A NEW STUDY THAT HAS NOT YET TAKEN PLACE
        // *****************************************************
        GetRNGstate();
        double beta_new = runif(*beta_a, *beta_b) ;
        PutRNGstate();
        GetRNGstate();
        double theta_new = rnorm(*vec_CTHETA, *vec_sigma_theta) ;
        PutRNGstate();
        GetRNGstate();
        double alpha_new = rnorm(*vec_LAMBDA, *vec_sigma_alpha) ;
        PutRNGstate();
        GetRNGstate();
        double Sens_new = 1.0 - pnorm( exp(-(beta_new)/2.0)*(theta_new - (alpha_new/2.0)), 0, 1, 1, 0 ) ;
        PutRNGstate();
        matrix_SNEW[ big_loop ] = Sens_new;
        //SNEW_file << Sens_new << " ";
        //SNEW_file << endl;
        GetRNGstate();
        double Spec_new = pnorm( exp(beta_new/2.0)*(theta_new + (alpha_new/2.0)), 0, 1, 1, 0 ) ;
        PutRNGstate();
        matrix_CNEW[ big_loop ] = Spec_new;
        //CNEW_file << Spec_new << " ";
        //CNEW_file << endl;

       // *****************************************************
//



        // RE-INITIALIZE SOME OF THE VARIABLES TO MAKE SURE THEY DO NOT ADD UP NUMBERS FROM PREVIOUS ITERATIONS
        for(int reinit=0; reinit<*n_studies; reinit++)
        {
            resY1[reinit] = 0.0;
            resY2[reinit] = 0.0;
            resY3[reinit] = 0.0;
            resY4[reinit] = 0.0;
            resPI_a[reinit] = 0.0;
            resPI_b[reinit] = 0.0;
            A[reinit] = 0;
            B[reinit] = 0;
        }
        for(int reinit_4=0; reinit_4<*total; reinit_4++)
        {
            res_probYij[reinit_4] = 0.0;
            res_Yij[reinit_4] = 0.0;
            alpha_rep[reinit_4] = 0;
            theta_rep[reinit_4] = 0;
            mu[reinit_4] = 0;
            sd[reinit_4] = 0;
            rij[reinit_4] = 0;
        }




	}//big_loop
	
	
	fwrite(matrix_PI, sizeof(matrix_PI[0]), (*iter)*(*n_studies), PI_file);
	fwrite(matrix_S2, sizeof(matrix_S2[0]), (*iter)*(*refstd), S2_file);
	fwrite(matrix_C2, sizeof(matrix_C2[0]), (*iter)*(*refstd), C2_file);
	fwrite(matrix_theta, sizeof(matrix_theta[0]), (*iter)*(*n_studies), theta_file);
	fwrite(matrix_alpha, sizeof(matrix_alpha[0]), (*iter)*(*n_studies), alpha_file);
	fwrite(matrix_LAMBDA, sizeof(matrix_LAMBDA[0]), *iter, LAMBDA_file);
	fwrite(matrix_beta, sizeof(matrix_beta[0]), *iter, beta_file);
	fwrite(matrix_CTHETA, sizeof(matrix_CTHETA[0]), *iter, CTHETA_file);
	fwrite(matrix_sd_alpha, sizeof(matrix_sd_alpha[0]), *iter, sd_alpha_file);
	fwrite(matrix_sd_theta, sizeof(matrix_sd_theta[0]), *iter, sd_theta_file);
	fwrite(matrix_S1, sizeof(matrix_S1[0]), (*iter)*(*n_studies), S1_file);
	fwrite(matrix_C1, sizeof(matrix_C2[0]), (*iter)*(*n_studies), C1_file);
	fwrite(matrix_pool_C, sizeof(matrix_pool_C[0]), *iter, pool_C_file);
	fwrite(matrix_pool_S, sizeof(matrix_pool_S[0]), *iter, pool_S_file);
	fwrite(matrix_SNEW, sizeof(matrix_SNEW[0]), *iter, SNEW_file);
	fwrite(matrix_CNEW, sizeof(matrix_CNEW[0]), *iter, CNEW_file);
	
	fclose(PI_file);
	fclose(S2_file);
	fclose(C2_file);
	fclose(theta_file);
	fclose(alpha_file);
	fclose(LAMBDA_file);
	fclose(beta_file);
	fclose(CTHETA_file);
	fclose(sd_alpha_file);
	fclose(sd_theta_file);
	fclose(S1_file);
	fclose(C1_file);
	fclose(pool_C_file);
	fclose(pool_S_file);
	fclose(SNEW_file);
	fclose(CNEW_file);


    
    }

}


