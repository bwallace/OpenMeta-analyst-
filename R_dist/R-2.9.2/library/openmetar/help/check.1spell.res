check.1spell.res          package:openmetar          R Documentation



_U_s_a_g_e:

     check.1spell.res(n, se)

_A_r_g_u_m_e_n_t_s:

       n: 

      se: 

_E_x_a_m_p_l_e_s:

     ##---- Should be DIRECTLY executable !! ----
     ##-- ==>  Define data, use random,
     ##--    or do  help(data=index)  for the standard data sets.

     ## The function is currently defined as
     function (n, se) 
     {
         succeeded <- TRUE
         comment <- ""
         if (!is.na(n)) {
             if (n <= 1) {
                 comment <- "n<=1"
                 succeeded <- FALSE
             }
         }
         if (se <= 0) {
             comment <- paste("se<=0", comment, sep = ", ")
             succeeded <- FALSE
         }
         return(list(succeeded = succeeded, comment = comment))
       }

