S3 read functions          package:foreign          R Documentation

_R_e_a_d _a_n _S_3 _B_i_n_a_r_y _o_r _d_a_t_a._d_u_m_p _F_i_l_e

_D_e_s_c_r_i_p_t_i_o_n:

     Reads binary data files or 'data.dump' files that were produced in
     S version 3.

_U_s_a_g_e:

       data.restore(file, print = FALSE, verbose = FALSE, env = .GlobalEnv)
       read.S(file)

_A_r_g_u_m_e_n_t_s:

    file: the filename of the S-PLUS 'data.dump' or binary file.

   print: whether to print the name of each object as read from the
          file.

 verbose: whether to print the name of every subitem within each
          object.

     env: environment within which to create the restored object(s).

_D_e_t_a_i_l_s:

     'read.S' can read the binary files produced in some older versions
     of S-PLUS on either Windows (versions 3.x, 4.x, 2000) or Unix
     (version 3.x with 4 byte integers).  It automatically detects
     whether the file was produced on a big- or little-endian machine
     and adapts itself accordingly.

     'data.restore' can read a similar range of files produced by
     'data.dump' and for newer versions of S-PLUS, those from
     'data.dump(....., oldStyle=TRUE)'.

     Not all S3 objects can be handled in the current version.  The
     most frequently encountered exceptions are functions and
     expressions; you will also have trouble with objects that contain
     model formulas.  In particular, comments will be lost from
     function bodies, and the argument lists of functions will often be
     changed.

_V_a_l_u_e:

     For 'read.S', an R version of the S3 object.

     For 'data.restore', the name of the file.

_A_u_t_h_o_r(_s):

     Duncan Murdoch

_E_x_a_m_p_l_e_s:

     ## Not run: 
     read.S(file.path("_Data", "myobj"))
     data.restore("dumpdata", print = TRUE)
     ## End(Not run)

