#include "vinecopulib-wrappers.hpp"
#include "kde1d-wrappers.hpp"

using namespace vinecopulib;

// tools exports -------------------------------------------

/* hereRCPPexporttag
 Eigen::MatrixXd pseudo_obs_cpp(Eigen::MatrixXd x, std::string ties_method)
 {
 return vinecopulib::tools_stats::to_pseudo_obs(x, ties_method);
 }

 // bicop exports -------------------------------------------

 // hereRCPPexporttag
 void bicop_check_cpp(const Rcpp::List& bicop_r)
 {
 bicop_wrap(bicop_r);
 }

 // [[Rcpp::export()]]
 Rcpp::List bicop_select_cpp(const Eigen::MatrixXd& data,
 std::vector<std::string> family_set,
 std::string par_method,
 std::string nonpar_method,
 double mult,
 std::string selcrit,
 const Eigen::VectorXd& weights,
 double psi0,
 bool presel,
 size_t num_threads,
 std::vector<std::string> var_types)
 {
 std::vector<BicopFamily> fam_set(family_set.size());
 for (unsigned int fam = 0; fam < fam_set.size(); ++fam) {
 fam_set[fam] = to_cpp_family(family_set[fam]);
 }
 FitControlsBicop controls(
 fam_set,
 par_method,
 nonpar_method,
 mult,
 selcrit,
 weights,
 psi0,
 presel,
 num_threads
 );
 Bicop bicop_cpp;
 bicop_cpp.set_var_types(var_types);
 bicop_cpp.select(data, controls);

 return bicop_wrap(bicop_cpp, TRUE);
 }
 */

// [[Rcpp::export()]]
Eigen::VectorXd bicop_pdf_cpp(const Eigen::MatrixXd& u,
                              const Rcpp::List& bicop_r)
{
  return bicop_wrap(bicop_r).pdf(u);
}

// [[Rcpp::export()]]
Eigen::VectorXd bicop_cdf_cpp(const Eigen::MatrixXd& u,
                              const Rcpp::List& bicop_r)
{
  return bicop_wrap(bicop_r).cdf(u);
}

// [[Rcpp::export()]]
Eigen::VectorXd bicop_hfunc1_cpp(const Eigen::MatrixXd& u,
                                 const Rcpp::List& bicop_r)
{
  return bicop_wrap(bicop_r).hfunc1(u);
}

// [[Rcpp::export()]]
Eigen::VectorXd bicop_hfunc2_cpp(const Eigen::MatrixXd& u,
                                 const Rcpp::List& bicop_r)
{
  return bicop_wrap(bicop_r).hfunc2(u);
}

// [[Rcpp::export()]]
Eigen::VectorXd bicop_hinv1_cpp(const Eigen::MatrixXd& u,
                                const Rcpp::List& bicop_r)
{
  return bicop_wrap(bicop_r).hinv1(u);
}

// [[Rcpp::export()]]
Eigen::VectorXd bicop_hinv2_cpp(const Eigen::MatrixXd& u,
                                const Rcpp::List& bicop_r)
{
  return bicop_wrap(bicop_r).hinv2(u);
}

/* hereRCPPexporttag
 Eigen::MatrixXd bicop_sim_cpp(const Rcpp::List& bicop_r,
 const size_t &n,
 const bool qrng,
 std::vector<int> seeds)
 {
 return bicop_wrap(bicop_r).simulate(n, qrng, seeds);
 }
 */

// [[Rcpp::export()]]
double bicop_loglik_cpp(Eigen::MatrixXd& u,
                        const Rcpp::List& bicop_r)
{
  return bicop_wrap(bicop_r).loglik(u);
}

/* hereRCPPexporttag
 double bicop_par_to_tau_cpp(const Rcpp::List& bicop_r)
 {
 Bicop bicop_cpp = bicop_wrap(bicop_r);
 return bicop_cpp.parameters_to_tau(bicop_cpp.get_parameters());
 }

 // hereRCPPexporttag
 Eigen::MatrixXd bicop_tau_to_par_cpp(const Rcpp::List& bicop_r,
 const double& tau)
 {
 Bicop bicop_cpp = bicop_wrap(bicop_r);
 return bicop_cpp.tau_to_parameters(tau);
 }
 */

// structure exports ---------------------------------------------
// deleted

// vinecop exports --------------------------------------------
// deleted


