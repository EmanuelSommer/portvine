// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// bicop_hfunc2_cpp
Eigen::VectorXd bicop_hfunc2_cpp(const Eigen::MatrixXd& u, const Rcpp::List& bicop_r);
RcppExport SEXP _portvine_bicop_hfunc2_cpp(SEXP uSEXP, SEXP bicop_rSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Eigen::MatrixXd& >::type u(uSEXP);
    Rcpp::traits::input_parameter< const Rcpp::List& >::type bicop_r(bicop_rSEXP);
    rcpp_result_gen = Rcpp::wrap(bicop_hfunc2_cpp(u, bicop_r));
    return rcpp_result_gen;
END_RCPP
}
// bicop_hinv2_cpp
Eigen::VectorXd bicop_hinv2_cpp(const Eigen::MatrixXd& u, const Rcpp::List& bicop_r);
RcppExport SEXP _portvine_bicop_hinv2_cpp(SEXP uSEXP, SEXP bicop_rSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Eigen::MatrixXd& >::type u(uSEXP);
    Rcpp::traits::input_parameter< const Rcpp::List& >::type bicop_r(bicop_rSEXP);
    rcpp_result_gen = Rcpp::wrap(bicop_hinv2_cpp(u, bicop_r));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_portvine_bicop_hfunc2_cpp", (DL_FUNC) &_portvine_bicop_hfunc2_cpp, 2},
    {"_portvine_bicop_hinv2_cpp", (DL_FUNC) &_portvine_bicop_hinv2_cpp, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_portvine(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
