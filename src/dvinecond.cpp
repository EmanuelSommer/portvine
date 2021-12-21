#include "vinecopulib-wrappers.hpp"

using namespace vinecopulib;

void iterate_aux_matrix(int start_column,
                                   int copula_dimension,
                                   Vinecop vinecop,
                                   Eigen::MatrixXd& auxiliary) {
  Eigen::MatrixXd hinv_input(1, 2);
  Eigen::MatrixXd hfunc_input(1, 2);
  for (size_t column = start_column; column < copula_dimension; ++column) {
    for (int row = column - 1; row >= 0; --row) {
      tools_interface::check_user_interrupt();
      auto edge_copula = vinecop.get_pair_copula(row, column - 1 - row);
      hinv_input(0, 0) = auxiliary(row + 1, column);
      hinv_input(0, 1) = auxiliary(row, column - 1);
      auxiliary(row, column) = edge_copula.hinv2(hinv_input)(0, 0);
      if (column < copula_dimension - 1) {
        hfunc_input(0, 0) = auxiliary(row, column - 1);
        hfunc_input(0, 1) = auxiliary(row, column);
        auxiliary(row + 1, column) = edge_copula.hfunc2(hfunc_input)(0, 0);
      }
    }
  }
}


// [[Rcpp::export]]
Eigen::MatrixXd
  cond_dvine1_cpp(
    const int n_samples,
    const double cond_alpha,
    const Rcpp::List& vinecop_r)
  {
    auto vinecop_cpp = vinecop_wrap(vinecop_r);
    auto vine_struct_ = vinecop_cpp.get_rvine_structure();
    auto copula_dimension = vine_struct_.get_dim();

    // initialize output matrix with n_samples rows and copula dimension columns
    Eigen::MatrixXd output_matrix(n_samples, int(copula_dimension));

    // create auxiliary matrix to build up the sample
    Eigen::MatrixXd auxiliary(copula_dimension, copula_dimension);

    // first loop over n_samples (just repeat n_samples times)
    for (size_t sample = 0; sample < n_samples; ++sample) {
      // create the uniform samples
      Rcpp::NumericVector uniform_samples = Rcpp::runif(copula_dimension - 1);
      // set the diagonal
      auxiliary(0, 0) = cond_alpha;
      for(int i = 1; i < auxiliary.rows(); ++i) {
        auxiliary(i, i) = uniform_samples[i - 1];
      }

      // iterate over the auxiliary matrix (columns first then rows)
      // according to the conditional sampling algorithm
      // first initialize two additional matrices to save hfunction inputs
      iterate_aux_matrix(1, copula_dimension,
                         vinecop_cpp, auxiliary);
      // write samples to row of the the output matrix
      output_matrix.row(sample) = auxiliary.row(0);
    }
    return output_matrix;
  }


// [[Rcpp::export]]
Eigen::MatrixXd
  cond_dvine2_cpp(
    const int n_samples,
    const double cond_alpha,
    const Rcpp::List& vinecop_r)
  {
    auto vinecop_cpp = vinecop_wrap(vinecop_r);
    auto vine_struct_ = vinecop_cpp.get_rvine_structure();
    auto copula_dimension = vine_struct_.get_dim();

    // initialize output matrix with n_samples rows and copula dimension columns
    Eigen::MatrixXd output_matrix(n_samples, int(copula_dimension));

    // create auxiliary matrix to build up the sample
    Eigen::MatrixXd auxiliary(copula_dimension, copula_dimension);

    // calculate the second conditional value and a needed h function value
    Eigen::MatrixXd second_cond_input(1, 2);
    second_cond_input(0, 0) = cond_alpha;
    second_cond_input(0, 1) = cond_alpha;
    auto indices_edge_copula = vinecop_cpp.get_pair_copula(0, 0);
    double second_cond_alpha = indices_edge_copula.hinv2(second_cond_input)(0, 0);
    second_cond_input(0, 0) = second_cond_alpha;
    double cond_hfunc_value = indices_edge_copula.hfunc2(second_cond_input)(0, 0);

    // first loop over n_samples (just repeat n_samples times)
    for (size_t sample = 0; sample < n_samples; ++sample) {
      // create the uniform samples
      Rcpp::NumericVector uniform_samples = Rcpp::runif(copula_dimension - 2);
      // set the fixed conditioning values
      auxiliary(0, 0) = second_cond_alpha;
      auxiliary(0, 1) = cond_alpha;
      auxiliary(1, 1) = cond_hfunc_value;
      // set the random samples on the diagonal
      for(int i = 2; i < auxiliary.rows(); ++i) {
        auxiliary(i, i) = uniform_samples[i - 2];
      }

      // iterate over the auxiliary matrix (columns first then rows)
      // according to the conditional sampling algorithm
      // first initialize two additional matrices to save hfunction inputs
      iterate_aux_matrix(2, copula_dimension,
                         vinecop_cpp, auxiliary);

      // write samples to row of the the output matrix
      output_matrix.row(sample) = auxiliary.row(0);
    }
    return output_matrix;
  }

