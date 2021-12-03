// #include "vinecopulib-wrappers.hpp"
//
// using namespace vinecopulib;
//
// // [[Rcpp::export]]
// Eigen::MatrixXd
//   cond_dvine1_cpp(
//     const double n_samples
//     const double cond_alpha,
//     const Rcpp::List& vinecop_r)
//   {
//     auto vinecop_cpp = vinecop_wrap(vinecop_r);
//     auto vine_struct_ = vinecop_cpp.get_rvine_structure();
//     auto copula_dimension = vine_struct_.get_dim();
//
//     auto order = vine_struct_.get_order();
//     auto inverse_order = tools_stl::invert_permutation(order);
//
//     // initialize output matrix with n_samples rows and copula dimension columns
//     Eigen::MatrixXd output_matrix(n_samples, copula_dimension)
//
//     // first loop over n_samples
//     for (size_t sample = 0; sample < n_samples; ++sample) {
//       // temporary object to store the results of the hfunctions
//       TriangularArray<Eigen::VectorXd> auxilary_matrix(d + 1, trunc_lvl + 1);
//
//       // simulate random numbers standard uniform
//       auto u_vector = Rcpp::runif(copula_dimension - 1)
//       // above is probably class NUmericVector (should i consider using
//      // the matrix also from rcpp)
//
//       // any kind of reordering necessary?
//
//       // fill diagonal of this auxiliary matrix with vector (cond_alpha, u_vector)
//       auxilary_matrix.diag() = blabla
//
//       // now loop over the copula to build up the sample
//       for (size_t tree = 1; tree <= copula_dimension; ++tree) {
//         for (size_t edge = tree - 2; edge < 0; --edge) {
//           tools_interface::check_user_interrupt();
//           auto edge_copula = vinecop_cpp.get_pair_copula(tree, tree - edge);
//           hinv_input = Eigen::MatrixXd(1, 2);
//           hinv_input.col(0) = aux_matrix(tree + 1, edge);
//           hinv_input.col(1) = aux_matrix(tree, edge - 1);
//           aux_matrix(k,j) = edge_copula.hinv2(hinv_input))
//           if (tree < copula_dimension) {
//             hfunc_input = Eigen::MatrixXd(1, 2);
//             hfunc_input.col(0) = aux_matrix(tree, edge - 1);
//             hfunc_input.col(1) = aux_matrix(tree, edge);
//             aux_matrix[k + 1, j] = edge_copula.hfunc2(hfunc_input));
//           }
//         }
//       }
//       // put first row in output matrix
//     }
//     return ouptut matrix
//   }
