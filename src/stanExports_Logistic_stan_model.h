// Generated by rstantools.  Do not edit by hand.

/*
    BayesGrowth is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    BayesGrowth is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with BayesGrowth.  If not, see <http://www.gnu.org/licenses/>.
*/
#ifndef MODELS_HPP
#define MODELS_HPP
#define STAN__SERVICES__COMMAND_HPP
#ifndef USE_STANC3
#define USE_STANC3
#endif
#include <rstan/rstaninc.hpp>
// Code generated by stanc v2.26.1-4-gd72b68b7-dirty
#include <stan/model/model_header.hpp>
namespace model_Logistic_stan_model_namespace {
inline void validate_positive_index(const char* var_name, const char* expr,
                                    int val) {
  if (val < 1) {
    std::stringstream msg;
    msg << "Found dimension size less than one in simplex declaration"
        << "; variable=" << var_name << "; dimension size expression=" << expr
        << "; expression value=" << val;
    std::string msg_str(msg.str());
    throw std::invalid_argument(msg_str.c_str());
  }
}
inline void validate_unit_vector_index(const char* var_name, const char* expr,
                                       int val) {
  if (val <= 1) {
    std::stringstream msg;
    if (val == 1) {
      msg << "Found dimension size one in unit vector declaration."
          << " One-dimensional unit vector is discrete"
          << " but the target distribution must be continuous."
          << " variable=" << var_name << "; dimension size expression=" << expr;
    } else {
      msg << "Found dimension size less than one in unit vector declaration"
          << "; variable=" << var_name << "; dimension size expression=" << expr
          << "; expression value=" << val;
    }
    std::string msg_str(msg.str());
    throw std::invalid_argument(msg_str.c_str());
  }
}
using std::istream;
using std::string;
using std::stringstream;
using std::vector;
using std::pow;
using stan::io::dump;
using stan::math::lgamma;
using stan::model::model_base_crtp;
using stan::model::rvalue;
using stan::model::cons_list;
using stan::model::index_uni;
using stan::model::index_max;
using stan::model::index_min;
using stan::model::index_min_max;
using stan::model::index_multi;
using stan::model::index_omni;
using stan::model::nil_index_list;
using namespace stan::math;
using stan::math::pow; 
stan::math::profile_map profiles__;
static int current_statement__= 0;
static const std::vector<string> locations_array__ = {" (found before start of program)",
                                                      " (in 'string', line 12, column 2 to column 19)",
                                                      " (in 'string', line 13, column 2 to column 21)",
                                                      " (in 'string', line 14, column 2 to column 18)",
                                                      " (in 'string', line 16, column 2 to column 22)",
                                                      " (in 'string', line 34, column 2 to column 20)",
                                                      " (in 'string', line 36, column 4 to column 99)",
                                                      " (in 'string', line 35, column 17 to line 37, column 3)",
                                                      " (in 'string', line 35, column 2 to line 37, column 3)",
                                                      " (in 'string', line 20, column 9 to column 10)",
                                                      " (in 'string', line 20, column 2 to column 18)",
                                                      " (in 'string', line 22, column 2 to column 41)",
                                                      " (in 'string', line 23, column 2 to column 39)",
                                                      " (in 'string', line 24, column 2 to column 28)",
                                                      " (in 'string', line 25, column 2 to column 32)",
                                                      " (in 'string', line 28, column 4 to column 67)",
                                                      " (in 'string', line 29, column 4 to column 53)",
                                                      " (in 'string', line 27, column 15 to line 30, column 3)",
                                                      " (in 'string', line 27, column 2 to line 30, column 3)",
                                                      " (in 'string', line 2, column 2 to column 17)",
                                                      " (in 'string', line 4, column 18 to column 19)",
                                                      " (in 'string', line 4, column 2 to column 25)",
                                                      " (in 'string', line 5, column 18 to column 19)",
                                                      " (in 'string', line 5, column 2 to column 28)",
                                                      " (in 'string', line 7, column 2 to column 19)",
                                                      " (in 'string', line 8, column 2 to column 31)",
                                                      " (in 'string', line 34, column 9 to column 10)"};
#include <stan_meta_header.hpp>
class model_Logistic_stan_model final : public model_base_crtp<model_Logistic_stan_model> {
private:
  int n;
  Eigen::Matrix<double, -1, 1> Age;
  Eigen::Matrix<double, -1, 1> Length;
  Eigen::Matrix<double, -1, 1> priors;
  Eigen::Matrix<double, -1, 1> priors_se;
 
public:
  ~model_Logistic_stan_model() { }
  
  inline std::string model_name() const final { return "model_Logistic_stan_model"; }
  inline std::vector<std::string> model_compile_info() const noexcept {
    return std::vector<std::string>{"stanc_version = stanc3 v2.26.1-4-gd72b68b7-dirty", "stancflags = "};
  }
  
  
  model_Logistic_stan_model(stan::io::var_context& context__,
                            unsigned int random_seed__ = 0,
                            std::ostream* pstream__ = nullptr) : model_base_crtp(0) {
    using local_scalar_t__ = double ;
    boost::ecuyer1988 base_rng__ = 
        stan::services::util::create_rng(random_seed__, 0);
    (void) base_rng__;  // suppress unused var warning
    static const char* function__ = "model_Logistic_stan_model_namespace::model_Logistic_stan_model";
    (void) function__;  // suppress unused var warning
    local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
    (void) DUMMY_VAR__;  // suppress unused var warning
    
    try {
      int pos__;
      pos__ = std::numeric_limits<int>::min();
      
      pos__ = 1;
      current_statement__ = 19;
      context__.validate_dims("data initialization","n","int",
          context__.to_vec());
      n = std::numeric_limits<int>::min();
      
      current_statement__ = 19;
      n = context__.vals_i("n")[(1 - 1)];
      current_statement__ = 19;
      current_statement__ = 19;
      check_greater_or_equal(function__, "n", n, 1);
      current_statement__ = 20;
      validate_non_negative_index("Age", "n", n);
      current_statement__ = 21;
      context__.validate_dims("data initialization","Age","double",
          context__.to_vec(n));
      Age = Eigen::Matrix<double, -1, 1>(n);
      stan::math::fill(Age, std::numeric_limits<double>::quiet_NaN());
      
      {
        std::vector<local_scalar_t__> Age_flat__;
        current_statement__ = 21;
        assign(Age_flat__, nil_index_list(), context__.vals_r("Age"),
          "assigning variable Age_flat__");
        current_statement__ = 21;
        pos__ = 1;
        current_statement__ = 21;
        for (int sym1__ = 1; sym1__ <= n; ++sym1__) {
          current_statement__ = 21;
          assign(Age, cons_list(index_uni(sym1__), nil_index_list()),
            Age_flat__[(pos__ - 1)], "assigning variable Age");
          current_statement__ = 21;
          pos__ = (pos__ + 1);}
      }
      current_statement__ = 21;
      for (int sym1__ = 1; sym1__ <= n; ++sym1__) {
        current_statement__ = 21;
        current_statement__ = 21;
        check_greater_or_equal(function__, "Age[sym1__]", Age[(sym1__ - 1)],
                               0);}
      current_statement__ = 22;
      validate_non_negative_index("Length", "n", n);
      current_statement__ = 23;
      context__.validate_dims("data initialization","Length","double",
          context__.to_vec(n));
      Length = Eigen::Matrix<double, -1, 1>(n);
      stan::math::fill(Length, std::numeric_limits<double>::quiet_NaN());
      
      {
        std::vector<local_scalar_t__> Length_flat__;
        current_statement__ = 23;
        assign(Length_flat__, nil_index_list(), context__.vals_r("Length"),
          "assigning variable Length_flat__");
        current_statement__ = 23;
        pos__ = 1;
        current_statement__ = 23;
        for (int sym1__ = 1; sym1__ <= n; ++sym1__) {
          current_statement__ = 23;
          assign(Length, cons_list(index_uni(sym1__), nil_index_list()),
            Length_flat__[(pos__ - 1)], "assigning variable Length");
          current_statement__ = 23;
          pos__ = (pos__ + 1);}
      }
      current_statement__ = 23;
      for (int sym1__ = 1; sym1__ <= n; ++sym1__) {
        current_statement__ = 23;
        current_statement__ = 23;
        check_greater_or_equal(function__, "Length[sym1__]",
                               Length[(sym1__ - 1)], 0);}
      current_statement__ = 24;
      context__.validate_dims("data initialization","priors","double",
          context__.to_vec(4));
      priors = Eigen::Matrix<double, -1, 1>(4);
      stan::math::fill(priors, std::numeric_limits<double>::quiet_NaN());
      
      {
        std::vector<local_scalar_t__> priors_flat__;
        current_statement__ = 24;
        assign(priors_flat__, nil_index_list(), context__.vals_r("priors"),
          "assigning variable priors_flat__");
        current_statement__ = 24;
        pos__ = 1;
        current_statement__ = 24;
        for (int sym1__ = 1; sym1__ <= 4; ++sym1__) {
          current_statement__ = 24;
          assign(priors, cons_list(index_uni(sym1__), nil_index_list()),
            priors_flat__[(pos__ - 1)], "assigning variable priors");
          current_statement__ = 24;
          pos__ = (pos__ + 1);}
      }
      current_statement__ = 25;
      context__.validate_dims("data initialization","priors_se","double",
          context__.to_vec(2));
      priors_se = Eigen::Matrix<double, -1, 1>(2);
      stan::math::fill(priors_se, std::numeric_limits<double>::quiet_NaN());
      
      {
        std::vector<local_scalar_t__> priors_se_flat__;
        current_statement__ = 25;
        assign(priors_se_flat__, nil_index_list(),
          context__.vals_r("priors_se"),
          "assigning variable priors_se_flat__");
        current_statement__ = 25;
        pos__ = 1;
        current_statement__ = 25;
        for (int sym1__ = 1; sym1__ <= 2; ++sym1__) {
          current_statement__ = 25;
          assign(priors_se, cons_list(index_uni(sym1__), nil_index_list()),
            priors_se_flat__[(pos__ - 1)], "assigning variable priors_se");
          current_statement__ = 25;
          pos__ = (pos__ + 1);}
      }
      current_statement__ = 25;
      for (int sym1__ = 1; sym1__ <= 2; ++sym1__) {
        current_statement__ = 25;
        current_statement__ = 25;
        check_greater_or_equal(function__, "priors_se[sym1__]",
                               priors_se[(sym1__ - 1)], 0);}
      current_statement__ = 26;
      validate_non_negative_index("log_lik", "n", n);
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
      // Next line prevents compiler griping about no return
      throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***"); 
    }
    num_params_r__ = 0U;
    
    try {
      num_params_r__ += 1;
      num_params_r__ += 1;
      num_params_r__ += 1;
      num_params_r__ += 1;
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
      // Next line prevents compiler griping about no return
      throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***"); 
    }
  }
  template <bool propto__, bool jacobian__, typename VecR, typename VecI, stan::require_vector_like_t<VecR>* = nullptr, stan::require_vector_like_vt<std::is_integral, VecI>* = nullptr>
  inline stan::scalar_type_t<VecR> log_prob_impl(VecR& params_r__,
                                                 VecI& params_i__,
                                                 std::ostream* pstream__ = nullptr) const {
    using T__ = stan::scalar_type_t<VecR>;
    using local_scalar_t__ = T__;
    T__ lp__(0.0);
    stan::math::accumulator<T__> lp_accum__;
    static const char* function__ = "model_Logistic_stan_model_namespace::log_prob";
(void) function__;  // suppress unused var warning
    stan::io::reader<local_scalar_t__> in__(params_r__, params_i__);
    local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
    (void) DUMMY_VAR__;  // suppress unused var warning
    
    try {
      local_scalar_t__ L0;
      L0 = DUMMY_VAR__;
      
      current_statement__ = 1;
      L0 = in__.scalar();
      current_statement__ = 1;
      if (jacobian__) {
        current_statement__ = 1;
        L0 = stan::math::lb_constrain(L0, 0, lp__);
      } else {
        current_statement__ = 1;
        L0 = stan::math::lb_constrain(L0, 0);
      }
      local_scalar_t__ Linf;
      Linf = DUMMY_VAR__;
      
      current_statement__ = 2;
      Linf = in__.scalar();
      current_statement__ = 2;
      if (jacobian__) {
        current_statement__ = 2;
        Linf = stan::math::lb_constrain(Linf, 0, lp__);
      } else {
        current_statement__ = 2;
        Linf = stan::math::lb_constrain(Linf, 0);
      }
      local_scalar_t__ k;
      k = DUMMY_VAR__;
      
      current_statement__ = 3;
      k = in__.scalar();
      current_statement__ = 3;
      if (jacobian__) {
        current_statement__ = 3;
        k = stan::math::lb_constrain(k, 0, lp__);
      } else {
        current_statement__ = 3;
        k = stan::math::lb_constrain(k, 0);
      }
      local_scalar_t__ sigma;
      sigma = DUMMY_VAR__;
      
      current_statement__ = 4;
      sigma = in__.scalar();
      current_statement__ = 4;
      if (jacobian__) {
        current_statement__ = 4;
        sigma = stan::math::lb_constrain(sigma, 0, lp__);
      } else {
        current_statement__ = 4;
        sigma = stan::math::lb_constrain(sigma, 0);
      }
      {
        current_statement__ = 9;
        validate_non_negative_index("PredL", "n", n);
        Eigen::Matrix<local_scalar_t__, -1, 1> PredL;
        PredL = Eigen::Matrix<local_scalar_t__, -1, 1>(n);
        stan::math::fill(PredL, DUMMY_VAR__);
        
        current_statement__ = 11;
        lp_accum__.add(
          normal_lpdf<propto__>(Linf, priors[(1 - 1)], priors_se[(1 - 1)]));
        current_statement__ = 12;
        lp_accum__.add(
          normal_lpdf<propto__>(L0, priors[(2 - 1)], priors_se[(2 - 1)]));
        current_statement__ = 13;
        lp_accum__.add(uniform_lpdf<propto__>(k, 0, priors[(3 - 1)]));
        current_statement__ = 14;
        lp_accum__.add(uniform_lpdf<propto__>(sigma, 0, priors[(4 - 1)]));
        current_statement__ = 18;
        for (int i = 1; i <= n; ++i) {
          current_statement__ = 15;
          assign(PredL, cons_list(index_uni(i), nil_index_list()),
            (((Linf * L0) * stan::math::exp((k * Age[(i - 1)]))) /
              (Linf + (L0 * (stan::math::exp((k * Age[(i - 1)])) - 1)))),
            "assigning variable PredL");
          current_statement__ = 16;
          lp_accum__.add(
            normal_lpdf<false>(Length[(i - 1)], PredL[(i - 1)], sigma));}
      }
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
      // Next line prevents compiler griping about no return
      throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***"); 
    }
    lp_accum__.add(lp__);
    return lp_accum__.sum();
    } // log_prob_impl() 
    
  template <typename RNG, typename VecR, typename VecI, typename VecVar, stan::require_vector_like_vt<std::is_floating_point, VecR>* = nullptr, stan::require_vector_like_vt<std::is_integral, VecI>* = nullptr, stan::require_std_vector_vt<std::is_floating_point, VecVar>* = nullptr>
  inline void write_array_impl(RNG& base_rng__, VecR& params_r__,
                               VecI& params_i__, VecVar& vars__,
                               const bool emit_transformed_parameters__ = true,
                               const bool emit_generated_quantities__ = true,
                               std::ostream* pstream__ = nullptr) const {
    using local_scalar_t__ = double;
    vars__.resize(0);
    stan::io::reader<local_scalar_t__> in__(params_r__, params_i__);
    static const char* function__ = "model_Logistic_stan_model_namespace::write_array";
(void) function__;  // suppress unused var warning
    (void) function__;  // suppress unused var warning
    double lp__ = 0.0;
    (void) lp__;  // dummy to suppress unused var warning
    stan::math::accumulator<double> lp_accum__;
    local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
    (void) DUMMY_VAR__;  // suppress unused var warning
    
    try {
      double L0;
      L0 = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 1;
      L0 = in__.scalar();
      current_statement__ = 1;
      L0 = stan::math::lb_constrain(L0, 0);
      double Linf;
      Linf = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 2;
      Linf = in__.scalar();
      current_statement__ = 2;
      Linf = stan::math::lb_constrain(Linf, 0);
      double k;
      k = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 3;
      k = in__.scalar();
      current_statement__ = 3;
      k = stan::math::lb_constrain(k, 0);
      double sigma;
      sigma = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 4;
      sigma = in__.scalar();
      current_statement__ = 4;
      sigma = stan::math::lb_constrain(sigma, 0);
      vars__.emplace_back(L0);
      vars__.emplace_back(Linf);
      vars__.emplace_back(k);
      vars__.emplace_back(sigma);
      if (logical_negation((primitive_value(emit_transformed_parameters__) ||
            primitive_value(emit_generated_quantities__)))) {
        return ;
      } 
      if (logical_negation(emit_generated_quantities__)) {
        return ;
      } 
      Eigen::Matrix<double, -1, 1> log_lik;
      log_lik = Eigen::Matrix<double, -1, 1>(n);
      stan::math::fill(log_lik, std::numeric_limits<double>::quiet_NaN());
      
      current_statement__ = 8;
      for (int i = 1; i <= n; ++i) {
        current_statement__ = 6;
        assign(log_lik, cons_list(index_uni(i), nil_index_list()),
          normal_lpdf<false>(Length[(i - 1)],
            (((Linf * L0) * stan::math::exp((k * Age[(i - 1)]))) /
              (Linf + (L0 * (stan::math::exp((k * Age[(i - 1)])) - 1)))),
            sigma), "assigning variable log_lik");}
      for (int sym1__ = 1; sym1__ <= n; ++sym1__) {
        vars__.emplace_back(log_lik[(sym1__ - 1)]);}
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
      // Next line prevents compiler griping about no return
      throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***"); 
    }
    } // write_array_impl() 
    
  template <typename VecVar, typename VecI, stan::require_std_vector_t<VecVar>* = nullptr, stan::require_vector_like_vt<std::is_integral, VecI>* = nullptr>
  inline void transform_inits_impl(const stan::io::var_context& context__,
                                   VecI& params_i__, VecVar& vars__,
                                   std::ostream* pstream__ = nullptr) const {
    using local_scalar_t__ = double;
    vars__.clear();
    vars__.reserve(num_params_r__);
    
    try {
      int pos__;
      pos__ = std::numeric_limits<int>::min();
      
      pos__ = 1;
      double L0;
      L0 = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 1;
      L0 = context__.vals_r("L0")[(1 - 1)];
      double L0_free__;
      L0_free__ = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 1;
      L0_free__ = stan::math::lb_free(L0, 0);
      double Linf;
      Linf = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 2;
      Linf = context__.vals_r("Linf")[(1 - 1)];
      double Linf_free__;
      Linf_free__ = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 2;
      Linf_free__ = stan::math::lb_free(Linf, 0);
      double k;
      k = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 3;
      k = context__.vals_r("k")[(1 - 1)];
      double k_free__;
      k_free__ = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 3;
      k_free__ = stan::math::lb_free(k, 0);
      double sigma;
      sigma = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 4;
      sigma = context__.vals_r("sigma")[(1 - 1)];
      double sigma_free__;
      sigma_free__ = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 4;
      sigma_free__ = stan::math::lb_free(sigma, 0);
      vars__.emplace_back(L0_free__);
      vars__.emplace_back(Linf_free__);
      vars__.emplace_back(k_free__);
      vars__.emplace_back(sigma_free__);
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
      // Next line prevents compiler griping about no return
      throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***"); 
    }
    } // transform_inits_impl() 
    
  inline void get_param_names(std::vector<std::string>& names__) const {
    
    names__.clear();
    names__.emplace_back("L0");
    names__.emplace_back("Linf");
    names__.emplace_back("k");
    names__.emplace_back("sigma");
    names__.emplace_back("log_lik");
    } // get_param_names() 
    
  inline void get_dims(std::vector<std::vector<size_t>>& dimss__) const {
    dimss__.clear();
    dimss__.emplace_back(std::vector<size_t>{});
    
    dimss__.emplace_back(std::vector<size_t>{});
    
    dimss__.emplace_back(std::vector<size_t>{});
    
    dimss__.emplace_back(std::vector<size_t>{});
    
    dimss__.emplace_back(std::vector<size_t>{static_cast<size_t>(n)});
    
    } // get_dims() 
    
  inline void constrained_param_names(
                                      std::vector<std::string>& param_names__,
                                      bool emit_transformed_parameters__ = true,
                                      bool emit_generated_quantities__ = true) const
    final {
    
    param_names__.emplace_back(std::string() + "L0");
    param_names__.emplace_back(std::string() + "Linf");
    param_names__.emplace_back(std::string() + "k");
    param_names__.emplace_back(std::string() + "sigma");
    if (emit_transformed_parameters__) {
      
    }
    
    if (emit_generated_quantities__) {
      for (int sym1__ = 1; sym1__ <= n; ++sym1__) {
        {
          param_names__.emplace_back(std::string() + "log_lik" + '.' + std::to_string(sym1__));
        }}
    }
    
    } // constrained_param_names() 
    
  inline void unconstrained_param_names(
                                        std::vector<std::string>& param_names__,
                                        bool emit_transformed_parameters__ = true,
                                        bool emit_generated_quantities__ = true) const
    final {
    
    param_names__.emplace_back(std::string() + "L0");
    param_names__.emplace_back(std::string() + "Linf");
    param_names__.emplace_back(std::string() + "k");
    param_names__.emplace_back(std::string() + "sigma");
    if (emit_transformed_parameters__) {
      
    }
    
    if (emit_generated_quantities__) {
      for (int sym1__ = 1; sym1__ <= n; ++sym1__) {
        {
          param_names__.emplace_back(std::string() + "log_lik" + '.' + std::to_string(sym1__));
        }}
    }
    
    } // unconstrained_param_names() 
    
  inline std::string get_constrained_sizedtypes() const {
    stringstream s__;
    s__ << "[{\"name\":\"L0\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"Linf\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"k\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"sigma\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"log_lik\",\"type\":{\"name\":\"vector\",\"length\":" << n << "},\"block\":\"generated_quantities\"}]";
    return s__.str();
    } // get_constrained_sizedtypes() 
    
  inline std::string get_unconstrained_sizedtypes() const {
    stringstream s__;
    s__ << "[{\"name\":\"L0\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"Linf\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"k\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"sigma\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"log_lik\",\"type\":{\"name\":\"vector\",\"length\":" << n << "},\"block\":\"generated_quantities\"}]";
    return s__.str();
    } // get_unconstrained_sizedtypes() 
    
  
    // Begin method overload boilerplate
    template <typename RNG>
    inline void write_array(RNG& base_rng,
                            Eigen::Matrix<double,Eigen::Dynamic,1>& params_r,
                            Eigen::Matrix<double,Eigen::Dynamic,1>& vars,
                            const bool emit_transformed_parameters = true,
                            const bool emit_generated_quantities = true,
                            std::ostream* pstream = nullptr) const {
      std::vector<double> vars_vec(vars.size());
      std::vector<int> params_i;
      write_array_impl(base_rng, params_r, params_i, vars_vec,
          emit_transformed_parameters, emit_generated_quantities, pstream);
      vars.resize(vars_vec.size());
      for (int i = 0; i < vars.size(); ++i) {
        vars.coeffRef(i) = vars_vec[i];
      }
    }
    template <typename RNG>
    inline void write_array(RNG& base_rng, std::vector<double>& params_r,
                            std::vector<int>& params_i,
                            std::vector<double>& vars,
                            bool emit_transformed_parameters = true,
                            bool emit_generated_quantities = true,
                            std::ostream* pstream = nullptr) const {
      write_array_impl(base_rng, params_r, params_i, vars, emit_transformed_parameters, emit_generated_quantities, pstream);
    }
    template <bool propto__, bool jacobian__, typename T_>
    inline T_ log_prob(Eigen::Matrix<T_,Eigen::Dynamic,1>& params_r,
                       std::ostream* pstream = nullptr) const {
      Eigen::Matrix<int, -1, 1> params_i;
      return log_prob_impl<propto__, jacobian__>(params_r, params_i, pstream);
    }
    template <bool propto__, bool jacobian__, typename T__>
    inline T__ log_prob(std::vector<T__>& params_r,
                        std::vector<int>& params_i,
                        std::ostream* pstream = nullptr) const {
      return log_prob_impl<propto__, jacobian__>(params_r, params_i, pstream);
    }
  
    inline void transform_inits(const stan::io::var_context& context,
                         Eigen::Matrix<double, Eigen::Dynamic, 1>& params_r,
                         std::ostream* pstream = nullptr) const final {
      std::vector<double> params_r_vec(params_r.size());
      std::vector<int> params_i;
      transform_inits_impl(context, params_i, params_r_vec, pstream);
      params_r.resize(params_r_vec.size());
      for (int i = 0; i < params_r.size(); ++i) {
        params_r.coeffRef(i) = params_r_vec[i];
      }
    }
    inline void transform_inits(const stan::io::var_context& context,
                                std::vector<int>& params_i,
                                std::vector<double>& vars,
                                std::ostream* pstream = nullptr) const final {
      transform_inits_impl(context, params_i, vars, pstream);
    }        
};
}
using stan_model = model_Logistic_stan_model_namespace::model_Logistic_stan_model;
#ifndef USING_R
// Boilerplate
stan::model::model_base& new_model(
        stan::io::var_context& data_context,
        unsigned int seed,
        std::ostream* msg_stream) {
  stan_model* m = new stan_model(data_context, seed, msg_stream);
  return *m;
}
stan::math::profile_map& get_stan_profile_data() {
  return model_Logistic_stan_model_namespace::profiles__;
}
#endif
#endif
