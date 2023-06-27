template<typename VecType>
VecType pdfnorm(const VecType& x, const VecType& Mu, const VecType& Sigma) {
    VecType pdf = exp(-0.5 * square(x - Mu) / square(Sigma)) / (sqrt(2.0 * arma::datum::pi) * Sigma);
    for (size_t i = 0; i < pdf.size(); i++) {
        if (pdf(i) == 0.0)
            pdf(i) = 2.22507e-24;
    }
    return pdf;
}

template<typename VecType>
VecType stddens(const typename VecType::elem_type& x, const VecType& Mu, const VecType& Sigma, const VecType& Nu) {
    const size_t K = Nu.size();
    VecType a(K);
    VecType b(K);
    VecType pdf(K);
    
    for (size_t i = 0; i < K; i++) {
        a(i) = Rf_gammafn((Nu(i) + 1.0) / 2.0) / (sqrt(arma::datum::pi * (Nu(i) - 2)) * Sigma(i));
        b(i) = Rf_gammafn(Nu(i) / 2.0) * pow((1 + pow((x - Mu(i)), 2) / (pow(Sigma(i), 2) * (Nu(i) - 2))), ((Nu(i) + 1) / 2));
    }
    pdf = a / b;
    return pdf;
}

template<typename VecType>
double MixtDensityScale(const VecType& vOmega, const VecType& vD_log, const int& M) {
  VecType wp_log = log(vOmega) + vD_log;
  double dK = max(wp_log);
  VecType wp_log_scaled = wp_log - dK;
  
  double dLK = 0;
  for (int i = 0; i < M; i++) {
    dLK += exp(wp_log_scaled(i));
  }
  double dLLK = dK + log(dLK);
  
  if (dLLK < -1e150) {
    dLLK = -1e50;
  }
  return dLLK;
}


template<typename VecType, typename MatType>
double getLLK(const VecType& vY, const MatType& mMu, const MatType& mSigma2, const MatType& mWeight,
              const std::string& iDist, const VecType& Nu) {
    int iT = vY.size();
    int K = mMu.n_rows;
    VecType vLLK(iT);

    for (int i = 0; i < iT; i++) {
        double y = vY(i);
        const VecType& muCol = mMu.col(i);
        const VecType& sigmaCol = sqrt(mSigma2.col(i));

        if (iDist == "norm") {
            vLLK(i) = MixtDensityScale(mWeight.col(i), log(pdfnorm(y, muCol, sigmaCol, K)), K);
        } else if (iDist == "std") {
            vLLK(i) = MixtDensityScale(mWeight.col(i), log(stddens(y, muCol, sigmaCol, Nu)), K);
        }
    }

    double output = accu(vLLK);
    return output;
}

// [[Rcpp::export]]
List Mixgas_DAMM(const arma::vec& vY, const List& lP, const List& Spec, const List& StartValues) {
  
  //Get the spec
  int iT = vY.size();
  const int K = Spec["K"];
  const std::string Model = Spec["Model"];
  const std::string iDist = Spec["iDist"];
  const arma::vec vSigma2_start = StartValues["vSigma2_start"];
  const arma::vec vW_start = StartValues["vW_start"];
  double llk;
  
  const std::string iType = Spec["iType"];
  arma::mat IndicatorGJR(K, (iT + 1));
  
  //Set double variables/vectors
  List tmp_Xi;
  
  //Paras W
  arma::vec wKappa = lP["wKappa"];
  arma::mat wA = lP["wA"];
  arma::mat wB = lP["wB"];
  
  //Paras Sigma_tilde
  arma::vec vKappa = lP["vKappa"];
  arma::vec vA = lP["vA"];
  arma::vec vB = lP["vB"];
  arma::vec vNu = lP["vNu"];
  arma::vec vPhi = lP["vPhi"];
  
  //Get matrix for time-varying parameters
  arma::mat mSigma2(K, (iT + 1));
  arma::mat mSigma_tilde(K, (iT + 1));
  arma::mat mW(K, (iT + 1));
  arma::mat mW_tilde((K - 1), (iT + 1));
  arma::mat tmp_Jac;
  arma::vec A_term;
  arma::vec vSigma(iT + 1);
  
  //Set initial values
  mSigma2.col(0) = vSigma2_start;
  mSigma_tilde.col(0) = log(mSigma2.col(0))/2;
  mW.col(0) = vW_start;
  mW_tilde.col(0) = WUnmap(mW.col(0));
  
  //Set mMu
  arma::vec vMu = lP["vMu"];
  arma::mat mMu(K, (iT + 1), fill::value(0));
  for(int j = 0; j<(K-1); j++) {
    mMu.row(j).fill(vMu(j));
  }
  arma::vec tmp_Mu = ((mW.col(0).rows(0,(K-2))).t()*vMu.rows(0,K-2))/mW((K-1),0);
  mMu((K-1),0) = -tmp_Mu(0);
  
  if(iType == "GJR") {
    for(int j = 0; j<K; j++) {
      if((vY(0) - mMu(j,0)) < 0) {
        IndicatorGJR(j,0) = 1;
      }
    }
  }
  
  //First conditinal volility
  vSigma(0) = sqrt(accu(mW.col(0) % (mSigma2.col(0) + pow(mMu.col(0), 2)) ));

  tmp_Xi =  Xi_cal(vY(0), mW.col(0), mMu.col(0), sqrt(mSigma2.col(0)), vNu, iDist);
  arma::vec tmp_WScore = tmp_Xi["WScore"];
  arma::vec tmp_SigmaXi = tmp_Xi["Xi"];
  A_term = A_cal(vY(0), mMu.col(0), sqrt(mSigma2.col(0)), Spec, vNu);
  tmp_Jac = WJac(mW_tilde.col(0));
  mW_tilde.col(1) = wKappa + wA*tmp_Jac.t()*tmp_WScore + wB*mW_tilde.col(0);
  
  arma::mat Score_test(K, iT);
  arma::mat Aterm_test(K, iT);
  arma::mat Xi_test(K, iT);
  arma::mat Jac_test(K, iT);
  
  for(int i=1;i<(iT+1); i++) {
    
    tmp_Xi =  Xi_cal(vY(i-1), mW.col(i - 1), mMu.col(i-1), sqrt(mSigma2.col(i-1)), vNu, iDist);
    arma::vec tmp_WScore = tmp_Xi["WScore"];
    arma::vec tmp_SigmaXi = tmp_Xi["Xi"];
    Score_test.col(i-1) = tmp_WScore;
    Xi_test.col(i-1) = tmp_SigmaXi;
    
    tmp_Jac = WJac(mW_tilde.col(i-1));
    
    mW_tilde.col(i) = wKappa + wA*tmp_Jac.t()*tmp_WScore + wB*mW_tilde.col(i - 1);
    mW.col(i) = WMap(mW_tilde.col(i));
    
    A_term = A_cal(vY(i - 1), mMu.col(i-1), sqrt(mSigma2.col(i-1)), Spec, vNu);
    Aterm_test.col(i-1) = A_term;
    
    arma::vec tmp_Mu = ((mW.col(i).rows(0,(K-2))).t()*vMu.rows(0,K-2))/mW((K-1),i);
    mMu((K-1),i) = -tmp_Mu(0);
    
    if(iType == "GJR") {
      for(int j = 0; j<K; j++) {
        if((vY((i - 1)) - mMu(j,(i - 1))) < 0) {
          IndicatorGJR(j,(i-1)) = 1;
        }
      }
    }
    
    for(int j=0; j<K; j++) {
    mSigma_tilde(j,i) = vKappa(j) + vA(j)*tmp_SigmaXi(j)*A_term(j) + vB(j)*mSigma_tilde(j,i-1) + pow((vY((i - 1)) - mMu(j,(i - 1))), 2)*IndicatorGJR(j,(i-1))*vPhi(j)/mSigma2(j,i-1);
      
    if(mSigma_tilde(j,i) > 2.5) {
      mSigma_tilde(j,i) = 2.5;
    }
    
    if(mSigma_tilde(j,i) < -2 ) {
      mSigma_tilde(j,i) = -2;
    }
      
    mSigma2(j,i) = exp(2*mSigma_tilde(j,i));
    
    }
    
    if(i < (iT + 1)) {
      vSigma(i) = sqrt(accu(mW.col(i) % (mSigma2.col(i) + pow(mMu.col(i), 2)) ));
    }
    
  }
  
  llk = getLLK(vY, mMu.cols(0,(iT-1)), mSigma2.cols(0, (iT-1)), mW.cols(0, (iT-1)), K, iDist, vNu);

  List lOut;
  lOut["llk"] = llk;
  lOut["K"] = K;
  lOut["iDist"] = iDist;
  lOut["mSigma2"] = mSigma2;
  lOut["vSigma"] = vSigma;
  lOut["mSigma2_tilde"] = mSigma_tilde;
  lOut["vSigma"] = vSigma;
  lOut["Aterm"] = Aterm_test;
  lOut["Xi"] = Xi_test;
  lOut["mW_tilde"] = mW_tilde;
  lOut["mW"] = mW;
  lOut["mMu"] = mMu;
  lOut["vNu"] = vNu;
  lOut["IndicatorGJR"] = IndicatorGJR;
  return lOut;
}


// [[Rcpp::export]]
List MixSim_DAMM(const List& Spec, const int& iT, const List& lP, const List& StartValues) {
  
  //const arma::vec& vNoise
  //Get the spec
  arma::vec vY(iT);
  arma::vec vState(iT);
  const int K = Spec["K"];
  const std::string Model = Spec["Model"];
  const std::string iDist = Spec["iDist"];
  const arma::vec vSigma2_start = StartValues["vSigma2_start"];
  const arma::vec vW_start = StartValues["vW_start"];
  
  const std::string iType = Spec["iType"];
  arma::mat IndicatorGJR(K, (iT));
  
  //Set double variables/vectors
  List tmp_Xi;
  
  //Paras W
  arma::vec wKappa = lP["wKappa"];
  arma::mat wA = lP["wA"];
  arma::mat wB = lP["wB"];
  
  //Paras Sigma_tilde
  arma::vec vKappa = lP["vKappa"];
  arma::vec vA = lP["vA"];
  arma::vec vB = lP["vB"];
  arma::vec vNu = lP["vNu"];
  arma::vec vPhi = lP["vPhi"];
  
  //Get matrix for time-varying parameters
  arma::mat mSigma2(K, (iT));
  arma::mat mSigma_tilde(K, (iT));
  arma::mat mW(K, (iT));
  arma::mat mW_tilde((K - 1), (iT));
  arma::mat tmp_Jac;
  arma::vec A_term;
  arma::vec vSigma(iT);
  
  //Set initial values
  mSigma2.col(0) = vSigma2_start;
  mSigma_tilde.col(0) = log(mSigma2.col(0))/2;
  mW.col(0) = vW_start;
  mW_tilde.col(0) = WUnmap(mW.col(0));
  
  //Set mMu
  arma::vec vMu = lP["vMu"];
  arma::mat mMu(K, (iT), fill::value(0));
  for(int j = 0; j<(K-1); j++) {
    mMu.row(j).fill(vMu(j));
  }
  
  arma::vec tmp_Mu = ((mW.col(0).rows(0,(K-2))).t()*vMu.rows(0,K-2))/mW((K-1),0);
  mMu((K-1),0) = -tmp_Mu(0);
  
  if(iType == "GJR") {
    for(int j = 0; j<K; j++) {
      if((vY(0) - mMu(j,0)) < 0) {
        IndicatorGJR(j,0) = 1;
      }
    }
  }
  
  //State sample
  NumericVector vK(K);
  
  for(int i = 0; i<K; i++) {
    vK(i) = (i + 1);
  }
  
  vState(0) = f_sample(vK, 1, 1, wrap(mW.col(0)));
  
  if(iDist == "std") {
    vY(0) = pow(mSigma2((vState(0) - 1),0), 0.5)*as<double>(rt(1, vNu( (vState(0) - 1) )))/sqrt(vNu( (vState(0) - 1) )/(vNu( (vState(0) - 1) ) - 2)) + mMu((vState(0) - 1),0);
  } else {
    vY(0) = pow(mSigma2((vState(0) - 1),0), 0.5)*Rf_rnorm(0.0, 1.0) + mMu((vState(0) - 1),0);
  }
  
  vSigma(0) = sqrt(accu(mW.col(0) % (mSigma2.col(0) + pow(mMu.col(0), 2)) ));
  
  for(int i=1;i<iT; i++) {
    
    tmp_Xi =  Xi_cal(vY(i-1), mW.col(i - 1), mMu.col(i-1), sqrt(mSigma2.col(i-1)), vNu, iDist);
    arma::vec tmp_WScore = tmp_Xi["WScore"];
    arma::vec tmp_SigmaXi = tmp_Xi["Xi"];
    
    tmp_Jac = WJac(mW_tilde.col(i-1));

    mW_tilde.col(i) = wKappa + wA*tmp_Jac.t()*tmp_WScore + wB*mW_tilde.col(i - 1);
    mW.col(i) = WMap(mW_tilde.col(i));
    
    A_term = A_cal(vY(i - 1), mMu.col(i-1), sqrt(mSigma2.col(i-1)), Spec, vNu);
    
    //Mu
    arma::vec tmp_Mu = ((mW.col(i).rows(0,(K-2))).t()*vMu.rows(0,K-2))/mW((K-1),i);
    mMu((K-1),i) = -tmp_Mu(0);
    
    if(iType == "GJR") {
      for(int j = 0; j<K; j++) {
        if((vY((i - 1)) - mMu(j,(i - 1))) < 0) {
          IndicatorGJR(j,(i-1)) = 1;
        }
      }
    }
    
    for(int j=0; j<K; j++) {
      mSigma_tilde(j,i) = vKappa(j) + vA(j)*tmp_SigmaXi(j)*A_term(j) + vB(j)*mSigma_tilde(j,i-1) + pow((vY((i - 1)) - mMu(j,(i - 1))), 2)*IndicatorGJR(j,(i-1))*vPhi(j)/mSigma2(j,i-1);
      
      if(mSigma_tilde(j,i) > 2.5) {
        mSigma_tilde(j,i) = 2.5;
      }
      
      if(mSigma_tilde(j,i) < -2 ) {
        mSigma_tilde(j,i) = -2;
      }
      
      mSigma2(j,i) = exp(2*mSigma_tilde(j,i));

    }
    
    vState(i) = f_sample(vK, 1, 1, wrap(mW.col(i)));
    if(iDist == "std") {
      vY(i) = pow(mSigma2((vState(i) - 1),i), 0.5)*Rf_rt(vNu( (vState(i) - 1) ))/sqrt(vNu( (vState(i) - 1) )/(vNu( (vState(i) - 1) ) - 2)) + mMu((vState(i) - 1),i);
    } else {
      vY(i) = pow(mSigma2((vState(i) - 1),i), 0.5)*Rf_rnorm(0.0, 1.0) + mMu((vState(i) - 1),i);  
    }
    
    vSigma(i) = sqrt(accu(mW.col(i) % (mSigma2.col(i) + pow(mMu.col(i), 2)) ));
    
  }
  
  List lOut;
  lOut["mSigma2"] = mSigma2;
  lOut["mSigma2_tilde"] = mSigma_tilde;
  lOut["vSigma"] = vSigma;
  lOut["mW_tilde"] = mW_tilde;
  lOut["mW"] = mW;
  lOut["mMu"] = mMu;
  lOut["vNu"] = vNu;
  lOut["vY"] = vY;
  lOut["vState"] = vState;
  return lOut;
}



