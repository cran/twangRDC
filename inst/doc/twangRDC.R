## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
  library(twangRDC) 
  data(nola_south)

## -----------------------------------------------------------------------------
  # factors need to be coded as such
  nola_south$metarea = as.factor(nola_south$metarea)
  nola_south$c00_age12 = as.factor(nola_south$c00_age12)
  nola_south$c00_race = as.factor(nola_south$c00_race)

## -----------------------------------------------------------------------------
  # only consider Orleans parish 
  nola_only = subset(nola_south , metarea==556)

  # keep only 10 tracts for computational speed
  to.keep = unique(nola_only$tract_id_str)[1:10]
  nola_only = nola_only[ nola_only$tract_id_str %in% to.keep, ]

## ---- results='hide' , warning=FALSE------------------------------------------
  # set the model parameters
  params = list(eta = .01 , max_depth = 10 , subsample = 1 , 
                max_delta_step=0 , gamma=0 , lambda=0 , alpha=0, 
                min_child_weight=5 , objective = "binary:logistic")

  # fit the xgboost model
  res.pik = ps.xgb(sim_pik ~ c00_age12 + c00_race + c00_sex , 
               strata="tract_id_str",
               data=nola_only,
               params=params,
               max.steps=10,
               iters.per.step=500,
               min.iter=500,
               id.var="id",
               linkage = TRUE)

## ---- fig.show='hold',fig.cap = "Convergence of gradient boosted model for linkage failure."----
plot(res.pik)

## ---- eval=FALSE--------------------------------------------------------------
#    bal.table(res.pik)

## ---- echo=FALSE, results='asis'----------------------------------------------
  knitr::kable(bal.table(res.pik),digits=4)

## ---- eval=FALSE--------------------------------------------------------------
#    bal.table(res.pik , type='strata' , include.var=TRUE , n=3 , decreasing = T)

## ---- echo=FALSE, results='asis'----------------------------------------------
  knitr::kable(bal.table(res.pik , type='strata' , include.var=TRUE , n=3 , decreasing = T),digits=4)

## ---- eval=FALSE--------------------------------------------------------------
#    # extract weights
#    w = get.weights(res.pik)
#  
#    # merge weights into data -- use data.table because its merge is faster than base R
#    dta=merge(dta , w , by='id' , all=TRUE)

## ---- results='hide' , warning=FALSE------------------------------------------
  # set the model parameters
  params = list(eta = .1 , max_depth = 10 , subsample = 1 , 
                max_delta_step=0 , gamma=0 , lambda=0 , alpha=0, 
                min_child_weight=5 , objective = "binary:logistic")

  # fit the xgboost model
  res.ps = ps.xgb(nola_rec ~ c00_age12 + c00_race + c00_sex + concdis + res_stab + imm_conc , 
               data=nola_south,
               params=params,
               max.steps=10,
               iters.per.step=500,
               min.iter=500,
               id.var="id",
               linkage = FALSE)

## ---- fig.show='hold',fig.cap = "Convergence of gradient boosted model for comparison group construction."----
plot(res.ps)

## ---- eval=FALSE--------------------------------------------------------------
#    bal.table(res.ps)

## ---- echo=FALSE, results='asis'----------------------------------------------
  knitr::kable(bal.table(res.ps),digits=4)

