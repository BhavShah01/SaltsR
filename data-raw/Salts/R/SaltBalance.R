SaltBalance <- function(m_s, # m_s: Dry Sample Mass, m_s (g)
                       V_w, # V_w: Amount of water added to sample for IC, V_w (ml)
                       conc # conc: Raw Ion Chromotgraphy Data (IC) (mg L^(-1))
                       # chlorine: Chlorine (Cl-) Concentration, c_Cl
                       # nitrate: Nitrate (NO3-) Concentration, c_NO3
                       # sulfate: Sulfate (SO42-) Concentration, c_SO4
                       # sodium: Sodium (Na+) Concentration, c_Na
                       # potassium: Potassium (K+) Concentration, c_K
                       # calcium: Calcium (Ca2+) Concentration, c_Ca
                       # magnesium: Magnesium (Mg2+) Concentration, c_Mg
                       ) {
  
  source('reference_data.R', local=TRUE)
  
  # Check input data
  
  if (m_s < 0) stop("'m_s' must be >= 0")
  if (V_w < 0) stop("'V_w' must be >= 0")
  if(any(conc < 0)) stop("'concentrations cannot be negative")
  
  if (!is.data.frame(conc)) stop("'conc must be a data frame")
  
  if (identical.check(names(conc),ion.names) == F) stop("'conc must have 7 named elements
                                                         corresponding to the required ions")
  
  # WEIGHT FRACTIONS w (kg/kg) [Equation 1]
  
  w <- (conc*V_w/m_s)/(1000^2)
  
  w_tot <- sum(w)
  
  # AMOUNT OF SUBSTANCE e (mEq/kg) [Equation 2]
  # Note: values are converted to mEq for readability
  
  e. <- (w*z_abs/M)
  
  # AMOUNT OF ANIONS e_ani (mEq/kg)
  e_ani <- sum(e.[ani])
  
  # AMOUNT OF CATIONS e_cat (mEq/kg)
  e_cat <- sum(e.[cat])
  
  # INITIAL BALANCE delta_E (mEq/kg) [Equation 3]
  delta_e <- abs(e_ani-e_cat)
  
  # PATHWAY SELECTION
  
  pathway <- if (delta_e <= max(e_ani,e_cat)*0.02 | e_ani > e_cat) {
    'I'} else {
      'II'
    }
  
  # PATHWAY I
  # Balance all ions equally
  
  
  if (pathway == 'I') {
    e_adj <- e. * (e_cat + e_ani) / (2 * c(rep(e_ani, 3),
                                           rep(e_cat, 4))) # [Equation 4]
  }
  
  # PATHWAY II
  # Excess is assumed to relate to the least soluble salt
  
  if (pathway == 'II') {
    e_adj <- e.
    
    # store the balance at each step for later calculation
    delta_e.Ca.Mg.Na.K <- data.frame(matrix(ncol=4,nrow=2,
                                            dimnames=list(NULL, cat.sort)))
    
    # sequentially balance cations in specific order [Equation 5]
    for(adj in cat.sort) { # Equations 5a-5d
      e_adj[adj] <- if(e_adj[adj] - delta_e >= 0) {e_adj[adj] - delta_e} else {0}
      delta_e.2 <- abs(sum(e_adj[ani]) - sum(e_adj[cat]))
      delta_e.Ca.Mg.Na.K[adj] <- c(delta_e, delta_e.2)
      delta_e <- delta_e.2
    }
    
    cat_adj <- names(which(colSums(delta_e.Ca.Mg.Na.K) != 0))
  }
  
  # GYPSUM ADJUSTMENT [Equations 6 and 7]
  
  # Amount of Calcium or Sulfate that limits CaSO4 production, mEq/kg [Equation 6]
  e_Ca.adj <- e_adj$calcium
  
  e_lim.CaSO4 <- min(e_adj[gypsum])
  e_adj[gypsum] <- e_adj[gypsum]-e_lim.CaSO4 #[Equation 7]
  
  # GENERATE ECOS INPUTS, Balanced mole fractions [Equation 8]
  
  x_adj <- (e_adj/z_abs)/sum(e_adj/z_abs)
  
  # BALANCED WEIGHT FRACTION [Equation 10]
  
  w_f <- e_adj*M/z_abs
  
  w_f.tot <- sum(w_f)
  
  # EVALUATION OF THE BALANCED GYPSUM CONTENT [Equation 11]
  
  # Gypsum content (-)
  w_gypsum <- 0.5*e_lim.CaSO4*(sum(M[gypsum]))
  
  # Total weight fraction of balanced ion content (excluding gypsum) (-)
  w_tot.adj <- (w_tot-w_f.tot)-w_gypsum
  
  
  # Total balanced weight fraction of Sodium and Potassium (-)
  ions <- c('sodium', 'potassium')
  w_Na.K.adj <- if(pathway == 'II') {
    abs(sum(w_f[ions])-sum(w[ions]))
    } else { 'Pathway I' }
  
  # Adjusted Calcium Weight Fraction (-)
  ion <- 'calcium'
  w_Ca.adj <- if(pathway == 'II') {
    abs(e_Ca.adj-e.[ion])*M[ion]/z_abs[ion]
    } else {'Pathway I'}
  
  # hypothetical Carbonate Content related to Magnesium, Sodium, and Potassium 
  w_CO3.h <- if(pathway == 'II') {
    sum(delta_e.Ca.Mg.Na.K[2,2:4])*M_CO3/z_abs.CO3
    } else {'Pathway I'}
  
  # rm(M, z_abs, ani, cat, cat.sort, gypsum, M_CO3, z_abs.CO3)
  
  return(list(
   pathway = pathway,
   cat.adj = if(pathway == 'I') {'All'} else {paste0(cat_adj, collapse=', ')},
   x_adj = x_adj,
   w_f = w_f,
   w_eval = data.frame(
     w_f.tot = w_f.tot,
     w_gypsum = w_gypsum,
     w_tot.adj = w_tot.adj,
     w_Na.K.adj = w_Na.K.adj,
     w_Ca.adj = unname(w_Ca.adj),
     w_CO3.h = w_CO3.h
   )))
}
