setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source('SaltBalance.R')

options(stringsAsFactors=FALSE)

library(data.table)

# For a single sample -----------------------------------------------------

out <- SaltBalance(
  m_s = 0.0333,
  V_w = 20,
  conc = data.frame(
    chlorine = 5.485,    # c_Cl
    nitrate = 16.185,    # c_NO3
    sulfate = 461.49,    # c_SO4
    sodium = 3.278,      # c_Na
    potassium = 1.9,     # c_K
    calcium = 234.5,     # c_Ca
    magnesium = 0.9896   # c_Mg)
  )
)

out

# Apply to every row of the input data (outputs a list) -------------------

dat.a <- read.csv('Example-Dataset_20-samples_Charge-Balance_calculations.csv',
                  stringsAsFactors = F)

out <- lapply(1:nrow(dat.a), function(ind) {
  m_s <- dat.a$m_s[ind]
  V_w <- dat.a$V_w[ind]
  
  conc <- data.frame(
    chlorine = dat.a$c_Cl[ind],
    nitrate = dat.a$c_NO3[ind],
    sulfate = dat.a$c_SO4[ind],
    sodium = dat.a$c_Na[ind],
    potassium = dat.a$c_K[ind],
    calcium = dat.a$c_Ca[ind],
    magnesium = dat.a$c_Mg[ind]
  )
  return(SaltBalance(m_s, V_w, conc))
})

# to get the output into a table format
tabled <- cbind(sample.id=dat.a$sample.id, rbindlist(lapply(out, as.data.frame)))

write.csv(tabled, 'sample_output.csv')
