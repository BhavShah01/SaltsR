# Molar Mass M (kg/mol)

M <- data.frame(
  chlorine = 0.0354527,
  nitrate = 0.0620049,
  sulfate = 0.096064,
  sodium = 0.022989768,
  potassium = 0.0390983,
  calcium = 0.040078,
  magnesium = 0.024305
)

M_CO3 <- 0.060009

# Absolute charge (-)

z_abs <- data.frame(
  chlorine = 1,
  nitrate = 1,
  sulfate = 2,
  sodium = 1,
  potassium = 1,
  calcium = 2,
  magnesium = 2
)

z_abs.CO3 <- 2

# Names

ion.names <-   c("chlorine",
                 "nitrate",
                 "sulfate",
                 "sodium",
                 "potassium",
                 "calcium",
                 "magnesium")

# Ion categories

ani <- c('chlorine', 'nitrate',	'sulfate')
cat <- c('sodium', 'potassium',	'calcium',	'magnesium')
gypsum <- c('calcium', 'sulfate')
cat.sort <- c('calcium', 'magnesium', 'sodium', 'potassium')

# Misc. functions

identical.check <- function(a,b) identical(sort(a),sort(b))