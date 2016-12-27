deltaRatio <- function(ag, hco3) {
  
  ratio = ( ag - 12 ) / ( 24 - hco3 )
  
  class = ifelse(ratio < 0.4, "Normal", 
                 ifelse(ratio > 0.4 & ratio <= 0.8, "Normal to High (Possible Metabolic Acidosis)", 
                        ifelse(ratio > 0.8, "Possible Kidney Failure",""
                               )
                        )
                 )
  
  data = data.frame("Anion Gap Delta Ratio" = ratio, "Anion Gap Risk Class" = class)
  
  return(data)
}

glucoseFasting <- function(glucose) {
  
  class = ifelse(glucose >= 70 & glucose <= 100, 'Normal', 
                 ifelse(glucose < 70, "Hypoglycemia",
                        ifelse(glucose > 100, "Hyperglycemia"
                               )
                        )
                 )
  
  data = data.frame("Glucose Value" = glucose, "Glucose Risk Class" = class)
  
  return(data)
}

agRatio <- function(ratio) {
  
  class = ifelse(ratio >= 1.7 & ratio <= 2.2, "Normal", 
                 ifelse(ratio > 2.2, "Hypothyroidism, High Protein/Carb diet, Excess Cortisol",
                        ifelse(ratio < 1.7, "Liver Dysfunction", ""
                               )
                        )
                 )
  
   data = data.frame( "Albumin/Globulin Ratio" = ratio, "A/G Risk Class" = class)
   
   return(data)
}

cholCheck <- function(ldl, hdl) {
  
  ldlClass = ifelse(ldl > 0 & ldl <= 130, "Normal",
                    ifelse(ldl > 130 & ldl <= 160, "Borderline High",
                           ifelse(ldl > 160 & ldl <= 190, "High",
                                  ifelse(ldl > 190, "Very High", ""
                                  )
                           )
                    )
  )
  
  hdlClass = ifelse(hdl > 40 & hdl <= 60, "Normal",
                    ifelse(hdl > 60, "Better",
                           ifelse(hdl < 40, "Poor", ""
                           )
                    )
  )
  
  total = hdl + ldl
  
  totalClass = ifelse(total < 200, "Normal",
                      ifelse(total > 200 & total <= 240, "Borderline High",
                             ifelse(total > 240, "High",""
                             )
                      )
  )
  
  
  data = data.frame("LDL Value" = ldl, 
                    "LDL Risk Class" = ldlClass,
                    "HDL Value" = hdl,
                    "HDL Risk Class" = hdlClass,
                    "Total" = total,
                    "Total Risk Class" = totalClass)
  
  return(data)
}

trigCheck <- function(trig, hdl) {
  
  tghdlRatio = trig/hdl
  
  tghdlClass = ifelse(tghdlRatio < 2, "Normal",
                      ifelse(tghdlRatio > 2 & tghdlRatio <= 4, "High",
                             ifelse(tghdlRatio > 4, " Very High", ""
                             )
                      )
  )
  
  data = data.frame("Triglyceride Value" = trig,
                    "Triglyceride/HDL Ratio" = tghdlRatio,
                    "Triglyceride/HDL Ratio Risk Class" = tghdlClass)
  
  data
}

compMeta <- function(anionGap,hco3,glucose,albgluRatio) {
  
  delta = deltaRatio(anionGap,hco3)
  
  Glucose = glucoseFasting(glucose)
  
  agratio = agRatio(albgluRatio)
  
  data = data.frame(delta, Glucose, agratio)
  
  data
}

lipidPanel <- function(hdl,ldl, trig) {
  
  cholesterol = cholCheck(ldl,hdl)
  
  triglycerides = trigCheck(trig,hdl)
  
  data = data.frame(cholesterol, triglycerides)
  
  data
  
}

drsOrders <- function(anionGap,hco3,glucose,albgluRatio,hdl,ldl, trig) {
  
  lipid = lipidPanel(hdl,ldl, trig)
  
  compmeta = compMeta(anionGap,hco3,glucose,albgluRatio)
  
  data = data.frame(lipid, compmeta)
  
  data
}

example <- drsOrders(13,27,90,2.1,39,213,166)

print(example)