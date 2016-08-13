# get legend from an existing ggplot object
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# numeric to english for inline text
# uses english package
#
# x numeric vector
# maxint maximum integer below which english is used
toeng <- function(x, maxint = 10){

  library(english)
  
  if(!is.numeric(x)) stop('x must be numeric')

  nms <- names(x)  
  xchr <- which(x < maxint) 
  xeng <- as.character(english(x[xchr]))
  x <- as.character(x)
  x[xchr] <- xeng
  x <- as.list(x)
  names(x) <- nms
  
  return(x)
  
}

# formatting of values in S expressions
form_fun <- function(x, rnd_val = 2, dig_val = 2, nsm_val = 2, ...) {
  format(round(x, rnd_val), digits = dig_val, nsmall = nsm_val, ...)
}

######
# scientific notation from R to LaTeX
# x is numeric to convert
# pow is minimum exponent (positive or negative) to use for notation
# digits numeric for rounding
# showdollar logical if output is enclosed in dollar for latex
# from https://dankelley.github.io/r/2015/03/22/scinot.html
scinot <- function(x, pow = 3, digits=2, showDollar=TRUE)
{
  x <- as.numeric(x)
  sign <- ""
  if (x < 0) {
      sign <- "-"
      x <- -x
  }
  exponent <- floor(log10(x))
  if (exponent & abs(exponent) > pow) {
      xx <- round(x / 10^exponent, digits=digits)
      e <- paste("\\times 10^{", as.integer(exponent), "}", sep="")
  } else {
      xx <- round(x, digits=digits)
      e <- ""
  }
  if (showDollar) paste("$", sign, xx, e, "$", sep="")
  else paste(sign, xx, e, sep="")
}

# alternative list structure for parameter categories
parcats2 <- function(as_df = FALSE){
  
  Optics = list(
    cats = 'Optics',
    shrt = c('Kw_1', 
      'Kcdom_1', 
      'Kspm_1', 
      'Kchla_1',
      'astar490_1',
      'aw490_1',
      'astarOMA_1',
      'astarOMZ_1',
      'astarOMR_1',
      'astarOMbC_1',
      'sink CDOM_1'
      ),
    lngs = c(
      'Kw: AOP, light attenuation due to water',
      'Kcdom: AOP, light attenuation due to CDOM',
      'Kspm: AOP, light attenuation due to SPM', 
      'Kchla: AOP, light attenuation due to chla ',
      'astar490: Chla specific absorption at 490 nm',
      'aw490: seawater absorption at 490 nm',
      'astarOMA: OM_A specific absorption at 490 nm',
      'astarOMZ: OM_Z specific absorption at 490 nm',
      'astarOMR: OM_R specific absorption at 490 nm',
      'astarOMBC: OM_BC specific absorption at 490 nm',
      'sink CDOM: sinking rate'
      ),
    vals = c(
      0.146,
      0.001,
      0.029,
      0.024,
      0.0375,
      0.015,
      0.1,
      0.1,
      0.1,
      0.1,
      0
    )
  )
  
  Temperature = list(
    cats = 'Temperature',
    shrt = c(
      'Tref(nospA+nospZ)_1',
      'Tref(nospA+nospZ)_2',
      'Tref(nospA+nospZ)_3',
      'Tref(nospA+nospZ)_4',
      'Tref(nospA+nospZ)_5',
      'Tref(nospA+nospZ)_6',
      'Tref(nospA+nospZ)_7',
      'Tref(nospA+nospZ)_8',
      'KTg1(nospA+nospZ)_1',
      'KTg1(nospA+nospZ)_2',
      'KTg1(nospA+nospZ)_3',
      'KTg1(nospA+nospZ)_4',
      'KTg1(nospA+nospZ)_5',
      'KTg1(nospA+nospZ)_6',
      'KTg1(nospA+nospZ)_7',
      'KTg1(nospA+nospZ)_8',
      'KTg2(nospA+nospZ)_1',
      'KTg2(nospA+nospZ)_2',
      'KTg2(nospA+nospZ)_3',
      'KTg2(nospA+nospZ)_4',
      'KTg2(nospA+nospZ)_5',
      'KTg2(nospA+nospZ)_6',
      'KTg2(nospA+nospZ)_7',
      'KTg2(nospA+nospZ)_8',
      'Ea_R(nospA+nospZ)_1',
      'Ea_R(nospA+nospZ)_2',
      'Ea_R(nospA+nospZ)_3',
      'Ea_R(nospA+nospZ)_4',
      'Ea_R(nospA+nospZ)_5',
      'Ea_R(nospA+nospZ)_6',
      'Ea_R(nospA+nospZ)_7',
      'Ea_R(nospA+nospZ)_8'
      ),
    lngs = c(
      'Tref(nospA+nospZ): Optimum temperature for growth(C)',
      'Tref(nospA+nospZ): Optimum temperature for growth(C)',
      'Tref(nospA+nospZ): Optimum temperature for growth(C)',
      'Tref(nospA+nospZ): Optimum temperature for growth(C)',
      'Tref(nospA+nospZ): Optimum temperature for growth(C)',
      'Tref(nospA+nospZ): Optimum temperature for growth(C)',
      'Tref(nospA+nospZ): Optimum temperature for growth(C)',
      'Tref(nospA+nospZ): Optimum temperature for growth(C)',
      'KTg1(nospA+nospZ): Effect of T below Topt(C^2)',
      'KTg1(nospA+nospZ): Effect of T below Topt(C^2)',
      'KTg1(nospA+nospZ): Effect of T below Topt(C^2)',
      'KTg1(nospA+nospZ): Effect of T below Topt(C^2)',
      'KTg1(nospA+nospZ): Effect of T below Topt(C^2)',
      'KTg1(nospA+nospZ): Effect of T below Topt(C^2)',
      'KTg1(nospA+nospZ): Effect of T below Topt(C^2)',
      'KTg1(nospA+nospZ): Effect of T below Topt(C^2)',
      'KTg2(nospA+nospZ): Effect of T above Topt(C^2)',
      'KTg2(nospA+nospZ): Effect of T above Topt(C^2)',
      'KTg2(nospA+nospZ): Effect of T above Topt(C^2)',
      'KTg2(nospA+nospZ): Effect of T above Topt(C^2)',
      'KTg2(nospA+nospZ): Effect of T above Topt(C^2)',
      'KTg2(nospA+nospZ): Effect of T above Topt(C^2)',
      'KTg2(nospA+nospZ): Effect of T above Topt(C^2)',
      'KTg2(nospA+nospZ): Effect of T above Topt(C^2)',
      'Ea_R(nospA+nospZ): Slope of Arrhenius plot',
      'Ea_R(nospA+nospZ): Slope of Arrhenius plot',
      'Ea_R(nospA+nospZ): Slope of Arrhenius plot',
      'Ea_R(nospA+nospZ): Slope of Arrhenius plot',
      'Ea_R(nospA+nospZ): Slope of Arrhenius plot',
      'Ea_R(nospA+nospZ): Slope of Arrhenius plot',
      'Ea_R(nospA+nospZ): Slope of Arrhenius plot',
      'Ea_R(nospA+nospZ): Slope of Arrhenius plot'
    ),
    vals = c(
      22, 
      22, 
      22, 
      22, 
      22, 
      22, 
      22,
      22,
      0.0035,
      0.0035,
      0.0035,
      0.0035,
      0.0035,
      0.0035,
      0.0035,
      0.0035,
      0.001,
      0.001,
      0.001,
      0.001,
      0.001,
      0.001,
      0.001,
      0.001,
      10000,
      10000,
      10000,
      10000,
      10000,
      10000,
      10000, 
      10000
      )
  )
  
  Phytoplankton = list(
    cats = 'Phytoplankton',
    shrt = c(
      'ediblevector(Z1)_1',
      'ediblevector(Z1)_2',
      'ediblevector(Z1)_3',
      'ediblevector(Z1)_4',
      'ediblevector(Z1)_5',
      'ediblevector(Z1)_6',
      'ediblevector(Z2)_1',
      'ediblevector(Z2)_2',
      'ediblevector(Z2)_3',
      'ediblevector(Z2)_4',
      'ediblevector(Z2)_5',
      'ediblevector(Z2)_6',
      'umax_1',
      'umax_2',
      'umax_3',
      'umax_4',
      'umax_5',
      'umax_6',
      'alpha_1',
      'alpha_2',
      'alpha_3',
      'alpha_4',
      'alpha_5',
      'alpha_6',
      'beta_1',
      'beta_2',
      'beta_3',
      'beta_4',
      'beta_5',
      'beta_6',
      'respg_1',
      'respg_2',
      'respg_3',
      'respg_4',
      'respg_5',
      'respg_6',
      'respb_1',
      'respb_2',
      'respb_3',
      'respb_4',
      'respb_5',
      'respb_6',
      'QminN_1',
      'QminN_2',
      'QminN_3',
      'QminN_4',
      'QminN_5',
      'QminN_6',
      'QminP_1',
      'QminP_2',
      'QminP_3',
      'QminP_4',
      'QminP_5',
      'QminP_6',
      'QmaxN_1',
      'QmaxN_2',
      'QmaxN_3',
      'QmaxN_4',
      'QmaxN_5',
      'QmaxN_6',
      'QmaxP_1',
      'QmaxP_2',
      'QmaxP_3',
      'QmaxP_4',
      'QmaxP_5',
      'QmaxP_6',
      'Kn_1',
      'Kn_2',
      'Kn_3',
      'Kn_4',
      'Kn_5',
      'Kn_6',
      'Kp_1',
      'Kp_2',
      'Kp_3',
      'Kp_4',
      'Kp_5',
      'Kp_6',
      'Ksi_1',
      'Ksi_2',
      'Ksi_3',
      'Ksi_4',
      'Ksi_5',
      'Ksi_6',
      'KQn_1',
      'KQn_2',
      'KQn_3',
      'KQn_4',
      'KQn_5',
      'KQn_6',
      'KQp_1',
      'KQp_2',
      'KQp_3',
      'KQp_4',
      'KQp_5',
      'KQp_6',
      'nfQs_1',
      'nfQs_2',
      'nfQs_3',
      'nfQs_4',
      'nfQs_5',
      'nfQs_6',
      'vmaxN_1',
      'vmaxN_2',
      'vmaxN_3',
      'vmaxN_4',
      'vmaxN_5',
      'vmaxN_6',
      'vmaxP_1',
      'vmaxP_2',
      'vmaxP_3',
      'vmaxP_4',
      'vmaxP_5',
      'vmaxP_6',
      'vmaxSi_1',
      'vmaxSi_2',
      'vmaxSi_3',
      'vmaxSi_4',
      'vmaxSi_5',
      'vmaxSi_6',
      'aN_1',
      'aN_2',
      'aN_3',
      'aN_4',
      'aN_5',
      'aN_6',
      'volcell_1',
      'volcell_2',
      'volcell_3',
      'volcell_4',
      'volcell_5',
      'volcell_6',
      'Qc_1',
      'Qc_2',
      'Qc_3',
      'Qc_4',
      'Qc_5',
      'Qc_6',
      'Athresh_1',
      'Athresh_2',
      'Athresh_3',
      'Athresh_4',
      'Athresh_5',
      'Athresh_6',
      'sink A_1',
      'sink A_2',
      'sink A_3',
      'sink A_4',
      'sink A_5',
      'sink A_6',
      'mA_1',
      'mA_2',
      'mA_3',
      'mA_4',
      'mA_5',
      'mA_6'
      ),
    lngs = c(
      "ediblevector(Z1): edibility vector for Z1", 
      "ediblevector(Z1): edibility vector for Z1", 
      "ediblevector(Z1): edibility vector for Z1", 
      "ediblevector(Z1): edibility vector for Z1", 
      "ediblevector(Z1): edibility vector for Z1", 
      "ediblevector(Z1): edibility vector for Z1", 
      "ediblevector(Z2): edibility vector for Z2", 
      "ediblevector(Z2): edibility vector for Z2", 
      "ediblevector(Z2): edibility vector for Z2", 
      "ediblevector(Z2): edibility vector for Z2", 
      "ediblevector(Z2): edibility vector for Z2", 
      "ediblevector(Z2): edibility vector for Z2", 
      "umax: maximum growth rate", 
      "umax: maximum growth rate",
      "umax: maximum growth rate", 
      "umax: maximum growth rate", 
      "umax: maximum growth rate", 
      "umax: maximum growth rate", 
      "alpha: initial slope of the photosynthesis-irradiance relationship", 
      "alpha: initial slope of the photosynthesis-irradiance relationship", 
      "alpha: initial slope of the photosynthesis-irradiance relationship", 
      "alpha: initial slope of the photosynthesis-irradiance relationship", 
      "alpha: initial slope of the photosynthesis-irradiance relationship", 
      "alpha: initial slope of the photosynthesis-irradiance relationship", 
      "beta: photoinhibition constant", 
      "beta: photoinhibition constant", 
      "beta: photoinhibition constant",
      "beta: photoinhibition constant", 
      "beta: photoinhibition constant",
      "beta: photoinhibition constant", 
      "respg: phytoplankton growth respiration coefficient",
      "respg: phytoplankton growth respiration coefficient", 
      "respg: phytoplankton growth respiration coefficient",
      "respg: phytoplankton growth respiration coefficient", 
      "respg: phytoplankton growth respiration coefficient",
      "respg: phytoplankton growth respiration coefficient", 
      "respb: phytoplankton basal respiration coefficient",
      "respb: phytoplankton basal respiration coefficient", 
      "respb: phytoplankton basal respiration coefficient",
      "respb: phytoplankton basal respiration coefficient", 
      "respb: phytoplankton basal respiration coefficient",
      "respb: phytoplankton basal respiration coefficient", 
      "QminN: minimum N cell-quota",
      "QminN: minimum N cell-quota", 
      "QminN: minimum N cell-quota",
      "QminN: minimum N cell-quota", 
      "QminN: minimum N cell-quota",
      "QminN: minimum N cell-quota", 
      "QminP: minimum P cell-quota",
      "QminP: minimum P cell-quota", 
      "QminP: minimum P cell-quota",
      "QminP: minimum P cell-quota", 
      "QminP: minimum P cell-quota",
      "QminP: minimum P cell-quota", 
      "QmaxN: maximum N cell-quota",
      "QmaxN: maximum N cell-quota", 
      "QmaxN: maximum N cell-quota", 
      "QmaxN: maximum N cell-quota", 
      "QmaxN: maximum N cell-quota", 
      "QmaxN: maximum N cell-quota", 
      "QmaxP: maximum P cell-quota",
      "QmaxP: maximum P cell-quota", 
      "QmaxP: maximum P cell-quota", 
      "QmaxP: maximum P cell-quota", 
      "QmaxP: maximum P cell-quota",
      "QmaxP: maximum P cell-quota", 
      "Kn: half-saturation constant for N",
      "Kn: half-saturation constant for N", 
      "Kn: half-saturation constant for N",
      "Kn: half-saturation constant for N", 
      "Kn: half-saturation constant for N",
      "Kn: half-saturation constant for N", 
      "Kp: half-saturation constant for P", 
      "Kp: half-saturation constant for P", 
      "Kp: half-saturation constant for P",
      "Kp: half-saturation constant for P", 
      "Kp: half-saturation constant for P", 
      "Kp: half-saturation constant for P", 
      "Ksi: half-saturation constant for Si uptake", 
      "Ksi: half-saturation constant for Si uptake", 
      "Ksi: half-saturation constant for Si uptake",
      "Ksi: half-saturation constant for Si uptake", 
      "Ksi: half-saturation constant for Si uptake", 
      "Ksi: half-saturation constant for Si uptake", 
      "KQn: Qn constant for Flynn nutrient dependent growth model", 
      "KQn: Qn constant for Flynn nutrient dependent growth model", 
      "KQn: Qn constant for Flynn nutrient dependent growth model", 
      "KQn: Qn constant for Flynn nutrient dependent growth model", 
      "KQn: Qn constant for Flynn nutrient dependent growth model", 
      "KQn: Qn constant for Flynn nutrient dependent growth model", 
      "KQp: Qp constant for Flynn nutrient dependent growth model", 
      "KQp: Qp constant for Flynn nutrient dependent growth model", 
      "KQp: Qp constant for Flynn nutrient dependent growth model", 
      "KQp: Qp constant for Flynn nutrient dependent growth model", 
      "KQp: Qp constant for Flynn nutrient dependent growth model", 
      "KQp: Qp constant for Flynn nutrient dependent growth model", 
      "nfQs: exponent for Geider nutrient uptake model",
      "nfQs: exponent for Geider nutrient uptake model", 
      "nfQs: exponent for Geider nutrient uptake model",
      "nfQs: exponent for Geider nutrient uptake model", 
      "nfQs: exponent for Geider nutrient uptake model",
      "nfQs: exponent for Geider nutrient uptake model", 
      "vmaxN: N-uptake rate measured at umax", 
      "vmaxN: N-uptake rate measured at umax", 
      "vmaxN: N-uptake rate measured at umax", 
      "vmaxN: N-uptake rate measured at umax", 
      "vmaxN: N-uptake rate measured at umax",
      "vmaxN: N-uptake rate measured at umax", 
      "vmaxP: P-uptake rate measured at umax", 
      "vmaxP: P-uptake rate measured at umax", 
      "vmaxP: P-uptake rate measured at umax",
      "vmaxP: P-uptake rate measured at umax", 
      "vmaxP: P-uptake rate measured at umax",
      "vmaxP: P-uptake rate measured at umax", 
      "vmaxSi: Si-uptake rate measured at umax",
      "vmaxSi: Si-uptake rate measured at umax", 
      "vmaxSi: Si-uptake rate measured at umax",
      "vmaxSi: Si-uptake rate measured at umax", 
      "vmaxSi: Si-uptake rate measured at umax",
      "vmaxSi: Si-uptake rate measured at umax", 
      "aN: coefficient for non-limiting nutrient",
      "aN: coefficient for non-limiting nutrient", 
      "aN: coefficient for non-limiting nutrient",
      "aN: coefficient for non-limiting nutrient", 
      "aN: coefficient for non-limiting nutrient",
      "aN: coefficient for non-limiting nutrient", 
      "volcell: phytoplankton volume/cell", 
      "volcell: phytoplankton volume/cell", 
      "volcell: phytoplankton volume/cell", 
      "volcell: phytoplankton volume/cell", 
      "volcell: phytoplankton volume/cell",
      "volcell: phytoplankton volume/cell", 
      "Qc: phytoplankton carbon/cell",
      "Qc: phytoplankton carbon/cell", 
      "Qc: phytoplankton carbon/cell",
      "Qc: phytoplankton carbon/cell", 
      "Qc: phytoplankton carbon/cell", 
      "Qc: phytoplankton carbon/cell", 
      "Athresh: Phytoplankton threshold for grazing, is multiplied by VOLcell", 
      "Athresh: Phytoplankton threshold for grazing, is multiplied by VOLcell", 
      "Athresh: Phytoplankton threshold for grazing, is multiplied by VOLcell", 
      "Athresh: Phytoplankton threshold for grazing, is multiplied by VOLcell", 
      "Athresh: Phytoplankton threshold for grazing, is multiplied by VOLcell", 
      "Athresh: Phytoplankton threshold for grazing, is multiplied by VOLcell", 
      "sink A: sinking rate of phytoplankton cells",
      "sink A: sinking rate of phytoplankton cells", 
      "sink A: sinking rate of phytoplankton cells",
      "sink A: sinking rate of phytoplankton cells", 
      "sink A: sinking rate of phytoplankton cells",
      "sink A: sinking rate of phytoplankton cells", 
      "mA: mortality coefficient", 
      "mA: mortality coefficient",
      "mA: mortality coefficient", 
      "mA: mortality coefficient", 
      "mA: mortality coefficient", 
      "mA: mortality coefficient"
      ),
    vals = c(
      0.25,
      0.25,
      0.25, 
      0.25,
      0.25,
      0.25, 
      0.25,
      0.25, 
      0.25,
      0.25, 
      0.25, 
      0.25, 
      0.41,
      0.41,
      0.41,
      0.41,
      0.41, 
      0.41,
      8.42e-17, 
      8.42e-17, 
      8.42e-17, 
      8.42e-17, 
      8.42e-17, 
      8.42e-17, 
      1.1e-18, 
      1.1e-18, 
      1.1e-18, 
      1.1e-18, 
      1.1e-18, 
      1.1e-18, 
      0.1, 
      0.1, 
      0.1, 
      0.1, 
      0.1, 
      0.1, 
      0.02,
      0.02,
      0.02,
      0.02,
      0.02,
      0.02,
      6.08e-09,
      6.08e-09,
      6.08e-09,
      6.08e-09,
      6.08e-09,
      6.08e-09,
      6.19e-10,
      6.19e-10, 
      6.19e-10,
      6.19e-10,
      6.19e-10,
      6.19e-10,
      2.04e-07,
      2.04e-07,
      2.04e-07,
      2.04e-07,
      2.04e-07,
      2.04e-07,
      1.28e-08,
      1.28e-08,
      1.28e-08,
      1.28e-08,
      1.28e-08,
      1.28e-08,
      4.51,
      4.51,
      4.51,
      4.51,
      4.51,
      4.51,
      2.86,
      2.86,
      2.86,
      2.86,
      2.86,
      2.86,
      4.51,
      4.51,
      4.51,
      4.51,
      4.51,
      4.51,
      5,
      5,
      5,
      5,
      5,
      5,
      0.2,
      0.2,
      0.2,
      0.2,
      0.2,
      0.2,
      1,
      1,
      1,
      1,
      1,
      1,
      4.10e-08,
      4.10e-08,
      4.10e-08,
      4.10e-08,
      4.10e-08,
      4.10e-08,
      2.68e-08,
      2.68e-08,
      2.68e-08,
      2.68e-08,
      2.68e-08,
      2.68e-08,
      4.10e-08,
      4.10e-08,
      4.10e-08,
      4.10e-08,
      4.10e-08,
      4.10e-08,
      1,
      1,
      1,
      1,
      1,
      1,
      33693,
      33693,
      33693,
      33693,
      33693,
      33693,
      1.35e-06,
      1.35e-06,
      1.35e-06,
      1.35e-06,
      1.35e-06,
      1.35e-06,
      1.721e8,
      1.721e8,
      1.721e8, 
      1.721e8,
      1.721e8,
      1.721e8,
      1.49,
      1.49,
      1.49,
      1.49,
      1.49,
      1.49,
      0.1,
      0.1,
      0.1,
      0.1,
      0.1,
      0.1
      )
  )
  
  Zooplankton = list(
    cats = 'Zooplankton',
    shrt = c(
      'Zeffic_1',
      'Zeffic_2',
      'Zslop_1',
      'Zslop_2',
      'Zvolcell_1',
      'Zvolcell_2',
      'ZQc_1', 
      'ZQc_2',
      'ZQn_1',
      'ZQn_2',
      'ZQp_1',
      'ZQp_2',
      'ZKa_1',
      'ZKa_2',
      'Zrespg_1',
      'Zrespg_2',
      'Zrespb_1',
      'Zrespb_2',
      'Zumax_1',
      'Zumax_2',
      'Zm_1',
      'Zm_2'
      ), 
    lngs = c(
      'Zeffic: assimilation efficiency as a fraction of ingestion',
      'Zeffic: assimilation efficiency as a fraction of ingestion',
      'Zslop: proportion of grazed phytoplankton lost to sloppy feeding',
      'Zslop: proportion of grazed phytoplankton lost to sloppy feeding',
      'Zvolcell: zooplankton volume/individual',
      'Zvolcell: zooplankton volume/individual',
      'ZQc: zooplankton carbon/individual',
      'ZQc: zooplankton carbon/individual',
      'ZQn: zooplankton nitrogen/individual',
      'ZQn: zooplankton nitrogen/individual',
      'ZQp: zooplankton phosphorus/individual',
      'ZQp: zooplankton phosphorus/individual',
      'ZKa: half saturation coefficient for grazing',
      'ZKa: half saturation coefficient for grazing',
      'Zrespg: Zooplankton growth-dependent respiration factor',
      'Zrespg: Zooplankton growth-dependent respiration factor',
      'Zrespb: Zooplankton biomass-dependent respiration factor',
      'Zrespb: Zooplankton biomass-dependent respiration factor',
      'Zumax: maximum growth rate of zooplankton',
      'Zumax: maximum growth rate of zooplankton',
      'Zm: Zooplankton mortality constant for quadratic mortality',
      'Zm: Zooplankton mortality constant for quadratic mortality'
      ), 
    vals = c(
      0.4,
      0.4,
      0.25,
      0.25,
      2.98e7,
      2.98e7,
      3.13e-4,
      3.13e-4,
      6.95e-05,
      6.95e-05,
      3.77e-06,
      3.77e-06,
      1.12e12,
      1.12e12,
      0.2,
      0.2,
      0.1,
      0.1,
      9.45e7,
      9.45e7,
      0.00072,
      0.00072
      )
  )
  
  `Organic Matter` = list(
    cats = 'Organic Matter',
    shrt = c(
      'KG1_1',
      'KG2_1',
      'KG1_R_1',
      'KG2_R_1',
      'KG1_BC_1',
      'KG2_BC_1',
      'KNH4_1',
      'nitmax_1',
      'KO2_1',
      'KstarO2_1',
      'KNO3_1',
      'pCO2_1',
      'stoich_x1R_1',
      'stoich_y1R_1',
      'stoich_x2R_1',
      'stoich_y2R_1',
      'stoich_x1BC_1',
      'stoich_y1BC_1',
      'stoich_x2BC_1',
      'stoich_y2BC_1',
      'sink OM1_A_1',
      'sink OM2_A_1',
      'sink OM1_Z_1',
      'sink OM2_Z_1',
      'sink OM1_R_1',
      'sink OM2_R_1',
      'sink OM1_BC_1',
      'sink OM2_BC_1',
      'KGcdom_1',
      'CF_SPM_1'
      ), 
    lngs = c(
      'KG1: turnover rate for OM1_A and OM1_G',	
      'KG2: turnover rate for OM2_A and OM2_G',
      'KG1_R: OM1 turnover rate for riverine',
      'KG2_R: OM2 turnover rate for riverine',
      'KG1_BC: OM1 turnover rate for initial and bc',
      'KG2_BC: OM2 turnover rate for initial and bc',
      'KNH4: NH4 rate constant for nitrification',
      'nitmax: maximum rate of nitrification per day',
      'KO2: half-saturation concentration for O2 utilization',
      'KstarO2: O2 concentration that inhibits denitrification',
      'KNO3: half-saturation concentration for NO3 used in denitrification',
      'pCO2: atmospheric CO2',
      'stoich_x1R:  C:P stoichiometry of OM1_R',
      'stoich_y1R:  N:P stoichiometry of OM1_R',
      'stoich_x2R:  C:P stoichiometry of OM2_R',
      'stoich_y2R:  N:P stoichiometry of OM2_R',
      'stoich_x1BC: C:P stoichiometry of OM1_BC',
      'stoich_y1BC: N:P stoichiometry of OM1_BC',
      'stoich_x2BC: C:P stoichiometry of OM2_BC',
      'stoich_y2BC: N:P stoichiometry of OM2_BC',
      'sink OM1_A:  sinking rate',
      'sink OM2_A:  sinking rate',
      'sink OM1_Z:  sinking rate',
      'sink OM2_Z:  sinking rate',
      'sink OM1_R:  sinking rate',
      'sink OM2_R:  sinking rate',
      'sink OM1_BC: sinking rate',
      'sink OM2_BC: sinking rate',
      'KGcdom: decay rate of CDOM, 1/day',
      'CF_SPM: conversion factor for river OM to river SPM'
      ), 
    vals = c(
      50,
      50,
      11,
      3.7,
      1,
      1,
      1,
      0.52,
      10,
      10,
      10,
      380,
      51,
      4.5,
      700,
      50,
      106,
      16,
      106,
      16,
      10,
      0,
      10,
      0,
      10,
      0,
      10,
      0,
      0.01,
      0.018
      )
  )
  
  # list
  out <- list(Optics = Optics, Temperature = Temperature, Phytoplankton = Phytoplankton, Zooplankton = Zooplankton, `Organic Matter` = `Organic Matter`)
  
  # return as data frame if T
  if(as_df){
    
    out <- lapply(out, data.frame)
    out <- do.call('rbind', out)
    row.names(out) <- 1:nrow(out)
    
    return(out)
  }
  
  return(out)
  
}  

######
# convert  parameter names to latex format
#
# parin chr vector of short names to convert
# frm chr string indicating format of output, tex or exp for latex or expression
par_txt <- function(parin, frm = 'tex'){

  library(dplyr)
  
  # sanity check
  if(!frm %in% c('tex', 'exp'))
    stop('frm argument must be "tex" or "exp"')

  parin <- as.character(parin) 
  
  # all parameters and names
  cats <- parcats2(as_df = TRUE)[, c('cats', 'shrt')]

  # get which row the parameter is in
  sels <- which(cats$shrt %in% parin)
  if(length(sels) != length(unique(parin))) stop('parin not completely matched in shrt')
  
  # split the names by category
  splits <- cats[sels, ] %>% 
    .[match(parin, .$shrt), ] # this is important to make sure the output order matches with input
  
  # gsub the shrt names differnet by category
  subs <- apply(splits, 1,  function(x){

    # 1-6 are phytos, 7-8 are zoops (changed to 1-2)
    if('Temperature' %in% x['cats']){

      x['shrt'] <- gsub('_([1-6])$', '_p\\1', x['shrt'])
      x['shrt'] <- gsub('_[7]$', '_z1', x['shrt']) 
      x['shrt'] <- gsub('_[8]$', '_z2', x['shrt'])  
      
    }
      
    # add p to subscript
    if('Phytoplankton' %in% x['cats']){
      
      x['shrt'] <- gsub('_([1-9])$', '_p\\1', x['shrt']) 
      
    }

    # add z to subscript
    if('Zooplankton' %in% x['cats']){
     
      x['shrt'] <- gsub('_([1-9])$', '_z\\1', x['shrt']) 
      
    }
    
    # remove subscript
    if(any(c('Optics', 'Organic Matter') %in% x['cats'])){
     
      x['shrt'] <- gsub('_[1-9]$', '', x['shrt'])
       
    }
        
    return(x)
    
  }) %>% 
  t %>% 
  data.frame(., stringsAsFactors = FALSE) %>% 
  .$shrt

  # convert output format for tex
  if(frm == 'tex'){
    
    out <- gsub('_([pz][1-9])$', '$_{\\1}$', subs)
    out <- paste0('\\textit{', out, '}')
      
  }
  
  # convert output format as expressions for R
  if(frm == 'exp'){
  
    out <- gsub('_([pz][1-9])$', '[italic(\\1)]', subs)
    out <- paste0('italic(', out, ')')
    out <- gsub('\\((.*)\\s(.*)\\)', '("\\1 \\2")', out)
    out <- parse(text = as.expression(out))
    
  }
    
  return(out)
  
}

######
# function for sensitivity tables, no phyto, zoop groupings
senstab1 <- function(categ, tablab, tabsize = 'normalsize'){
  
  library(Hmisc)
  library(dplyr)
  library(tidyr)
  
  load(file = 'data/sens_ests_cat.RData')
  load(file = 'data/sens_ests.RData')
  
  labs <- parcats2()[[categ]] %>% 
    data.frame %>% 
    rename(Parameter = shrt)
  
  totab <- filter(sens_ests_cat, Category %in% categ) %>% 
    select(Parameter) %>% 
    left_join(., sens_ests$sens, by = 'Parameter') %>% 
    left_join(., labs, by = 'Parameter') %>% 
    select(lngs, Parameter, vals, L1, L2) %>% 
    arrange(-L1) %>% 
    mutate(
      Parameter = par_txt(Parameter),
      lngs = gsub('^[a-z,A-Z,0-9]*:\\s', '', lngs), 
      lngs = gsub('_', '', lngs), 
      vals = sapply(vals, scinot),
      L1 = sapply(L1, scinot), 
      L2 = sapply(L2, scinot)
    ) %>% 
    rename(
      Description = lngs, 
      Value = vals
    )

  # final table formatting
  Description <- totab$Description
  totab <- totab[,-1]
  cap.val<- paste('Sensitivities of \\ac{do} to perturbation of', tolower(categ), 'parameters.  Sensitivities are based on a 50\\% increase from the default parameter value, where $L1$ and $L2$ summarize differences in model output from the default (see \\cref{l1,l2}).  Parameters that did not affect \\ac{do} are not shown.')

  latex( 
    totab,
    file = '',
    rowlabel = 'Description',
    caption = cap.val,
    caption.loc = 'top',
    rowname = Description,
    size = tabsize,
    label = paste0('tab:', tablab)
    )
  
}
  
######
# function for sensitivity tables, with phyto, zoop groupings
senstab2 <- function(categ, tablab, tabsize = 'normalsize', sub = NULL, sub.txt = NULL){

  library(Hmisc)
  library(dplyr)
  library(tidyr)
  
  load(file = 'data/sens_ests_cat.RData')
  load(file = 'data/sens_ests.RData')

  labs <- parcats2()[[categ]] %>% 
    data.frame %>% 
    rename(Parameter = shrt)
  
  totab <- filter(sens_ests_cat, Category %in% categ) %>% 
    select(Parameter) %>% 
    left_join(., sens_ests$sens, by = 'Parameter') %>% 
    left_join(., labs, by = 'Parameter') %>% 
    select(Parameter, lngs, vals, L1, L2)
  
  if(!is.null(sub))
    totab <- filter(totab, L1 > sub)
  
  totab <- mutate(totab,
      lngs = as.character(lngs),
      lngs = gsub('^.*:\\s', '', lngs), 
      lngs = tolower(gsub('_', '', lngs)),
      Parameter = par_txt(Parameter)
    ) %>% 
    arrange(lngs, -L1) %>% 
    mutate(
      vals = sapply(vals, scinot),
      L1 = sapply(L1, scinot), 
      L2 = sapply(L2, scinot)
    ) %>% 
    rename(
      Description = lngs,
      Value = vals
    ) %>% 
    select(Description, Parameter, Value, L1, L2)

  # final table formatting
  Description <- totab$Description
  Parameter <- totab$Parameter
  totab <- totab[,-c(1, 2)]
  cap.val<- paste('Sensitivities of \\ac{do} to perturbation of', tolower(categ), 'parameters.  Sensitivities are based on a 50\\% increase from the default parameter value, where $L1$ and $L2$ summarize differences in model output from the default (see \\cref{l1,l2}).  Parameters that did not affect \\ac{do} are not shown.  Subscripts show the phytoplankton or zooplankton group that applies for the parameter.')
  
  if(!is.null(sub.txt))
    cap.val <- paste(cap.val, sub.txt)
    
  latex( 
    totab,
    file = '',
    rowlabel = 'Description, Parameter',
    caption = cap.val,
    caption.loc = 'top',
    rgroup = unique(Description),
    n.rgroup = as.numeric(table(Description)),
    rgroupTexCmd = NULL,
    rowname = Parameter,
    size = tabsize,
    label = paste0('tab:', tablab)
    )

}
