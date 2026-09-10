#' Create the Stock Synthesis MG_parms Parameter Table
#'
#' Creates an empty data frame with the correct row names and column
#' structure for the \code{inputs$ctl$MG_parms} table used by Stock
#' Synthesis.
#'
#' The number and order of rows in the Stock Synthesis \code{MG_parms}
#' table depend on the number of growth patterns, areas, and growth
#' patterns assigned to each area. This function generates the parameter
#' table structure automatically based on these specifications.
#'
#' Biological parameters are created separately for females and males
#' for each growth pattern. Sex-specific maturity and fecundity parameters
#' are included for females, but are omitted for males.
#'
#' When multiple areas are specified, recruitment-distribution parameters
#' are also added according to the number of growth patterns and the
#' number of growth patterns assigned per area.
#'
#' The function currently creates the parameter structure only and does
#' not populate the parameter values. Movement parameters are not currently
#' included.
#'
#' @param N_growth_patterns Integer specifying the total number of growth
#'   patterns. Defaults to 1.
#' @param n_areas Integer specifying the number of areas in the model.
#'   Defaults to 1.
#' @param GP_per_area Integer specifying the number of growth patterns
#'   assigned to each area. Defaults to 1.
#'
#' @return A data frame containing the row names and column structure
#'   required for the Stock Synthesis \code{MG_parms} table. The rows
#'   correspond to biological, recruitment-distribution, cohort-growth,
#'   and female-fraction parameters. The 14 columns are:
#'   \describe{
#'     \item{LO}{Lower bound for the parameter.}
#'     \item{HI}{Upper bound for the parameter.}
#'     \item{INIT}{Initial parameter value.}
#'     \item{PRIOR}{Prior value.}
#'     \item{PR_SD}{Prior standard deviation.}
#'     \item{PR_type}{Prior distribution type.}
#'     \item{PHASE}{Estimation phase.}
#'     \item{env_var&link}{Environmental variable and link specification.}
#'     \item{dev_link}{Link function for deviations.}
#'     \item{dev_minyr}{First year for deviations.}
#'     \item{dev_maxyr}{Last year for deviations.}
#'     \item{dev_PH}{Estimation phase for deviations.}
#'     \item{Block}{Block specification.}
#'     \item{Block_Fxn}{Block function specification.}
#'   }
#'
#' @details
#' The biological parameters generated for each growth pattern include:
#' \itemize{
#'   \item Natural mortality (\code{NatM_p_1})
#'   \item Length at \code{Amin} (\code{L_at_Amin})
#'   \item Length at \code{Amax} (\code{L_at_Amax})
#'   \item Von Bertalanffy growth coefficient (\code{VonBert_K})
#'   \item Coefficient of variation for young fish (\code{CV_young})
#'   \item Coefficient of variation for old fish (\code{CV_old})
#'   \item Length-weight parameter 1 (\code{Wtlen_1})
#'   \item Length-weight parameter 2 (\code{Wtlen_2})
#'   \item Length at 50 percent maturity (\code{Mat50})
#'   \item Maturity slope (\code{Mat_slope})
#'   \item Fecundity parameter alpha (\code{Eggs_alpha})
#'   \item Fecundity parameter beta (\code{Eggs_beta})
#' }
#'
#' Maturity and fecundity parameters are generated only for females,
#' because these parameters are not included in the male parameter rows
#' created by this function.
#'
#' If more than one area is specified, recruitment-distribution
#' parameters (\code{RecrDist}) are added. The exact parameter structure
#' depends on the number of growth patterns and the value of
#' \code{GP_per_area}.
#'
#' The final rows contain \code{CohortGrowDev}, followed by
#' \code{FracFemale_GP_#} for each growth pattern. These parameters
#' describe cohort growth deviations and the fraction of females
#' associated with each growth pattern, respectively.
#'
#' If \code{GP_per_area} is greater than \code{N_growth_patterns}, the
#' function issues a message because the specified number of growth
#' patterns per area is inconsistent with the total number of growth
#' patterns.
#'
#' The function returns an empty parameter table. Parameter bounds,
#' initial values, priors, estimation phases, and other model-specific
#' settings must be populated separately before the table is supplied
#' to Stock Synthesis.
#'
#' Movement parameters are currently not included in the generated table
#' and will need to be added separately if movement among areas is
#' incorporated into the Stock Synthesis model.
#'
#' @examples
#' # One growth pattern and one area
#' MG_parms <- create_MG_Parms_df()
#'
#' # Two growth patterns and one area
#' MG_parms <- create_MG_Parms_df(
#'   N_growth_patterns = 2
#' )
#'
#' # Two growth patterns and three areas
#' MG_parms <- create_MG_Parms_df(
#'   N_growth_patterns = 2,
#'   n_areas = 3,
#'   GP_per_area = 1
#' )
#'
#' # Inspect the parameter names
#' rownames(MG_parms)
#'
#' # Inspect the MG_parms table structure
#' str(MG_parms)
#'
#' @author
#' Jonathan Smart, Australia
#'
#' @export
create_MG_Parms_df <- function(N_growth_patterns = 1, n_areas = 1, GP_per_area = 1){
  
  if(GP_per_area > N_growth_patterns) message("More GP per area specified than number of GP requested. Check outputs")
  
  Biopars <-c("NatM_p_1_SEX_GP_#",
              "L_at_Amin_SEX_GP_#",
              "L_at_Amax_SEX_GP_#",
              "VonBert_K_SEX_GP_#",
              "CV_young_SEX_GP_#",
              "CV_old_SEX_GP_#",
              "Wtlen_1_SEX_GP_#",
              "Wtlen_2_SEX_GP_#",
              "Mat50%_SEX_GP_#",
              "Mat_slope_SEX_GP_#",
              "Eggs_alpha_SEX_GP_#",
              "Eggs_beta_SEX_GP_#")
  
  PARROWS <- NULL
  for(sex in c("Fem", "Mal")){
    for(gp in 1:N_growth_patterns){
      tmpa <- gsub(pattern = "SEX", replacement = sex, Biopars)
      tmpb <- gsub(pattern = "#", replacement = gp, tmpa)
      
      if(sex == "Mal") tmpb <- tmpb[!grepl("Mat50%|Mat_slope|Eggs",tmpb)]
      PARROWS <- c(PARROWS, tmpb)
    }
  }
  
  if(n_areas > 1){
    if(GP_per_area == 1 & N_growth_patterns == 1 ){
      
      for(area in 1:n_areas){
        tmpc <- paste0("RecrDist_GP_1_area_",area,"_month_1")
        PARROWS <- c(PARROWS, tmpc)
      }
    }else if(GP_per_area == 1){
      for(area in 1:n_areas){
        tmpc <- paste0("RecrDist_GP_",area,"_area_",area,"_month_1")
        PARROWS <- c(PARROWS, tmpc)
      }
    }else {
      for(gp in 1:N_growth_patterns){
        for(area in 1:n_areas){
          tmpc <- paste0("RecrDist_GP_",gp,"_area_",area,"_month_1")
          PARROWS <- c(PARROWS, tmpc)
        }
      }
    }
  }
  PARROWS <- c(PARROWS, "CohortGrowDev")
  for(gp in 1:N_growth_patterns){
    tmpd <- paste0("FracFemale_GP_",gp)
    PARROWS <- c(PARROWS, tmpd)
  }
  
  PARCOLS <- c("LO","HI", "INIT", "PRIOR" , "PR_SD", "PR_type", "PHASE",
               "env_var&link", "dev_link", "dev_minyr", "dev_maxyr", 
               "dev_PH", "Block", "Block_Fxn")
  
  df <-as.data.frame( matrix(ncol = 14,
                             nrow = length(PARROWS),
                             dimnames = list(PARROWS,
                                             PARCOLS)))
  
  return(df)
  
}