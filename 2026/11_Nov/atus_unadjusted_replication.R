# SWD November 2026 — unadjusted ATUS inference
# Produces estimates, changes, and BLS SDR confidence intervals from public raw data.
# Important: this script DOES NOT reproduce the historical composition-adjusted model
# or the historical predeclared selection gate; its exact specification was not supplied.

required <- c('readr','dplyr','tidyr','stringr','tibble')
missing <- required[!vapply(required, requireNamespace, logical(1), quietly=TRUE)]
if(length(missing)) stop('Install packages: ',paste(missing,collapse=', '))
library(readr); library(dplyr); library(tidyr); library(stringr); library(tibble)

base <- file.path('2026','11_Nov')
summary_file <- file.path(base,'atussum_0325.dat')
replicate_file <- file.path(base,'atuswgts_0325.dat')
for(f in c(summary_file,replicate_file)) if(!file.exists(f)) stop('Missing file: ',f)

s <- read_csv(summary_file, show_col_types=FALSE,
              col_types=cols(TUCASEID=col_character(),.default=col_double()))
fields <- c('TUCASEID','TESEX','TEAGE','TRSPPRES','TUYEAR','TUFNWGTP')
stopifnot(all(fields %in% names(s)))
hhcols <- grep('^t02',names(s),value=TRUE,ignore.case=TRUE)
if(!length(hhcols)) stop('No t02 household-activity columns in summary file')
if(anyNA(s[,hhcols])) stop('Missing household-activity values; investigate before summing')

s <- s %>% mutate(hh_min=rowSums(across(all_of(hhcols)))) %>%
 filter(between(TEAGE,25,64), TUYEAR != 2020,
        between(TUYEAR,2003,2007) | between(TUYEAR,2021,2025),
        TRSPPRES %in% c(1,3), TESEX %in% c(1,2)) %>%
 mutate(period=if_else(TUYEAR<=2007,'early','late'),
        group=if_else(TRSPPRES==1,'married','no_partner'),
        sex=if_else(TESEX==2,'women','men'))
if(anyDuplicated(s$TUCASEID)) stop('Duplicate respondent ID in summary subset')
if(anyNA(s$TUFNWGTP) || any(s$TUFNWGTP<=0)) stop('Invalid final weights')

# Read only the header first so that replicate-weight family is never guessed.
rep_header <- names(read_csv(replicate_file,n_max=0,show_col_types=FALSE))
if(!'TUCASEID' %in% rep_header) stop('TUCASEID missing in replicate file')
# The supplied BLS combined file can contain multiple weight families.
# Set this to the prefix for the 160 replicates corresponding to TUFNWGTP.
# Verify the naming against the downloaded BLS replicate-weight documentation.
replicate_prefix <- Sys.getenv('ATUS_REPLICATE_PREFIX',unset='')
if(!nzchar(replicate_prefix)) {
  cat('Available replicate weight fields (first 35):\n')
  print(head(setdiff(rep_header,'TUCASEID'),35))
  stop('Set ATUS_REPLICATE_PREFIX to the BLS-documented family for TUFNWGTP. Do not guess.')
}
# Match exact prefix followed by three digits (001..160) or 1..160.
possible <- rep_header[startsWith(toupper(rep_header), toupper(replicate_prefix))]
number <- substring(possible,nchar(replicate_prefix)+1)
keep <- grepl('^[0-9]{1,3}$',number) & suppressWarnings(as.integer(number) %in% 1:160)
rep_names <- possible[keep]
rep_index <- suppressWarnings(as.integer(substring(rep_names,nchar(replicate_prefix)+1)))
if(length(rep_names)!=160L || !setequal(rep_index,1:160)) stop('Expected exactly one 001..160 replicate family')
rep_names <- rep_names[order(rep_index)]

r <- read_csv(replicate_file,col_select=all_of(c('TUCASEID',rep_names)),
              col_types=cols(TUCASEID=col_character(),.default=col_double()),
              show_col_types=FALSE)
if(anyDuplicated(r$TUCASEID)) stop('Duplicate ID in replicate file')
x <- left_join(s,r,by='TUCASEID')
if(nrow(x)!=nrow(s) || anyNA(x[,rep_names])) stop('Missing replicate weights for analysis rows')
if(any(as.matrix(x[,rep_names]) < 0)) stop('Negative replicate weight')

# All four estimates in both periods, evaluated with one weight vector.
statistic <- function(w) {
  dat <- tibble(group=x$group, period=x$period, sex=x$sex,
                numerator=x$hh_min*w, weight=w) %>%
    group_by(group,period,sex) %>%
    summarise(value=sum(numerator)/sum(weight),.groups='drop')
  if(nrow(dat)!=8L || any(!is.finite(dat$value))) stop('Incomplete analysis groups')
  val <- function(g,p,z) dat$value[dat$group==g & dat$period==p & dat$sex==z]
  gap <- function(g,p) val(g,p,'women')-val(g,p,'men')
  mc <- gap('married','late')-gap('married','early')
  nc <- gap('no_partner','late')-gap('no_partner','early')
  c(married_change=mc,no_partner_change=nc,
    difference_in_changes=nc-mc,
    married_men_change=val('married','late','men')-val('married','early','men'),
    married_women_change=val('married','late','women')-val('married','early','women'))
}

theta0 <- statistic(x$TUFNWGTP)
rep_values <- vapply(rep_names,function(k) statistic(x[[k]]),numeric(length(theta0)))
# ATUS successive-difference replication, 160 replicates and factor 4/160.
se <- sqrt((4/160)*rowSums((rep_values-theta0)^2))
result <- tibble(estimate=names(theta0), value=as.numeric(theta0),
                 se=as.numeric(se), lower=value-1.96*se, upper=value+1.96*se)
print(result,n=Inf)
write_csv(result,file.path(base,'atus_unadjusted_replication_results.csv'))
# The known historical claim is difference_in_changes ~13.6, CI~[2.8,24.3].
# It is a comparison target, NOT a substituted result.
did <- result %>% filter(estimate=='difference_in_changes')
if(abs(did$value-13.6)>0.25) warning('Point estimate differs from historical result; inspect sample/definitions')
cat('Saved unadjusted replicate-weight results. Adjusted model and original selection gate not reproduced.\n')
