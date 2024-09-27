#### This file defines the R settings that should be used with this project

# Try to keep line numbers for debugging
###### nolint start: undesirable_functions(options)
options( keep.source = TRUE )
options( show.error.locations = TRUE )
###### nolint end: undesirable_functions(options)

# decode DATABRICKS_TOKEN if it's not already set and we have an encrypted copy

if (Sys.getenv("DATABRICKS_TOKEN") == "") {
  # DATABRICKS_TOKEN isn't set yet ... check if we have encrypted copy (and key)

  if (Sys.getenv("PARAM_DBTA") != "" && Sys.getenv("PARAM_DBTB") != "") {
    # We have an encrypted copy, so let's decrypt

    ###### nolint start: undesirable_functions(Sys.setenv)
    Sys.setenv(
      "DATABRICKS_TOKEN"=
        system('echo -n $PARAM_DBTB |base64 --decode|openssl enc -d -base64 -aes-256-ctr -nopad -nosalt -pass env:PARAM_DBTA -pbkdf2 ', intern = TRUE)
    )
    ###### nolint end: undesirable_functions(Sys.setenv)

  }
}

# decode DATABRICKS_HOST if it's not already set and we have an encrypted copy

if (Sys.getenv("DATABRICKS_HOST") == "") {
  # DATABRICKS_TOKEN isn't set yet ... check if we have encrypted copy (and key)

  if (Sys.getenv("PARAM_DBHA") != "" && Sys.getenv("PARAM_DBHB") != "") {
    # We have an encrypted copy, so let's decrypt

    ###### nolint start: undesirable_functions(Sys.setenv)
    Sys.setenv(
      "DATABRICKS_HOST"=
        system('echo -n $PARAM_DBHB |base64 --decode|openssl enc -d -base64 -aes-256-ctr -nopad -nosalt -pass env:PARAM_DBHA -pbkdf2 ', intern = TRUE)
    )
    ###### nolint end: undesirable_functions(Sys.setenv)

  }
}

# set main_path to the working directory if we haven't already
if (Sys.getenv("PARAM_MAIN_PATH") == "") {
  ###### nolint start: undesirable_functions(Sys.setenv)
  Sys.setenv("PARAM_MAIN_PATH" = normalizePath(getwd()))
  ###### nolint end: undesirable_functions(Sys.setenv)
}

main_path <- Sys.getenv("PARAM_MAIN_PATH")

# The tests path is relative to the main path
manual_test_path <- file.path(main_path, "tests", "manual")

### specify a particular R snapshot date

## specify what snapshot we're using

desired_package_date <- "2022-01-13"
library_description <- "default"
# if on sas grid library_root should be "/sasfiles/rsrv/method/r/libraries/"
# if on databricks library_root should be "/mnt/central-config/r-libraries/"

## To determine if we are on sas grid server, check the
##   server name against sas grid server patterns
## for example
##   lpwsasd01.npd.com
##   lpscsasp05.npd.com
##   lpwsasgsubd02.npd.com
##   lpscsasgsubp02.npd.com
## Regular expression logic in plain English:
##   starts with either 'lpw' or 'lpsc'
##   followed by 'sas'
##   optionally followed by 'gsub'
##   followed by either 'p' or 'd'
##   followed by two numeric digits
##   followed by '.npd.com'
if (Sys.info()["nodename"] %in% c("JumpBoxSrv")) {
  this_is_databricks <- FALSE
  library_root <- "C:/central-config/r-libraries/"
} else if (grepl(
  "^(lpw|lpsc)sas(gsub)?(p|d)[[:digit:]][[:digit:]].npd.com$",
  Sys.info()["nodename"])
) {
  this_is_databricks <- FALSE
  library_root <- "/sasfiles/rsrv/method/r/libraries/"
} else {
  this_is_databricks <- TRUE
  library_root <- "/mnt/central-config/r-libraries/"
}

# R version
r_major <- R.Version()$major
r_minor <- substr(R.Version()$minor, 1, 1)

## library location meeting these criteria
library_to_use <- paste0(library_root, r_major, ".", r_minor, "_", desired_package_date, "_", library_description)
# for example this might resolve to
#   something like: "/sasfiles/rsrv/method/r/libraries/3.4_2022-01-26_default"
# you can also manually overwrite this,
#   something like:  library_to_use<-"/sasfiles/rsrv/method/r/libraries/3.4_2020-07-01"

if (this_is_databricks) {
  desired_libpaths <- c(library_to_use, "/databricks/spark/R/lib")
} else {
  desired_libpaths <- c(library_to_use)
}

### Use this R library (and only this R library)

###### nolint start: undesirable_functions(.libPaths)
if ( !identical( .libPaths(),desired_libpaths) ) {

  unlockBinding(".lib.loc", environment(.libPaths))
  assign('.lib.loc',
         desired_libpaths,
         envir = environment(.libPaths)
  )
}
###### nolint end: undesirable_functions(.libPaths)

###### nolint start: undesirable_functions(Sys.setenv)
Sys.setenv(
  "R_LIBS_USER" = desired_libpaths[1],
  "R_LIBS_SITE"= desired_libpaths[1],
  "R_LIBS" = desired_libpaths[1]
)
###### nolint end: undesirable_functions(Sys.setenv)

### In case we decide to install an additional package, make it compatible with the libraries already selected

mran_repository <- paste0("https://mran.revolutionanalytics.com/snapshot/", desired_package_date)

desired_repositories <- c(MRAN = mran_repository) # TODO: decide if we need anything other than mran

## Set repository
###### nolint start: undesirable_functions(options)
local({
  options(repos = desired_repositories)
})
###### nolint end: undesirable_functions(options)

### desired memory limit

## change this if you need more memory to complete your tasks

default_memory_fraction <- 0.8

if (Sys.getenv("PARAM_MEM_FRACTION") == "") {
  memory_fraction <- default_memory_fraction
} else {
  memory_fraction <- as.numeric( Sys.getenv("PARAM_MEM_FRACTION") )
}

if (is.null(memory_fraction)) {
  memory_fraction <- default_memory_fraction
}

if (is.na(memory_fraction)) {
  memory_fraction <- default_memory_fraction
}

if (memory_fraction <= 0) {
  memory_fraction <- default_memory_fraction
}

if (memory_fraction >= 1) {
  memory_fraction <- default_memory_fraction
}

memory_total_system <- as.numeric(memuse::Sys.meminfo()[[1]])
memory_current_available <- as.numeric(memuse::Sys.meminfo()[[2]])

desired_memory_limit <- memory_fraction * memory_total_system

mem_limit <- try(unix::rlimit_as(desired_memory_limit), silent = TRUE)
if (!("try-error" %in% class(mem_limit))) {

  memory_limit_r_allowed <- mem_limit[[1]]
  memory_limit_r_allowed_pretty <- memory_limit_r_allowed
  class(memory_limit_r_allowed_pretty) <- "object_size"
  memory_total_system_pretty <- memory_total_system
  class(memory_total_system_pretty) <- "object_size"
  cat(paste0("\n", "Memory limits on this session set with `unix`", "\n", "\n"))
  cat(paste0("Memory for total system: ",  format(memory_total_system_pretty, units = "auto"), "\n"))
  cat(paste0("Max memory available to R: ", format(memory_limit_r_allowed_pretty, units = "auto"), "\n"))
  cat("\n")

} else {
  message("error trying to set memory limits")
}

if (mem_limit[[1]] > memory_current_available) {
  memory_current_available_pretty <- memory_current_available
  class(memory_current_available) <- "object_size"
  cat(paste0("Note: memory allocation above ", format(memory_current_available, units = "auto"), "\n"))
  cat(paste0("  may require utilizing swap space", "\n"))
  cat("\n")
}

rm(library_description)
rm(library_root)
rm(library_to_use)
rm(desired_libpaths)
rm(desired_memory_limit)
rm(desired_package_date)
rm(desired_repositories)
rm(mran_repository)
rm(r_major)
rm(r_minor)
rm(memory_fraction)
rm(memory_current_available)
rm(default_memory_fraction)
rm(mem_limit)
rm(memory_limit_r_allowed_pretty)
rm(memory_total_system_pretty)
rm(memory_current_available_pretty)



### Change default to q()/quit() to save = "no" without changing
### available functionality

## https://twitter.com/kearneymw/status/1032988340998627328

###### nolint start: camel_case
quit <- function(save = "no", status = 0, runLast = TRUE) {
  .Internal(quit(save, status, runLast))
}
###### nolint end: camel_case

unlockBinding("quit", .BaseNamespaceEnv)
assign("quit", quit, envir = baseenv())
lockBinding("quit", .BaseNamespaceEnv)

unlockBinding("q", .BaseNamespaceEnv)
assign("q", quit, envir = baseenv())
lockBinding("q", .BaseNamespaceEnv)

rm(quit)

###### nolint start: camel_case

## If RStudio session starts with different libpath, change it back
if (Sys.getenv("RSTUDIO") == "1") {
  ## Establishes a delayed running of the code after
  ## RStudio's startup procedure occurs.

  setHook("rstudio.sessionInit", function(newSession) {
    if (newSession){
      if( !identical( .libPaths(),desired_libpaths) ) {
        unlockBinding(".lib.loc", environment(.libPaths))
        assign('.lib.loc',
               desired_libpaths,
               envir = environment(.libPaths)
        )
      }
    }
  }, action = "append")
}
###### nolint end: camel_case

# ---- Adding code to specify server type

if (!this_is_databricks && .Platform$OS.type != "windows") {
  ## infer server type if not set manually
  # determine if on dev/qc/uat server or prod server
  if (!exists("on_prem_server_type")) on_prem_server_type <- NULL

  if (!is.null(on_prem_server_type) && !(on_prem_server_type %in% c("uat", "prod"))) {
    stop("invalid value specified for on_prem_server_type")
  }

  if (is.null(on_prem_server_type)) {
    home_dir <- try(system("realpath $HOME", intern = TRUE))
    if (grepl("uat", home_dir) | grepl("dev", home_dir)) {
      on_prem_server_type <- "uat"
    } else if (grepl("prod", home_dir)) {
      on_prem_server_type <- "prod"
    } else{
      stop("cannot infer server_type and not specified in parameters")
    }
  }
  home_dir <- NULL
  rm(home_dir)
}

###### nolint start: undesirable_function(Sys.setenv)
if (Sys.getenv("PARAM_CURRENT_ENVIRONMENT") == "") {
  if (Sys.getenv("RSTUDIO") == "1") {
    if (this_is_databricks) {
      Sys.setenv("PARAM_CURRENT_ENVIRONMENT"="LOCAL_DATABRICKS")
    } else {
      Sys.setenv("PARAM_CURRENT_ENVIRONMENT"="LOCAL_ONPREM")
    }
  }
}

execution_environment <- Sys.getenv("PARAM_CURRENT_ENVIRONMENT")

if (Sys.getenv("PARAM_JOB_ID") == "") {
  Sys.setenv("PARAM_JOB_ID" = "manual_run")
}

job_id <- Sys.getenv("PARAM_JOB_ID")

if (Sys.getenv("DATABRICKS_TOKEN") == "") {
  if (Sys.getenv("PARAM_DB_TOKEN") != "") {
    Sys.setenv("DATABRICKS_TOKEN" = Sys.getenv("PARAM_DB_TOKEN"))
  }
}
###### nolint end: undesirable_function(Sys.setenv)

necessary_environment_params <- c(
  "trace_path",
  "logging_path",
  "input_path",
  "output_path",
  "run_control_path",
  "large_supports_path",
  "large_test_files_path",
  "shared_temp_path"
)

env_specs <- utils::read.csv(
  file.path(main_path, "config", "env_specs.csv"),
  colClasses = "character"
)

environment_params <- env_specs[env_specs$environment == execution_environment & env_specs$type == "parameters", ]

env_specs <- NULL
rm(env_specs)

load_env_parameter <- function(df, param_name) {

  testthat::expect_warning({
    temp_param_value <- normalizePath(
      glue::glue(
        df[df$name == param_name, "value"]
      )
    )
    warning("No such file or directory") # convoluted way to make warning optional
    }
    ,
    regexp = "No such file or directory"
  )

  testthat::expect_equal(length(temp_param_value), 1) # check that 1 and only 1
  assign(param_name, temp_param_value, envir = .GlobalEnv)

}

for (this_param in necessary_environment_params) {
  load_env_parameter(df = environment_params, param_name = this_param)
}

# clean up global environment
load_env_parameter <- NULL
rm(load_env_parameter)
necessary_environment_params <- NULL
rm(necessary_environment_params)
this_param <- NULL
rm(this_param)

###### nolint start: undesirable_function(Sys.setenv)
if (Sys.getenv("PARAM_WORKING_DIR") == "") {
  Sys.setenv("PARAM_WORKING_DIR" = trace_path)
  if(!file.exists(trace_path)) dir.create(trace_path,recursive=TRUE)
}

if(Sys.getenv("PARAM_CHECK_RESULTS")==""){
  Sys.setenv("PARAM_CHECK_RESULTS"=file.path(logging_path,"check_results.txt"))
}

current_working_directory <- Sys.getenv("PARAM_WORKING_DIR")
setwd(current_working_directory)

if(Sys.getenv("PARAM_ENABLE_PROFILING")=="1"){
  utils::Rprof(line.profiling=TRUE)
}
###### nolint end: undesirable_function(Sys.setenv)

### Special on error behavior so that we save a file that we can debug if needed

###### nolint start: undesirable_function(options)
options(error = function () {
  utils::Rprof(NULL)
  cat(paste0("\n\n", paste(rep("=", 20), collapse = "")))
  cat("\nSaving `last.dump` due to error\n")

  utils::dump.frames()

  if (!interactive()) {
    if (Sys.getenv("PARAM_WORKING_DIR")!="") {
      save.image(file = file.path(Sys.getenv("PARAM_WORKING_DIR"),paste0(basename(getwd()),"_",format(Sys.time(), "%Y_%m_%d_%H_%M_%OS3"),"_last.dump.rda")))
      sink(file=file.path(Sys.getenv("PARAM_WORKING_DIR"),paste0(basename(getwd()),"_traceback_",format(Sys.time(), "%Y_%m_%d_%H_%M_%OS3"),".txt")))
      if (Sys.getenv("PARAM_ENABLE_PROFILING")=="1") {
        saveRDS(utils::summaryRprof(lines="hide"),file.path(Sys.getenv("PARAM_WORKING_DIR"),paste0(basename(getwd()),"_",format(Sys.time(), "%Y_%m_%d_%H_%M_%OS3"),"_profile_summary_functions.rds")))
        saveRDS(utils::summaryRprof(lines="show"),file.path(Sys.getenv("PARAM_WORKING_DIR"),paste0(basename(getwd()),"_",format(Sys.time(), "%Y_%m_%d_%H_%M_%OS3"),"_profile_summary_lines.rds")))
        saveRDS(utils::summaryRprof(lines="both"),file.path(Sys.getenv("PARAM_WORKING_DIR"),paste0(basename(getwd()),"_",format(Sys.time(), "%Y_%m_%d_%H_%M_%OS3"),"_profile_summary_both.rds")))
      }
    } else {
      save.image(file = paste0(basename(getwd()),"_",format(Sys.time(), "%Y_%m_%d_%H_%M_%OS3"),"_last.dump.rda"))
      sink(file=file.path(paste0(basename(getwd()),"_traceback_",format(Sys.time(), "%Y_%m_%d_%H_%M_%OS3"),".txt")))
      if (Sys.getenv("PARAM_ENABLE_PROFILING")=="1") {
        saveRDS(utils::summaryRprof(lines="hide"),paste0(basename(getwd()),"_",format(Sys.time(), "%Y_%m_%d_%H_%M_%OS3"),"_profile_summary_functions.rds"))
        saveRDS(utils::summaryRprof(lines="lines"),paste0(basename(getwd()),"_",format(Sys.time(), "%Y_%m_%d_%H_%M_%OS3"),"_profile_summary_lines.rds"))
        saveRDS(utils::summaryRprof(lines="both"),paste0(basename(getwd()),"_",format(Sys.time(), "%Y_%m_%d_%H_%M_%OS3"),"_profile_summary_both.rds"))
      }
    }

    # Print traceback to file
    cat(attr(last.dump,"error.message")); # Print error message to file, along with simplified stack trace.
    cat('\nTraceback:');
    cat('\n');
    traceback(3); # Print full traceback leaving out the outermost 3 function calls.
    sink();

    cat("\nTerminating early due to error in non-interactive session\n")
    cat(paste0(paste(rep("=", 20),  collapse = ""), "\n"))
    quit(status = 2, save = "no")
  } else {

    utils::dump.frames()
    profile_summary<-utils::summaryRprof()
    cat(paste0(paste(rep("=", 20),  collapse = ""), "\n"))
  }
})



# On error, save dump file,
# and if batch session, exit
# immediately
###### nolint end: undesirable_function(options)

# If for some reason we don't already have a Spark connection, get one

###### nolint start: undesirable_function(Sys.setenv)
if (this_is_databricks && Sys.getenv("DATABRICKS_GUID") == "" && !exists("dbutils.secrets.help")) {
  # No spark backend exists and we aren't in a notebook

  Sys.setenv("SPARK_HOME" = "/databricks/spark")
  assign("DATABRICKS_GUID", system('wget -qO - \'http://localhost:6061/?type="com.databricks.backend.common.rpc.DriverMessages$GetRStudioBackendGUID"\' --post-data=\'{"@class":"com.databricks.backend.common.rpc.DriverMessages$GetRStudioBackendGUID"}\' --no-check-certificate | tr -d \\" ', intern = TRUE), envir = .GlobalEnv)
  Sys.setenv("EXISTING_SPARKR_BACKEND_PORT" = system(paste0('wget -qO - \'http://localhost:6061/?type="com.databricks.backend.common.rpc.DriverMessages$StartRStudioSparkRBackend"\' --post-data=\'{"@class":"com.databricks.backend.common.rpc.DriverMessages$StartRStudioSparkRBackend", "guid": "', DATABRICKS_GUID, '"}\' --no-check-certificate'), intern = TRUE))
  Sys.setenv("SPARKR_BACKEND_AUTH_SECRET" = system(paste0('wget -qO - \'http://localhost:6061/?type="com.databricks.backend.common.rpc.DriverMessages$GetRStudioRAuthSecret"\' --post-data=\'{"@class":"com.databricks.backend.common.rpc.DriverMessages$GetRStudioRAuthSecret", "port": "', Sys.getenv("EXISTING_SPARKR_BACKEND_PORT"), '"}\' --no-check-certificate | tr -d \\" '), intern = TRUE))
  assign(".Last", function() {
    system(paste0('wget -qO - \'http://localhost:6061/?type="com.databricks.backend.common.rpc.DriverMessages$StopRStudioSparkRBackend"\' --post-data=\'{"@class":"com.databricks.backend.common.rpc.DriverMessages$StopRStudioSparkRBackend", "port": "', Sys.getenv("EXISTING_SPARKR_BACKEND_PORT") , '"}\' --no-check-certificate'), intern = TRUE)
  }, envir=globalenv())

}
###### nolint end: undesirable_function(Sys.setenv)

if (
  !is.null( shared_temp_path ) &&
  !is.na( shared_temp_path ) &&
  nchar( shared_temp_path ) > 0 &&
  dir.exists( shared_temp_path )
  ) {
  ###### nolint start: undesirable_function(options)
  options( shared_temp_path = as.character( shared_temp_path ) )
  ###### nolint end: undesirable_function(options)
}

# End with a comment
