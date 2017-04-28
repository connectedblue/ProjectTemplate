context('Custom Template')

tidy_up <- function () {
        objs <- setdiff(ls(envir = .TargetEnv), "tidy_up")
        rm(list = objs, envir = .TargetEnv)
        # disable templates
        Sys.setenv("PT_ROOT_TEMPLATE_DIR"="NULL")
}

root_template_dir <- system.file('example_data/example_templates', package = 'ProjectTemplate')
github_root_template_dir <- system.file('example_data/example_templates/github_config', package = 'ProjectTemplate')

template1_dir <- system.file('inst/example_data/example_templates/template1', package = 'ProjectTemplate')
template2_dir <- system.file('inst/example_data/example_templates/template2', package = 'ProjectTemplate')

template1_dir <- gsub("^[^/]*(.*)$", "local::\\1", template1_dir)
template2_dir <- gsub("^[^/]*(.*)$", "local::\\1", template2_dir)

template_dcf <- system.file('example_data/example_templates/ProjectTemplateRootConfig.dcf', package = 'ProjectTemplate')

test_that('adding new templates works correctly ', {
  
  # Set environment variable to the root template location
  Sys.setenv("PT_ROOT_TEMPLATE_DIR"=root_template_dir)
  on.exit(Sys.unsetenv("PT_ROOT_TEMPLATE_DIR"))
  
  # Create a project based on template 1
  this_dir <- getwd()
  
  test_project <- tempfile('test_project')
  suppressMessages(create.project(test_project, "template1", minimal = FALSE))
  on.exit(unlink(test_project, recursive = TRUE), add = TRUE)

  oldwd <- setwd(test_project)
  on.exit(setwd(oldwd), add = TRUE)
  
  # template 1 has a custom config variable called template which is set to 1
  # and a file called readme.md
  suppressMessages(load.project())
  expect_true(file.exists("readme.md"))
  expect_equal(config$template, 1)
  
  
  # Create a project based on template 2
  setwd(this_dir)
  
  test_project2 <- tempfile('test_project')
  suppressMessages(create.project(test_project2, "Template_2", minimal = FALSE))
  on.exit(unlink(test_project2, recursive = TRUE), add = TRUE)
  
  oldwd2 <- setwd(test_project2)
  on.exit(setwd(oldwd2), add = TRUE)
  
  # template 2 has a custom config variable called template which is set to 2
  # and a file called readme.md
  suppressMessages(load.project())
  expect_true(file.exists("readme.md"))
  expect_equal(config$template, 2)
  
  
  # Create a project based on template 2 again, but this time reference template by number
  setwd(this_dir)
  
  test_project3 <- tempfile('test_project')
  suppressMessages(create.project(test_project3, 2, minimal = FALSE))
  on.exit(unlink(test_project3, recursive = TRUE), add = TRUE)
  
  oldwd3 <- setwd(test_project3)
  on.exit(setwd(oldwd3), add = TRUE)
  
  # template 2 has a custom config variable called template which is set to 2
  # and a file called readme.md
  # expect no warning - the tiny config file should have been replaced with the missing values
  expect_warning(load.project(), NA)
  expect_true(file.exists("readme.md"))
  expect_equal(config$template, 2)
  
  # custom config from template should be preserved
  expect_equal(config$data_loading, FALSE)
  
  # cache_loaded_data was not included in the template, but it should still be present and 
  # have the same value as a newly created project which is TRUE  
  # (nb this parameter is FALSE if not included when load.project() is run and it's not present ....)
  expect_equal(config$cache_loaded_data, TRUE)
  
  # Also template 2 had a config with an old version number, data_loading set to FALSE and no other items
  # Check that the config file has been repaired but retaining the set value for data_loading
  
  # load up config from disk
  config1 <- .read.config("config/global.dcf")
  
  expect_equal(config1$version, as.character(.package.version()))
  expect_equal(config1$data_loading, FALSE)
  expect_equal(config1$munging, TRUE)
  expect_true(is.character(config1$libraries))
  
  
  tidy_up()
})


test_that('adding templates hosted on github works correctly', {
        
        
        # The following template repository has been set up to support this test.
        # Once merged into master, a new github account should be set up for this purpose and
        # the login held by the ProjectTemplate maintainers.  In the meantime access rights
        # can be granted
        
        # Set environment variable to the root template location
        Sys.setenv("PT_ROOT_TEMPLATE_DIR"=github_root_template_dir)
        on.exit(Sys.unsetenv("PT_ROOT_TEMPLATE_DIR"))
        
        # Create a project based on template 1
        this_dir <- getwd()
        
        test_project <- tempfile('test_project')
        suppressMessages(create.project(test_project, "template1", minimal = FALSE))
        on.exit(unlink(test_project, recursive = TRUE), add = TRUE)
        
        oldwd <- setwd(test_project)
        on.exit(setwd(oldwd), add = TRUE)
        
        # template 1 has a custom config variable called template which is set to 1
        # and a file called readme.md
        suppressMessages(load.project())
        expect_true(file.exists("readme.md"))
        expect_equal(config$template, 1)
        
        
        # Create a project based on template 2
        setwd(this_dir)
        
        test_project2 <- tempfile('test_project')
        suppressMessages(create.project(test_project2, 2, minimal = FALSE))
        on.exit(unlink(test_project2, recursive = TRUE), add = TRUE)
        
        oldwd2 <- setwd(test_project2)
        on.exit(setwd(oldwd2), add = TRUE)
        
        # template 2 has a custom config variable called template which is set to 2
        # and a file called readme.md
        suppressMessages(load.project())
        expect_true(file.exists("readme.md"))
        expect_equal(config$template, 2)
        
        # be sure that this is the github template
        expect_equal(config$github, "github")
        
        
        tidy_up()
})
