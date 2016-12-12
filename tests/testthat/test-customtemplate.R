context('Custom Template')

tidy_up <- function () {
        objs <- setdiff(ls(envir = .TargetEnv), "tidy_up")
        rm(list = objs, envir = .TargetEnv)
}

template1_dir <- system.file('example_data/example_templates/template1', package = 'ProjectTemplate')
template2_dir <- system.file('example_data/example_templates/template2', package = 'ProjectTemplate')

template1_dir <- gsub("^[^/]*(.*)$", "local::\\1", template1_dir)
template2_dir <- gsub("^[^/]*(.*)$", "local::\\1", template2_dir)

test_that('adding new templates works correctly ', {

  expect_message(templates("clear"), "Templates not configured")
  expect_message(templates(), "Custom Templates not configured")  
  
  # add the first one in - should take the name of the directory
  expect_message(templates("add", location = template1_dir), "template1")
  
  # add the second one in - should take the given name 
  expect_message(templates("add", "Template_2", location = template2_dir), "Template_2")
  
  on.exit(templates("clear"), add=TRUE)
  
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
  
  oldwd <- setwd(test_project2)
  on.exit(setwd(oldwd), add = TRUE)
  
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
  
  oldwd <- setwd(test_project3)
  on.exit(setwd(oldwd), add = TRUE)
  
  # template 2 has a custom config variable called template which is set to 2
  # and a file called readme.md
  # expect no warning - the tiny config file should have been replaced with the missing values
  expect_warning(load.project(), NA)
  expect_true(file.exists("readme.md"))
  expect_equal(config$template, 2)
  expect_equal(config$data_loading, FALSE)
  
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

test_that('creating projects with no template works correctly', {
        
        templates("clear")
        
        
        # add the first one in - should take the name of the directory
        templates("add", location = template1_dir)
        
        # add the second one in - should take the given name 
        templates("add", "Template_2", location = template2_dir)
        
        on.exit(templates("clear"), add=TRUE)
        
        # check that templates are loaded
        expect_message(templates(), "template1")
        expect_message(templates(), "Template_2")
        
        # Create a project based on template 1
        this_dir <- getwd()
        
        test_project <- tempfile('test_project')
        suppressMessages(create.project(test_project,  minimal = FALSE))
        on.exit(unlink(test_project, recursive = TRUE), add = TRUE)
        
        oldwd <- setwd(test_project)
        on.exit(setwd(oldwd), add = TRUE)
        
        # Should be no warnings
        expect_warning(load.project(), NA)
        
        # no template variable should be present
        expect_true(is.null(config$template))
        
        tidy_up()
})

test_that('setting and clearing default template works correctly', {
        
        templates("clear")
        
        
        # add the first one in - should take the name of the directory
        templates("add", location = template1_dir)
        
        # add the second one in - should take the given name 
        templates("add", "Template_2", location = template2_dir)
        
        on.exit(templates("clear"), add=TRUE)
        
        # set  default template - should be tagged with a star
        expect_message(templates("setdefault", 1), "1.(*)")
        
        # Create a project based on default
        this_dir <- getwd()
        
        test_project <- tempfile('test_project')
        suppressMessages(create.project(test_project,  minimal = FALSE))
        on.exit(unlink(test_project, recursive = TRUE), add = TRUE)
        
        oldwd <- setwd(test_project)
        on.exit(setwd(oldwd), add = TRUE)
        
        # template 1 has a custom config variable called template which is set to 1
        # and a file called readme.md
        suppressMessages(load.project())
        expect_true(file.exists("readme.md"))
        expect_equal(config$template, 1)
        
        # clear  defaults template - should be no star
        expect_message(templates("nodefault"), "1.   ")
        
        tidy_up()
})

