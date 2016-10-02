# This file has the main infrastructure for the custom template subsystem.  There 
# are no functions exported to users from here - instead the user facing functions make
# calls here

# Custom templates can be configured for each different installation of ProjectTemplate
# There can be multiple templates, and one of them is a default.  They are invoked by
#     create.project("project-name", "template-name")
# or
#     create.project("project-name") to invoke the default template
#
# First of all, the standard ProjectTemplate structure is created.  Then the structure
# defined for each template name is copied into the standard structure, overwriting 
# anything already there, and adding any new files.
#
# The location of the templates is a single location on the local file system or github.
# This is called the root template location.  Each sub directory under the root 
# location is the template-name used in the call to create.project().
#
# A user function configure.templates() provides a management interface to manage the
# installed templates on a system.
#
# Template designers can simply create directory structures for any functionality they 
# want to, e.g. knitr templates, shinyapps, corporate analysis standards etc.
# There is an advanced template design feature, whereby the template is described in
# a template definition file.  This allows for re-use of functionalility between templates
# (e.g. a lib function that you'd like to make available in many templates).  It also
# allows more fine grained control than just plain copy-and-overwrite.  For example, it
# can be specifed for a .gitignore segment on a template to be appended to an existing
# .gitignore in the target project template directory.
#
# Template definitions are stored in a DCF format file, and loaded into a dataframe for 
# processing.  There can be multiple DCF records defined in a single file.  Each record
# has the following fields:
#      location:        where template content can be found
#      type:            whether the template is stored local or on github
#      merge:           overwrite/append/duplicate determines how content is handled
#                       when an item with similar name exists in the target project
#
# Root template definition file uses this format with some restrictions:  
#       location:  If the first record has this field set to NULL, this means that
#                  templates are not configured for this installation.  Otherwise, it
#                  points to the location where template names are defined
#                  are defined, as sub directories.
#       merge:     not used
# The file is stored in the ProjectTemplate package under the inst/defaults directory
# and is named RootConfig.dcf.  A backup is kept in the etc folder of the R installation.
# This is used to restore configuration if a new version of ProjectTemplate has been 
# installed and the RootConfig.dcf got written over.
#
# General template definition files reside directly under the template subdirectory
# with the name template-definition.dcf.  If this file exists, it is used to build the
# template structure.  If not, any files or folders are copied directly (with over-write)
# to the target project directory.  Note that General template definition files can have
# multiple records - each separated by a newline in the standard dcf manner.

#
# Custom template functions start here .....
#

# First, Some short cut definitions to aid readability

# Where is the root template location defined
.root.template.dir <- file.path(.libPaths(), "ProjectTemplate", "defaults", "customtemplate")
.root.template.file <- file.path(.root.template.dir, "RootConfig.dcf")

# A backup is needed otherwise it will be overwritten when ProjectTemplate is updated 
.root.templatebackup.dir <- file.path(R.home(), "etc")
.root.templatebackup.file <- file.path(.root.templatebackup.dir, "ProjectTemplateRootConfig.dcf")

# allow templates to be defined in the local filesystem, or on github
.available.location.types <- c("local", "github")

# Helper function to remove the first item in a list
.remove.first <- function (x) rev(head(rev(x), -1))

# Set a new location for the root template for the current ProjectTemplate installation
.set.root.location <- function (location, type) {
        if (!(type %in% .available.types)) {
                message(paste0("Invalid type: ", type))
                return(invisible(NULL))
        }
        if (is.null(location)) {
                location <- "NULL"
        }
        else if (!.is.dir(location)) {
                message(paste0("Invalid template location: ", location))
                return(invisible(NULL))
        }
        
        location <- data.frame(location=location, type=type)
        write.dcf(location, .root.template.file)
        
        # Create a backup of the root location
        if(!.is.dir(.root.templatebackup.dir)) dir.create(.root.templatebackup.dir)
        write.dcf(location, .root.templatebackup.file)
}

# Get the currently configured root template location
.get.root.location <- function () {
        location <- .read.template.definition(.root.template.file)
        location <- location[1,]
        if(location$location == "NULL") return (NULL)
        location
}

# Read a template definition file and return the contents as a dataframe
.read.template.definition <- function (template.file) {
        definition <- as.data.frame(read.dcf(template.file), 
                                    stringsAsFactors = FALSE)
        invalid_types <- setdiff(definition$type, .available.location.types)
        if(length(invalid_types)>0) {
                stop(paste0("Invalid template types in ", template.file, ": ", invalid_types))
        }
        definition
}

# Extract info about the root template file 
.extract.roottemplate.info <- function () {
        template.root <- .get.root.location()
        if (is.null(template.root)) return(NULL)
        
        # read raw template information
        if (template.root$type == "github") {
                templates <- .download.github(template.root$location)
        }
        else if (template.root$type == "local") {
                templates <- template.root$location
                sub.dirs <- list.dirs(templates)
                template.names <- basename(.remove.first(sub.dirs))
                template.info <- data.frame(
                        clean.names = sub("(.*)_default$", "\\1", 
                                          template.names),
                        default = grepl("_default$", template.names),
                        path = file.path(templates, template.names)
                )        
        }
        else {
                template.info <- NULL
        }
        if (nrow(template.info)>0){
                # sort into order - default first
                template.info <- template.info[with(template.info, 
                                                    order(-default, clean.names)),]
                # and make sure there is only one default
                template.info$default <- c(TRUE, rep(FALSE, nrow(template.info)-1))
        }
        template.info
}

# Provide the status of templates defined under the template root
.root.template.status <- function () {
        template.info <- .get.template.names()
        if (is.null(template.info)) {
                message <- paste0(c("Custom Templates not configured for this installation.",
                                    "Run configure.template() to set up where ProjectTemplate should look for your custom templates"),
                                  collapse = "\n")
#root <- "no_root"
        }
        else if (nrow(template.info)==0) {
                return(
                        message(paste0(c(paste0("No templates are located at ", .get.root.location()$location),
                                         "Add sub directories there to start using custom templates"),
                                       collapse = "\n"))
                )
        }
        else {
                templates <- ifelse(template.info$default, 
                                    paste0("(*) ", template.info$clean.names),
                                    paste0("    ", template.info$clean.names))
                message(paste0(c("The following templates are available:", 
                                 templates,
                                 "If no template specified in create.project(), the default (*) will be used"),
                               collapse = "\n"))
        }
        
}

.download.github <- function (location) {
        stop(".download.github not implemented")
}