# This file has the main infrastructure for the plugin subsystem.  There 
# are no functions exported to users from here. 
#
# These functions are called when ProjectTemplate is loaded into Global Env
# from the .onLoad() function.  Also a user function called enable.plugins() 
# which initialises the plugin functionality

# Plugins can be written by anyone to extend or enhance the functionality of 
# ProjectTemplate without impacting the core code.  Users can choose which 
# plugins they want to configure for their own use.
#
# The Plugin architecture is simple:
#
#    - A list of available plugins is stored in the users home directory in a file
# .ProjectTemplatePlugins.dcf
#
#    - This file is read when Projecttemplate is loaded and plugins are loaded in the
# order specified in the file
#
# The file .ProjectTemplatePlugins.dcf cannot be created by ProjectTemplate (it is 
# forbidden by CRAN for a package to write to the users home directory).  However, a
# plugin has been developed to help manage this file.
#
# Plugin definitions are stored in a DCF format file, and loaded into a dataframe for 
# processing.  There can be multiple DCF records defined in a single file.  Each record
# can the following fields (depending on template_type):
#
#
#      plugin_name:            Name of package containing the plugin
#                                       root - template root file for a site installation
#                                       project - individual template definition 
#
#      plugin_location:        where the plugin is installed from in the format
#                                       github:username/repo@branch
#                              @branch is optional - defaults to master if not present
#
# In future, locations other than github may be supported


#
# plugin functions start here .....
#

# First, Some short cut definitions to aid readability

# Where is the plugin file located 
.plugin.file <- file.path(Sys.getenv("R_USER"), "ProjectTemplatePlugins.dcf")


# Internal functions called by onload


# Internal functions to manipulate the root template file

# Read a template definition file, validate it and return the contents as a dataframe
# If no file parameter specified, the function reads the .root.template.file, otherwise
# another dcf file can be specified which is validated and then saved in place of the
# current .root.template.file
# Note that the definition object is passed to validate and save routines
.read.root.template <- function (template.file=.root.template.file) {
        
        .require.root.template()
        
        # read the file from disk and perform basic validation
        definition <- .read.template.definition(template.file)
        
        # if no root template defined return NULL
        if(!.templates.defined(definition))  
                return (NULL)
        
        # validate the root template
        definition <- .validate.root.template(definition)
        
        # Save any validation fixes back
        .save.root.template(definition)
        
        definition
}

# Check the root template definition has the right format
.validate.root.template <- function(definition) {
        
        # return if no templates
        if(!.templates.defined(definition))  
                return (NULL)
        
        # Make sure only root items are included in the definition
        definition <- definition[definition$template_type == "root",]
        
        # Mandatory fields for root templates should be present
        missing_names <- setdiff(.root.template.field.names, names(definition))
        if (length(missing_names) != 0) {
                stop(paste0("Missing template field: ", missing_names, "\n"))
        }
        
        # make the default column a logical value
        definition$default <- as.logical(definition$default)
        
        # Make sure there are no duplicate template_name
        duplicates <- definition$template_name[duplicated(definition$template_name)]
        if (length(duplicates) > 0) {
                stop(paste0("Duplicate template name found in template_name field: ", duplicates, "\n"))
        }
        
        # Create a user friendly display name for the templates, numbering each one
        # and marking the default with a (*)
        definition$display_name <- paste0(row.names(definition), ".",
                                          ifelse(definition$default, "(*) ", "    "),
                                          definition$template_name)
        
        definition
}



# Check if plugins are defined
.plugins.defined <- function() {
        if(file.exists(.plugin.file)) {
                return(TRUE)
        }
        return(FALSE)
}


# Read in the plugin configuration
.read.plugin <- function () {
        plugins <- as.data.frame(read.dcf(.plugin.file), 
                                    stringsAsFactors = FALSE)
        plugins
}



# Attempt to load the plugin, or install it if it doesn't exist

.load.plugin <- function(plugin_name, plugin_location) {
        require.package(plugin_name, 
                        FUN=.install.plugin, 
                        plugin_location=plugin_location)
}


.install.plugin <- function (plugin_location) {
        
        # location is in format type:location
        # Supported types:
        #      type:     github
        #      location: github_user/repo_name@branch
        
        # Parse the location field to extract the embedded information
        location <- strsplit(plugin_location, ":")
        
        type <- location[[1]][1]
        install_location <- location[[1]][2]
        
        # Validate the type
        valid_types <- c("github")        
        invalid_types <- setdiff(type, valid_locations)
        if(length(invalid_types)>0) {
                stop(paste0("Invalid location type: ", invalid_types))
        }

        if (type=="github") {
                devtools::install_github(install_location)
        }
        
}

