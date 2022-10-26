#!/usr/bin/env Rscript

#Terms_MakeR.R


##################
#
# USAGE
#
##################

#This script takes the model property file from a CBIIT data model and creates a set of term lists, term dictionaries and a list of terms to add to specific properties.

#Run the following command in a terminal where R is installed for help.

#Rscript --vanilla Terms_MakeR.R --help

##################
#
# Env. Setup
#
##################

#List of needed packages
list_of_packages=c("dplyr","stringr","yaml","stringi","readxl","curl","jsonlite","janitor","optparse","tools")

#Based on the packages that are present, install ones that are required.
new.packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
suppressMessages(if(length(new.packages)) install.packages(new.packages))

#Load libraries.
suppressMessages(library(dplyr,verbose = F))
suppressMessages(library(yaml,verbose = F))
suppressMessages(library(stringi,verbose = F))
suppressMessages(library(curl,verbose = F))
suppressMessages(library(jsonlite,verbose = F))
suppressMessages(library(janitor,verbose = F))
suppressMessages(library(optparse,verbose = F))
suppressMessages(library(tools,verbose = F))
suppressMessages(library(stringr,verbose = F))

#remove objects that are no longer used.
rm(list_of_packages)
rm(new.packages)


##################
#
# Arg parse
#
##################

#Option list for arg parse
option_list = list(
  make_option(c("-p", "--property"), type="character", default=NULL, 
              help="Model property file yaml", metavar="character")
)

#create list of options and values for file input
opt_parser = OptionParser(option_list=option_list, description = "\nTerms_MakeR.R version 1.0")
opt = parse_args(opt_parser)

#If no options are presented, return --help, stop and print the following message.
if (is.null(opt$property)){
  print_help(opt_parser)
  cat("Please supply the model property file.\n\n")
  suppressMessages(stop(call.=FALSE))
}

#Data property pathway
property_path=file_path_as_absolute(opt$property)

#A start message for the user that the validation is underway.
cat("The term files are being created at this time.\nIf warnings appear, these are expected, as they are in reference to multiple connections being open and closed in parallel with 'garbage collection' in between.\n\n")


###############
#
# Read in files
#
###############

prop_yaml=read_yaml(file = property_path)


###########
#
# File name rework
#
###########

#Rework the file path to obtain a file name, this will be used for the output file.
file_name=stri_reverse(stri_split_fixed(stri_reverse(basename(file_path)),pattern = ".", n=2)[[1]][2])

path=paste(dirname(file_path),"/",sep = "")

#Output file.
output_file=paste(file_name,
                  "_terms",
                  stri_replace_all_fixed(
                    str = Sys.Date(),
                    pattern = "-",
                    replacement = ""),
                  sep="")


#################
#
# Data Manipulation
#
#################

prop_yaml_new=prop_yaml

df_prop_code=data.frame(matrix(ncol = 3,nrow=0))
df_prop_code_add=data.frame(matrix(ncol = 3,nrow=1))

colnames(df_prop_code)<-c("Property","Code","Source")
colnames(df_prop_code_add)<-c("Property","Code","Source")

#Create list of properties and their CDE codes
for (x in 1:length(names(prop_yaml$PropDefinitions))){
  if(any(names(prop_yaml$PropDefinitions[[x]])%in%"Term")){
    if (any(grepl(pattern = "caDSR", x = prop_yaml$PropDefinitions[[x]]["Term"][[1]]))){
      num_codes=grep(pattern = "caDSR", x = prop_yaml$PropDefinitions[[x]]["Term"][[1]])
      for (y in num_codes){
        df_prop_code_add$Property=names(prop_yaml$PropDefinitions[x])
        df_prop_code_add$Code=prop_yaml$PropDefinitions[[x]]["Term"][[1]][[y]]["Code"][[1]]
        df_prop_code_add$Source="caDSR"
        df_prop_code=rbind(df_prop_code, df_prop_code_add)
      }
    }
    if(any(grepl(pattern = "NCIt", x = prop_yaml$PropDefinitions[[x]]["Term"][[1]]))){
      num_codes=grep(pattern = "NCIt", x = prop_yaml$PropDefinitions[[x]]["Term"][[1]])
      for (y in num_codes){
        df_prop_code_add$Property=names(prop_yaml$PropDefinitions[x])
        df_prop_code_add$Code=prop_yaml$PropDefinitions[[x]]["Term"][[1]][[y]]["Code"][[1]]
        df_prop_code_add$Source="NCIt"
        df_prop_code=rbind(df_prop_code, df_prop_code_add)
      }
    }
  }
}

#Create empty list for all property and enumerated values.
terms=list()

values_list=list()

#Pull enumerated values based on CDE values via the API
for (x in 1:dim(df_prop_code)[1]){
  values_vec=c()
  if (df_prop_code$Source[x]=="caDSR"){
    #Use curl to real API with readLines functions
    contents=suppressWarnings(readLines(curl(url = paste("https://cadsrapi.cancer.gov/invoke/caDSR/GetJSON?query=PermissibleValue,ValueDomainPermissibleValue,ValueDomain&DataElement[@publicId=", df_prop_code$Code[x],"]",sep = "")), warn = F))
    
    #Close readLines function after saving output to variable, this will avoid warnings later.
    on.exit(close(contents))
    #insert sleep to prevent spamming the API
    Sys.sleep(0.5)
    
    #Grep of contents that finds the location of the line right before the value.
    congrep_value=grep(pattern = "\"@name\" : \"value\",",contents)
    congrep_link=grep(pattern = 'ValueMeaning&PermissibleValue', contents)
    constr_link=contents[congrep_link]
    constr_link_count=0
    #If there are enumerated values in the contents, it will go through each of the primary, then check for any alt CDEs, make a unique list and remove any values that are redundant and are all UPPERCASE.
    if (length(congrep_value)!=0){
      for (grepnum in congrep_value){
        y=grepnum+1
        constr_link_count=constr_link_count+1
        prop_name=stri_replace_all_fixed(str = stri_replace_all_fixed(str = contents[y],pattern = "          \"*body\" : \"",replacement = ""),pattern = "\"",replacement = "")
        
        values_vec=c(values_vec,prop_name)
        
        #Create URL to then grab all information about the property
        prop_url= stri_replace_all_fixed(str = constr_link[constr_link_count],pattern = '          \"@xlink:href\" : ',replacement = "")
        prop_url=str_sub(string = prop_url, 2,-2)
        prop_contents=suppressWarnings(readLines(curl(url = prop_url), warn = F))
        on.exit(close(prop_contents))
        #insert sleep to prevent spamming the API
        Sys.sleep(0.5)
        
        prop_grep_id=grep(pattern =  '"@name\" : \"publicID\",', x = prop_contents)
        prop_grep_id=prop_grep_id+1
        prop_public_id=prop_contents[prop_grep_id]
        prop_public_id = stri_replace_all_fixed(str = prop_public_id,pattern = '          \"*body\" : ',replacement = "")
        prop_public_id=as.integer(str_sub(string = prop_public_id, 2,-2))
        
        prop_grep_ver=grep(pattern =  '\"@name\" : \"version\"', x = prop_contents)
        prop_grep_ver=prop_grep_ver+1
        prop_ver=prop_contents[prop_grep_ver]
        prop_ver = stri_replace_all_fixed(str = prop_ver,pattern = '          \"*body\" : ',replacement = "")
        prop_ver=as.integer(str_sub(string = prop_ver, 2,-2))
        
        prop_grep_def=grep(pattern =  '\"@name\" : \"preferredDefinition\"', x = prop_contents)
        prop_grep_def=prop_grep_def+1
        prop_def=prop_contents[prop_grep_def]
        prop_def = stri_replace_all_fixed(str = prop_def,pattern = '          \"*body\" : ',replacement = "")
        prop_def=str_sub(string = prop_def, 2,-2)
        
        prop_grep_value=grep(pattern =  '\"@name\" : \"shortMeaning\"', x = prop_contents)
        prop_grep_value=prop_grep_value+1
        prop_value=prop_contents[prop_grep_value]
        prop_value = stri_replace_all_fixed(str = prop_value,pattern = '          \"*body\" : ',replacement = "")
        prop_value=str_sub(string = prop_value, 2,-2)
        
        origin="caDSR"
        
        #See how many versions there are, and take the highest number, the most recent version
        ver=length(prop_ver)
        
        tmp_list=list(list(Origin=origin,Definition=prop_def[ver],Code=prop_public_id[ver],Version=prop_ver[ver],Value=prop_value[ver]))
        names(tmp_list)<-prop_name[ver]
        
        terms$Terms=append(terms$Terms,tmp_list)
      }
    }
    #Add the property term information
    #Use curl to real API with readLines functions
    prop_contents=suppressWarnings(readLines(curl(url = paste("https://cadsrapi.cancer.gov/invoke/caDSR/GetJSON?query=DataElement[@publicId=", df_prop_code$Code[x],"]",sep = "")), warn = F))
    
    #Close readLines function after saving output to variable, this will avoid warnings later.
    on.exit(close(prop_contents))
    #insert sleep to prevent spamming the API
    Sys.sleep(0.5)

    prop_grep_id=grep(pattern =  '"@name\" : \"publicID\",', x = prop_contents)
    prop_grep_id=prop_grep_id+1
    prop_public_id=prop_contents[prop_grep_id]
    prop_public_id = stri_replace_all_fixed(str = prop_public_id,pattern = '          \"*body\" : ',replacement = "")
    prop_public_id=as.integer(str_sub(string = prop_public_id, 2,-2))
    
    prop_grep_ver=grep(pattern =  '\"@name\" : \"version\"', x = prop_contents)
    prop_grep_ver=prop_grep_ver+1
    prop_ver=prop_contents[prop_grep_ver]
    prop_ver = stri_replace_all_fixed(str = prop_ver,pattern = '          \"*body\" : ',replacement = "")
    prop_ver=as.integer(str_sub(string = prop_ver, 2,-2))
    
    prop_grep_def=grep(pattern =  '\"@name\" : \"preferredDefinition\"', x = prop_contents)
    prop_grep_def=prop_grep_def+1
    prop_def=prop_contents[prop_grep_def]
    prop_def = stri_replace_all_fixed(str = prop_def,pattern = '          \"*body\" : ',replacement = "")
    prop_def=str_sub(string = prop_def, 2,-2)
    
    prop_grep_value=grep(pattern =  '\"@name\" : \"longName\"', x = prop_contents)
    prop_grep_value=prop_grep_value+1
    prop_value=prop_contents[prop_grep_value]
    prop_value = stri_replace_all_fixed(str = prop_value,pattern = '          \"*body\" : ',replacement = "")
    prop_value=str_sub(string = prop_value, 2,-2)
    
    origin="caDSR"
    
    #See how many versions there are, and take the highest number, the most recent version
    ver=length(prop_ver)
    
    tmp_list=list(list(Origin=origin,Definition=prop_def[ver],Code=prop_public_id[ver],Version=prop_ver[ver],Value=prop_value[ver]))
    names(tmp_list)<-df_prop_code$Property[x]
    
    terms$Terms=append(terms$Terms,tmp_list)
  }
  if (!is.null(values_vec)){
    values_list[df_prop_code$Property[x]][[1]]=c(values_list[df_prop_code$Property[x]][[1]],values_vec)
  }
}

#Clean up enumerated values for the properties.
for (property in names(values_list)){
  tmp_vec=values_list[property][[1]]
  #Make the list of values unique
  tmp_vec=unique(tmp_vec)
  tmp_vec=sort(tmp_vec)
  
  #Create vector of positions to remove if they meet certain requirements
  remove_pos=c()
  
  #Test for unique ALL CAP strings
  for (tmp_pos in 1:length(tmp_vec)){
    if (toupper(tmp_vec[tmp_pos])==tmp_vec[tmp_pos]){
      if (tolower(tmp_vec[tmp_pos])%in%tolower(tmp_vec[-tmp_pos])){
        remove_pos=c(remove_pos,tmp_pos)
      }
    }
  }
  
  #Remove entry at index location if it is part of the remove position vector.
  if (length(remove_pos)>0){
    tmp_vec=tmp_vec[-remove_pos]
  }
  
  #Create vector of positions to remove if they meet
  remove_pos=c()
  
  #Test for unique when Upper Cases are not consistent
  for (tmp_pos in 1:length(tmp_vec)){
    if (tolower(tmp_vec[tmp_pos])%in%tolower(tmp_vec[-tmp_pos])){
      possible_remove_pos=grep(pattern = tolower(tmp_vec[tmp_pos]),x = tolower(tmp_vec))
      for (subremove in possible_remove_pos){
        if (tmp_vec[tmp_pos]==tmp_vec[subremove]){
        }else{
          if (stri_count(tmp_vec[tmp_pos],regex = "[A-Z]")>stri_count(tmp_vec[subremove],regex = "[A-Z]")){
            remove_pos=c(remove_pos,subremove)
          }
        }
      }
    }
  }
  
  #Remove entry at index location if it is part of the remove position vector.
  if (length(remove_pos)>0){
    tmp_vec=tmp_vec[-remove_pos]
  }
  
  #As long as the vector is not empty, make it the new values in the list for the property.
  if (!is.null(tmp_vec)){
    values_list[property][[1]]=tmp_vec
  } 
}


#If there are new properties to be added to the already existing properties, this will add those terms in the new yaml.
enum_to_add=list()
prop_yaml_new=prop_yaml

for (prop in names(values_list)){
  values_list[prop][[1]]
  prop_yaml_new[[1]][prop][[1]]["Enum"][[1]]
  if (!all(values_list[prop][[1]]%in%prop_yaml_new[[1]][prop][[1]]["Enum"][[1]])){
    newly_added_term=values_list[prop][[1]][!values_list[prop][[1]]%in%prop_yaml_new[[1]][prop][[1]]["Enum"][[1]]]
    
    enum_to_add[prop][[1]]=c(newly_added_term)
    
    prop_yaml_new[[1]][prop][[1]]["Enum"][[1]]=unique(c(prop_yaml_new[[1]][prop][[1]]["Enum"][[1]],values_list[prop][[1]]))
    
  }
}


##################
#
# Write Out
#
##################

#Enums and full terminology
terms2=list()
terms2$Terms=terms$Terms[!duplicated(names(terms$Terms))]
terms_ordered2=list()
terms_ordered2$Terms=terms2$Terms[order(names(terms2$Terms))]

df_all_terms_JSON=toJSON(terms_ordered2,pretty = F,auto_unbox = T)
df_value_list_JSON=toJSON(values_list,pretty = F,auto_unbox = T)
#Write out the JSON/YAML pairs for the properties and list of enum values, plus the terminology with properties file.
#Also a write out for the enums that need to be added to each property based on the diff between what is there and the API pull.
write_yaml(x = enum_to_add, file = paste(path,output_file,"_to_add.yaml",sep = ""))

write(x = df_value_list_JSON, file = paste(path,output_file,"_list.json",sep = ""))

write_yaml(x = values_list, file = paste(path,output_file,"_list.yaml",sep = ""))

write(x = df_all_terms_JSON, file = paste(path,output_file,".json",sep = ""))

write_yaml(x = terms_ordered2, file = paste(path,output_file,".yaml",sep = ""))

cat(paste("\n\nProcess Complete.\n\nThe output files can be found here: ",path,"\n\n",sep = ""))
