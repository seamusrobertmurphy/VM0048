# Running a SNAP xml model using SNAP's graph processing tool
#

# gpt=function(graph_folder, gpt_path){
  # 
# }


# Define path to the executable of SNAP's graph processing tool
gpt = "C:/PROGRA~1/snap/bin/gpt.exe"


# Get a list of all xml modelling files in the respective folder (i.e. snap_models).
#graphXml_file = "D:/sentinel/KILI-SentinelWorkflow/workflows/snap_models/400_arvi.xml"
graphXml_files = list.files("D:/sentinel/KILI-SentinelWorkflow/workflows/snap_models",
                            pattern=glob2rx("*.xml"),
                            full.names = TRUE)

for(xml_file in graphXml_files){
  print(xml_file)
}


# Define input dataset path
# Define the folder where all input files are located.
# A loop will run over each input dataset within this folder and process it.
input_path = "D:/sentinel/data/l1c"


# Define output dataset path
output_path = "D:/sentinel/data/products"

# Start processing
# Get list of all input datasets
input_files = list.files(input_path, 
                         pattern=glob2rx("S2A_MSIL2A*.SAFE"),
                         full.names = TRUE)

# Loop over all input files and process them.
for(infile in input_files){
  print(infile)
}

# For testing: infile = input_files[[1]]
for(infile in input_files){
  print(paste0("Processing file ", infile))
   
  # Loop over all xml files, create the output filename and run the command.
  # For testing: xml_file = graphXml_files[[2]]
  for(xml_file in graphXml_files){
    print(xml_file)
    # Set output filepath
    # Get product name from xml filename and use it as prefix for the output file
    prefix = substring(basename(xml_file), 1, nchar(basename(xml_file))-4)  
    # outname = basename(infile) #remove .SAFE
    # outname = basename(file_path_sans_ext(infile))
    # outname = nchar(basename(infile), ,-5)
    outname = substring(basename(infile), 1, nchar(basename(infile))-5)
    outfile = paste0(output_path, "/", prefix, "_", outname, ".dim")
    
    # Compile command for graph processing tool
    cmd = paste0(gpt, " ", xml_file, " -e -t ", outfile, " -Pinfile=", infile)
    system(cmd)
  }
}

