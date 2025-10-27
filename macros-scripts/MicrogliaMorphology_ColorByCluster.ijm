// Color-by-cluster morphology script
// Last modified October 27, 2025

// ============== FUNCTIONS ==============
// These should be at the top of your script.

// Function to find an ROI by its exact name
function findRoiWithName(roiName) { 
    nR = roiManager("Count"); 
    for (i=0; i<nR; i++) { 
        roiManager("Select", i); 
        rName = Roi.getName(); 
        if (matches(rName, roiName)) { 
            return i; // Found it! Return the index.
        } 
    }
    return -1; // Return -1 if no match is found
}

// Function to find a matching file in a directory
// Function to find a matching file in a directory (with corrected matching logic)
function listFiles(dir, fileList, searchString, openFunction) {
    searchLength = lengthOf(searchString);
    for (i = 0; i < fileList.length; i++) {
        fileName = fileList[i];
        
        // Check if the filename starts with the base search string
        if (startsWith(fileName, searchString)) {
            
            // This check prevents "image_1" from matching "image_10".
            if (lengthOf(fileName) > searchLength) {
                nextChar = substring(fileName, searchLength, searchLength + 1);
                
                // --- THIS LINE IS THE FIX ---
                // We use the built-in 'matches' function to check if the character is a digit from 0-9.
                if (matches(nextChar, "[0-9]")) {
                    continue; // This is a false match (like the '0' in '..._10'), so skip it.
                }
            }
            
            // If the check passes, we have a true match.
            if (openFunction) { open(dir + fileName); }
            print("Found matching file: " + dir + fileName);
            return dir + fileName;
        }
    }
    return ""; // Return an empty string if no file is found
}


// ============== MACRO STARTS HERE ==============

// Welcome messages (optional, can be removed)
Dialog.create("MicrogliaMorphology");
Dialog.addMessage("Welcome to the updated ColorByCluster feature!");
Dialog.addCheckbox("Do you want to use batch mode for a set of images?", true);
Dialog.show();
batchmodechoice = Dialog.getCheckbox();

// Define cluster colors for the entire macro
colorArray = newArray("BBCC33", "44BB99", "EEDD88", "EE8866", "red", "green", "blue", "yellow", "orange", "cyan");

// Main logic for batch vs. single image mode
if (batchmodechoice) {
    // --- BATCH PROCESSING MODE ---
    setOption("JFileChooser",true);
    
    original_dir = getDirectory("Choose folder with original .tiff images");
    original_list = getFileList(original_dir);
    roi_dir = getDirectory("Choose folder with ROI sets (.zip files)");
    roi_list = getFileList(roi_dir);
    clusters_dir = getDirectory("Choose folder with ColorByCluster .csv files");
    clusters_list = getFileList(clusters_dir);
    output_dir = getDirectory("Choose directory to save your final ColorByCluster images");

    run("ROI Manager...");

    // ... inside if (batchmodechoice) ...

    run("ROI Manager...");

    for (i=0; i < original_list.length; i++) {
        
        // Reset the ROI Manager for each new image.
        roiManager("reset");
        
        original_image_name = original_list[i];
        searchString = replace(original_image_name, ".tif", ""); 
        print("Processing: " + searchString);

        // Open original image and find corresponding files
        open(original_dir + original_image_name);

        roi_zip_path = listFiles(roi_dir, roi_list, searchString, false); 
        if (roi_zip_path != "") {
            roiManager("Open", roi_zip_path);
        } else {
            print("!!! WARNING: No matching ROI set found for " + searchString);
            close("*");
            continue; 
        }
        
	cluster_csv_path = listFiles(clusters_dir, clusters_list, searchString, false);  // Don't auto-open
	if (cluster_csv_path == "") {
   	 print("!!! WARNING: No matching CSV file found for " + searchString);
    	close("*");
    	continue; 
	}

	// Close ONLY the CSV table from the previous iteration (not images or ROI Manager)
	list = getList("window.titles");
	for (j = 0; j < list.length; j++) {
	    if (endsWith(list[j], ".csv")) {
	        selectWindow(list[j]);
	        run("Close");
	    }
	}

// NOW open the CSV for this image
open(cluster_csv_path);

// Add a small delay to ensure the CSV is fully loaded
wait(100);

        // Color the ROIs based on the CSV data (this part is unchanged)
        roiManager("Show All without labels");
		roiManager("Set Color", "black");
		
		// Get the number of rows from the CSV table (not ROI Manager)
		num_rows = Table.size;
		print("CSV has " + num_rows + " cells, ROI Manager has " + roiManager("Count") + " ROIs");
		
		for(n=0; n < num_rows; n++) {
		    cluster_num = parseInt(Table.getString("Cluster", n)); 
		    id_from_csv = Table.getString("ID", n);
		    roi_idx = findRoiWithName(id_from_csv);
		    
		    // THIS IS THE PROBLEM CASE - cell in CSV but not in ROI Manager
		    if (roi_idx < 0) {
		        print("!!! ERROR: ROI '" + id_from_csv + "' from CSV not found in ROI Manager for image " + searchString);
		        // This shouldn't happen - means something is wrong with your data
		        continue;
		    }
		    
		    // Color the ROI if cluster number is valid
		    if (cluster_num >= 1 && cluster_num <= colorArray.length) {
		        roiManager("Select", roi_idx);
		        Roi.setFillColor(colorArray[cluster_num - 1]);
		    } else {
		        print("Warning: Invalid cluster number " + cluster_num + " for ROI " + id_from_csv);
		    }
		}
		
		// After coloring, report how many ROIs were NOT colored (filtered out in R)
		colored_count = num_rows;
		total_rois = roiManager("Count");
		uncolored_count = total_rois - colored_count;
		print("Colored " + colored_count + " ROIs, " + uncolored_count + " ROIs left uncolored (filtered out in R)");
		        
        // --- THIS IS THE CORRECTED AND SIMPLIFIED SECTION ---
        // Flatten the image. This creates a new RGB image with the colored ROIs.
        run("Flatten");
        
        // Save the newly created flattened image.
        saveAs("Tiff", output_dir + searchString + "_ColorByCluster.tif");
        
        // Reliably close ALL open windows (original, flattened, and CSV table)
        // before starting the next loop.
        close("*");
        // --------------------------------------------------------
    }

    // This part runs after the loop is completely finished
    selectWindow("ROI Manager");
    run("Close");
    print("Batch processing complete!");

} else {
    // --- SINGLE IMAGE MODE ---
    setOption("JFileChooser",true);
    
    original_image_path = File.openDialog("Select the original .tiff image");
    roi_zip_path = File.openDialog("Select the corresponding ROI set (.zip file)");
    cluster_csv_path = File.openDialog("Select the corresponding cluster .csv file");

    if (original_image_path=="" || roi_zip_path=="" || cluster_csv_path=="") {
        print("User cancelled the operation. Macro stopped.");
        return; 
    }
    
    open(original_image_path);
    run("ROI Manager...");
    roiManager("Open", roi_zip_path);
    open(cluster_csv_path);
    
    roiManager("Show All without labels");
    roiManager("Set Color", "black");

    num_rows = Table.size;
    for(n=0; n < num_rows; n++) {
        cluster_num = parseInt(Table.getString("Cluster", n)); 
        id_from_csv = Table.getString("ID", n);
        roi_idx = findRoiWithName(id_from_csv);
        
        if (roi_idx >= 0 && cluster_num >= 1 && cluster_num <= colorArray.length) {
            roiManager("Select", roi_idx);
            Roi.setFillColor(colorArray[cluster_num - 1]);
        } else if (roi_idx < 0) {
            print("Warning: ROI with name '" + id_from_csv + "' not found in ROI Manager.");
        }
    }

    run("Flatten");
    
	output_dir = getDirectory("Choose a folder to save the final image");
	
	if (output_dir != "") {
	    original_name = File.getName(original_image_path);
	    output_name = replace(original_name, ".tif", "_ColorByCluster.tif");
	    save_path = output_dir + output_name;
	    
	    saveAs("Tiff", save_path);
	    print("Saved final image to: " + save_path);
	} else {
	    print("Save cancelled by user. The colored image will remain open.");
}
}
