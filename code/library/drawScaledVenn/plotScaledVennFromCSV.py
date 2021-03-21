from os.path import *
from os import getcwd, makedirs
from optparse import *
from pandas import *
from numpy import *
import matplotlib
matplotlib.use('Agg')
from matplotlib_venn import venn2, venn2_circles, venn3, venn3_circles
import matplotlib.pyplot as plt


def scaledVenn(input_filename, sets_list, sets_names, title_for_venn, venn_output_file):
    sets_list_length = len(sets_list)
    venn_diag = ""
    plt.figure(figsize=(8,8))
    if sets_list_length == 2:
        venn_diag = venn2(subsets = sets_list, set_labels = (sets_names), set_colors=('lightgray','dimgray'))
	c=venn2_circles(sets_list, linewidth=3, color="black")
        lblA = venn_diag.get_label_by_id("A")
        xA, yA = lblA.get_position()
        lblA.set_position((xA-0.25, yA+0.1))
        lblB = venn_diag.get_label_by_id("B")
        xB, yB = lblB.get_position()
        lblB.set_position((xB+0.25, yB+0.1))
    elif sets_list_length == 3:
        venn_diag = venn3(subsets = sets_list, set_labels = (sets_names), set_colors=('blueviolet','darkturquoise','tomato'))
	c=venn3_circles(sets_list, linewidth=3, color="black")
	lblA = venn_diag.get_label_by_id('A')
	xA, yA = lblA.get_position()
	lblA.set_position((xA-0.1, yA-0.1))
	lblB = venn_diag.get_label_by_id("B")
	xB, yB = lblB.get_position()
	lblB.set_position((xB-0.25, yB+0.05))
	lblC = venn_diag.get_label_by_id("C")
	xC, yC = lblC.get_position()
	lblC.set_position((xC-0.25, yC))
    for text in venn_diag.set_labels:
	text.set_fontsize(18)
	text.set_family('arial')
	text.set_weight('bold')
    for text in venn_diag.subset_labels:
	if text is not None:	
		text.set_fontsize(18)
		text.set_family('arial')
		text.set_weight('bold')

    plt.setp(plt.gca().spines.values(), linewidth=3)
    plt.gca().set_axis_on()
    plt.title(title_for_venn, fontsize=30, fontname='arial', weight='bold')

    plt.savefig(venn_output_file)
    plt.close()
    return("done")

# Read the tsv file as a data frame
# TSV file shoud have set names as headers and data as column values
# For example: 3 columns for 3 way venn diagram
def processCSVFile(input_filename, delim_operator):
    venn_data_df = read_csv(input_filename, sep=delim_operator, engine = 'python')
    return venn_data_df

def processAndPlotVenn(input_filename, delim_operator, title_for_venn, venn_output_file):
    venn_data_df = processCSVFile(input_filename, delim_operator)
    sets_names = list(venn_data_df)
    sets_list = [set(venn_data_df[str(one_col_name)].tolist()) for one_col_name in sets_names]
    scaled_venn_pdf = scaledVenn(input_filename, sets_list, sets_names, title_for_venn, venn_output_file)

    return(venn_data_df)


#
# Read command line options
#
def readAndParseCommandlineArgs():
    usage = "usage: %prog [options]	 (Use -h or --help to see all options)"
    cl=OptionParser(usage=usage)
    cl.add_option('--infile', '-i', action='store',
								help="input \"\" csv file (REQUIRED)",
								dest="infile")
    cl.add_option('--sep', '-n', action='store',
								help="Separation method for input file (Options: , or \t or \s)",
								dest="delimsep", default=",")
    cl.add_option('--title', '-t' , action='store',
								help="Main title for the Venn diagram",
								dest="title_venn")
    cl.add_option('--output', '-o' , action='store',
								help="Venn output filename",
								dest="venn_output")
    (options, args) = cl.parse_args()

### Need to add defaults for flexibility
	#
	# Check the command line options
    if options.infile:
        if isfile(options.infile):
            input_filename = options.infile
            delim_operator = str(options.delimsep)
            title_for_venn = str(options.title_venn)
            venn_output_file = str(options.venn_output)
            return(input_filename, delim_operator, title_for_venn, venn_output_file)
	else:
		cl.error("Please specify an input - TSV or CSV file to run the search. Use -h for more information.\n")


def main():
    input_filename, delim_operator, title_for_venn, venn_output_file = readAndParseCommandlineArgs()

    process_results = processAndPlotVenn(input_filename, delim_operator, title_for_venn, venn_output_file)
    print "Done!"


if __name__ == "__main__":
	 # stuff only to run when not called via 'import' here
	 main()
