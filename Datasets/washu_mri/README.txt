https://openneuro.org/datasets/ds000243/versions/00001

These 120 MRI datasets are being released to the public along as part of the materials for “Temporal interpolation alters motion in fMRI scans: magnitudes and consequences for artifact detection” by Power et al. in PLOS ONE.

Included for each subject is a T1-weighted anatomical image (MP-RAGE) and one or more T2*-weighted scans (resting state BOLD scans)

All subjects
	- were “typical” young adults that reported no significant neurological or psychiatric history
	- were right-handed and reported that English was their first language
	- were scanned at Washington University in Saint Louis on a Siemens MAGNETOM Tim Trio 3T scanner with a Siemens 12-channel head coil
	- were scanned using interleaved ascending product sequences for T2* data
	- were scanned in the eyes-open resting state fixating a white crosshair on a black background

The data have been described in multiple publications from the Petersen/Schlaggar group,
	- beginning with Power et al., 2013 “Evidence for hubs in human brain networks” in Neuron
	- and most comprehensively in Power et al., 2014 “Methods to detect, characterize, and remove motion artifact in resting state fMRI” in Neuroimage
	- as well as several other publications
	- see these publications for further details on acquisitions and demographics


Becky Coalson of the Petersen/Schlaggar group collated these scans and de-identified them for public release
	- the accompanying file “WU120_subject_information.txt” contains for each subject
		- the release subject number (1-120)
			- the same number used in the present publication
		- the subject number used in Power et al., 2014
			- see the WU120_Supplememtal_Cohort_Illustration.pdf from Power et al., 2014
		- the subject number to publicly reference the subject
			- for question/communication purposes with Becky or the Petersen/Schlaggar group
		- the owner/contributor of the data
		- age of subject at scanning
		- sex
		- handedness
		- English only speaker
		- paradigm of the resting state scans
		- total scan time in resting state
		- number of rest runs
		- number of volumes per run
		- TR of the runs
		- scanner used (Siemens Tim Trio was in bay3)
		- slice order (AFNI convention); sequences were ascending interleaved

If there are questions about the scans, please contact Becky Coalson (becky@npg.wustl.edu) or contact the Petersen/Schlaggar group via their website.

JDP 12/23/15


### Comments added by Openfmri Curators ###
===========================================

General Comments
----------------


Defacing
--------
Pydeface was used on all anatomical images to ensure deindentification of subjects. The code
can be found at https://github.com/poldracklab/pydeface

Quality Control
---------------
Mriqc was run on the dataset. Results are located in derivatives/mriqc. Learn more about it here: https://mriqc.readthedocs.io/en/latest/

Where to discuss the dataset
----------------------------
1) www.openfmri.org/dataset/ds000243/ See the comments section at the bottom of the dataset
page.
2) www.neurostars.org Please tag any discussion topics with the tags openfmri and ds000243.
3) Send an email to submissions@openfmri.org. Please include the accession number in
your email.

Known Issues
------------
- As per one of the published paper explaining the dataset, its is noted that 2 subjects out of all have different TR for functional runs. However, when checked with submitter, they confirmed that all subjects data shared here have same TR. 
