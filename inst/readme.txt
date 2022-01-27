Each data directory corresponds to one data package ID starting from 1. Directories are named *packageID*_*datasetnickname* for easy organization. Within each data directory, files are roughly organized like so:

1. One top-level folder for files received from researchers. We've called this one "FromPI" or "frompi" so far.
	- there will be back and forth communications and clarifications and corrections, so files received are organized into sub-directories named after the date they were received, e.g. "2022-01-13".
	- No manual edits on data files. Reproducible scripts that read data from this directory only. I have edited metadata files we received in the past, but I try to not overwrite the original files received from researchers.

2. One top-level folder for data files that are cleaned and formatted and basically ready to go. We've called this "Clean" so far.
	- However, any ready to go state is a fluid one, since even the "clean" data basically goes out of date the moment there is an update or revision, or if for some reason its hash is mismatched with what is stated in the metadata. So, I do not treat this directory as an archive or a reference, only as a designated place to park your finished data files when you go to upload them to EDI. I will overwrite these files when I make new revisions, and when making appending new data I download the archival version from EDI.

3. One folder for scripts, R project files (since I use R), and the EML files that results from those.
	- This one used to be a jumble. Starting 2021/2022 I decided to group each year's scripts and EML files into sub-directories that are named after the newest DATA YEAR. E.g. script to append 2021 data into previous data plus EML files from that will live in "2021", even if I worked on them in 2022.

It's clear that this organization system was not the same in the past, and I've changed it when issues or needs I did not anticipate arose. In these cases I've tried my best to make it consistent for datasets that I had a chance to update or touch at the time, and leave it alone for ones that are one-off or do not yet need updates. Even as I wrote this I had some ideas to improve the system again. So, my apologies for any inconsistencies you may notice.

2022-01-27
