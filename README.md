# mutinet

Simple functions to read in multinet data.

Summary statistics for each net are calculated after exclusion of:
   - Depths >0m 
   - Nets going upwards
   - Nets not reaching depths of min.depth.dive (default = 5m)
   - recordings of <0m after a net already surfaced
   
When reading in the data, a plot can be shown with colored dots indicating the selected data for each net.

Not packaged.