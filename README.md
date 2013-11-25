B2BVAR
======

###Batch to Batch variation statistics, IMR charts, Cpk

The code in this repo has been used to analyze production data over a range of commercial part numbers 
to evaluate the stability of the manufacturing process.  Different part numbers typically have different
target values for the same metric, and thus a "deviation" statistic (target - observation) is 
calculated.  It is this deviation statistic that is charted.  Nominally the deviation "target" is equal
to zero, and the LSL/USL are then established based on a stable process (in statistical control) to
achieve a Cpk of about 1.3 minimum (higher is better).
