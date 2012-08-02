
# Overview

Back-end RRD writer for time-series power usage and environmental data
collected from distributed wireless probes.

The writer sources JSON-formatted data from a ZeroMQ SUB socket connected
to an instance of a
[hap-probe-collector](https://github.com/djoyner/hap-probe-collector).
As data messages arrive they are decoded and probe sources are identified
via hardware address.  Individual data points are written to RRD files
using direct librrd FFI calls.

Probe addresses and mappings to RRD data sources (DS) and
round-robin archives (RRA) are fully specified in an external config
file.

# Author

David Joyner, <david@joynerhome.net>

