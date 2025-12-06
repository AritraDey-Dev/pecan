#!/bin/bash
set -e

# Source utility functions
source /tmp/add.util.sh

# Add SIPNET model
# We use the binary path that is known to exist in the sipnet container or host
# For this local test, we are registering it in the postgres DB.
# The actual execution happens in the sipnet container where the binary should be.
# However, the executor needs to know about it.
# In the CI, it uses /usr/local/bin/sipnet.git
# 1: hostname, 2: model_name, 3: model_type, 4: revision, 5: binary_name, 6: binary_path
echo "Calling addModelFile with: docker SIPNET SIPNET git sipnet.git /usr/local/bin"
addModelFile "docker" "SIPNET" "SIPNET" "git" "sipnet.git" "/usr/local/bin"
