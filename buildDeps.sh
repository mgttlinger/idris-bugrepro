#! /bin/sh
cd dependencies/containers/
sh ./fetch-deps.sh
idris --install containers.ipkg
cd ../config/
sh ./fetch-deps.sh
idris --install config.ipkg
cd ../../
