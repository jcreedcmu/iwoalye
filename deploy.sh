#!/bin/bash
set -e
set -x
lein clean 
lein with-profile prod cljsbuild once
rm -rf resources/public/js/out
rsync -av resources/public/ jcreed.org:o/js/iwoalye
