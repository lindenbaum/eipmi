#!/bin/bash

# get edown from github
mkdir -p tmp
git clone https://github.com/hdiedrich/markedoc.git tmp

# create overview.edoc
mkdir -p doc
perl -pi -e "s/\`\`\`/\'\'\'/g" README.md
perl -pi -e "s/\'\'\'erlang/\`\`\`/g" README.md
echo "@title EIPMI - A native Erlang IMPI library" > doc/overview.edoc
echo "@copyright 2012 Lindenbaum GmbH" >> doc/overview.edoc
echo "" >> doc/overview.edoc
sed -r -f tmp/bin/markedoc.sed README.md >> doc/overview.edoc
git checkout README.md

# create edoc and cleanup
rebar clean compile doc
rm -rf tmp

# deploy to gh-pages
if [ "$1" == "deploy" ]; then
    echo deploy
fi
