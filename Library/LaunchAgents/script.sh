#!/usr/local/bin/bash
git checkout ajax-data
/Users/camdennarzt/.rbenv/shims/bundle exec padrino rake update_once
git checkout gh-pages;
/usr/local/bin/psql -c "select json_object_agg(day,data) from records;" -d arewesmallyet -o tmp.json -t;
cat tmp.json | tr -d ' ' > data.json;
rm tmp.json;
git add data.json
git commit -m 'daily json update'
git push
