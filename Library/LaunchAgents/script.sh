#!/usr/local/bin/bash
git checkout ajax-data
/Users/camdennarzt/.rbenv/shims/bundle exec padrino rake update_once
DATABASE_URL=postgres://camdennarzt@localhost:5432/arewesmallyet RACK_ENV=production /Users/camdennarzt/.rbenv/shims/bundle exec rake pipeline:compile
mv app/public/stylesheets/application-*.css tmp/application.css
mv app/public/javascripts/application-*.js tmp/application_min.js
curl -O arewesmallyet.dev/data.json
curl -O arewesmallyet.dev/stats.json
mv {data,stats}.json tmp/
rm -rf app/public

git checkout gh-pages
mv tmp/application_min.js application_min.js
mv tmp/application.css application.css
cat tmp/data.json | tr -d ' ' > data.json
cat tmp/stats.json | tr -d ' ' > stats.json

git add {data,stats}.json application.css application_min.js

git commit -m 'daily json/version update'
git push
git checkout ajax-data
