#!/usr/local/bin/bash
git checkout ajax-data
/Users/camdennarzt/.rbenv/shims/bundle exec padrino rake update_once
DATABASE_URL=postgres://camdennarzt@localhost:5432/arewesmallyet RACK_ENV=production /Users/camdennarzt/.rbenv/shims/bundle exec rake pipeline:compile
mv app/public/stylesheets/application-*.css tmp/application.css
mv app/public/javascripts/application-*.js tmp/application_min.js
rm -rf app/public

git checkout gh-pages
/usr/local/bin/psql -c "select json_object_agg(day,data order by day) from records;" -d arewesmallyet -o tmp.json -t
cat tmp.json | tr -d ' ' > data.json
rm tmp.json
mv tmp/application_min.js application_min.js
mv tmp/application.css application.css

git add data.json application.css application_min.js

git commit -m 'daily json/version update'
git push
