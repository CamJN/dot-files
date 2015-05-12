#!/usr/local/bin/bash
git checkout ajax-data
/Users/camdennarzt/.rbenv/shims/bundle exec padrino rake update_once
DATABASE_URL=postgres://camdennarzt@localhost:5432/arewesmallyet RACK_ENV=production /Users/camdennarzt/.rbenv/shims/bundle exec rake pipeline:compile
mv app/public/stylesheets/application-*.css tmp/application.css
mv app/public/javascripts/application-*.js tmp/application_min.js
curl -O arewesmallyet.dev/data.json
curl -O arewesmallyet.dev/stats.json
\curl arewesmallyet.dev > tmp/index.html
mv {data,stats}.json tmp/
rm -rf app/public
if git diff-index --quiet HEAD ; then
  echo 'no change'
else
  git add app/assets/javascripts/releases.js
  git commit -m 'update releases'
  git push
fi

git checkout gh-pages
mv tmp/application_min.js application.js
mv tmp/application.css application.css
cat tmp/data.json | tr -d ' ' > data.json
cat tmp/stats.json | tr -d ' ' > stats.json
sed -e 's|/public/assets/[^/]*/||g' tmp/index.html > index.html
git add {data,stats}.json application.css application_min.js index.html

git commit -m 'daily json/version update'
git push
git checkout ajax-data
