#!/bin/bash
defaults delete com.apple.calculateframework currencyCache;
defaults delete com.apple.calculateframework currencyCacheRefreshDate;
osascript -e 'quit app "Spotlight"';
osascript -e 'activate app "Spotlight"';
