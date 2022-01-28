/**
 * $File: main.js $
 * $Date: 2022-01-22 23:53:52 $
 * $Revision: $
 * $Creator: Jen-Chieh Shen $
 * $Notice: See LICENSE.txt for modification and distribution information
 *                   Copyright © 2022 by Shen, Jen-Chieh $
 */

"use strict";

const base_url = "https://github.com/jcs-emacs/jcs-elpa/blob/master/";
const base_url_raw = "https://raw.githubusercontent.com/jcs-emacs/jcs-elpa/";
const archive_url = base_url_raw + "master/docs/archive.json";

var archive = null;

$(document).ready(function(){
  var packageList = $('#package-list');
  var filter = $('#filter');
  var state = $('#state');

  $.getJSON(archive_url, function(data){
    archive = data;
    disply('');
  }).fail(function(){
    console.log("An error has occurred.");
  });

  filter.on('input', function() {
    disply(filter.val());
  });

  function filtering(query, desc) {
    if (query === '' ||
        desc.name.includes(query) ||
        desc.summary.includes(query))
      return true;
    return false;
  }

  function disply(query) {
    packageList.empty();
    state.text('Current List of ' + archive.length + ' Packages');

    for (let index = 0; index < archive.length; ++index) {
      let desc = archive[index];
      if (!filtering(query, desc))
        continue;
      packageList.append(
        '<tr>' +
          '<td><a href="' + desc.url + '">' + desc.name + '</a></td>' +
          '<td><a href="' + desc.url + '">' + desc.summary + '</a></td>' +
          '<td><a href="' + desc.url + '">' + desc.version + ' ⮛</a></td>' +
          '<td><a href="' + base_url + 'recipes/' + desc.name + '">🍴</a></td>' +
          '<td><a href="' + desc.url + '">' + desc.source + '</a></td>' +
          '<td><img src="' + base_url_raw + 'master/badges/v/' + desc.name + '.svg"/></td>' +
          '</tr>');
    }
  }
});
